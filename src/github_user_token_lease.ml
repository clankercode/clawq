(** Callback-scoped opaque leases at the GitHub HTTP boundary (P21.M2.E4.T003).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module V = Github_user_token_vault

let default_ttl_seconds = 300.0

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type handle = string

type binding = {
  principal_id : string;
  github_user_id : int64;
  app_id : int;
  host : string;
  vault_id : string;
  generation : int;
  binding_id : string option;
}

type identity = {
  handle : handle;
  binding : binding;
  scopes : string list;
  token_expires_at : string;
  issued_at : float;
  lease_expires_at : float;
  revoked : bool;
}

type lease = {
  handle : handle;
  binding : binding;
  scopes : string list;
  token_expires_at : string;
  issued_at : float;
  lease_expires_at : float;
  mutable revoked : bool;
}

type denial =
  | Lease_not_found
  | Lease_expired
  | Lease_revoked
  | Generation_mismatch of { expected : int; actual : int }
  | Vault_not_active
  | Token_expired
  | Account_mismatch of { expected : V.account_key; found : V.account_key }
  | Vault of V.denial
  | Invalid_input of string
  | Forbidden_surface of string

(* -------------------------------------------------------------------------- *)
(* Process-local registry                                                     *)
(* -------------------------------------------------------------------------- *)

(** Process-local live leases. Short critical sections; no cross-domain lock. *)
let registry : (string, lease) Hashtbl.t = Hashtbl.create 64

let register (l : lease) = Hashtbl.replace registry l.handle l
let find_registered handle = Hashtbl.find_opt registry handle

(* -------------------------------------------------------------------------- *)
(* Handle helpers                                                             *)
(* -------------------------------------------------------------------------- *)

let handle_to_string (h : handle) = h

let handle_of_string s =
  let t = String.trim s in
  if t = "" then Error "lease handle must be non-empty" else Ok t

let generate_handle ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghlease_%d_%06d" ts rand

(* -------------------------------------------------------------------------- *)
(* Denials                                                                    *)
(* -------------------------------------------------------------------------- *)

let string_of_denial = function
  | Lease_not_found -> "lease_not_found"
  | Lease_expired -> "lease_expired"
  | Lease_revoked -> "lease_revoked"
  | Generation_mismatch { expected; actual } ->
      Printf.sprintf "generation_mismatch:expected=%d actual=%d" expected actual
  | Vault_not_active -> "vault_not_active"
  | Token_expired -> "token_expired"
  | Account_mismatch { expected; found } ->
      Printf.sprintf "account_mismatch:expected=%s/%Ld/%d@%s found=%s/%Ld/%d@%s"
        expected.principal_id expected.github_user_id expected.app_id
        expected.host found.principal_id found.github_user_id found.app_id
        found.host
  | Vault d -> "vault:" ^ V.string_of_denial d
  | Invalid_input msg -> Printf.sprintf "invalid_input:%s" msg
  | Forbidden_surface surface -> Printf.sprintf "forbidden_surface:%s" surface

let denial_exposes_token ~denial ~plaintext =
  if plaintext = "" then false
  else String_util.contains (string_of_denial denial) plaintext

(* -------------------------------------------------------------------------- *)
(* Time helpers                                                               *)
(* -------------------------------------------------------------------------- *)

(** Parse ISO-8601 UTC ("YYYY-MM-DDTHH:MM:SSZ" or with fractional seconds) to
    epoch float. Returns [None] on malformed input (fail closed at call sites).
*)
let parse_iso8601_utc_opt s =
  try
    let s = String.trim s in
    let len = String.length s in
    let s =
      if len > 0 && s.[len - 1] = 'Z' then String.sub s 0 (len - 1) else s
    in
    let date_part, time_part =
      match String.split_on_char 'T' s with
      | [ d; t ] -> (d, t)
      | _ -> failwith "no T"
    in
    let year, month, day =
      match String.split_on_char '-' date_part with
      | [ y; m; d ] -> (int_of_string y, int_of_string m, int_of_string d)
      | _ -> failwith "bad date"
    in
    let time_part =
      match String.split_on_char '.' time_part with
      | t :: _ -> t
      | [] -> time_part
    in
    let hour, minute, second =
      match String.split_on_char ':' time_part with
      | [ h; m; s ] -> (int_of_string h, int_of_string m, int_of_string s)
      | _ -> failwith "bad time"
    in
    let is_leap y = (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0 in
    let days_in_month y m =
      match m with
      | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
      | 4 | 6 | 9 | 11 -> 30
      | 2 -> if is_leap y then 29 else 28
      | _ -> 30
    in
    let year_days = ref 0 in
    for y = 1970 to year - 1 do
      year_days := !year_days + if is_leap y then 366 else 365
    done;
    let month_days = ref 0 in
    for m = 1 to month - 1 do
      month_days := !month_days + days_in_month year m
    done;
    let total_days = !year_days + !month_days + day - 1 in
    let total_seconds =
      (total_days * 86400) + (hour * 3600) + (minute * 60) + second
    in
    Some (float_of_int total_seconds)
  with _ -> None

let account_equal (a : V.account_key) (b : V.account_key) =
  String.equal a.principal_id b.principal_id
  && Int64.equal a.github_user_id b.github_user_id
  && a.app_id = b.app_id && String.equal a.host b.host

(* -------------------------------------------------------------------------- *)
(* Identity accessors                                                         *)
(* -------------------------------------------------------------------------- *)

let identity_of (l : lease) : identity =
  {
    handle = l.handle;
    binding = l.binding;
    scopes = l.scopes;
    token_expires_at = l.token_expires_at;
    issued_at = l.issued_at;
    lease_expires_at = l.lease_expires_at;
    revoked = l.revoked;
  }

let handle (l : lease) = l.handle
let binding (l : lease) = l.binding
let generation (l : lease) = l.binding.generation
let vault_id (l : lease) = l.binding.vault_id
let is_revoked (l : lease) = l.revoked

let is_expired ?(now = Unix.gettimeofday ()) (l : lease) =
  now >= l.lease_expires_at

(* -------------------------------------------------------------------------- *)
(* Issue                                                                      *)
(* -------------------------------------------------------------------------- *)

let issue_from_record ?(now = Unix.gettimeofday ())
    ?(ttl_seconds = default_ttl_seconds) ?binding_id ~(record : V.vault_record)
    () =
  if ttl_seconds <= 0. then Error (Invalid_input "ttl_seconds must be positive")
  else if record.generation < 1 then
    Error (Invalid_input "record generation must be positive")
  else if not record.active then Error Vault_not_active
  else
    let token_expires_at = String.trim record.expires_at in
    if token_expires_at = "" then
      Error (Invalid_input "token expires_at must be non-empty")
    else
      let lease_cap =
        match parse_iso8601_utc_opt token_expires_at with
        | None ->
            (* Malformed token expiry — still issue a short lease; use-time
               open will fail closed on token expiry parse. *)
            now +. ttl_seconds
        | Some tok_exp -> Float.min (now +. ttl_seconds) tok_exp
      in
      if lease_cap <= now then Error Token_expired
      else
        let handle = generate_handle ~now () in
        let binding_id =
          match binding_id with
          | Some s when String.trim s <> "" -> Some (String.trim s)
          | _ -> None
        in
        let binding =
          {
            principal_id = record.account.principal_id;
            github_user_id = record.account.github_user_id;
            app_id = record.account.app_id;
            host = record.account.host;
            vault_id = record.id;
            generation = record.generation;
            binding_id;
          }
        in
        let lease =
          {
            handle;
            binding;
            scopes = record.scopes;
            token_expires_at;
            issued_at = now;
            lease_expires_at = lease_cap;
            revoked = false;
          }
        in
        register lease;
        Ok lease

let issue ~db ?(now = Unix.gettimeofday ()) ?ttl_seconds ?binding_id ?expected
    ~vault_id () =
  let vault_id = String.trim vault_id in
  if vault_id = "" then Error (Invalid_input "vault_id must be non-empty")
  else
    match V.get_meta ~db ~id:vault_id with
    | Error e -> Error (Vault e)
    | Ok None -> Error Lease_not_found
    | Ok (Some record) -> (
        match expected with
        | Some exp when not (account_equal exp record.account) ->
            Error (Account_mismatch { expected = exp; found = record.account })
        | _ -> issue_from_record ~now ?ttl_seconds ?binding_id ~record ())

(* -------------------------------------------------------------------------- *)
(* Use: open raw token only inside callback                                   *)
(* -------------------------------------------------------------------------- *)

let check_lease_live ?(now = Unix.gettimeofday ()) (l : lease) =
  if l.revoked then Error Lease_revoked
  else if now >= l.lease_expires_at then Error Lease_expired
  else Ok ()

let check_token_not_expired ~now ~expires_at =
  match parse_iso8601_utc_opt expires_at with
  | None -> Error (Invalid_input "token expires_at is not valid ISO-8601 UTC")
  | Some exp when now >= exp -> Error Token_expired
  | Some _ -> Ok ()

let with_token ~db ~keys ?(now = Unix.gettimeofday ()) ~lease:l ~f () =
  match check_lease_live ~now l with
  | Error e -> Error e
  | Ok () -> (
      let expected_account : V.account_key =
        {
          principal_id = l.binding.principal_id;
          github_user_id = l.binding.github_user_id;
          app_id = l.binding.app_id;
          host = l.binding.host;
        }
      in
      match
        V.read ~db ~keys ~expected:expected_account ~id:l.binding.vault_id ()
      with
      | Error V.Not_found -> Error Lease_not_found
      | Error (V.Account_mismatch { expected; found }) ->
          Error (Account_mismatch { expected; found })
      | Error e -> Error (Vault e)
      | Ok opened -> (
          if not opened.record.active then Error Vault_not_active
          else
            let actual_gen = opened.record.generation in
            if actual_gen <> l.binding.generation then
              Error
                (Generation_mismatch
                   { expected = l.binding.generation; actual = actual_gen })
            else
              match
                check_token_not_expired ~now
                  ~expires_at:opened.record.expires_at
              with
              | Error e -> Error e
              | Ok () ->
                  let access = String.trim opened.tokens.access_token in
                  if access = "" then
                    Error
                      (Vault (V.Invalid_input "access_token empty after open"))
                  else
                    (* Token exists only for the duration of f. Refresh is never
                       passed out. *)
                    Ok (f ~access_token:access)))

let with_authorization_header ~db ~keys ?(now = Unix.gettimeofday ())
    ?(header_name = "Authorization") ~lease ~f () =
  with_token ~db ~keys ~now ~lease
    ~f:(fun ~access_token ->
      let headers =
        [
          (header_name, "Bearer " ^ access_token);
          ("Accept", "application/vnd.github+json");
          ("X-GitHub-Api-Version", "2022-11-28");
        ]
      in
      f ~headers)
    ()

(* -------------------------------------------------------------------------- *)
(* Revoke / discard                                                           *)
(* -------------------------------------------------------------------------- *)

let revoke (l : lease) =
  l.revoked <- true;
  Hashtbl.replace registry l.handle l

let revoke_handle ~handle =
  match find_registered handle with
  | None -> false
  | Some l ->
      revoke l;
      true

let discard_for_vault ~vault_id =
  let vault_id = String.trim vault_id in
  let count = ref 0 in
  Hashtbl.iter
    (fun _ (l : lease) ->
      if (not l.revoked) && String.equal l.binding.vault_id vault_id then (
        l.revoked <- true;
        incr count))
    registry;
  !count

let invalidate_generation ~vault_id ~generation =
  let vault_id = String.trim vault_id in
  let count = ref 0 in
  Hashtbl.iter
    (fun _ (l : lease) ->
      if
        (not l.revoked)
        && String.equal l.binding.vault_id vault_id
        && l.binding.generation <= generation
      then (
        l.revoked <- true;
        incr count))
    registry;
  !count

let discard_all () =
  let count = ref 0 in
  Hashtbl.iter
    (fun _ (l : lease) ->
      if not l.revoked then (
        l.revoked <- true;
        incr count))
    registry;
  Hashtbl.clear registry;
  !count

let live_count () =
  let n = ref 0 in
  Hashtbl.iter (fun _ (l : lease) -> if not l.revoked then incr n) registry;
  !n

(* -------------------------------------------------------------------------- *)
(* Export / introspection (always redacted)                                   *)
(* -------------------------------------------------------------------------- *)

let binding_to_json (b : binding) : Yojson.Safe.t =
  `Assoc
    [
      ("principal_id", `String b.principal_id);
      ("github_user_id", `Intlit (Int64.to_string b.github_user_id));
      ("app_id", `Int b.app_id);
      ("host", `String b.host);
      ("vault_id", `String b.vault_id);
      ("generation", `Int b.generation);
      ( "binding_id",
        match b.binding_id with None -> `Null | Some id -> `String id );
    ]

let identity_to_json (id : identity) : Yojson.Safe.t =
  `Assoc
    [
      ("handle", `String (handle_to_string id.handle));
      ("binding", binding_to_json id.binding);
      ("scopes", `List (List.map (fun s -> `String s) id.scopes));
      ("token_expires_at", `String id.token_expires_at);
      ("issued_at", `Float id.issued_at);
      ("lease_expires_at", `Float id.lease_expires_at);
      ("revoked", `Bool id.revoked);
    ]

let to_json (l : lease) = identity_to_json (identity_of l)

let identity_of_json (json : Yojson.Safe.t) : (identity, string) result =
  match json with
  | `Assoc fields -> (
      let find k = List.assoc_opt k fields in
      let string_field k =
        match find k with
        | Some (`String s) -> Ok s
        | _ -> Error (Printf.sprintf "missing string field %s" k)
      in
      let float_field k =
        match find k with
        | Some (`Float f) -> Ok f
        | Some (`Int i) -> Ok (float_of_int i)
        | Some (`Intlit s) -> (
            try Ok (float_of_string s)
            with _ -> Error (Printf.sprintf "bad float field %s" k))
        | _ -> Error (Printf.sprintf "missing float field %s" k)
      in
      let bool_field k =
        match find k with
        | Some (`Bool b) -> Ok b
        | _ -> Error (Printf.sprintf "missing bool field %s" k)
      in
      match
        ( string_field "handle",
          find "binding",
          find "scopes",
          string_field "token_expires_at",
          float_field "issued_at",
          float_field "lease_expires_at",
          bool_field "revoked" )
      with
      | ( Ok handle_s,
          Some (`Assoc bfields),
          Some (`List scope_items),
          Ok token_expires_at,
          Ok issued_at,
          Ok lease_expires_at,
          Ok revoked ) -> (
          let bfind k = List.assoc_opt k bfields in
          let bstring k =
            match bfind k with
            | Some (`String s) -> Ok s
            | _ -> Error (Printf.sprintf "binding missing string %s" k)
          in
          let bint k =
            match bfind k with
            | Some (`Int i) -> Ok i
            | Some (`Intlit s) -> (
                try Ok (int_of_string s)
                with _ -> Error (Printf.sprintf "binding bad int %s" k))
            | _ -> Error (Printf.sprintf "binding missing int %s" k)
          in
          let bint64 k =
            match bfind k with
            | Some (`Int i) -> Ok (Int64.of_int i)
            | Some (`Intlit s) -> (
                try Ok (Int64.of_string s)
                with _ -> Error (Printf.sprintf "binding bad int64 %s" k))
            | _ -> Error (Printf.sprintf "binding missing int64 %s" k)
          in
          let scopes_res =
            let rec go acc = function
              | [] -> Ok (List.rev acc)
              | `String s :: rest -> go (s :: acc) rest
              | _ -> Error "scopes must be strings"
            in
            go [] scope_items
          in
          let binding_id =
            match bfind "binding_id" with
            | Some (`String s) when String.trim s <> "" -> Some s
            | _ -> None
          in
          match
            ( handle_of_string handle_s,
              bstring "principal_id",
              bint64 "github_user_id",
              bint "app_id",
              bstring "host",
              bstring "vault_id",
              bint "generation",
              scopes_res )
          with
          | ( Ok handle,
              Ok principal_id,
              Ok github_user_id,
              Ok app_id,
              Ok host,
              Ok vault_id,
              Ok generation,
              Ok scopes ) ->
              Ok
                {
                  handle;
                  binding =
                    {
                      principal_id;
                      github_user_id;
                      app_id;
                      host;
                      vault_id;
                      generation;
                      binding_id;
                    };
                  scopes;
                  token_expires_at;
                  issued_at;
                  lease_expires_at;
                  revoked;
                }
          | Error e, _, _, _, _, _, _, _
          | _, Error e, _, _, _, _, _, _
          | _, _, Error e, _, _, _, _, _
          | _, _, _, Error e, _, _, _, _
          | _, _, _, _, Error e, _, _, _
          | _, _, _, _, _, Error e, _, _
          | _, _, _, _, _, _, Error e, _
          | _, _, _, _, _, _, _, Error e ->
              Error e)
      | _ -> Error "lease identity JSON missing required fields")
  | _ -> Error "lease identity JSON must be an object"

let string_of_identity (id : identity) =
  Printf.sprintf
    "ghlease handle=%s principal=%s user=%Ld app=%d host=%s vault=%s gen=%d \
     revoked=%b"
    (handle_to_string id.handle)
    id.binding.principal_id id.binding.github_user_id id.binding.app_id
    id.binding.host id.binding.vault_id id.binding.generation id.revoked

let rec json_contains_plaintext ~json ~plaintext =
  if plaintext = "" then false
  else
    match json with
    | `String s -> String_util.contains s plaintext
    | `List xs ->
        List.exists (fun j -> json_contains_plaintext ~json:j ~plaintext) xs
    | `Assoc fields ->
        List.exists
          (fun (k, v) ->
            String_util.contains k plaintext
            || json_contains_plaintext ~json:v ~plaintext)
          fields
    | `Intlit s -> String_util.contains s plaintext
    | _ -> false

let identity_contains_plaintext ~(identity : identity) ~plaintext =
  if plaintext = "" then false
  else
    let haystacks =
      [
        handle_to_string identity.handle;
        identity.binding.principal_id;
        Int64.to_string identity.binding.github_user_id;
        string_of_int identity.binding.app_id;
        identity.binding.host;
        identity.binding.vault_id;
        string_of_int identity.binding.generation;
        identity.token_expires_at;
        Option.value identity.binding.binding_id ~default:"";
      ]
      @ identity.scopes
    in
    List.exists (fun s -> String_util.contains s plaintext) haystacks

(* -------------------------------------------------------------------------- *)
(* Explicit refuse surfaces (non-HTTP)                                        *)
(* -------------------------------------------------------------------------- *)

type non_http_surface =
  | Runner_env
  | Process_env
  | Shell
  | Git_transport
  | Worktree
  | Prompt
  | Tool_data
  | Job_payload
  | Crash_output
  | Scheduled_ambient

let string_of_non_http_surface = function
  | Runner_env -> "runner_env"
  | Process_env -> "process_env"
  | Shell -> "shell"
  | Git_transport -> "git_transport"
  | Worktree -> "worktree"
  | Prompt -> "prompt"
  | Tool_data -> "tool_data"
  | Job_payload -> "job_payload"
  | Crash_output -> "crash_output"
  | Scheduled_ambient -> "scheduled_ambient"

let all_non_http_surfaces =
  [
    Runner_env;
    Process_env;
    Shell;
    Git_transport;
    Worktree;
    Prompt;
    Tool_data;
    Job_payload;
    Crash_output;
    Scheduled_ambient;
  ]

let refuse_message = function
  | Runner_env ->
      "github_user_token must not be injected into runner environment"
  | Process_env ->
      "github_user_token must not be injected into process environment"
  | Shell -> "github_user_token must not be injected into shell commands"
  | Git_transport -> "github_user_token must not be injected into Git transport"
  | Worktree ->
      "github_user_token must not be written into worktrees or worktree git \
       config"
  | Prompt -> "github_user_token must not enter prompts or Session history"
  | Tool_data ->
      "github_user_token must not enter tool arguments or tool results"
  | Job_payload ->
      "github_user_token must not enter durable job / outbox payloads"
  | Crash_output ->
      "github_user_token must not appear in crash output or operator dumps"
  | Scheduled_ambient ->
      "github_user_token must not authorize scheduled / ambient automation \
       (App identity only)"

let refuse (_l : lease) (surface : non_http_surface) =
  Error (Forbidden_surface (refuse_message surface))

let refuse_runner_env l = refuse l Runner_env
let refuse_process_env l = refuse l Process_env
let refuse_shell_injection l = refuse l Shell
let refuse_git_transport l = refuse l Git_transport
let refuse_worktree l = refuse l Worktree
let refuse_prompt l = refuse l Prompt
let refuse_tool_data l = refuse l Tool_data
let refuse_job_payload l = refuse l Job_payload
let refuse_crash_output l = refuse l Crash_output
let refuse_scheduled_ambient l = refuse l Scheduled_ambient

let assert_non_http_refused (l : lease) =
  let rec go = function
    | [] -> Ok ()
    | surface :: rest -> (
        match refuse l surface with
        | Error (Forbidden_surface _) -> go rest
        | Error d ->
            Error
              (Printf.sprintf
                 "non_http_surface %s returned unexpected denial: %s"
                 (string_of_non_http_surface surface)
                 (string_of_denial d))
        | Ok () ->
            Error
              (Printf.sprintf
                 "non_http_surface %s incorrectly permitted user-token lease"
                 (string_of_non_http_surface surface)))
  in
  go all_non_http_surfaces

(* -------------------------------------------------------------------------- *)
(* Shape-based scanning                                                       *)
(* -------------------------------------------------------------------------- *)

let token_shape_res =
  lazy
    [
      (* GitHub classic / App user / refresh / server-to-server shapes. *)
      Str.regexp "\\(ghp\\|gho\\|ghu\\|ghs\\|ghr\\)_[A-Za-z0-9_]+";
      Str.regexp "github_pat_[A-Za-z0-9_]+";
      Str.regexp "[Bb]earer [A-Za-z0-9._+/=-]\\{8,\\}";
    ]

let text_contains_token_shape s =
  if s = "" then false
  else
    List.exists
      (fun re ->
        try
          ignore (Str.search_forward re s 0);
          true
        with Not_found -> false)
      (Lazy.force token_shape_res)

let materials_contain_token_shape xs = List.exists text_contains_token_shape xs

let env_entries_contain_token_shape entries =
  materials_contain_token_shape entries

let argv_contains_token_shape argv =
  materials_contain_token_shape (Array.to_list argv)

let refuse_scanned_material ~surface ~material =
  if text_contains_token_shape material then
    Error
      (Forbidden_surface
         (Printf.sprintf
            "token shape detected on %s; github_user_token must not leave the \
             HTTP boundary"
            (string_of_non_http_surface surface)))
  else Ok ()

let assert_materials_token_free ~materials =
  let rec go = function
    | [] -> Ok ()
    | (surface, material) :: rest -> (
        match refuse_scanned_material ~surface ~material with
        | Ok () -> go rest
        | Error _ as e -> e)
  in
  go materials
