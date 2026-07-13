(** Refresh expiring GitHub App user tokens from server-returned lifetimes
    (P21.M3.E1.T001). See github_user_token_refresh.mli and
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module V = Github_user_token_vault
module L = Github_user_token_lease
module C = Github_user_token_cas
module B = Github_account_binding
module S = Github_user_token_store

(* -------------------------------------------------------------------------- *)
(* Skew                                                                       *)
(* -------------------------------------------------------------------------- *)

(** Documented refresh skew: refresh when the access token expires within five
    minutes (or is already expired). Keeps HTTP dispatch off the edge of
    GitHub's server-returned access lifetime without assuming 8h tokens. *)
let default_refresh_skew_seconds = 300.0

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

let needs_refresh ?(now = Unix.gettimeofday ())
    ?(skew_seconds = default_refresh_skew_seconds) ~access_expires_at () =
  if skew_seconds < 0. then true
  else
    match parse_iso8601_utc_opt access_expires_at with
    | None -> true
    | Some exp -> now +. skew_seconds >= exp

let is_at_or_before ~now expires_at =
  match parse_iso8601_utc_opt expires_at with
  | None -> true
  | Some exp -> now >= exp

let expires_at_iso ~now ~expires_in =
  Time_util.iso8601_utc ~t:(now +. float_of_int expires_in) ()

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type http_post =
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, string) result

type resolve_client =
  client_id_handle:string -> (string * string, string) result

type refresh_response = {
  access_token : string;
  refresh_token : string;
  expires_in : int;
  refresh_token_expires_in : int;
  token_type : string;
  scope : string;
}

type lifetimes = { access_expires_at : string; refresh_expires_at : string }

type outcome = {
  record : V.vault_record;
  leases_invalidated : int;
  lifetimes : lifetimes;
  lineage_id : string option;
  binding : B.binding option;
  refreshed : bool;
}

type denial =
  | Not_in_skew
  | Refresh_token_missing
  | Refresh_token_expired
  | Vault_not_active
  | Account_mismatch of { expected : V.account_key; found : V.account_key }
  | Lineage_mismatch of { expected : string; actual : string }
  | Binding of string
  | Client_resolve of string
  | Transport of string
  | Http_denial of int
  | Malformed_response of string
  | Invalid_token_type of string
  | Nonempty_scope of string
  | Vault of V.denial
  | Cas of C.denial
  | Lease of L.denial
  | Invalid_input of string
  | Storage of string

let string_of_denial = function
  | Not_in_skew -> "not_in_skew"
  | Refresh_token_missing -> "refresh_token_missing"
  | Refresh_token_expired -> "refresh_token_expired"
  | Vault_not_active -> "vault_not_active"
  | Account_mismatch { expected; found } ->
      Printf.sprintf "account_mismatch:expected=%s/%Ld/%d@%s found=%s/%Ld/%d@%s"
        expected.principal_id expected.github_user_id expected.app_id
        expected.host found.principal_id found.github_user_id found.app_id
        found.host
  | Lineage_mismatch { expected; actual } ->
      Printf.sprintf "lineage_mismatch:expected=%s actual=%s" expected actual
  | Binding msg -> Printf.sprintf "binding:%s" msg
  | Client_resolve msg -> Printf.sprintf "client_resolve:%s" msg
  | Transport msg -> Printf.sprintf "transport:%s" msg
  | Http_denial code -> Printf.sprintf "http_denial:%d" code
  | Malformed_response msg -> Printf.sprintf "malformed_response:%s" msg
  | Invalid_token_type t -> Printf.sprintf "invalid_token_type:%s" t
  | Nonempty_scope s -> Printf.sprintf "nonempty_scope:%s" s
  | Vault d -> "vault:" ^ V.string_of_denial d
  | Cas d -> "cas:" ^ C.string_of_denial d
  | Lease d -> "lease:" ^ L.string_of_denial d
  | Invalid_input msg -> Printf.sprintf "invalid_input:%s" msg
  | Storage msg -> Printf.sprintf "storage:%s" msg

let denial_exposes_token ~denial ~plaintext =
  if plaintext = "" then false
  else String_util.contains (string_of_denial denial) plaintext

let denial_exposes_secret ~denial ~secret =
  if secret = "" then false
  else String_util.contains (string_of_denial denial) secret

(* -------------------------------------------------------------------------- *)
(* Token endpoint                                                             *)
(* -------------------------------------------------------------------------- *)

let token_endpoint ?(host = "github.com") () =
  let h = String.lowercase_ascii (String.trim host) in
  let h =
    if String.length h >= 8 && String.sub h 0 8 = "https://" then
      String.sub h 8 (String.length h - 8)
    else if String.length h >= 7 && String.sub h 0 7 = "http://" then
      String.sub h 7 (String.length h - 7)
    else h
  in
  let h =
    match String.split_on_char '/' h with
    | host_part :: _ -> host_part
    | [] -> h
  in
  let h =
    match String.split_on_char ':' h with
    | host_part :: _ -> host_part
    | [] -> h
  in
  if h = "" || h = "github.com" then
    "https://github.com/login/oauth/access_token"
  else Printf.sprintf "https://%s/login/oauth/access_token" h

let token_headers =
  [
    ("Accept", "application/json");
    ("Content-Type", "application/x-www-form-urlencoded");
    ("User-Agent", "clawq-github-user-token-refresh");
  ]

let build_refresh_body ~client_id ~client_secret ~refresh_token =
  Uri.encoded_of_query
    [
      ("client_id", [ client_id ]);
      ("client_secret", [ client_secret ]);
      ("grant_type", [ "refresh_token" ]);
      ("refresh_token", [ refresh_token ]);
    ]

(* -------------------------------------------------------------------------- *)
(* Response parsing                                                           *)
(* -------------------------------------------------------------------------- *)

let json_string_opt j name =
  match Yojson.Safe.Util.member name j with
  | `String s -> Some (String.trim s)
  | _ -> None

let json_int_req j name =
  match Yojson.Safe.Util.member name j with
  | `Int i when i > 0 -> Ok i
  | `Intlit s -> (
      try
        let i = int_of_string s in
        if i > 0 then Ok i
        else Error (Printf.sprintf "token response %s must be positive" name)
      with Failure _ ->
        Error (Printf.sprintf "token response %s is not an integer" name))
  | `Float f when f > 0. && Float.is_integer f -> Ok (int_of_float f)
  | `Null -> Error (Printf.sprintf "token response missing %s" name)
  | `Int _ -> Error (Printf.sprintf "token response %s must be positive" name)
  | _ -> Error (Printf.sprintf "token response %s must be an integer" name)

let validate_token_type token_type =
  let t = String.lowercase_ascii (String.trim token_type) in
  if t = "bearer" then Ok t
  else Error (Printf.sprintf "token_type must be bearer, got %S" token_type)

let validate_empty_scope scope =
  let s = String.trim scope in
  if s = "" then Ok ""
  else
    Error
      (Printf.sprintf "scope must be empty for GitHub App user tokens, got %S" s)

let finish_refresh_response ~access_token ~refresh_token ~expires_in
    ~refresh_token_expires_in ~token_type ~scope =
  let access_token = String.trim access_token in
  let refresh_token = String.trim refresh_token in
  if access_token = "" then Error "token response missing access_token"
  else if refresh_token = "" then Error "token response missing refresh_token"
  else if expires_in <= 0 then
    Error "token response expires_in must be positive"
  else if refresh_token_expires_in <= 0 then
    Error "token response refresh_token_expires_in must be positive"
  else
    match validate_token_type token_type with
    | Error e -> Error e
    | Ok token_type -> (
        match validate_empty_scope scope with
        | Error e -> Error e
        | Ok scope ->
            Ok
              {
                access_token;
                refresh_token;
                expires_in;
                refresh_token_expires_in;
                token_type;
                scope;
              })

let parse_refresh_json body =
  try
    let j = Yojson.Safe.from_string body in
    match json_string_opt j "error" with
    | Some e ->
        let desc =
          match json_string_opt j "error_description" with
          | Some d when d <> "" -> ": " ^ d
          | _ -> ""
        in
        Error (Printf.sprintf "token endpoint returned OAuth error %s%s" e desc)
    | None -> (
        match json_string_opt j "access_token" with
        | None | Some "" -> Error "token response missing access_token"
        | Some access_token -> (
            match json_string_opt j "refresh_token" with
            | None | Some "" -> Error "token response missing refresh_token"
            | Some refresh_token -> (
                match json_int_req j "expires_in" with
                | Error e -> Error e
                | Ok expires_in -> (
                    match json_int_req j "refresh_token_expires_in" with
                    | Error e -> Error e
                    | Ok refresh_token_expires_in ->
                        let token_type =
                          match json_string_opt j "token_type" with
                          | Some t -> t
                          | None -> ""
                        in
                        let scope =
                          match json_string_opt j "scope" with
                          | Some s -> s
                          | None -> ""
                        in
                        finish_refresh_response ~access_token ~refresh_token
                          ~expires_in ~refresh_token_expires_in ~token_type
                          ~scope))))
  with Yojson.Json_error msg ->
    Error (Printf.sprintf "malformed token JSON: %s" msg)

let parse_refresh_form body =
  let pairs =
    body |> String.split_on_char '&'
    |> List.filter_map (fun part ->
        match String.split_on_char '=' part with
        | [ k; v ] ->
            Some (Uri.pct_decode (String.trim k), Uri.pct_decode (String.trim v))
        | _ -> None)
  in
  let find k = List.assoc_opt k pairs in
  match find "error" with
  | Some e when String.trim e <> "" ->
      let desc =
        match find "error_description" with
        | Some d when String.trim d <> "" -> ": " ^ Uri.pct_decode d
        | _ -> ""
      in
      Error
        (Printf.sprintf "token endpoint returned OAuth error %s%s"
           (String.trim e) desc)
  | _ -> (
      match find "access_token" with
      | None | Some "" -> Error "token response missing access_token"
      | Some access_token -> (
          match find "refresh_token" with
          | None | Some "" -> Error "token response missing refresh_token"
          | Some refresh_token -> (
              match find "expires_in" with
              | None | Some "" -> Error "token response missing expires_in"
              | Some exp_s -> (
                  match int_of_string_opt (String.trim exp_s) with
                  | None -> Error "token response expires_in is not an integer"
                  | Some expires_in when expires_in <= 0 ->
                      Error "token response expires_in must be positive"
                  | Some expires_in -> (
                      match find "refresh_token_expires_in" with
                      | None | Some "" ->
                          Error
                            "token response missing refresh_token_expires_in"
                      | Some rtexp_s -> (
                          match int_of_string_opt (String.trim rtexp_s) with
                          | None ->
                              Error
                                "token response refresh_token_expires_in is \
                                 not an integer"
                          | Some refresh_token_expires_in
                            when refresh_token_expires_in <= 0 ->
                              Error
                                "token response refresh_token_expires_in must \
                                 be positive"
                          | Some refresh_token_expires_in ->
                              let token_type =
                                match find "token_type" with
                                | Some t -> t
                                | None -> ""
                              in
                              let scope =
                                match find "scope" with
                                | Some s -> s
                                | None -> ""
                              in
                              finish_refresh_response ~access_token
                                ~refresh_token ~expires_in
                                ~refresh_token_expires_in ~token_type ~scope))))
          ))

let parse_refresh_response ~body =
  let body = String.trim body in
  if body = "" then Error "token response body is empty"
  else if String.length body > 0 && body.[0] = '{' then parse_refresh_json body
  else parse_refresh_form body

let map_parse_error e =
  let lower = String.lowercase_ascii e in
  if String_util.contains lower "token_type" then Invalid_token_type e
  else if String_util.contains lower "scope must be empty" then Nonempty_scope e
  else Malformed_response e

(* -------------------------------------------------------------------------- *)
(* Lifetime schema                                                            *)
(* -------------------------------------------------------------------------- *)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_user_token_refresh schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let ensure_schema db =
  exec_schema db
    {|CREATE TABLE IF NOT EXISTS github_user_token_refresh_lifetimes (
      vault_id TEXT PRIMARY KEY NOT NULL,
      access_expires_at TEXT NOT NULL,
      refresh_expires_at TEXT NOT NULL,
      generation INTEGER NOT NULL,
      updated_at TEXT NOT NULL
    )|}

let record_lifetimes ~db ~vault_id ~generation ~(lifetimes : lifetimes) ~now =
  ensure_schema db;
  let updated_at = Time_util.iso8601_utc ~t:now () in
  let sql =
    {|INSERT INTO github_user_token_refresh_lifetimes
        (vault_id, access_expires_at, refresh_expires_at, generation, updated_at)
      VALUES (?,?,?,?,?)
      ON CONFLICT(vault_id) DO UPDATE SET
        access_expires_at = excluded.access_expires_at,
        refresh_expires_at = excluded.refresh_expires_at,
        generation = excluded.generation,
        updated_at = excluded.updated_at|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i d = ignore (Sqlite3.bind stmt i d) in
      bind 1 (Sqlite3.Data.TEXT vault_id);
      bind 2 (Sqlite3.Data.TEXT lifetimes.access_expires_at);
      bind 3 (Sqlite3.Data.TEXT lifetimes.refresh_expires_at);
      bind 4 (Sqlite3.Data.INT (Int64.of_int generation));
      bind 5 (Sqlite3.Data.TEXT updated_at);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Storage
               (Printf.sprintf "record lifetimes failed: %s (%s)"
                  (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

let get_recorded_lifetimes ~db ~vault_id =
  ensure_schema db;
  let vault_id = String.trim vault_id in
  if vault_id = "" then Error "vault_id must be non-empty"
  else
    let sql =
      {|SELECT access_expires_at, refresh_expires_at
        FROM github_user_token_refresh_lifetimes WHERE vault_id = ? LIMIT 1|}
    in
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT vault_id));
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match (Sqlite3.column stmt 0, Sqlite3.column stmt 1) with
            | Sqlite3.Data.TEXT a, Sqlite3.Data.TEXT r ->
                Ok (Some { access_expires_at = a; refresh_expires_at = r })
            | _ -> Error "lifetimes row has non-text columns")
        | Sqlite3.Rc.DONE -> Ok None
        | rc ->
            Error
              (Printf.sprintf "load lifetimes failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

(* -------------------------------------------------------------------------- *)
(* Binding / account helpers                                                  *)
(* -------------------------------------------------------------------------- *)

let account_of_binding (b : B.binding) : V.account_key =
  {
    principal_id = Principal_identity.principal_id_to_string b.principal_id;
    github_user_id = b.identity.github_user_id;
    app_id = b.identity.app_id;
    host = b.identity.host;
  }

let account_equal (a : V.account_key) (b : V.account_key) =
  String.equal a.principal_id b.principal_id
  && Int64.equal a.github_user_id b.github_user_id
  && a.app_id = b.app_id && String.equal a.host b.host

let load_binding_for_refresh ~db ~binding_id ~expected ~vault_id
    ?expected_lineage_id () =
  let binding_id = String.trim binding_id in
  if binding_id = "" then Error (Invalid_input "binding_id must be non-empty")
  else
    match B.get ~db ~id:binding_id with
    | Error e -> Error (Binding e)
    | Ok None -> Error (Binding "binding not found")
    | Ok (Some b) -> (
        let found = account_of_binding b in
        if not (account_equal expected found) then
          Error (Account_mismatch { expected; found })
        else
          match b.vault_ref with
          | None -> Error (Binding "binding has no vault_ref")
          | Some vr when not (String.equal (B.vault_ref_to_string vr) vault_id)
            ->
              Error (Binding "binding vault_ref does not match vault id")
          | Some _ -> (
              match b.authorization_status with
              | B.Authorized -> (
                  match expected_lineage_id with
                  | Some exp when String.trim exp <> "" ->
                      let exp = String.trim exp in
                      if not (String.equal exp b.lineage_id) then
                        Error
                          (Lineage_mismatch
                             { expected = exp; actual = b.lineage_id })
                      else Ok b
                  | _ -> Ok b)
              | status ->
                  Error
                    (Binding
                       (Printf.sprintf
                          "binding authorization_status is %s; refresh \
                           requires Authorized"
                          (B.string_of_authorization_status status)))))

(* -------------------------------------------------------------------------- *)
(* Core refresh                                                               *)
(* -------------------------------------------------------------------------- *)

let refresh ~db ~keys ~http_post ~resolve_client ~client_id_handle
    ?(now = Unix.gettimeofday ()) ?(skew_seconds = default_refresh_skew_seconds)
    ?(force = false) ?binding_id ?expected_lineage_id ?expected ~vault_id () =
  let vault_id = String.trim vault_id in
  let client_id_handle = String.trim client_id_handle in
  if vault_id = "" then Error (Invalid_input "vault_id must be non-empty")
  else if client_id_handle = "" then
    Error (Invalid_input "client_id_handle must be non-empty")
  else if skew_seconds < 0. then
    Error (Invalid_input "skew_seconds must be non-negative")
  else
    match V.get_meta ~db ~id:vault_id with
    | Error e -> Error (Vault e)
    | Ok None -> Error (Vault V.Not_found)
    | Ok (Some meta) -> (
        if not meta.active then Error Vault_not_active
        else
          let account_check =
            match expected with
            | Some exp when not (account_equal exp meta.account) ->
                Error
                  (Account_mismatch { expected = exp; found = meta.account })
            | _ -> Ok ()
          in
          match account_check with
          | Error e -> Error e
          | Ok () -> (
              let binding_res =
                match binding_id with
                | None -> Ok None
                | Some bid -> (
                    match
                      load_binding_for_refresh ~db ~binding_id:bid
                        ~expected:meta.account ~vault_id ?expected_lineage_id ()
                    with
                    | Error e -> Error e
                    | Ok b -> Ok (Some b))
              in
              match binding_res with
              | Error e -> Error e
              | Ok binding_opt -> (
                  if
                    (not force)
                    && not
                         (needs_refresh ~now ~skew_seconds
                            ~access_expires_at:meta.expires_at ())
                  then Error Not_in_skew
                  else
                    (* Recorded refresh expiry fail-closed when known. *)
                    (match get_recorded_lifetimes ~db ~vault_id with
                      | Ok (Some lt)
                        when is_at_or_before ~now lt.refresh_expires_at ->
                          Error Refresh_token_expired
                      | Ok _ -> Ok ()
                      | Error e -> Error (Storage e))
                    |> function
                    | Error e -> Error e
                    | Ok () -> (
                        match
                          V.read ~db ~keys ~expected:meta.account ~id:vault_id
                            ()
                        with
                        | Error e -> Error (Vault e)
                        | Ok opened -> (
                            if not opened.record.active then
                              Error Vault_not_active
                            else
                              match opened.tokens.refresh_token with
                              | None | Some "" -> Error Refresh_token_missing
                              | Some refresh_token_plain -> (
                                  match resolve_client ~client_id_handle with
                                  | Error e -> Error (Client_resolve e)
                                  | Ok (client_id, client_secret) -> (
                                      let client_id = String.trim client_id in
                                      let client_secret =
                                        String.trim client_secret
                                      in
                                      if client_id = "" then
                                        Error
                                          (Client_resolve
                                             "resolved client_id is empty")
                                      else if client_secret = "" then
                                        Error
                                          (Client_resolve
                                             "resolved client_secret is empty")
                                      else
                                        let url =
                                          token_endpoint ~host:meta.account.host
                                            ()
                                        in
                                        let body =
                                          build_refresh_body ~client_id
                                            ~client_secret
                                            ~refresh_token:refresh_token_plain
                                        in
                                        match
                                          http_post ~url ~headers:token_headers
                                            ~body
                                        with
                                        | Error transport ->
                                            Error (Transport transport)
                                        | Ok (status, resp_body) -> (
                                            if status < 200 || status >= 300
                                            then
                                              let lower =
                                                String.lowercase_ascii resp_body
                                              in
                                              if
                                                String_util.contains lower
                                                  "bad_refresh_token"
                                                || String_util.contains lower
                                                     "incorrect_client_credentials"
                                                || String_util.contains lower
                                                     "expired"
                                              then Error Refresh_token_expired
                                              else Error (Http_denial status)
                                            else
                                              match
                                                parse_refresh_response
                                                  ~body:resp_body
                                              with
                                              | Error e ->
                                                  Error (map_parse_error e)
                                              | Ok resp -> (
                                                  let access_expires_at =
                                                    expires_at_iso ~now
                                                      ~expires_in:
                                                        resp.expires_in
                                                  in
                                                  let refresh_expires_at =
                                                    expires_at_iso ~now
                                                      ~expires_in:
                                                        resp
                                                          .refresh_token_expires_in
                                                  in
                                                  let lifetimes =
                                                    {
                                                      access_expires_at;
                                                      refresh_expires_at;
                                                    }
                                                  in
                                                  let tokens :
                                                      S.plaintext_tokens =
                                                    {
                                                      access_token =
                                                        resp.access_token;
                                                      refresh_token =
                                                        Some resp.refresh_token;
                                                    }
                                                  in
                                                  (* Preserve prior OAuth scope
                                                     list recorded at activation;
                                                     GitHub App refresh returns
                                                     empty scope by design. *)
                                                  let scopes =
                                                    opened.record.scopes
                                                  in
                                                  let expected_generation =
                                                    opened.record.generation
                                                  in
                                                  match
                                                    C.replace ~db ~keys ~now
                                                      ~id:vault_id
                                                      ~expected_generation
                                                      ~expected:meta.account
                                                      ?binding_id ~tokens
                                                      ~scopes
                                                      ~expires_at:
                                                        access_expires_at ()
                                                  with
                                                  | Error e -> Error (Cas e)
                                                  | Ok transition -> (
                                                      match
                                                        record_lifetimes ~db
                                                          ~vault_id
                                                          ~generation:
                                                            transition.record
                                                              .generation
                                                          ~lifetimes ~now
                                                      with
                                                      | Error e -> Error e
                                                      | Ok () ->
                                                          let lineage_id =
                                                            match
                                                              binding_opt
                                                            with
                                                            | Some b ->
                                                                Some
                                                                  b.lineage_id
                                                            | None ->
                                                                expected_lineage_id
                                                          in
                                                          Ok
                                                            {
                                                              record =
                                                                transition
                                                                  .record;
                                                              leases_invalidated =
                                                                transition
                                                                  .leases_invalidated;
                                                              lifetimes;
                                                              lineage_id;
                                                              binding =
                                                                binding_opt;
                                                              refreshed = true;
                                                            }))))))))))

(* -------------------------------------------------------------------------- *)
(* Lease acquisition                                                          *)
(* -------------------------------------------------------------------------- *)

let issue_lease_for ~now ?ttl_seconds ?binding_id ?expected
    ~(record : V.vault_record) () =
  match L.issue_from_record ~now ?ttl_seconds ?binding_id ~record () with
  | Error e -> Error (Lease e)
  | Ok l -> (
      match expected with
      | Some exp when not (account_equal exp record.account) ->
          L.revoke l;
          Error (Account_mismatch { expected = exp; found = record.account })
      | _ -> Ok l)

let acquire_lease ~db ~keys ?http_post ?resolve_client ?client_id_handle
    ?(now = Unix.gettimeofday ()) ?(skew_seconds = default_refresh_skew_seconds)
    ?ttl_seconds ?binding_id ?expected_lineage_id ?expected ~vault_id () =
  let vault_id = String.trim vault_id in
  if vault_id = "" then Error (Invalid_input "vault_id must be non-empty")
  else if skew_seconds < 0. then
    Error (Invalid_input "skew_seconds must be non-negative")
  else
    match V.get_meta ~db ~id:vault_id with
    | Error e -> Error (Vault e)
    | Ok None -> Error (Vault V.Not_found)
    | Ok (Some meta) -> (
        if not meta.active then Error Vault_not_active
        else
          let account_check =
            match expected with
            | Some exp when not (account_equal exp meta.account) ->
                Error
                  (Account_mismatch { expected = exp; found = meta.account })
            | _ -> Ok ()
          in
          match account_check with
          | Error e -> Error e
          | Ok () -> (
              let binding_res =
                match binding_id with
                | None -> Ok None
                | Some bid -> (
                    match
                      load_binding_for_refresh ~db ~binding_id:bid
                        ~expected:meta.account ~vault_id ?expected_lineage_id ()
                    with
                    | Error e -> Error e
                    | Ok b -> Ok (Some b))
              in
              match binding_res with
              | Error e -> Error e
              | Ok binding_opt -> (
                  let in_skew =
                    needs_refresh ~now ~skew_seconds
                      ~access_expires_at:meta.expires_at ()
                  in
                  if not in_skew then
                    match
                      issue_lease_for ~now ?ttl_seconds ?binding_id ?expected
                        ~record:meta ()
                    with
                    | Error e -> Error e
                    | Ok lease ->
                        let lifetimes =
                          match get_recorded_lifetimes ~db ~vault_id with
                          | Ok (Some lt) -> lt
                          | _ ->
                              {
                                access_expires_at = meta.expires_at;
                                refresh_expires_at = "";
                              }
                        in
                        let lineage_id =
                          match binding_opt with
                          | Some b -> Some b.lineage_id
                          | None -> expected_lineage_id
                        in
                        Ok
                          ( lease,
                            {
                              record = meta;
                              leases_invalidated = 0;
                              lifetimes;
                              lineage_id;
                              binding = binding_opt;
                              refreshed = false;
                            } )
                  else
                    match (http_post, resolve_client, client_id_handle) with
                    | None, _, _ | _, None, _ | _, _, None ->
                        Error
                          (Invalid_input
                             "refresh required inside skew: provide http_post, \
                              resolve_client, and client_id_handle")
                    | Some http_post, Some resolve_client, Some client_id_handle
                      -> (
                        match
                          refresh ~db ~keys ~http_post ~resolve_client
                            ~client_id_handle ~now ~skew_seconds ~force:true
                            ?binding_id ?expected_lineage_id ?expected ~vault_id
                            ()
                        with
                        | Error e -> Error e
                        | Ok outcome -> (
                            match
                              issue_lease_for ~now ?ttl_seconds ?binding_id
                                ?expected ~record:outcome.record ()
                            with
                            | Error e -> Error e
                            | Ok lease -> Ok (lease, outcome))))))
