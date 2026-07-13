(* Unlink and Principal / Connector removal invalidation (P21.M3.E1.T004).
   See github_user_auth_invalidate.mli. *)

module V = Github_user_token_vault
module B = Github_account_binding
module C = Github_user_token_cas
module L = Github_user_token_lease
module P = Principal_identity
module M = Principal_merge

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type kind = Disable | Revoke | Unlink | Principal_removal | Connector_split

let string_of_kind = function
  | Disable -> "disable"
  | Revoke -> "revoke"
  | Unlink -> "unlink"
  | Principal_removal -> "principal_removal"
  | Connector_split -> "connector_split"

let kind_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "disable" -> Ok Disable
  | "revoke" -> Ok Revoke
  | "unlink" -> Ok Unlink
  | "principal_removal" -> Ok Principal_removal
  | "connector_split" -> Ok Connector_split
  | s -> Error (Printf.sprintf "unknown invalidate kind: %s" s)

type remote_mode = Skip | Revoke_token | Revoke_grant

let string_of_remote_mode = function
  | Skip -> "skip"
  | Revoke_token -> "revoke_token"
  | Revoke_grant -> "revoke_grant"

let remote_mode_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "skip" -> Ok Skip
  | "revoke_token" -> Ok Revoke_token
  | "revoke_grant" -> Ok Revoke_grant
  | s -> Error (Printf.sprintf "unknown remote_mode: %s" s)

let default_remote_mode = function
  | Disable -> Skip
  | Revoke | Unlink | Principal_removal | Connector_split -> Revoke_grant

let is_destructive = function
  | Disable -> false
  | Revoke | Unlink | Principal_removal | Connector_split -> true

let binding_status_of_kind = function
  | Disable -> B.Disabled
  | Revoke | Principal_removal -> B.Revoked
  | Unlink | Connector_split -> B.Unlinked

let clear_vault_ref_of_kind = function
  | Disable -> false
  | Revoke | Unlink | Principal_removal | Connector_split -> true

type http_delete =
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, string) result

type resolve_client =
  client_id_handle:string -> (string * string, string) result

type remote_outcome =
  | Remote_skipped of string
  | Remote_succeeded of { status_code : int; mode : remote_mode }
  | Remote_failed of { summary : string; mode : remote_mode }

type binding_effect = {
  binding_id : string;
  principal_id : string;
  host : string;
  app_id : int;
  github_user_id : int64;
  vault_id : string option;
  prior_generation : int option;
  new_generation : int option;
  prior_lineage_id : string;
  new_lineage_id : string option;
  local_disabled : bool;
  leases_invalidated : int;
  secrets_destroyed : bool;
  vault_ref_cleared : bool;
  already_terminal : bool;
  remote : remote_outcome;
  status_after : string;
}

type receipt = {
  id : string;
  kind : kind;
  principal_id : string option;
  actor_key : string option;
  related_id : string option;
  effects : binding_effect list;
  bindings_matched : int;
  pending_auth_invalidated : int;
  secrets_destroyed : int;
  leases_invalidated : int;
  lineages_broken : int;
  remote_attempted : int;
  remote_succeeded : int;
  remote_failed : int;
  created_at : string;
  notes : string list;
}

type denial =
  | Binding of string
  | Vault of V.denial
  | Cas of C.denial
  | Storage of string
  | Invalid_input of string

let string_of_denial = function
  | Binding msg -> "binding:" ^ msg
  | Vault d -> "vault:" ^ V.string_of_denial d
  | Cas d -> "cas:" ^ C.string_of_denial d
  | Storage msg -> "storage:" ^ msg
  | Invalid_input msg -> "invalid_input:" ^ msg

let denial_exposes_token ~denial ~plaintext =
  if plaintext = "" then false
  else String_util.contains (string_of_denial denial) plaintext

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let truncate_summary s ~max_len =
  let s = String.trim s in
  if String.length s <= max_len then s else String.sub s 0 max_len ^ "..."

let generate_receipt_id ?(now = Unix.gettimeofday ()) () =
  let ms = Int64.of_float (now *. 1000.) in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghinv_%Ld_%06d" ms rand

let account_of_binding (b : B.binding) : V.account_key =
  {
    principal_id = P.principal_id_to_string b.principal_id;
    github_user_id = b.identity.github_user_id;
    app_id = b.identity.app_id;
    host = b.identity.host;
  }

let api_base ?(host = "github.com") () =
  let host = String.trim host in
  if host = "" || String.equal host "github.com" then "https://api.github.com"
  else Printf.sprintf "https://api.%s" host

let revoke_endpoint ?(host = "github.com") ~client_id ~mode () =
  match mode with
  | Skip -> ""
  | Revoke_token ->
      Printf.sprintf "%s/applications/%s/token" (api_base ~host ())
        (Uri.pct_encode client_id)
  | Revoke_grant ->
      Printf.sprintf "%s/applications/%s/grant" (api_base ~host ())
        (Uri.pct_encode client_id)

let basic_auth_header ~client_id ~client_secret =
  let raw = client_id ^ ":" ^ client_secret in
  let b64 = Base64.encode_exn raw in
  ("Authorization", "Basic " ^ b64)

let remote_outcome_to_json = function
  | Remote_skipped reason ->
      `Assoc [ ("kind", `String "skipped"); ("reason", `String reason) ]
  | Remote_succeeded { status_code; mode } ->
      `Assoc
        [
          ("kind", `String "succeeded");
          ("status_code", `Int status_code);
          ("mode", `String (string_of_remote_mode mode));
        ]
  | Remote_failed { summary; mode } ->
      `Assoc
        [
          ("kind", `String "failed");
          ("summary", `String summary);
          ("mode", `String (string_of_remote_mode mode));
        ]

let remote_outcome_of_json = function
  | `Assoc fields -> (
      let get k =
        match List.assoc_opt k fields with
        | Some (`String s) -> Some s
        | _ -> None
      in
      let get_int k =
        match List.assoc_opt k fields with Some (`Int i) -> Some i | _ -> None
      in
      match get "kind" with
      | Some "skipped" ->
          Ok (Remote_skipped (Option.value (get "reason") ~default:""))
      | Some "succeeded" -> (
          match
            ( get_int "status_code",
              Option.bind (get "mode") (fun s ->
                  match remote_mode_of_string s with
                  | Ok m -> Some m
                  | Error _ -> None) )
          with
          | Some status_code, Some mode ->
              Ok (Remote_succeeded { status_code; mode })
          | _ -> Error "remote_outcome succeeded fields incomplete")
      | Some "failed" -> (
          match
            ( get "summary",
              Option.bind (get "mode") (fun s ->
                  match remote_mode_of_string s with
                  | Ok m -> Some m
                  | Error _ -> None) )
          with
          | Some summary, Some mode -> Ok (Remote_failed { summary; mode })
          | _ -> Error "remote_outcome failed fields incomplete")
      | Some k -> Error (Printf.sprintf "unknown remote_outcome kind: %s" k)
      | None -> Error "remote_outcome missing kind")
  | _ -> Error "remote_outcome must be object"

let opt_string_json = function None -> `Null | Some s -> `String s
let opt_int_json = function None -> `Null | Some i -> `Int i

let binding_effect_to_json (e : binding_effect) : Yojson.Safe.t =
  `Assoc
    [
      ("binding_id", `String e.binding_id);
      ("principal_id", `String e.principal_id);
      ("host", `String e.host);
      ("app_id", `Int e.app_id);
      ("github_user_id", `String (Int64.to_string e.github_user_id));
      ("vault_id", opt_string_json e.vault_id);
      ("prior_generation", opt_int_json e.prior_generation);
      ("new_generation", opt_int_json e.new_generation);
      ("prior_lineage_id", `String e.prior_lineage_id);
      ("new_lineage_id", opt_string_json e.new_lineage_id);
      ("local_disabled", `Bool e.local_disabled);
      ("leases_invalidated", `Int e.leases_invalidated);
      ("secrets_destroyed", `Bool e.secrets_destroyed);
      ("vault_ref_cleared", `Bool e.vault_ref_cleared);
      ("already_terminal", `Bool e.already_terminal);
      ("remote", remote_outcome_to_json e.remote);
      ("status_after", `String e.status_after);
    ]

let binding_effect_of_json = function
  | `Assoc fields as json ->
      let get_s k =
        match List.assoc_opt k fields with
        | Some (`String s) -> Ok s
        | _ -> Error (Printf.sprintf "missing string %s" k)
      in
      let get_b k =
        match List.assoc_opt k fields with
        | Some (`Bool b) -> Ok b
        | _ -> Error (Printf.sprintf "missing bool %s" k)
      in
      let get_i k =
        match List.assoc_opt k fields with
        | Some (`Int i) -> Ok i
        | _ -> Error (Printf.sprintf "missing int %s" k)
      in
      let get_opt_s k =
        match List.assoc_opt k fields with
        | None | Some `Null -> None
        | Some (`String s) -> Some s
        | _ -> None
      in
      let get_opt_i k =
        match List.assoc_opt k fields with
        | None | Some `Null -> None
        | Some (`Int i) -> Some i
        | _ -> None
      in
      let ( let* ) = Result.bind in
      let* binding_id = get_s "binding_id" in
      let* principal_id = get_s "principal_id" in
      let* host = get_s "host" in
      let* app_id = get_i "app_id" in
      let* github_user_id =
        match List.assoc_opt "github_user_id" fields with
        | Some (`String s) -> (
            try Ok (Int64.of_string s)
            with _ -> Error "github_user_id not int64")
        | Some (`Int i) -> Ok (Int64.of_int i)
        | Some (`Intlit s) -> (
            try Ok (Int64.of_string s)
            with _ -> Error "github_user_id not int64")
        | _ -> Error "github_user_id missing"
      in
      let* prior_lineage_id = get_s "prior_lineage_id" in
      let* local_disabled = get_b "local_disabled" in
      let* leases_invalidated = get_i "leases_invalidated" in
      let* secrets_destroyed = get_b "secrets_destroyed" in
      let* vault_ref_cleared = get_b "vault_ref_cleared" in
      let* already_terminal = get_b "already_terminal" in
      let* status_after = get_s "status_after" in
      let* remote =
        match List.assoc_opt "remote" fields with
        | Some j -> remote_outcome_of_json j
        | None -> Error "remote missing"
      in
      ignore json;
      Ok
        {
          binding_id;
          principal_id;
          host;
          app_id;
          github_user_id;
          vault_id = get_opt_s "vault_id";
          prior_generation = get_opt_i "prior_generation";
          new_generation = get_opt_i "new_generation";
          prior_lineage_id;
          new_lineage_id = get_opt_s "new_lineage_id";
          local_disabled;
          leases_invalidated;
          secrets_destroyed;
          vault_ref_cleared;
          already_terminal;
          remote;
          status_after;
        }
  | _ -> Error "binding_effect must be object"

let receipt_to_json (r : receipt) : Yojson.Safe.t =
  `Assoc
    [
      ("id", `String r.id);
      ("kind", `String (string_of_kind r.kind));
      ("principal_id", opt_string_json r.principal_id);
      ("actor_key", opt_string_json r.actor_key);
      ("related_id", opt_string_json r.related_id);
      ("effects", `List (List.map binding_effect_to_json r.effects));
      ("bindings_matched", `Int r.bindings_matched);
      ("pending_auth_invalidated", `Int r.pending_auth_invalidated);
      ("secrets_destroyed", `Int r.secrets_destroyed);
      ("leases_invalidated", `Int r.leases_invalidated);
      ("lineages_broken", `Int r.lineages_broken);
      ("remote_attempted", `Int r.remote_attempted);
      ("remote_succeeded", `Int r.remote_succeeded);
      ("remote_failed", `Int r.remote_failed);
      ("created_at", `String r.created_at);
      ("notes", `List (List.map (fun s -> `String s) r.notes));
    ]

let receipt_of_json = function
  | `Assoc fields ->
      let get_s k =
        match List.assoc_opt k fields with
        | Some (`String s) -> Ok s
        | _ -> Error (Printf.sprintf "missing string %s" k)
      in
      let get_i k =
        match List.assoc_opt k fields with
        | Some (`Int i) -> Ok i
        | _ -> Error (Printf.sprintf "missing int %s" k)
      in
      let get_opt_s k =
        match List.assoc_opt k fields with
        | None | Some `Null -> None
        | Some (`String s) -> Some s
        | _ -> None
      in
      let ( let* ) = Result.bind in
      let* id = get_s "id" in
      let* kind_s = get_s "kind" in
      let* kind = kind_of_string kind_s in
      let* bindings_matched = get_i "bindings_matched" in
      let* pending_auth_invalidated = get_i "pending_auth_invalidated" in
      let* secrets_destroyed = get_i "secrets_destroyed" in
      let* leases_invalidated = get_i "leases_invalidated" in
      let* lineages_broken = get_i "lineages_broken" in
      let* remote_attempted = get_i "remote_attempted" in
      let* remote_succeeded = get_i "remote_succeeded" in
      let* remote_failed = get_i "remote_failed" in
      let* created_at = get_s "created_at" in
      let* effects =
        match List.assoc_opt "effects" fields with
        | Some (`List xs) ->
            let rec go acc = function
              | [] -> Ok (List.rev acc)
              | x :: rest -> (
                  match binding_effect_of_json x with
                  | Error e -> Error e
                  | Ok e -> go (e :: acc) rest)
            in
            go [] xs
        | _ -> Error "effects missing"
      in
      let notes =
        match List.assoc_opt "notes" fields with
        | Some (`List xs) ->
            List.filter_map (function `String s -> Some s | _ -> None) xs
        | _ -> []
      in
      Ok
        {
          id;
          kind;
          principal_id = get_opt_s "principal_id";
          actor_key = get_opt_s "actor_key";
          related_id = get_opt_s "related_id";
          effects;
          bindings_matched;
          pending_auth_invalidated;
          secrets_destroyed;
          leases_invalidated;
          lineages_broken;
          remote_attempted;
          remote_succeeded;
          remote_failed;
          created_at;
          notes;
        }
  | _ -> Error "receipt must be object"

let string_of_receipt (r : receipt) =
  Printf.sprintf
    "invalidate id=%s kind=%s matched=%d secrets=%d leases=%d lineages=%d \
     remote_ok=%d remote_fail=%d pending=%d"
    r.id (string_of_kind r.kind) r.bindings_matched r.secrets_destroyed
    r.leases_invalidated r.lineages_broken r.remote_succeeded r.remote_failed
    r.pending_auth_invalidated

let rec json_contains_plaintext ~(json : Yojson.Safe.t) ~plaintext =
  if plaintext = "" then false
  else
    match json with
    | `String s -> String.equal s plaintext || String_util.contains s plaintext
    | `Intlit s -> String.equal s plaintext || String_util.contains s plaintext
    | `Assoc fields ->
        List.exists
          (fun (_k, v) -> json_contains_plaintext ~json:v ~plaintext)
          fields
    | `List items ->
        List.exists (fun v -> json_contains_plaintext ~json:v ~plaintext) items
    | `Bool _ | `Int _ | `Float _ | `Null -> false

let receipt_contains_plaintext ~receipt ~plaintext =
  json_contains_plaintext ~json:(receipt_to_json receipt) ~plaintext

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let schema_error db sql rc =
  Printf.sprintf "github_user_auth_invalidate schema error: %s (sql: %s)"
    (Sqlite3.Rc.to_string rc) sql

let exec_sql db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> Ok ()
  | rc -> Error (schema_error db sql rc)

let ensure_schema db =
  B.ensure_schema db;
  V.ensure_schema db;
  M.ensure_schema db;
  let sql =
    {|CREATE TABLE IF NOT EXISTS github_user_auth_invalidate_receipts (
        id TEXT PRIMARY KEY NOT NULL,
        kind TEXT NOT NULL,
        principal_id TEXT,
        actor_key TEXT,
        related_id TEXT,
        receipt_json TEXT NOT NULL,
        created_at TEXT NOT NULL
      )|}
  in
  match exec_sql db sql with
  | Error e -> failwith e
  | Ok () -> (
      let idx =
        {|CREATE INDEX IF NOT EXISTS idx_gh_user_auth_invalidate_principal
          ON github_user_auth_invalidate_receipts(principal_id)|}
      in
      match exec_sql db idx with Error e -> failwith e | Ok () -> ())

let store_receipt ~db (r : receipt) =
  let sql =
    {|INSERT INTO github_user_auth_invalidate_receipts
        (id, kind, principal_id, actor_key, related_id, receipt_json, created_at)
      VALUES (?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i d = ignore (Sqlite3.bind stmt i d) in
      bind 1 (Sqlite3.Data.TEXT r.id);
      bind 2 (Sqlite3.Data.TEXT (string_of_kind r.kind));
      (match r.principal_id with
      | None -> bind 3 Sqlite3.Data.NULL
      | Some s -> bind 3 (Sqlite3.Data.TEXT s));
      (match r.actor_key with
      | None -> bind 4 Sqlite3.Data.NULL
      | Some s -> bind 4 (Sqlite3.Data.TEXT s));
      (match r.related_id with
      | None -> bind 5 Sqlite3.Data.NULL
      | Some s -> bind 5 (Sqlite3.Data.TEXT s));
      bind 6 (Sqlite3.Data.TEXT (Yojson.Safe.to_string (receipt_to_json r)));
      bind 7 (Sqlite3.Data.TEXT r.created_at);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Storage
               (Printf.sprintf "store invalidate receipt failed: %s (%s)"
                  (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

let get_receipt ~db ~id =
  let sql =
    {|SELECT receipt_json FROM github_user_auth_invalidate_receipts
      WHERE id = ? LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT s -> (
              match
                try Ok (Yojson.Safe.from_string s)
                with exn ->
                  Error
                    (Printf.sprintf "parse invalidate receipt json: %s"
                       (Printexc.to_string exn))
              with
              | Error e -> Error e
              | Ok json -> (
                  match receipt_of_json json with
                  | Error e -> Error e
                  | Ok r -> Ok (Some r)))
          | _ -> Error "invalidate receipt_json not text")
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get invalidate receipt failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

(* -------------------------------------------------------------------------- *)
(* Narrow revocation open                                                     *)
(* -------------------------------------------------------------------------- *)

let with_revocation_token ~db ~keys ?expected ~vault_id ~f () =
  let vault_id = String.trim vault_id in
  if vault_id = "" then Error (V.Invalid_input "vault_id must be non-empty")
  else
    match V.read ~db ~keys ?expected ~id:vault_id () with
    | Error e -> Error e
    | Ok opened ->
        let access = String.trim opened.tokens.access_token in
        if access = "" then
          Error (V.Invalid_input "access_token empty after open")
        else
          let refresh =
            match opened.tokens.refresh_token with
            | None -> None
            | Some s ->
                let t = String.trim s in
                if t = "" then None else Some t
          in
          (* Tokens exist only for the duration of f. Vault active flag is not
             consulted and is never flipped true here. *)
          Ok (f ~access_token:access ~refresh_token:refresh)

(* -------------------------------------------------------------------------- *)
(* Local disable + lineage break                                              *)
(* -------------------------------------------------------------------------- *)

let status_is_terminal_for ~kind (status : B.authorization_status) =
  match (kind, status) with
  | Disable, B.Disabled -> true
  | ( (Revoke | Principal_removal | Unlink | Connector_split),
      (B.Revoked | B.Unlinked) ) ->
      true
  | _ -> false

let mark_binding_status ~db ~now ~(binding : B.binding) ~status ~clear_vault_ref
    =
  if
    binding.authorization_status = status
    && ((not clear_vault_ref) || binding.vault_ref = None)
  then Ok (binding, true)
  else
    match
      if clear_vault_ref then
        B.update ~db ~expected_revision:binding.revision ~now ~id:binding.id
          ~authorization_status:status ~vault_ref:None ()
      else
        B.update_authorization_status ~db ~expected_revision:binding.revision
          ~now ~id:binding.id ~status ()
    with
    | Error e -> Error (Binding e)
    | Ok updated -> Ok (updated, false)

let local_deactivate_vault ~db ~keys ~now ~(binding : B.binding) ~kind ~vault_id
    =
  match V.get_meta ~db ~id:vault_id with
  | Error V.Not_found -> Ok (None, None, 0, false)
  | Error d -> Error (Vault d)
  | Ok None -> Ok (None, None, 0, false)
  | Ok (Some meta) -> (
      let expected = account_of_binding binding in
      let prior_generation = meta.generation in
      if not meta.active then
        let leases = L.discard_for_vault ~vault_id in
        Ok (Some prior_generation, Some prior_generation, leases, true)
      else
        let cas =
          match kind with
          | Disable ->
              C.disable ~db ~keys ~now ~id:vault_id
                ~expected_generation:prior_generation ~expected
                ~binding_id:binding.id ()
          | Unlink | Connector_split ->
              C.unlink ~db ~keys ~now ~id:vault_id
                ~expected_generation:prior_generation ~expected
                ~binding_id:binding.id ()
          | Revoke | Principal_removal ->
              C.revoke ~db ~keys ~now ~id:vault_id
                ~expected_generation:prior_generation ~expected
                ~binding_id:binding.id ()
        in
        match cas with
        | Ok t ->
            Ok
              ( Some prior_generation,
                Some t.record.generation,
                t.leases_invalidated,
                true )
        | Error (C.Vault (V.Not_active | V.Active_conflict _))
        | Error (C.Vault (V.Generation_conflict _)) ->
            (* Concurrent local disable won: remain fail-closed. *)
            let leases = L.discard_for_vault ~vault_id in
            let _ =
              ignore
                (mark_binding_status ~db ~now ~binding
                   ~status:(binding_status_of_kind kind)
                   ~clear_vault_ref:(clear_vault_ref_of_kind kind))
            in
            Ok (Some prior_generation, Some prior_generation, leases, true)
        | Error d -> Error (Cas d))

let destroy_vault_secret ~db ~vault_id =
  match V.destroy ~db ~id:vault_id with
  | Ok () -> Ok true
  | Error V.Not_found -> Ok false
  | Error d -> Error (Vault d)

let perform_remote ~db ~keys ~host ~vault_id ~mode ~http_delete ~resolve_client
    ~client_id_handle ~expected =
  match mode with
  | Skip -> Ok (Remote_skipped "mode=skip")
  | mode -> (
      match (http_delete, resolve_client, client_id_handle) with
      | None, _, _ | _, None, _ | _, _, None ->
          Ok
            (Remote_skipped
               "remote revoke not configured (http_delete / resolve_client / \
                client_id_handle)")
      | Some http_delete, Some resolve_client, Some client_id_handle -> (
          match resolve_client ~client_id_handle with
          | Error msg ->
              Ok
                (Remote_failed
                   {
                     summary =
                       "client resolve failed: "
                       ^ truncate_summary msg ~max_len:120;
                     mode;
                   })
          | Ok (client_id, client_secret) -> (
              let client_id = String.trim client_id in
              let client_secret = String.trim client_secret in
              if client_id = "" || client_secret = "" then
                Ok
                  (Remote_failed
                     {
                       summary = "resolved client_id/client_secret empty";
                       mode;
                     })
              else
                let url = revoke_endpoint ~host ~client_id ~mode () in
                match
                  with_revocation_token ~db ~keys ~expected ~vault_id
                    ~f:(fun ~access_token ~refresh_token:_ ->
                      let body =
                        Yojson.Safe.to_string
                          (`Assoc [ ("access_token", `String access_token) ])
                      in
                      let headers =
                        [
                          basic_auth_header ~client_id ~client_secret;
                          ("Accept", "application/vnd.github+json");
                          ("Content-Type", "application/json");
                          ("X-GitHub-Api-Version", "2022-11-28");
                          ("User-Agent", "clawq-github-user-auth-invalidate");
                        ]
                      in
                      http_delete ~url ~headers ~body)
                    ()
                with
                | Error d ->
                    Ok
                      (Remote_failed
                         {
                           summary =
                             "open for remote revoke failed: "
                             ^ V.string_of_denial d;
                           mode;
                         })
                | Ok (Error transport) ->
                    Ok
                      (Remote_failed
                         {
                           summary =
                             "transport: "
                             ^ truncate_summary transport ~max_len:120;
                           mode;
                         })
                | Ok (Ok (status, _body)) ->
                    if status = 204 || status = 200 || status = 404 then
                      (* 404: already revoked upstream — success for our fence. *)
                      Ok (Remote_succeeded { status_code = status; mode })
                    else
                      Ok
                        (Remote_failed
                           {
                             summary =
                               Printf.sprintf "unexpected status %d" status;
                             mode;
                           }))))

(* -------------------------------------------------------------------------- *)
(* One binding                                                                *)
(* -------------------------------------------------------------------------- *)

let invalidate_one_binding ~db ~keys ~kind ~remote_mode ?http_delete
    ?resolve_client ?client_id_handle ?(snapshot = true) ~now
    ~(binding : B.binding) () : (binding_effect, denial) result =
  let principal_id = P.principal_id_to_string binding.principal_id in
  let host = binding.identity.host in
  let app_id = binding.identity.app_id in
  let github_user_id = binding.identity.github_user_id in
  let prior_lineage_id = binding.lineage_id in
  let vault_id_opt =
    match binding.vault_ref with
    | Some vr -> Some (B.vault_ref_to_string vr)
    | None -> None
  in
  let target_status = binding_status_of_kind kind in
  let clear_ref = clear_vault_ref_of_kind kind in
  let already_terminal =
    status_is_terminal_for ~kind binding.authorization_status
  in
  (* 0. Snapshot first so historical evidence retains prior lineage/status. *)
  let _snap =
    if snapshot then
      ignore
        (B.snapshot ~db ~now
           ~reason:("pre_invalidate_" ^ string_of_kind kind)
           ~id:binding.id ())
  in
  (* 1. LOCAL DISABLE FIRST (precedes network). *)
  let local_result =
    match vault_id_opt with
    | Some vault_id ->
        local_deactivate_vault ~db ~keys ~now ~binding ~kind ~vault_id
    | None -> Ok (None, None, 0, false)
  in
  match local_result with
  | Error e -> Error e
  | Ok (prior_gen, new_gen, leases_from_cas, local_disabled_vault) -> (
      (* Ensure binding status even when CAS already updated it. *)
      let binding_after_local =
        match B.get ~db ~id:binding.id with Ok (Some b) -> b | _ -> binding
      in
      let status_result =
        if
          binding_after_local.authorization_status = target_status
          && ((not clear_ref) || binding_after_local.vault_ref = None)
        then Ok (binding_after_local, already_terminal)
        else
          mark_binding_status ~db ~now ~binding:binding_after_local
            ~status:target_status ~clear_vault_ref:clear_ref
      in
      match status_result with
      | Error e -> Error e
      | Ok (binding_statused, _already) -> (
          (* 2. Logical lineage break for destructive kinds (before network). *)
          let lineage_result =
            if not (is_destructive kind) then
              Ok (binding_statused, None, prior_lineage_id)
            else
              match
                B.break_lineage ~db ~expected_revision:binding_statused.revision
                  ~now ~id:binding_statused.id ()
              with
              | Error e -> Error (Binding e)
              | Ok (updated, prior) ->
                  Ok (updated, Some updated.lineage_id, prior)
          in
          match lineage_result with
          | Error e -> Error e
          | Ok (binding_broken, new_lineage_id, prior_lineage) -> (
              let leases_extra =
                match vault_id_opt with
                | Some vault_id -> L.discard_for_vault ~vault_id
                | None -> 0
              in
              let leases_invalidated = leases_from_cas + leases_extra in
              (* 3. Optional remote revoke under narrow open. Local stays denied. *)
              let remote =
                match vault_id_opt with
                | None ->
                    Remote_skipped
                      "no vault attachment; nothing to revoke remotely"
                | Some vault_id when remote_mode = Skip ->
                    Remote_skipped "remote_mode=skip"
                | Some vault_id -> (
                    match
                      perform_remote ~db ~keys ~host ~vault_id ~mode:remote_mode
                        ~http_delete ~resolve_client ~client_id_handle
                        ~expected:(account_of_binding binding)
                    with
                    | Ok o -> o
                    | Error _ ->
                        (* perform_remote returns Ok outcomes only; belt+suspenders *)
                        Remote_failed
                          {
                            summary = "remote revoke internal error";
                            mode = remote_mode;
                          })
              in
              (* 4. ALWAYS destroy secrets for destructive kinds, regardless of
                 remote outcome. Disable preserves sealed material. *)
              let destroy_result =
                match (is_destructive kind, vault_id_opt) with
                | false, _ -> Ok false
                | true, None -> Ok false
                | true, Some vault_id -> destroy_vault_secret ~db ~vault_id
              in
              match destroy_result with
              | Error e -> Error e
              | Ok secrets_destroyed ->
                  (* Re-clear vault_ref after destroy for destructive kinds. *)
                  let final_binding =
                    if clear_ref then
                      match B.get ~db ~id:binding_broken.id with
                      | Ok (Some b) when b.vault_ref <> None -> (
                          match
                            B.set_vault_ref ~db ~expected_revision:b.revision
                              ~now ~id:b.id ~vault_ref:None ()
                          with
                          | Ok u -> u
                          | Error _ -> b)
                      | Ok (Some b) -> b
                      | _ -> binding_broken
                    else binding_broken
                  in
                  (* Remote failure must never re-enable: assert inactive. *)
                  let _ =
                    match vault_id_opt with
                    | None -> ()
                    | Some vault_id -> (
                        match V.get_meta ~db ~id:vault_id with
                        | Ok (Some meta) when meta.active && is_destructive kind
                          ->
                            (* Should be destroyed already; if still present,
                               force inactive without re-enabling path. *)
                            ignore
                              (V.cas_set_active ~db ~keys ~now ~id:vault_id
                                 ~expected_generation:meta.generation
                                 ~expected_active:true ~active:false ())
                        | _ -> ())
                  in
                  let vault_ref_cleared =
                    Option.is_some binding.vault_ref
                    && Option.is_none final_binding.vault_ref
                  in
                  Ok
                    {
                      binding_id = final_binding.id;
                      principal_id;
                      host;
                      app_id;
                      github_user_id;
                      vault_id = vault_id_opt;
                      prior_generation = prior_gen;
                      new_generation = new_gen;
                      prior_lineage_id = prior_lineage;
                      new_lineage_id;
                      local_disabled =
                        local_disabled_vault
                        || final_binding.authorization_status <> B.Authorized;
                      leases_invalidated;
                      secrets_destroyed;
                      vault_ref_cleared;
                      already_terminal;
                      remote;
                      status_after =
                        B.string_of_authorization_status
                          final_binding.authorization_status;
                    })))

let summarize_effects ~kind ~principal_id ~actor_key ~related_id ~effects
    ~pending_auth_invalidated ~now ~notes =
  let secrets_destroyed =
    List.fold_left
      (fun acc (e : binding_effect) ->
        acc + if e.secrets_destroyed then 1 else 0)
      0 effects
  in
  let leases_invalidated =
    List.fold_left
      (fun acc (e : binding_effect) -> acc + e.leases_invalidated)
      0 effects
  in
  let lineages_broken =
    List.fold_left
      (fun acc (e : binding_effect) ->
        acc + if Option.is_some e.new_lineage_id then 1 else 0)
      0 effects
  in
  let remote_attempted, remote_succeeded, remote_failed =
    List.fold_left
      (fun (a, s, f) (e : binding_effect) ->
        match e.remote with
        | Remote_skipped _ -> (a, s, f)
        | Remote_succeeded _ -> (a + 1, s + 1, f)
        | Remote_failed _ -> (a + 1, s, f + 1))
      (0, 0, 0) effects
  in
  {
    id = generate_receipt_id ~now ();
    kind;
    principal_id;
    actor_key;
    related_id;
    effects;
    bindings_matched = List.length effects;
    pending_auth_invalidated;
    secrets_destroyed;
    leases_invalidated;
    lineages_broken;
    remote_attempted;
    remote_succeeded;
    remote_failed;
    created_at = Time_util.iso8601_utc ~t:now ();
    notes;
  }

let invalidate_pending_auth ~db ~principal_id =
  match M.get_pending_authorization_count ~db ~principal_id with
  | Error _ -> 0
  | Ok n ->
      ignore (M.set_pending_authorization_count ~db ~principal_id ~count:0);
      n

(* -------------------------------------------------------------------------- *)
(* Public entry points                                                        *)
(* -------------------------------------------------------------------------- *)

let invalidate_binding ~db ~keys ~kind ?remote_mode ?http_delete ?resolve_client
    ?client_id_handle ?related_id ?(now = Unix.gettimeofday ())
    ?(snapshot = true) ~binding_id () =
  ensure_schema db;
  let binding_id = String.trim binding_id in
  if binding_id = "" then Error (Invalid_input "binding_id must be non-empty")
  else
    let remote_mode =
      match remote_mode with Some m -> m | None -> default_remote_mode kind
    in
    match B.get ~db ~id:binding_id with
    | Error e -> Error (Binding e)
    | Ok None -> Error (Binding ("binding not found: " ^ binding_id))
    | Ok (Some binding) -> (
        match
          invalidate_one_binding ~db ~keys ~kind ~remote_mode ?http_delete
            ?resolve_client ?client_id_handle ~snapshot ~now ~binding ()
        with
        | Error e -> Error e
        | Ok effect -> (
            let pending =
              invalidate_pending_auth ~db ~principal_id:binding.principal_id
            in
            let receipt =
              summarize_effects ~kind
                ~principal_id:
                  (Some (P.principal_id_to_string binding.principal_id))
                ~actor_key:None ~related_id ~effects:[ effect ]
                ~pending_auth_invalidated:pending ~now
                ~notes:
                  [
                    "local disable and lineage break precede network work";
                    "remote failure never re-enables access";
                    "secrets destroyed for destructive kinds regardless of \
                     remote outcome";
                    "pending confirmations on the old lineage fail rather than \
                     following a relink";
                  ]
            in
            match store_receipt ~db receipt with
            | Error e -> Error e
            | Ok () -> Ok receipt))

let invalidate_for_principal ~db ~keys ~kind ?remote_mode ?http_delete
    ?resolve_client ?client_id_handle ?related_id ?(now = Unix.gettimeofday ())
    ~principal_id () =
  ensure_schema db;
  let remote_mode =
    match remote_mode with Some m -> m | None -> default_remote_mode kind
  in
  match B.list_for_principal ~db ~principal_id with
  | Error e -> Error (Binding e)
  | Ok bindings -> (
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | b :: rest -> (
            match
              invalidate_one_binding ~db ~keys ~kind ~remote_mode ?http_delete
                ?resolve_client ?client_id_handle ~snapshot:true ~now ~binding:b
                ()
            with
            | Error e -> Error e
            | Ok effect -> go (effect :: acc) rest)
      in
      match go [] bindings with
      | Error e -> Error e
      | Ok effects -> (
          let pending = invalidate_pending_auth ~db ~principal_id in
          let receipt =
            summarize_effects ~kind
              ~principal_id:(Some (P.principal_id_to_string principal_id))
              ~actor_key:None ~related_id ~effects
              ~pending_auth_invalidated:pending ~now
              ~notes:
                [
                  "principal-scoped invalidation";
                  "local disable precedes remote revoke";
                  "secrets destroyed regardless of remote outcome";
                  "pending authorization zeroed for principal";
                ]
          in
          match store_receipt ~db receipt with
          | Error e -> Error e
          | Ok () -> Ok receipt))

let invalidate_for_connector_split ~db ?keys ?remote_mode ?http_delete
    ?resolve_client ?client_id_handle ?related_id ?(now = Unix.gettimeofday ())
    ~source_principal_id ~actor_key ?binding_ids () =
  ensure_schema db;
  let kind = Connector_split in
  let remote_mode =
    match remote_mode with Some m -> m | None -> default_remote_mode kind
  in
  let actor_key = String.trim actor_key in
  if actor_key = "" then Error (Invalid_input "actor_key must be non-empty")
  else
    let binding_ids =
      match binding_ids with
      | None -> []
      | Some ids ->
          List.filter_map
            (fun s ->
              let t = String.trim s in
              if t = "" then None else Some t)
            ids
    in
    if binding_ids <> [] && keys = None then
      Error
        (Invalid_input
           "keys required when binding_ids are supplied for connector split \
            invalidation")
    else
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | id :: rest -> (
            match B.get ~db ~id with
            | Error e -> Error (Binding e)
            | Ok None -> Error (Binding ("binding not found: " ^ id))
            | Ok (Some binding) -> (
                if
                  not
                    (P.principal_id_equal binding.principal_id
                       source_principal_id)
                then
                  Error
                    (Binding
                       (Printf.sprintf
                          "binding %s is not owned by source principal" id))
                else
                  let keys = Option.get keys in
                  match
                    invalidate_one_binding ~db ~keys ~kind ~remote_mode
                      ?http_delete ?resolve_client ?client_id_handle
                      ~snapshot:true ~now ~binding ()
                  with
                  | Error e -> Error e
                  | Ok effect -> go (effect :: acc) rest))
      in
      match go [] binding_ids with
      | Error e -> Error e
      | Ok effects -> (
          let pending =
            invalidate_pending_auth ~db ~principal_id:source_principal_id
          in
          let receipt =
            summarize_effects ~kind
              ~principal_id:
                (Some (P.principal_id_to_string source_principal_id))
              ~actor_key:(Some actor_key) ~related_id ~effects
              ~pending_auth_invalidated:pending ~now
              ~notes:
                [
                  "connector unlink/split shares the canonical invalidation \
                   lifecycle";
                  "pending auth invalidated on source principal";
                  (if binding_ids = [] then
                     "no binding_ids: credentials retained on source; delayed \
                      work on the old actor lineage fails via \
                      Principal_changed"
                   else
                     "listed bindings fully invalidated (local → remote → \
                      destroy)");
                ]
          in
          match store_receipt ~db receipt with
          | Error e -> Error e
          | Ok () -> Ok receipt)
