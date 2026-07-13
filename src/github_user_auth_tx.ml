(* Persist one-time Principal-bound GitHub user authorization transactions.
   See github_user_auth_tx.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

let schema_version = 1
let default_ttl_seconds = 900.0

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type flow_kind = Web_pkce | Device
type source = Room of string | Session of string
type app_client = { host : string; app_id : int; client_id_handle : string }

type intended_account = {
  github_user_id : int64 option;
  login_hint : string option;
}

let empty_intended_account = { github_user_id = None; login_hint = None }

type status = Open | Completed | Cancelled | Expired | Superseded | Rejected

type t = {
  version : int;
  id : string;
  flow_kind : flow_kind;
  principal_id : string;
  connector_actor : Principal_identity.connector_actor_key;
  source : source;
  app : app_client;
  intended_account : intended_account;
  one_time_state : string;
  base_revision : string;
  continuation_handle : string;
  created_at : string;
  expires_at : string;
  status : status;
  terminal_reason : string option;
  completed_at : string option;
  cancelled_at : string option;
  updated_at : string;
}

type bound_context = {
  principal_id : string;
  connector_actor : Principal_identity.connector_actor_key;
  source : source;
  app_id : int;
  base_revision : string;
}

(* -------------------------------------------------------------------------- *)
(* Codecs                                                                     *)
(* -------------------------------------------------------------------------- *)

let string_of_flow_kind = function Web_pkce -> "web_pkce" | Device -> "device"

let flow_kind_of_string = function
  | "web_pkce" -> Ok Web_pkce
  | "device" -> Ok Device
  | s -> Error (Printf.sprintf "unknown github_user_auth_tx flow_kind: %s" s)

let string_of_status = function
  | Open -> "open"
  | Completed -> "completed"
  | Cancelled -> "cancelled"
  | Expired -> "expired"
  | Superseded -> "superseded"
  | Rejected -> "rejected"

let status_of_string = function
  | "open" -> Ok Open
  | "completed" -> Ok Completed
  | "cancelled" -> Ok Cancelled
  | "expired" -> Ok Expired
  | "superseded" -> Ok Superseded
  | "rejected" -> Ok Rejected
  | s -> Error (Printf.sprintf "unknown github_user_auth_tx status: %s" s)

let status_is_terminal = function Open -> false | _ -> true
let status_is_resumable = function Open -> true | _ -> false

let source_kind_and_id = function
  | Room id -> ("room", id)
  | Session id -> ("session", id)

let source_of_kind_id ~kind ~id =
  let id = String.trim id in
  if id = "" then Error "source id must be non-empty"
  else
    match kind with
    | "room" -> Ok (Room id)
    | "session" -> Ok (Session id)
    | s -> Error (Printf.sprintf "unknown source kind: %s" s)

let string_of_source = function
  | Room id -> "room:" ^ id
  | Session id -> "session:" ^ id

let actor_key_string (tx : t) =
  Principal_identity.actor_identity_key tx.connector_actor

let context_matches (tx : t) (ctx : bound_context) =
  String.equal tx.principal_id ctx.principal_id
  && Principal_identity.connector_actor_key_equal tx.connector_actor
       ctx.connector_actor
  && String.equal (string_of_source tx.source) (string_of_source ctx.source)
  && tx.app.app_id = ctx.app_id
  && String.equal tx.base_revision ctx.base_revision

let is_expired ?(now = Unix.gettimeofday ()) (tx : t) =
  let now_s = Time_util.iso8601_utc ~t:now () in
  String.compare now_s tx.expires_at > 0

let intended_account_to_json (a : intended_account) : Yojson.Safe.t =
  let fields = [] in
  let fields =
    match a.github_user_id with
    | None -> fields
    | Some id -> ("github_user_id", `String (Int64.to_string id)) :: fields
  in
  let fields =
    match a.login_hint with
    | None -> fields
    | Some h -> ("login_hint", `String h) :: fields
  in
  `Assoc (List.rev fields)

let intended_account_of_json (j : Yojson.Safe.t) :
    (intended_account, string) result =
  let open Yojson.Safe.Util in
  try
    let github_user_id =
      match member "github_user_id" j with
      | `Null -> None
      | `String s -> Some (Int64.of_string (String.trim s))
      | `Int i -> Some (Int64.of_int i)
      | `Intlit s -> Some (Int64.of_string s)
      | _ -> failwith "github_user_id must be string or int"
    in
    let login_hint =
      match member "login_hint" j with
      | `Null -> None
      | `String s ->
          let t = String.trim s in
          if t = "" then None else Some t
      | _ -> failwith "login_hint must be string"
    in
    Ok { github_user_id; login_hint }
  with
  | Failure msg -> Error msg
  | Yojson.Json_error msg -> Error msg
  | _ -> Error "invalid intended_account json"

(* -------------------------------------------------------------------------- *)
(* RNG / ids                                                                  *)
(* -------------------------------------------------------------------------- *)

let ensure_rng_initialized = lazy (Mirage_crypto_rng_unix.use_default ())

let generate_one_time_state () =
  Lazy.force ensure_rng_initialized;
  let raw = Mirage_crypto_rng.generate 32 in
  Digestif.SHA256.(digest_string raw |> to_hex)

let generate_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "gh_user_auth_tx_%d_%06d" ts rand

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let ensure_schema db =
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_user_auth_tx (
      id TEXT PRIMARY KEY NOT NULL,
      version INTEGER NOT NULL,
      flow_kind TEXT NOT NULL,
      principal_id TEXT NOT NULL,
      connector TEXT NOT NULL,
      tenant_or_workspace TEXT NOT NULL,
      immutable_user_id TEXT NOT NULL,
      actor_key TEXT NOT NULL,
      source_kind TEXT NOT NULL,
      source_id TEXT NOT NULL,
      host TEXT NOT NULL,
      app_id INTEGER NOT NULL,
      client_id_handle TEXT NOT NULL,
      intended_account_json TEXT NOT NULL,
      one_time_state TEXT NOT NULL UNIQUE,
      base_revision TEXT NOT NULL,
      continuation_handle TEXT NOT NULL,
      created_at TEXT NOT NULL,
      expires_at TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'open',
      terminal_reason TEXT,
      completed_at TEXT,
      cancelled_at TEXT,
      updated_at TEXT NOT NULL
    )|}
  in
  let idx_principal_source_flow =
    {|CREATE INDEX IF NOT EXISTS idx_github_user_auth_tx_principal_source_flow
      ON github_user_auth_tx(principal_id, source_kind, source_id, flow_kind, status)|}
  in
  let idx_one_time =
    {|CREATE INDEX IF NOT EXISTS idx_github_user_auth_tx_one_time_state
      ON github_user_auth_tx(one_time_state)|}
  in
  let idx_actor =
    {|CREATE INDEX IF NOT EXISTS idx_github_user_auth_tx_actor
      ON github_user_auth_tx(actor_key, status)|}
  in
  List.iter
    (fun sql ->
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf "github_user_auth_tx schema error: %s"
               (Sqlite3.Rc.to_string rc)))
    [ table_sql; idx_principal_source_flow; idx_one_time; idx_actor ]

(* -------------------------------------------------------------------------- *)
(* Row load / store                                                           *)
(* -------------------------------------------------------------------------- *)

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | _ -> ""

let opt_text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None

let int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | Sqlite3.Data.TEXT s -> int_of_string s
  | _ -> 0

let select_columns =
  {|id, version, flow_kind, principal_id, connector, tenant_or_workspace,
    immutable_user_id, source_kind, source_id, host, app_id, client_id_handle,
    intended_account_json, one_time_state, base_revision, continuation_handle,
    created_at, expires_at, status, terminal_reason, completed_at, cancelled_at,
    updated_at|}

let load_from_stmt stmt : (t, string) result =
  let id = text_col stmt 0 in
  let version = int_col stmt 1 in
  let flow_s = text_col stmt 2 in
  let principal_id = text_col stmt 3 in
  let connector_s = text_col stmt 4 in
  let tenant = text_col stmt 5 in
  let user_id = text_col stmt 6 in
  let source_kind = text_col stmt 7 in
  let source_id = text_col stmt 8 in
  let host = text_col stmt 9 in
  let app_id = int_col stmt 10 in
  let client_id_handle = text_col stmt 11 in
  let intended_json_s = text_col stmt 12 in
  let one_time_state = text_col stmt 13 in
  let base_revision = text_col stmt 14 in
  let continuation_handle = text_col stmt 15 in
  let created_at = text_col stmt 16 in
  let expires_at = text_col stmt 17 in
  let status_s = text_col stmt 18 in
  let terminal_reason = opt_text_col stmt 19 in
  let completed_at = opt_text_col stmt 20 in
  let cancelled_at = opt_text_col stmt 21 in
  let updated_at = text_col stmt 22 in
  match flow_kind_of_string flow_s with
  | Error e -> Error e
  | Ok flow_kind -> (
      match status_of_string status_s with
      | Error e -> Error e
      | Ok status -> (
          match Principal_identity.connector_of_string connector_s with
          | Error e -> Error e
          | Ok connector -> (
              match
                Principal_identity.make_connector_actor_key ~connector
                  ~tenant_or_workspace:tenant ~immutable_user_id:user_id
              with
              | Error e -> Error e
              | Ok connector_actor -> (
                  match source_of_kind_id ~kind:source_kind ~id:source_id with
                  | Error e -> Error e
                  | Ok source -> (
                      match
                        intended_account_of_json
                          (Yojson.Safe.from_string intended_json_s)
                      with
                      | Error e -> Error e
                      | Ok intended_account ->
                          Ok
                            {
                              version;
                              id;
                              flow_kind;
                              principal_id;
                              connector_actor;
                              source;
                              app = { host; app_id; client_id_handle };
                              intended_account;
                              one_time_state;
                              base_revision;
                              continuation_handle;
                              created_at;
                              expires_at;
                              status;
                              terminal_reason;
                              completed_at;
                              cancelled_at;
                              updated_at;
                            })))))

let get ~db ~id =
  let sql =
    Printf.sprintf "SELECT %s FROM github_user_auth_tx WHERE id = ? LIMIT 1"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match load_from_stmt stmt with
        | Ok tx -> Ok (Some tx)
        | Error e -> Error e)
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (Printf.sprintf "github_user_auth_tx get failed: %s"
             (Sqlite3.Rc.to_string rc))
  in
  ignore (Sqlite3.finalize stmt);
  result

let find_by_one_time_state ~db ~one_time_state =
  let sql =
    Printf.sprintf
      "SELECT %s FROM github_user_auth_tx WHERE one_time_state = ? LIMIT 1"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT one_time_state));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match load_from_stmt stmt with
        | Ok tx -> Ok (Some tx)
        | Error e -> Error e)
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (Printf.sprintf
             "github_user_auth_tx find_by_one_time_state failed: %s"
             (Sqlite3.Rc.to_string rc))
  in
  ignore (Sqlite3.finalize stmt);
  result

let find_open ~db ~principal_id ~source ~flow_kind =
  let source_kind, source_id = source_kind_and_id source in
  let sql =
    Printf.sprintf
      {|SELECT %s FROM github_user_auth_tx
        WHERE principal_id = ? AND source_kind = ? AND source_id = ?
          AND flow_kind = ? AND status = 'open'
        ORDER BY created_at DESC LIMIT 1|}
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT principal_id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT source_kind));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT source_id));
  ignore
    (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT (string_of_flow_kind flow_kind)));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match load_from_stmt stmt with
        | Ok tx -> Ok (Some tx)
        | Error e -> Error e)
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (Printf.sprintf "github_user_auth_tx find_open failed: %s"
             (Sqlite3.Rc.to_string rc))
  in
  ignore (Sqlite3.finalize stmt);
  result

let insert_tx ~db (tx : t) =
  let source_kind, source_id = source_kind_and_id tx.source in
  let actor_key = Principal_identity.actor_identity_key tx.connector_actor in
  let sql =
    {|INSERT INTO github_user_auth_tx
      (id, version, flow_kind, principal_id, connector, tenant_or_workspace,
       immutable_user_id, actor_key, source_kind, source_id, host, app_id,
       client_id_handle, intended_account_json, one_time_state, base_revision,
       continuation_handle, created_at, expires_at, status, terminal_reason,
       completed_at, cancelled_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i v = ignore (Sqlite3.bind stmt i v) in
  bind 1 (Sqlite3.Data.TEXT tx.id);
  bind 2 (Sqlite3.Data.INT (Int64.of_int tx.version));
  bind 3 (Sqlite3.Data.TEXT (string_of_flow_kind tx.flow_kind));
  bind 4 (Sqlite3.Data.TEXT tx.principal_id);
  bind 5
    (Sqlite3.Data.TEXT
       (Principal_identity.string_of_connector tx.connector_actor.connector));
  bind 6 (Sqlite3.Data.TEXT tx.connector_actor.scope.tenant_or_workspace);
  bind 7 (Sqlite3.Data.TEXT tx.connector_actor.scope.immutable_user_id);
  bind 8 (Sqlite3.Data.TEXT actor_key);
  bind 9 (Sqlite3.Data.TEXT source_kind);
  bind 10 (Sqlite3.Data.TEXT source_id);
  bind 11 (Sqlite3.Data.TEXT tx.app.host);
  bind 12 (Sqlite3.Data.INT (Int64.of_int tx.app.app_id));
  bind 13 (Sqlite3.Data.TEXT tx.app.client_id_handle);
  bind 14
    (Sqlite3.Data.TEXT
       (Yojson.Safe.to_string (intended_account_to_json tx.intended_account)));
  bind 15 (Sqlite3.Data.TEXT tx.one_time_state);
  bind 16 (Sqlite3.Data.TEXT tx.base_revision);
  bind 17 (Sqlite3.Data.TEXT tx.continuation_handle);
  bind 18 (Sqlite3.Data.TEXT tx.created_at);
  bind 19 (Sqlite3.Data.TEXT tx.expires_at);
  bind 20 (Sqlite3.Data.TEXT (string_of_status tx.status));
  bind 21
    (match tx.terminal_reason with
    | None -> Sqlite3.Data.NULL
    | Some r -> Sqlite3.Data.TEXT r);
  bind 22
    (match tx.completed_at with
    | None -> Sqlite3.Data.NULL
    | Some t -> Sqlite3.Data.TEXT t);
  bind 23
    (match tx.cancelled_at with
    | None -> Sqlite3.Data.NULL
    | Some t -> Sqlite3.Data.TEXT t);
  bind 24 (Sqlite3.Data.TEXT tx.updated_at);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "github_user_auth_tx insert failed: %s"
           (Sqlite3.Rc.to_string rc))

let supersede_open ~db ~principal_id ~source ~flow_kind
    ?(now = Unix.gettimeofday ()) () =
  let source_kind, source_id = source_kind_and_id source in
  let updated_at = Time_util.iso8601_utc ~t:now () in
  let sql =
    {|UPDATE github_user_auth_tx
      SET status = 'superseded', updated_at = ?,
          terminal_reason = COALESCE(terminal_reason, 'superseded by newer authorization')
      WHERE principal_id = ? AND source_kind = ? AND source_id = ?
        AND flow_kind = ? AND status = 'open'|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT updated_at));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT principal_id));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT source_kind));
  ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT source_id));
  ignore
    (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT (string_of_flow_kind flow_kind)));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "github_user_auth_tx supersede failed: %s"
           (Sqlite3.Rc.to_string rc))

let begin_immediate ~db =
  match Sqlite3.exec db "BEGIN IMMEDIATE" with
  | Sqlite3.Rc.OK -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "github_user_auth_tx begin failed: %s"
           (Sqlite3.Rc.to_string rc))

let rollback ~db = ignore (Sqlite3.exec db "ROLLBACK")

let commit ~db =
  match Sqlite3.exec db "COMMIT" with
  | Sqlite3.Rc.OK -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "github_user_auth_tx commit failed: %s"
           (Sqlite3.Rc.to_string rc))

let replace_open ~db ~principal_id ~source ~flow_kind ~now (tx : t) =
  match begin_immediate ~db with
  | Error _ as e -> e
  | Ok () -> (
      match supersede_open ~db ~principal_id ~source ~flow_kind ~now () with
      | Error e ->
          rollback ~db;
          Error e
      | Ok () -> (
          match insert_tx ~db tx with
          | Error e ->
              rollback ~db;
              Error e
          | Ok () -> (
              match commit ~db with
              | Ok () -> Ok tx
              | Error e ->
                  rollback ~db;
                  Error e)))

(* -------------------------------------------------------------------------- *)
(* Validation helpers                                                         *)
(* -------------------------------------------------------------------------- *)

let require_non_empty name s =
  if String.trim s = "" then Error (name ^ " is required") else Ok ()

let validate_app (app : app_client) =
  match require_non_empty "app.host" app.host with
  | Error _ as e -> e
  | Ok () ->
      if app.app_id <= 0 then Error "app.app_id must be positive"
      else require_non_empty "app.client_id_handle" app.client_id_handle

let validate_source = function
  | Room id | Session id -> require_non_empty "source id" id

(* -------------------------------------------------------------------------- *)
(* Create                                                                     *)
(* -------------------------------------------------------------------------- *)

let create ~db ~flow_kind ~principal_id ~connector_actor ~source ~app
    ?(intended_account = empty_intended_account) ~base_revision
    ~continuation_handle ?(ttl_seconds = default_ttl_seconds)
    ?(now = Unix.gettimeofday ()) ?id ?one_time_state () =
  match require_non_empty "principal_id" principal_id with
  | Error _ as e -> e
  | Ok () -> (
      match validate_source source with
      | Error _ as e -> e
      | Ok () -> (
          match validate_app app with
          | Error _ as e -> e
          | Ok () -> (
              match require_non_empty "base_revision" base_revision with
              | Error _ as e -> e
              | Ok () -> (
                  match
                    require_non_empty "continuation_handle" continuation_handle
                  with
                  | Error _ as e -> e
                  | Ok () ->
                      if ttl_seconds <= 0. then
                        Error "ttl_seconds must be positive"
                      else
                        let tx_id =
                          match id with
                          | Some i -> i
                          | None -> generate_id ~now ()
                        in
                        let state =
                          match one_time_state with
                          | Some s -> s
                          | None -> generate_one_time_state ()
                        in
                        if String.trim state = "" then
                          Error "one_time_state must be non-empty"
                        else if String.trim tx_id = "" then
                          Error "id must be non-empty"
                        else
                          let created_at = Time_util.iso8601_utc ~t:now () in
                          let expires_at =
                            Time_util.iso8601_utc ~t:(now +. ttl_seconds) ()
                          in
                          let tx : t =
                            {
                              version = schema_version;
                              id = tx_id;
                              flow_kind;
                              principal_id = String.trim principal_id;
                              connector_actor;
                              source;
                              app =
                                {
                                  host = String.trim app.host;
                                  app_id = app.app_id;
                                  client_id_handle =
                                    String.trim app.client_id_handle;
                                };
                              intended_account;
                              one_time_state = String.trim state;
                              base_revision = String.trim base_revision;
                              continuation_handle =
                                String.trim continuation_handle;
                              created_at;
                              expires_at;
                              status = Open;
                              terminal_reason = None;
                              completed_at = None;
                              cancelled_at = None;
                              updated_at = created_at;
                            }
                          in
                          replace_open ~db ~principal_id:tx.principal_id ~source
                            ~flow_kind ~now tx))))

(* -------------------------------------------------------------------------- *)
(* Status transitions                                                         *)
(* -------------------------------------------------------------------------- *)

let mark_status ~db ~id ~status ?terminal_reason ?completed_at ?cancelled_at
    ?(now = Unix.gettimeofday ()) () =
  let updated_at = Time_util.iso8601_utc ~t:now () in
  let sql =
    {|UPDATE github_user_auth_tx
      SET status = ?, updated_at = ?,
          terminal_reason = COALESCE(?, terminal_reason),
          completed_at = COALESCE(?, completed_at),
          cancelled_at = COALESCE(?, cancelled_at)
      WHERE id = ? AND status = 'open'|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT (string_of_status status)));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT updated_at));
  ignore
    (Sqlite3.bind stmt 3
       (match terminal_reason with
       | None -> Sqlite3.Data.NULL
       | Some r -> Sqlite3.Data.TEXT r));
  ignore
    (Sqlite3.bind stmt 4
       (match completed_at with
       | None -> Sqlite3.Data.NULL
       | Some t -> Sqlite3.Data.TEXT t));
  ignore
    (Sqlite3.bind stmt 5
       (match cancelled_at with
       | None -> Sqlite3.Data.NULL
       | Some t -> Sqlite3.Data.TEXT t));
  ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.TEXT id));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE ->
      if Sqlite3.changes db = 0 then
        Error
          "authorization transaction is no longer open (terminal or concurrent \
           transition)"
      else Ok updated_at
  | rc ->
      Error
        (Printf.sprintf "github_user_auth_tx mark_status failed: %s"
           (Sqlite3.Rc.to_string rc))

let maybe_expire_open ~db ~(tx : t) ~now =
  if tx.status = Open && is_expired ~now tx then
    match
      mark_status ~db ~id:tx.id ~status:Expired
        ~terminal_reason:"authorization transaction expired" ~now ()
    with
    | Ok updated_at ->
        Ok
          {
            tx with
            status = Expired;
            terminal_reason = Some "authorization transaction expired";
            updated_at;
          }
    | Error _ -> (
        (* Concurrent transition already terminal — reload. *)
        match get ~db ~id:tx.id with
        | Ok (Some reloaded) -> Ok reloaded
        | Ok None -> Error "authorization transaction disappeared"
        | Error e -> Error e)
  else Ok tx

let resume ~db ?id ~(context : bound_context) ~flow_kind
    ?(now = Unix.gettimeofday ()) () =
  match require_non_empty "principal_id" context.principal_id with
  | Error _ as e -> e
  | Ok () -> (
      match require_non_empty "base_revision" context.base_revision with
      | Error _ as e -> e
      | Ok () -> (
          match validate_source context.source with
          | Error _ as e -> e
          | Ok () -> (
              let loaded =
                match id with
                | Some tid -> (
                    match get ~db ~id:tid with
                    | Error e -> Error e
                    | Ok None ->
                        Error
                          (Printf.sprintf
                             "authorization transaction not found: %s" tid)
                    | Ok (Some tx) -> Ok tx)
                | None -> (
                    match
                      find_open ~db ~principal_id:context.principal_id
                        ~source:context.source ~flow_kind
                    with
                    | Error e -> Error e
                    | Ok None ->
                        Error
                          "no open GitHub user authorization transaction for \
                           this principal, source, and flow"
                    | Ok (Some tx) -> Ok tx)
              in
              match loaded with
              | Error e -> Error e
              | Ok tx -> (
                  match maybe_expire_open ~db ~tx ~now with
                  | Error e -> Error e
                  | Ok tx ->
                      if status_is_terminal tx.status then
                        Error
                          (Printf.sprintf
                             "authorization transaction is terminal \
                              (status=%s); terminal states never reopen"
                             (string_of_status tx.status))
                      else if tx.flow_kind <> flow_kind then
                        Error "flow_kind mismatch"
                      else if not (context_matches tx context) then
                        Error
                          "swapped context: principal, connector actor, \
                           source, app, or base_revision does not match the \
                           bound authorization transaction"
                      else Ok tx))))

let cancel ~db ~id ~(context : bound_context) ?reason
    ?(now = Unix.gettimeofday ()) () =
  match get ~db ~id with
  | Error e -> Error e
  | Ok None ->
      Error (Printf.sprintf "authorization transaction not found: %s" id)
  | Ok (Some tx) -> (
      match maybe_expire_open ~db ~tx ~now with
      | Error e -> Error e
      | Ok tx -> (
          if status_is_terminal tx.status then
            Error
              (Printf.sprintf
                 "cannot cancel terminal authorization transaction (status=%s)"
                 (string_of_status tx.status))
          else if not (context_matches tx context) then
            Error
              "swapped context: cannot cancel another principal's or \
               mismatched authorization transaction"
          else
            let reason =
              match reason with
              | Some r when String.trim r <> "" -> String.trim r
              | _ -> "cancelled by principal"
            in
            let cancelled_at = Time_util.iso8601_utc ~t:now () in
            match
              mark_status ~db ~id:tx.id ~status:Cancelled
                ~terminal_reason:reason ~cancelled_at ~now ()
            with
            | Error e -> Error e
            | Ok updated_at ->
                Ok
                  {
                    tx with
                    status = Cancelled;
                    terminal_reason = Some reason;
                    cancelled_at = Some cancelled_at;
                    updated_at;
                  }))

let expire ~db ~id ?(now = Unix.gettimeofday ()) () =
  match get ~db ~id with
  | Error e -> Error e
  | Ok None ->
      Error (Printf.sprintf "authorization transaction not found: %s" id)
  | Ok (Some tx) -> (
      if status_is_terminal tx.status then
        Error
          (Printf.sprintf
             "cannot expire terminal authorization transaction (status=%s)"
             (string_of_status tx.status))
      else if not (is_expired ~now tx) then
        Error "authorization transaction is not yet expired"
      else
        match
          mark_status ~db ~id:tx.id ~status:Expired
            ~terminal_reason:"authorization transaction expired" ~now ()
        with
        | Error e -> Error e
        | Ok updated_at ->
            Ok
              {
                tx with
                status = Expired;
                terminal_reason = Some "authorization transaction expired";
                updated_at;
              })

let complete ~db ~id ~(context : bound_context) ~one_time_state
    ?(now = Unix.gettimeofday ()) () =
  if String.trim one_time_state = "" then Error "one_time_state is required"
  else
    match get ~db ~id with
    | Error e -> Error e
    | Ok None ->
        Error (Printf.sprintf "authorization transaction not found: %s" id)
    | Ok (Some tx) -> (
        if
          (* Replay against already-completed: terminal, never reopen. *)
          tx.status = Completed
          && String.equal tx.one_time_state (String.trim one_time_state)
        then
          Error
            "authorization transaction already completed (replay); terminal \
             states never reopen"
        else if status_is_terminal tx.status then
          Error
            (Printf.sprintf
               "authorization transaction is terminal (status=%s); cannot \
                complete"
               (string_of_status tx.status))
        else
          match maybe_expire_open ~db ~tx ~now with
          | Error e -> Error e
          | Ok tx when status_is_terminal tx.status ->
              Error
                (Printf.sprintf
                   "authorization transaction is terminal (status=%s); cannot \
                    complete"
                   (string_of_status tx.status))
          | Ok tx -> (
              if
                not
                  (String.equal tx.one_time_state (String.trim one_time_state))
              then Error "one_time_state mismatch"
              else if not (context_matches tx context) then
                (* Matching one-time state with swapped context: fail closed and
                   terminate the transaction so it cannot be reused. *)
                let reason =
                  "rejected: swapped context with valid one-time state"
                in
                match
                  mark_status ~db ~id:tx.id ~status:Rejected
                    ~terminal_reason:reason ~now ()
                with
                | Error e -> Error e
                | Ok _ ->
                    Error
                      "swapped context: principal, connector actor, source, \
                       app, or base_revision does not match; transaction \
                       rejected"
              else
                let completed_at = Time_util.iso8601_utc ~t:now () in
                match
                  mark_status ~db ~id:tx.id ~status:Completed
                    ~terminal_reason:"completed" ~completed_at ~now ()
                with
                | Error _ ->
                    (* CAS lost: competing completion or concurrent cancel/expire. *)
                    Error
                      "competing completion: authorization transaction is no \
                       longer open"
                | Ok updated_at ->
                    Ok
                      {
                        tx with
                        status = Completed;
                        terminal_reason = Some "completed";
                        completed_at = Some completed_at;
                        updated_at;
                      }))

let redacted_summary (tx : t) =
  let intended =
    match
      (tx.intended_account.github_user_id, tx.intended_account.login_hint)
    with
    | None, None -> "(none)"
    | Some id, None -> Printf.sprintf "user_id=%Ld" id
    | None, Some h -> "login_hint=" ^ h
    | Some id, Some h -> Printf.sprintf "user_id=%Ld login_hint=%s" id h
  in
  String.concat "\n"
    [
      "GitHub user authorization";
      Printf.sprintf "  status: %s" (string_of_status tx.status);
      Printf.sprintf "  id: %s" tx.id;
      Printf.sprintf "  flow: %s" (string_of_flow_kind tx.flow_kind);
      Printf.sprintf "  principal: %s" tx.principal_id;
      Printf.sprintf "  actor: %s" (actor_key_string tx);
      Printf.sprintf "  source: %s" (string_of_source tx.source);
      Printf.sprintf "  host: %s app_id: %d" tx.app.host tx.app.app_id;
      Printf.sprintf "  client_id_handle: %s" tx.app.client_id_handle;
      Printf.sprintf "  intended_account: %s" intended;
      Printf.sprintf "  base_revision: %s" tx.base_revision;
      Printf.sprintf "  continuation_handle: %s" tx.continuation_handle;
      Printf.sprintf "  expires_at: %s" tx.expires_at;
      (match tx.terminal_reason with
      | None -> "  terminal_reason: (none)"
      | Some r -> "  terminal_reason: " ^ r);
      "  (one_time_state and secrets are never printed)";
    ]
