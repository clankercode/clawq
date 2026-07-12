(* Resumable GitHub App manifest setup transactions.
   See github_app_setup_tx.mli and docs/plans/2026-07-12-github-item-room-routing.md. *)

type principal = { id : string; kind : string; label : string option }
type bind_target = Room of string | Session of string
type repo_selection = All_repos | Selected of string list

type requested_scope = {
  org : string option;
  selection : repo_selection;
  permissions : (string * string) list;
  events : string list;
}

type status = Open | Consumed | Expired | Superseded

type t = {
  id : string;
  principal : principal;
  bind : bind_target;
  scope : requested_scope;
  base_revision : string;
  state : string;
  manifest_url : string;
  manifest_json : Yojson.Safe.t;
  public_base_url : string;
  created_at : string;
  expires_at : string;
  status : status;
}

let default_ttl_seconds = 1800.0
let default_hook_path = "/github/app/webhook"
let default_callback_path = "/github/app/setup/callback"

(* Conservative defaults for live App routes: PR/Issue/CI ingress plus
   installation scope reconciliation (T003/T004). *)
let default_permissions =
  [
    ("metadata", "read");
    ("contents", "read");
    ("issues", "write");
    ("pull_requests", "write");
    ("checks", "write");
    ("statuses", "read");
    ("actions", "read");
  ]

let default_events =
  [
    "issues";
    "issue_comment";
    "pull_request";
    "pull_request_review";
    "pull_request_review_comment";
    "check_run";
    "check_suite";
    "workflow_run";
    "installation";
    "installation_repositories";
  ]

let ensure_rng_initialized = lazy (Mirage_crypto_rng_unix.use_default ())

let generate_state () =
  Lazy.force ensure_rng_initialized;
  let raw = Mirage_crypto_rng.generate 32 in
  Digestif.SHA256.(digest_string raw |> to_hex)

let generate_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghapp_tx_%d_%06d" ts rand

let status_to_string = function
  | Open -> "open"
  | Consumed -> "consumed"
  | Expired -> "expired"
  | Superseded -> "superseded"

let status_of_string = function
  | "open" -> Ok Open
  | "consumed" -> Ok Consumed
  | "expired" -> Ok Expired
  | "superseded" -> Ok Superseded
  | s -> Error (Printf.sprintf "unknown github_app_setup_tx status: %s" s)

let bind_kind_and_id = function
  | Room id -> ("room", id)
  | Session id -> ("session", id)

let bind_of_kind_id kind id =
  match kind with
  | "room" -> Ok (Room id)
  | "session" -> Ok (Session id)
  | s -> Error (Printf.sprintf "unknown bind kind: %s" s)

let bind_to_string = function
  | Room id -> "room:" ^ id
  | Session id -> "session:" ^ id

let string_of_bind = bind_to_string

let trim_trailing_slash s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '/' then String.sub s 0 (len - 1) else s

let join_url base path =
  let base = trim_trailing_slash base in
  let path =
    if String.length path > 0 && path.[0] = '/' then path else "/" ^ path
  in
  base ^ path

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let permissions_to_json (perms : (string * string) list) : Yojson.Safe.t =
  `Assoc (sort_assoc (List.map (fun (k, v) -> (k, `String v)) perms))

let events_to_json events = `List (List.map (fun e -> `String e) events)

let selection_to_json = function
  | All_repos -> `String "all"
  | Selected repos -> `List (List.map (fun r -> `String r) repos)

let selection_of_json = function
  | `String "all" -> Ok All_repos
  | `List items ->
      let rec loop acc = function
        | [] -> Ok (Selected (List.rev acc))
        | `String r :: rest -> loop (r :: acc) rest
        | _ -> Error "invalid selected repo list"
      in
      loop [] items
  | _ -> Error "invalid repo selection"

let scope_to_json (s : requested_scope) : Yojson.Safe.t =
  let org = match s.org with None -> [] | Some o -> [ ("org", `String o) ] in
  `Assoc
    (sort_assoc
       (org
       @ [
           ("selection", selection_to_json s.selection);
           ("permissions", permissions_to_json s.permissions);
           ("events", events_to_json s.events);
         ]))

let scope_of_json (j : Yojson.Safe.t) : (requested_scope, string) result =
  let open Yojson.Safe.Util in
  try
    let org =
      match member "org" j with
      | `Null -> None
      | `String o -> Some o
      | _ -> None
    in
    let selection =
      match selection_of_json (member "selection" j) with
      | Ok s -> s
      | Error e -> failwith e
    in
    let permissions =
      match member "permissions" j with
      | `Assoc fields ->
          List.map
            (fun (k, v) ->
              match v with
              | `String level -> (k, level)
              | _ -> failwith ("permission value must be string: " ^ k))
            fields
      | _ -> failwith "permissions must be object"
    in
    let events =
      match member "events" j with
      | `List items ->
          List.map
            (function `String e -> e | _ -> failwith "event must be string")
            items
      | _ -> failwith "events must be array"
    in
    Ok { org; selection; permissions; events }
  with
  | Failure msg -> Error msg
  | Yojson.Json_error msg -> Error msg
  | _ -> Error "invalid requested_scope json"

let build_manifest_json ~app_name ~public_base_url ?description
    ?(hook_path = default_hook_path) ?(callback_path = default_callback_path)
    ?url ~permissions ~events () =
  let base = trim_trailing_slash public_base_url in
  let homepage = match url with Some u -> u | None -> base in
  let hook_url = join_url base hook_path in
  let callback_url = join_url base callback_path in
  let fields =
    [
      ("name", `String app_name);
      ("url", `String homepage);
      ("hook_url", `String hook_url);
      ("redirect_url", `String callback_url);
      ("callback_urls", `List [ `String callback_url ]);
      ("public", `Bool false);
      ("default_permissions", permissions_to_json permissions);
      ("default_events", events_to_json events);
    ]
  in
  let fields =
    match description with
    | None -> fields
    | Some d -> fields @ [ ("description", `String d) ]
  in
  `Assoc (sort_assoc fields)

let build_manifest_url ?org ~state ~manifest_json () =
  let path =
    match org with
    | None | Some "" -> "/settings/apps/new"
    | Some org_name ->
        Printf.sprintf "/organizations/%s/settings/apps/new" org_name
  in
  let manifest_str = Yojson.Safe.to_string manifest_json in
  Uri.make ~scheme:"https" ~host:"github.com" ~path
    ~query:[ ("state", [ state ]); ("manifest", [ manifest_str ]) ]
    ()
  |> Uri.to_string

let default_scope ?org ?(selection = All_repos) () : requested_scope =
  { org; selection; permissions = default_permissions; events = default_events }

let is_expired ?(now = Unix.gettimeofday ()) (tx : t) =
  let now_s = Time_util.iso8601_utc ~t:now () in
  String.compare now_s tx.expires_at > 0

let ensure_schema db =
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_app_setup_tx (
      id TEXT PRIMARY KEY NOT NULL,
      principal_id TEXT NOT NULL,
      principal_kind TEXT NOT NULL,
      principal_label TEXT,
      bind_kind TEXT NOT NULL,
      bind_id TEXT NOT NULL,
      scope_json TEXT NOT NULL,
      base_revision TEXT NOT NULL,
      state TEXT NOT NULL UNIQUE,
      manifest_url TEXT NOT NULL,
      manifest_json TEXT NOT NULL,
      public_base_url TEXT NOT NULL,
      created_at TEXT NOT NULL,
      expires_at TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'open',
      updated_at TEXT NOT NULL
    )|}
  in
  let idx_bind =
    {|CREATE INDEX IF NOT EXISTS idx_github_app_setup_tx_principal_bind
      ON github_app_setup_tx(principal_id, bind_kind, bind_id, status)|}
  in
  let idx_state =
    {|CREATE INDEX IF NOT EXISTS idx_github_app_setup_tx_state
      ON github_app_setup_tx(state)|}
  in
  List.iter
    (fun sql ->
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf "github_app_setup_tx schema error: %s"
               (Sqlite3.Rc.to_string rc)))
    [ table_sql; idx_bind; idx_state ]

let row_to_tx ~id ~principal_id ~principal_kind ~principal_label ~bind_kind
    ~bind_id ~scope_json ~base_revision ~state ~manifest_url ~manifest_json_s
    ~public_base_url ~created_at ~expires_at ~status_s =
  match status_of_string status_s with
  | Error e -> Error e
  | Ok status -> (
      match bind_of_kind_id bind_kind bind_id with
      | Error e -> Error e
      | Ok bind -> (
          match scope_of_json (Yojson.Safe.from_string scope_json) with
          | Error e -> Error e
          | Ok scope -> (
              try
                let manifest_json = Yojson.Safe.from_string manifest_json_s in
                Ok
                  {
                    id;
                    principal =
                      {
                        id = principal_id;
                        kind = principal_kind;
                        label = principal_label;
                      };
                    bind;
                    scope;
                    base_revision;
                    state;
                    manifest_url;
                    manifest_json;
                    public_base_url;
                    created_at;
                    expires_at;
                    status;
                  }
              with Yojson.Json_error msg -> Error msg)))

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | _ -> ""

let opt_text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None

let load_from_stmt stmt =
  row_to_tx ~id:(text_col stmt 0) ~principal_id:(text_col stmt 1)
    ~principal_kind:(text_col stmt 2) ~principal_label:(opt_text_col stmt 3)
    ~bind_kind:(text_col stmt 4) ~bind_id:(text_col stmt 5)
    ~scope_json:(text_col stmt 6) ~base_revision:(text_col stmt 7)
    ~state:(text_col stmt 8) ~manifest_url:(text_col stmt 9)
    ~manifest_json_s:(text_col stmt 10) ~public_base_url:(text_col stmt 11)
    ~created_at:(text_col stmt 12) ~expires_at:(text_col stmt 13)
    ~status_s:(text_col stmt 14)

let select_columns =
  {|id, principal_id, principal_kind, principal_label, bind_kind, bind_id,
    scope_json, base_revision, state, manifest_url, manifest_json,
    public_base_url, created_at, expires_at, status|}

let get ~db ~id =
  let sql =
    Printf.sprintf "SELECT %s FROM github_app_setup_tx WHERE id = ? LIMIT 1"
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
          (Printf.sprintf "github_app_setup_tx get failed: %s"
             (Sqlite3.Rc.to_string rc))
  in
  ignore (Sqlite3.finalize stmt);
  result

let find_by_state ~db ~state =
  let sql =
    Printf.sprintf "SELECT %s FROM github_app_setup_tx WHERE state = ? LIMIT 1"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT state));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match load_from_stmt stmt with
        | Ok tx -> Ok (Some tx)
        | Error e -> Error e)
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (Printf.sprintf "github_app_setup_tx find_by_state failed: %s"
             (Sqlite3.Rc.to_string rc))
  in
  ignore (Sqlite3.finalize stmt);
  result

let set_status ~db ~id ~status ?(now = Unix.gettimeofday ()) () =
  let updated_at = Time_util.iso8601_utc ~t:now () in
  let sql =
    {|UPDATE github_app_setup_tx SET status = ?, updated_at = ? WHERE id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT (status_to_string status)));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT updated_at));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT id));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "github_app_setup_tx set_status failed: %s"
           (Sqlite3.Rc.to_string rc))

let supersede_open ~db ~principal_id ~bind ?(now = Unix.gettimeofday ()) () =
  let bind_kind, bind_id = bind_kind_and_id bind in
  let updated_at = Time_util.iso8601_utc ~t:now () in
  let sql =
    {|UPDATE github_app_setup_tx
      SET status = 'superseded', updated_at = ?
      WHERE principal_id = ? AND bind_kind = ? AND bind_id = ?
        AND status = 'open'|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT updated_at));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT principal_id));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT bind_kind));
  ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT bind_id));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "github_app_setup_tx supersede failed: %s"
           (Sqlite3.Rc.to_string rc))

let insert_tx ~db (tx : t) =
  let bind_kind, bind_id = bind_kind_and_id tx.bind in
  let updated_at = tx.created_at in
  let sql =
    {|INSERT INTO github_app_setup_tx
      (id, principal_id, principal_kind, principal_label, bind_kind, bind_id,
       scope_json, base_revision, state, manifest_url, manifest_json,
       public_base_url, created_at, expires_at, status, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i v = ignore (Sqlite3.bind stmt i v) in
  bind 1 (Sqlite3.Data.TEXT tx.id);
  bind 2 (Sqlite3.Data.TEXT tx.principal.id);
  bind 3 (Sqlite3.Data.TEXT tx.principal.kind);
  bind 4
    (match tx.principal.label with
    | None -> Sqlite3.Data.NULL
    | Some l -> Sqlite3.Data.TEXT l);
  bind 5 (Sqlite3.Data.TEXT bind_kind);
  bind 6 (Sqlite3.Data.TEXT bind_id);
  bind 7 (Sqlite3.Data.TEXT (Yojson.Safe.to_string (scope_to_json tx.scope)));
  bind 8 (Sqlite3.Data.TEXT tx.base_revision);
  bind 9 (Sqlite3.Data.TEXT tx.state);
  bind 10 (Sqlite3.Data.TEXT tx.manifest_url);
  bind 11 (Sqlite3.Data.TEXT (Yojson.Safe.to_string tx.manifest_json));
  bind 12 (Sqlite3.Data.TEXT tx.public_base_url);
  bind 13 (Sqlite3.Data.TEXT tx.created_at);
  bind 14 (Sqlite3.Data.TEXT tx.expires_at);
  bind 15 (Sqlite3.Data.TEXT (status_to_string tx.status));
  bind 16 (Sqlite3.Data.TEXT updated_at);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "github_app_setup_tx insert failed: %s"
           (Sqlite3.Rc.to_string rc))

let create ~db ~(principal : principal) ~(bind : bind_target) ~base_revision
    ~public_base_url ?(app_name = "Clawq") ?description
    ?(scope : requested_scope option) ?(ttl_seconds = default_ttl_seconds)
    ?(now = Unix.gettimeofday ()) ?id ?state () =
  if String.trim principal.id = "" then Error "principal.id is required"
  else if String.trim public_base_url = "" then
    Error "public_base_url is required"
  else if String.trim base_revision = "" then Error "base_revision is required"
  else
    let scope : requested_scope =
      match scope with Some s -> s | None -> default_scope ?org:None ()
    in
    (* Fill empty permission/event lists with documented defaults. *)
    let scope =
      match scope.permissions with
      | [] -> { scope with permissions = default_permissions }
      | _ -> scope
    in
    let scope =
      match scope.events with
      | [] -> { scope with events = default_events }
      | _ -> scope
    in
    match supersede_open ~db ~principal_id:principal.id ~bind ~now () with
    | Error e -> Error e
    | Ok () -> (
        let tx_id = match id with Some i -> i | None -> generate_id ~now () in
        let state_token =
          match state with Some s -> s | None -> generate_state ()
        in
        if String.trim state_token = "" then Error "state must be non-empty"
        else
          let manifest_json =
            build_manifest_json ~app_name ~public_base_url ?description
              ~permissions:scope.permissions ~events:scope.events ()
          in
          let manifest_url =
            build_manifest_url ?org:scope.org ~state:state_token ~manifest_json
              ()
          in
          let created_at = Time_util.iso8601_utc ~t:now () in
          let expires_at = Time_util.iso8601_utc ~t:(now +. ttl_seconds) () in
          let tx : t =
            {
              id = tx_id;
              principal;
              bind;
              scope;
              base_revision;
              state = state_token;
              manifest_url;
              manifest_json;
              public_base_url = trim_trailing_slash public_base_url;
              created_at;
              expires_at;
              status = Open;
            }
          in
          match insert_tx ~db tx with Ok () -> Ok tx | Error e -> Error e)

let find_open ~db ~principal_id ~bind =
  let bind_kind, bind_id = bind_kind_and_id bind in
  let sql =
    Printf.sprintf
      {|SELECT %s FROM github_app_setup_tx
        WHERE principal_id = ? AND bind_kind = ? AND bind_id = ?
          AND status = 'open'
        ORDER BY created_at DESC LIMIT 1|}
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT principal_id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT bind_kind));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT bind_id));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match load_from_stmt stmt with
        | Ok tx -> Ok (Some tx)
        | Error e -> Error e)
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (Printf.sprintf "github_app_setup_tx find_open failed: %s"
             (Sqlite3.Rc.to_string rc))
  in
  ignore (Sqlite3.finalize stmt);
  result

let resume ~db ?id ~principal_id ~bind ?(now = Unix.gettimeofday ()) () =
  if String.trim principal_id = "" then Error "principal_id is required"
  else
    let loaded =
      match id with
      | Some tid -> (
          match get ~db ~id:tid with
          | Error e -> Error e
          | Ok None ->
              Error (Printf.sprintf "setup transaction not found: %s" tid)
          | Ok (Some tx) -> Ok tx)
      | None -> (
          match find_open ~db ~principal_id ~bind with
          | Error e -> Error e
          | Ok None ->
              Error
                "no open GitHub App setup transaction for this principal and \
                 bind target"
          | Ok (Some tx) -> Ok tx)
    in
    match loaded with
    | Error e -> Error e
    | Ok tx ->
        if tx.principal.id <> principal_id then
          Error "principal mismatch: cannot resume another actor's setup"
        else if bind_to_string tx.bind <> bind_to_string bind then
          Error "bind target mismatch"
        else if tx.status <> Open then
          Error
            (Printf.sprintf "setup transaction is not open (status=%s)"
               (status_to_string tx.status))
        else if is_expired ~now tx then (
          ignore (set_status ~db ~id:tx.id ~status:Expired ~now ());
          Error "setup transaction expired")
        else Ok tx

let mark_consumed ~db ~id ~principal_id ?(now = Unix.gettimeofday ()) () =
  match get ~db ~id with
  | Error e -> Error e
  | Ok None -> Error (Printf.sprintf "setup transaction not found: %s" id)
  | Ok (Some tx) -> (
      if tx.principal.id <> principal_id then
        Error "principal mismatch: cannot consume another actor's setup"
      else if tx.status <> Open then
        Error
          (Printf.sprintf "setup transaction is not open (status=%s)"
             (status_to_string tx.status))
      else if is_expired ~now tx then (
        ignore (set_status ~db ~id:tx.id ~status:Expired ~now ());
        Error "setup transaction expired")
      else
        (* Atomic consume: only one concurrent caller wins (status still open). *)
        let updated_at = Time_util.iso8601_utc ~t:now () in
        let sql =
          {|UPDATE github_app_setup_tx
            SET status = 'consumed', updated_at = ?
            WHERE id = ? AND status = 'open' AND principal_id = ?|}
        in
        let stmt = Sqlite3.prepare db sql in
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT updated_at));
        ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT id));
        ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT principal_id));
        let rc = Sqlite3.step stmt in
        ignore (Sqlite3.finalize stmt);
        match rc with
        | Sqlite3.Rc.DONE ->
            if Sqlite3.changes db = 0 then
              Error
                "setup transaction already consumed or no longer open \
                 (concurrent exchange)"
            else Ok { tx with status = Consumed }
        | rc ->
            Error
              (Printf.sprintf "github_app_setup_tx mark_consumed failed: %s"
                 (Sqlite3.Rc.to_string rc)))

let channel_render (tx : t) =
  (* Channel-safe: public metadata + exact browser URL only.
     Never include private_key, client_secret, webhook_secret, or PEM material.
     Manifest JSON body is secret-free by construction and is omitted from chat
     (summary of permissions/events only). The one-time state appears only as
     the state= query on the manifest URL (required for the browser flow); it is
     not a GitHub secret and is not logged as a standalone field. *)
  let bind_s = bind_to_string tx.bind in
  let org_s =
    match tx.scope.org with None -> "(user account)" | Some o -> o
  in
  let selection_s =
    match tx.scope.selection with
    | All_repos -> "all repositories"
    | Selected repos ->
        Printf.sprintf "selected (%d): %s" (List.length repos)
          (String.concat ", " repos)
  in
  let perms =
    tx.scope.permissions
    |> List.map (fun (k, v) -> k ^ ":" ^ v)
    |> String.concat ", "
  in
  let events = String.concat ", " tx.scope.events in
  let status_s = status_to_string tx.status in
  String.concat "\n"
    [
      "GitHub App setup";
      Printf.sprintf "  status: %s" status_s;
      Printf.sprintf "  id: %s" tx.id;
      Printf.sprintf "  bind: %s" bind_s;
      Printf.sprintf "  org: %s" org_s;
      Printf.sprintf "  selection: %s" selection_s;
      Printf.sprintf "  base_revision: %s" tx.base_revision;
      Printf.sprintf "  expires_at: %s" tx.expires_at;
      Printf.sprintf "  permissions: %s" perms;
      Printf.sprintf "  events: %s" events;
      "  open this URL in a browser to create the App:";
      Printf.sprintf "  %s" tx.manifest_url;
      "  secrets (private key / client secret / webhook secret) are never";
      "  posted here; they are stored via the credential boundary after \
       callback.";
    ]
