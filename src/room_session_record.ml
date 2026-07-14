(** Room session record assembler.

    Assembles an immutable snapshot of a room-origin work session at the moment
    work begins. Each record captures:

    - Effective access snapshot reference (tools, grants, bundles)
    - Room agent config (profile model, prompt digest, tool policy)
    - Connector context (connector, room, thread, requester)
    - Delivery state (last known room notification status)
    - Transcript and session links for drill-down

    Records are immutable once persisted. Queryable by room_id, session_key,
    config_hash, or access_snapshot_id. *)

(** {1 Room agent config} *)

type room_agent_config = {
  profile_id : string;
  display_name : string option;
  model : string;
  system_prompt_digest : string;
  max_tool_iterations : int;
  status : string;
  allowed_tools : string list;
  denied_tools : string list;
  access_bundle_ids : string list;
  ambient_enabled : bool;
  ambient_quiet_start : int;
  ambient_quiet_end : int;
  ambient_rate_limit_rph : int;
  low_volume : bool;
}
(** Immutable snapshot of the room profile configuration at record creation.

    - [profile_id] is the config-level profile identifier.
    - [display_name] is the optional human-readable profile name.
    - [model] is the model identifier (e.g. "openai:gpt-5.4").
    - [system_prompt_digest] is the SHA-256 hex digest of the system prompt.
    - [max_tool_iterations] is the configured iteration cap.
    - [status] is the profile status string ("active", "disabled", etc.).
    - [allowed_tools] / [denied_tools] are the profile-level tool lists.
    - [ambient_enabled] indicates whether ambient work is allowed.
    - [low_volume] indicates quiet presentation (suppress tool/cron chatter). *)

let system_prompt_digest (prompt : string) : string =
  Digestif.SHA256.(digest_string prompt |> to_hex)

let config_of_room_profile (p : Runtime_config.room_profile) : room_agent_config
    =
  {
    profile_id = p.id;
    display_name = p.display_name;
    model = p.model;
    system_prompt_digest = system_prompt_digest p.system_prompt;
    max_tool_iterations = p.max_tool_iterations;
    status = p.status;
    allowed_tools = p.allowed_tools;
    denied_tools = p.denied_tools;
    access_bundle_ids = p.access_bundle_ids;
    ambient_enabled = p.ambient_enabled;
    ambient_quiet_start = p.ambient_quiet_start;
    ambient_quiet_end = p.ambient_quiet_end;
    ambient_rate_limit_rph = p.ambient_rate_limit_rph;
    low_volume = p.low_volume;
  }

(** {1 Connector context} *)

type connector_context = {
  connector : string option;
  workspace_id : string option;
  room_id : string option;
  requester_id : string option;
  requester_name : string option;
  source_message_id : string option;
  thread_id : string option;
  service_url : string option;
  profile_id : int option;
}
(** Immutable snapshot of the connector/room origin at record creation.

    Mirrors [Room_origin.t] fields. All fields are optional so the type works
    for CLI and API origins that lack connector context. *)

let connector_context_of_origin (o : Room_origin.t) : connector_context =
  {
    connector = o.connector;
    workspace_id = o.workspace_id;
    room_id = o.room_id;
    requester_id = o.requester_id;
    requester_name = o.requester_name;
    source_message_id = o.source_message_id;
    thread_id = o.thread_id;
    service_url = o.service_url;
    profile_id = o.profile_id;
  }

let empty_connector_context : connector_context =
  {
    connector = None;
    workspace_id = None;
    room_id = None;
    requester_id = None;
    requester_name = None;
    source_message_id = None;
    thread_id = None;
    service_url = None;
    profile_id = None;
  }

(** {1 Delivery state snapshot} *)

type delivery_snapshot = {
  state : string;
  last_update : string;
  message_id : string option;
  error_detail : string option;
}
(** Snapshot of the last known delivery state for the room session.

    - [state] is a string encoding of [Delivery_types.delivery_state].
    - [last_update] is the ISO-8601 timestamp of the last state change.
    - [message_id] is the connector-assigned message identifier, if known.
    - [error_detail] is the extended error description for failed deliveries. *)

(** {1 Session record} *)

type t = {
  id : string;
  created_at : string;
  session_key : string option;
  room_id : string option;
  config_hash : string;
  access_snapshot_id : string;
  agent_config : room_agent_config option;
  connector_context : connector_context;
  delivery : delivery_snapshot option;
  transcript_url : string option;
  session_url : string option;
}
(** An immutable room session record.

    - [id] is the unique record identifier.
    - [created_at] is the ISO-8601 creation timestamp.
    - [session_key] is the full session key (e.g. "slack:C123:U456").
    - [room_id] is the extracted room/channel identifier.
    - [config_hash] is the SHA-256 of the runtime config at creation.
    - [access_snapshot_id] references the [Access_snapshot.t] persisted row.
    - [agent_config] is the room profile config snapshot, if a profile matched.
    - [connector_context] captures the connector/room origin metadata.
    - [delivery] is the last known delivery state, if any.
    - [transcript_url] / [session_url] are optional drill-down links. *)

(** {1 ID generation} *)

let generate_id () =
  let ts = int_of_float (Unix.gettimeofday ()) in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "rsr_%d_%06d" ts rand

let timestamp_now () = Time_util.iso8601_utc_micros ()

(** {1 Schema} *)

let init_schema db =
  let exec sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc ->
        failwith
          (Printf.sprintf "room_session_record schema error: %s (sql: %s)"
             (Sqlite3.Rc.to_string rc) sql)
  in
  exec
    "CREATE TABLE IF NOT EXISTS room_session_records (\n\
    \     id TEXT PRIMARY KEY,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     session_key TEXT,\n\
    \     room_id TEXT,\n\
    \     config_hash TEXT NOT NULL,\n\
    \     access_snapshot_id TEXT NOT NULL,\n\
    \     agent_config_json TEXT,\n\
    \     connector_context_json TEXT NOT NULL DEFAULT '{}',\n\
    \     delivery_json TEXT,\n\
    \     transcript_url TEXT,\n\
    \     session_url TEXT\n\
    \   )";
  exec
    "CREATE INDEX IF NOT EXISTS idx_rsr_room ON room_session_records(room_id)";
  exec
    "CREATE INDEX IF NOT EXISTS idx_rsr_session_key ON \
     room_session_records(session_key)";
  exec
    "CREATE INDEX IF NOT EXISTS idx_rsr_config_hash ON \
     room_session_records(config_hash)";
  exec
    "CREATE INDEX IF NOT EXISTS idx_rsr_snapshot ON \
     room_session_records(access_snapshot_id)";
  exec
    "CREATE INDEX IF NOT EXISTS idx_rsr_created ON \
     room_session_records(created_at)"

(** {1 JSON helpers} *)

let json_of_opt_string = function None -> `Null | Some s -> `String s

let json_of_agent_config (c : room_agent_config) : Yojson.Safe.t =
  `Assoc
    [
      ("profile_id", `String c.profile_id);
      ("display_name", json_of_opt_string c.display_name);
      ("model", `String c.model);
      ("system_prompt_digest", `String c.system_prompt_digest);
      ("max_tool_iterations", `Int c.max_tool_iterations);
      ("status", `String c.status);
      ("allowed_tools", `List (List.map (fun s -> `String s) c.allowed_tools));
      ("denied_tools", `List (List.map (fun s -> `String s) c.denied_tools));
      ( "access_bundle_ids",
        `List (List.map (fun s -> `String s) c.access_bundle_ids) );
      ("ambient_enabled", `Bool c.ambient_enabled);
      ("ambient_quiet_start", `Int c.ambient_quiet_start);
      ("ambient_quiet_end", `Int c.ambient_quiet_end);
      ("ambient_rate_limit_rph", `Int c.ambient_rate_limit_rph);
      ("low_volume", `Bool c.low_volume);
    ]

let agent_config_of_json (json : Yojson.Safe.t) :
    (room_agent_config, string) result =
  match json with
  | `Assoc pairs -> (
      match List.assoc_opt "profile_id" pairs with
      | Some (`String profile_id) ->
          let opt_str key =
            match List.assoc_opt key pairs with
            | Some (`String s) when s <> "" -> Some s
            | _ -> None
          in
          let str_list key =
            match List.assoc_opt key pairs with
            | Some (`List items) ->
                List.filter_map
                  (function `String s -> Some s | _ -> None)
                  items
            | _ -> []
          in
          let int_val key default =
            match List.assoc_opt key pairs with
            | Some (`Int n) -> n
            | _ -> default
          in
          let bool_val key default =
            match List.assoc_opt key pairs with
            | Some (`Bool b) -> b
            | _ -> default
          in
          Ok
            {
              profile_id;
              display_name = opt_str "display_name";
              model = (match opt_str "model" with Some m -> m | None -> "");
              system_prompt_digest =
                (match opt_str "system_prompt_digest" with
                | Some d -> d
                | None -> "");
              max_tool_iterations = int_val "max_tool_iterations" 25;
              status =
                (match opt_str "status" with Some s -> s | None -> "active");
              allowed_tools = str_list "allowed_tools";
              denied_tools = str_list "denied_tools";
              access_bundle_ids = str_list "access_bundle_ids";
              ambient_enabled = bool_val "ambient_enabled" false;
              ambient_quiet_start = int_val "ambient_quiet_start" 0;
              ambient_quiet_end = int_val "ambient_quiet_end" 0;
              ambient_rate_limit_rph = int_val "ambient_rate_limit_rph" 0;
              low_volume = bool_val "low_volume" false;
            }
      | _ -> Error "agent_config: missing profile_id")
  | _ -> Error "agent_config: expected JSON object"

let json_of_connector_context (c : connector_context) : Yojson.Safe.t =
  `Assoc
    [
      ("connector", json_of_opt_string c.connector);
      ("workspace_id", json_of_opt_string c.workspace_id);
      ("room_id", json_of_opt_string c.room_id);
      ("requester_id", json_of_opt_string c.requester_id);
      ("requester_name", json_of_opt_string c.requester_name);
      ("source_message_id", json_of_opt_string c.source_message_id);
      ("thread_id", json_of_opt_string c.thread_id);
      ("service_url", json_of_opt_string c.service_url);
      ("profile_id", match c.profile_id with Some n -> `Int n | None -> `Null);
    ]

let connector_context_of_json (json : Yojson.Safe.t) :
    (connector_context, string) result =
  match json with
  | `Assoc pairs ->
      let opt_str key =
        match List.assoc_opt key pairs with
        | Some (`String s) when s <> "" -> Some s
        | _ -> None
      in
      let opt_int key =
        match List.assoc_opt key pairs with
        | Some (`Int n) -> Some n
        | _ -> None
      in
      Ok
        {
          connector = opt_str "connector";
          workspace_id = opt_str "workspace_id";
          room_id = opt_str "room_id";
          requester_id = opt_str "requester_id";
          requester_name = opt_str "requester_name";
          source_message_id = opt_str "source_message_id";
          thread_id = opt_str "thread_id";
          service_url = opt_str "service_url";
          profile_id = opt_int "profile_id";
        }
  | `Null -> Ok empty_connector_context
  | _ -> Error "connector_context: expected JSON object or null"

let json_of_delivery (d : delivery_snapshot) : Yojson.Safe.t =
  let fields =
    [ ("state", `String d.state); ("last_update", `String d.last_update) ]
    @ (match d.message_id with
      | Some id -> [ ("message_id", `String id) ]
      | None -> [])
    @
    match d.error_detail with
    | Some detail -> [ ("error_detail", `String detail) ]
    | None -> []
  in
  `Assoc fields

let delivery_of_json (json : Yojson.Safe.t) : (delivery_snapshot, string) result
    =
  match json with
  | `Assoc pairs -> (
      match List.assoc_opt "state" pairs with
      | Some (`String state) ->
          let last_update =
            match List.assoc_opt "last_update" pairs with
            | Some (`String s) -> s
            | _ -> ""
          in
          let opt_str key =
            match List.assoc_opt key pairs with
            | Some (`String s) when s <> "" -> Some s
            | _ -> None
          in
          Ok
            {
              state;
              last_update;
              message_id = opt_str "message_id";
              error_detail = opt_str "error_detail";
            }
      | _ -> Error "delivery: missing state")
  | `Null -> Error "delivery: null"
  | _ -> Error "delivery: expected JSON object"

(** {1 Record creation} *)

let create ~(config : Runtime_config.t) ~(access_snapshot_id : string)
    ?(origin : Room_origin.t option) ?(delivery : delivery_snapshot option)
    ?transcript_url ?session_url ?session_key ?room_id () : t =
  let ts = timestamp_now () in
  let derived_room_id =
    match room_id with
    | Some _ -> room_id
    | None -> ( match origin with Some o -> o.room_id | None -> None)
  in
  let resolved_session_key =
    Access_snapshot.access_session_key ?session_key derived_room_id
  in
  (* Resolve room profile for agent config snapshot *)
  let agent_config =
    let profile =
      Runtime_config.resolve_room_profile config
        ~session_key:resolved_session_key
    in
    Option.map config_of_room_profile profile
  in
  let connector_ctx =
    match origin with
    | Some o -> connector_context_of_origin o
    | None -> empty_connector_context
  in
  {
    id = generate_id ();
    created_at = ts;
    session_key;
    room_id = derived_room_id;
    config_hash = Access_snapshot.config_hash config;
    access_snapshot_id;
    agent_config;
    connector_context = connector_ctx;
    delivery;
    transcript_url;
    session_url;
  }

(** {1 Persistence} *)

let persist ~(db : Sqlite3.db) (record : t) =
  let stmt =
    Sqlite3.prepare db
      "INSERT INTO room_session_records (id, created_at, session_key, room_id, \
       config_hash, access_snapshot_id, agent_config_json, \
       connector_context_json, delivery_json, transcript_url, session_url) \
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind_text pos value =
        ignore (Sqlite3.bind stmt pos (Sqlite3.Data.TEXT value))
      in
      let bind_opt_text pos = function
        | None -> ignore (Sqlite3.bind stmt pos Sqlite3.Data.NULL)
        | Some v -> bind_text pos v
      in
      bind_text 1 record.id;
      bind_text 2 record.created_at;
      bind_opt_text 3 record.session_key;
      bind_opt_text 4 record.room_id;
      bind_text 5 record.config_hash;
      bind_text 6 record.access_snapshot_id;
      (match record.agent_config with
      | Some cfg ->
          bind_text 7 (Yojson.Safe.to_string (json_of_agent_config cfg))
      | None -> ignore (Sqlite3.bind stmt 7 Sqlite3.Data.NULL));
      bind_text 8
        (Yojson.Safe.to_string
           (json_of_connector_context record.connector_context));
      (match record.delivery with
      | Some d -> bind_text 9 (Yojson.Safe.to_string (json_of_delivery d))
      | None -> ignore (Sqlite3.bind stmt 9 Sqlite3.Data.NULL));
      bind_opt_text 10 record.transcript_url;
      bind_opt_text 11 record.session_url;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "room_session_record persist error: %s"
               (Sqlite3.Rc.to_string rc)))

(** {1 Row deserialization} *)

let text_column = Sql_util.opt_text_column
let text_column_nn = Sql_util.text_column

let json_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.TEXT s -> (
      try Some (Yojson.Safe.from_string s) with _ -> None)
  | _ -> None

let record_of_stmt stmt : t =
  let agent_config =
    match json_column stmt 6 with
    | Some json -> (
        match agent_config_of_json json with Ok c -> Some c | Error _ -> None)
    | None -> None
  in
  let connector_ctx =
    match json_column stmt 7 with
    | Some json -> (
        match connector_context_of_json json with
        | Ok c -> c
        | Error _ -> empty_connector_context)
    | None -> empty_connector_context
  in
  let delivery =
    match json_column stmt 8 with
    | Some json -> (
        match delivery_of_json json with Ok d -> Some d | Error _ -> None)
    | None -> None
  in
  {
    id = text_column_nn stmt 0;
    created_at = text_column_nn stmt 1;
    session_key = text_column stmt 2;
    room_id = text_column stmt 3;
    config_hash = text_column_nn stmt 4;
    access_snapshot_id = text_column_nn stmt 5;
    agent_config;
    connector_context = connector_ctx;
    delivery;
    transcript_url = text_column stmt 9;
    session_url = text_column stmt 10;
  }

let select_columns =
  "id, created_at, session_key, room_id, config_hash, access_snapshot_id, \
   agent_config_json, connector_context_json, delivery_json, transcript_url, \
   session_url"

(** {1 Query} *)

let get ~(db : Sqlite3.db) ~id () =
  let sql =
    Printf.sprintf "SELECT %s FROM room_session_records WHERE id = ?"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (record_of_stmt stmt)
      | Sqlite3.Rc.DONE -> None
      | rc ->
          failwith
            (Printf.sprintf "room_session_record get error: %s"
               (Sqlite3.Rc.to_string rc)))

let query ~(db : Sqlite3.db) ?room_id ?session_key ?config_hash
    ?access_snapshot_id ?(limit = 50) () =
  let conditions = ref [] in
  let bindings = ref [] in
  let add_cond cond value =
    conditions := !conditions @ [ cond ];
    bindings := !bindings @ [ value ]
  in
  (match room_id with
  | Some rid -> add_cond "room_id = ?" (Sqlite3.Data.TEXT rid)
  | None -> ());
  (match session_key with
  | Some key -> add_cond "session_key = ?" (Sqlite3.Data.TEXT key)
  | None -> ());
  (match config_hash with
  | Some hash -> add_cond "config_hash = ?" (Sqlite3.Data.TEXT hash)
  | None -> ());
  (match access_snapshot_id with
  | Some sid -> add_cond "access_snapshot_id = ?" (Sqlite3.Data.TEXT sid)
  | None -> ());
  let where =
    match !conditions with
    | [] -> ""
    | conds -> " WHERE " ^ String.concat " AND " conds
  in
  let sql =
    Printf.sprintf
      "SELECT %s FROM room_session_records%s ORDER BY created_at DESC LIMIT ?"
      select_columns where
  in
  let results = ref [] in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let all_bindings =
        !bindings @ [ Sqlite3.Data.INT (Int64.of_int (max 0 limit)) ]
      in
      List.iteri
        (fun i value -> ignore (Sqlite3.bind stmt (i + 1) value))
        all_bindings;
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            results := record_of_stmt stmt :: !results;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "room_session_record query error: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ();
      List.rev !results)

let get_latest_for_room ~(db : Sqlite3.db) ~room_id () =
  let sql =
    Printf.sprintf
      "SELECT %s FROM room_session_records WHERE room_id = ? ORDER BY \
       created_at DESC LIMIT 1"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (record_of_stmt stmt)
      | Sqlite3.Rc.DONE -> None
      | rc ->
          failwith
            (Printf.sprintf "room_session_record get_latest_for_room error: %s"
               (Sqlite3.Rc.to_string rc)))

(** {1 JSON serialization} *)

let to_json (record : t) : Yojson.Safe.t =
  let fields =
    [
      ("id", `String record.id);
      ("created_at", `String record.created_at);
      ("config_hash", `String record.config_hash);
      ("access_snapshot_id", `String record.access_snapshot_id);
      ("connector_context", json_of_connector_context record.connector_context);
    ]
    @ (match record.session_key with
      | Some s -> [ ("session_key", `String s) ]
      | None -> [])
    @ (match record.room_id with
      | Some s -> [ ("room_id", `String s) ]
      | None -> [])
    @ (match record.agent_config with
      | Some cfg -> [ ("agent_config", json_of_agent_config cfg) ]
      | None -> [])
    @ (match record.delivery with
      | Some d -> [ ("delivery", json_of_delivery d) ]
      | None -> [])
    @ (match record.transcript_url with
      | Some url -> [ ("transcript_url", `String url) ]
      | None -> [])
    @
    match record.session_url with
    | Some url -> [ ("session_url", `String url) ]
    | None -> []
  in
  `Assoc fields

(** {1 Assembler convenience} *)

(** [assemble_and_persist ~db ~config ~access_snapshot_id ?origin ?delivery
     ?transcript_url ?session_url ?session_key ?room_id ()] creates a room
    session record, persists it, and returns the record. Wraps [create] and
    [persist] for the common one-call assembly pattern. *)
let assemble_and_persist ~(db : Sqlite3.db) ~(config : Runtime_config.t)
    ~(access_snapshot_id : string) ?origin ?delivery ?transcript_url
    ?session_url ?session_key ?room_id () =
  let record =
    create ~config ~access_snapshot_id ?origin ?delivery ?transcript_url
      ?session_url ?session_key ?room_id ()
  in
  persist ~db record;
  record

(** {1 Deletion} *)

(** [delete_before ~db ~before_timestamp ()] removes records older than the
    given timestamp. Returns the number of deleted records. *)
let delete_before ~db ~before_timestamp () =
  let stmt =
    Sqlite3.prepare db "DELETE FROM room_session_records WHERE created_at < ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT before_timestamp));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db
      | rc ->
          failwith
            (Printf.sprintf "room_session_record delete_before failed: %s"
               (Sqlite3.Rc.to_string rc)))
