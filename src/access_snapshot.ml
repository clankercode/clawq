(* Effective-access snapshots for executable work.
   Records an immutable snapshot of the resolved access policy at the moment
   work begins (room turn, background task, ambient tick, GitHub trigger,
   routine). Each snapshot is self-contained: it carries the config hash,
   resolved grants/denials, bundle sources, and a redacted summary. *)

type work_type =
  | Room_turn
  | Background_task
  | Ambient_work
  | GitHub_trigger
  | Routine

let work_type_to_string = function
  | Room_turn -> "room_turn"
  | Background_task -> "background_task"
  | Ambient_work -> "ambient_work"
  | GitHub_trigger -> "github_trigger"
  | Routine -> "routine"

let work_type_of_string = function
  | "room_turn" -> Some Room_turn
  | "background_task" -> Some Background_task
  | "ambient_work" -> Some Ambient_work
  | "github_trigger" -> Some GitHub_trigger
  | "routine" -> Some Routine
  | _ -> None

type bundle_source = { bundle_id : string; layer : string; source_id : string }

type t = {
  id : string;
  timestamp : string;
  config_hash : string;
  session_key : string option;
  work_type : work_type;
  room_id : string option;
  profile_id : string option;
  bundle_sources : bundle_source list;
  allowed_tools : string list;
  denied_tools : string list;
  codebase_grants : string list;
  blocked_codebase_grants : string list;
  mcp_servers : string list;
  skills : string list;
  repositories : string list;
  repo_grants : string list;
  blocked_repo_grants : string list;
  domains : string list;
  credential_handles : string list;
  memory_grants : string list;
  budget_refs : string list;
  egress_rules_count : int;
      (** Number of egress rules in the snapshot. Full rules are not serialized
          to avoid leaking host/path patterns. *)
  instruction_digests : string list;
  redacted_summary : string;
  room_classification : Runtime_config_types.room_scope;
      (** Classification of the room at the time work began. *)
  room_policy_decision : string;
      (** Human-readable policy decision (e.g., "allow", "warn: ...", "deny:
          ..."). Empty when no policy evaluation was performed. *)
}

let room_id_from_session_key key =
  match String.split_on_char ':' key with
  | _connector :: room_id :: _ -> Some room_id
  | _ -> None

let generate_id () =
  let ts = int_of_float (Unix.gettimeofday ()) in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "snap_%d_%06d" ts rand

let config_hash (cfg : Runtime_config.t) : string =
  let json = Runtime_config.to_json cfg in
  let text = Yojson.Safe.to_string json in
  Digestif.SHA256.(digest_string text |> to_hex)

let redact_secret value =
  let len = String.length value in
  if len <= 6 then String.make len '*'
  else String.sub value 0 3 ^ String.make (len - 3) '*'

let redacted_summary_of_access (access : Runtime_config.effective_access) :
    string =
  let tool_count = List.length access.allowed_tools in
  let deny_count = List.length access.denied_tools in
  let grant_count = List.length access.codebase_grants in
  let blocked_count = List.length access.blocked_codebase_grants in
  let repo_grant_count = List.length access.repo_grants in
  let blocked_repo_grant_count = List.length access.blocked_repo_grants in
  Printf.sprintf
    "tools:%d/%d grants:%d+%d servers:%d skills:%d repos:%d repo_grants:%d+%d \
     domains:%d credentials:%d instructions:%d"
    tool_count deny_count grant_count blocked_count
    (List.length access.mcp_servers)
    (List.length access.skills)
    (List.length access.repositories)
    repo_grant_count blocked_repo_grant_count
    (List.length access.domains)
    (List.length access.credential_handles)
    (List.length access.instructions)

let extract_bundle_sources (access : Runtime_config.effective_access) :
    bundle_source list =
  let bundle_id_from_source_id source_id =
    let marker = ":access_bundle_ids:" in
    let mlen = String.length marker in
    let slen = String.length source_id in
    let rec find i =
      if i + mlen > slen then None
      else if String.sub source_id i mlen = marker then
        Some (String.sub source_id (i + mlen) (slen - i - mlen))
      else find (i + 1)
    in
    find 0
  in
  let extract_from_provenance items field =
    List.concat_map
      (fun (item : Runtime_config.effective_access_item) ->
        List.filter_map
          (fun (p : Runtime_config.access_provenance) ->
            if p.field = field then
              Option.map
                (fun bundle_id ->
                  { bundle_id; layer = p.layer; source_id = p.source_id })
                (bundle_id_from_source_id p.source_id)
            else None)
          item.provenance)
      items
  in
  let all =
    [
      (access.allowed_tools, "allowed_tools");
      (access.denied_tools, "denied_tools");
      (access.codebase_grants, "codebase_grants");
      (access.blocked_codebase_grants, "codebase_grants");
      (access.instructions, "instructions");
      (access.mcp_servers, "mcp_servers");
      (access.skills, "skills");
      (access.repositories, "repositories");
      (access.repo_grants, "repo_grants");
      (access.blocked_repo_grants, "repo_grants");
      (access.domains, "domains");
      (access.credential_handles, "credential_handles");
      (access.memory_grants, "memory_grants");
      (access.budget_refs, "budget_refs");
    ]
    |> List.concat_map (fun (items, field) ->
        extract_from_provenance items field)
  in
  let seen = Hashtbl.create (List.length all) in
  List.filter_map
    (fun (src : bundle_source) ->
      let key = src.bundle_id ^ ":" ^ src.layer in
      if Hashtbl.mem seen key then None
      else begin
        Hashtbl.add seen key ();
        Some src
      end)
    all

let extract_instruction_digests (access : Runtime_config.effective_access) :
    string list =
  (* Prefer digests from instruction_records when available; fall back to
     computing from the text-only effective_access_item values. *)
  if access.instruction_items <> [] then
    List.map
      (fun (item : Runtime_config.effective_instruction_item) ->
        Runtime_config.instruction_record_digest item.instruction)
      access.instruction_items
  else
    List.map
      (fun (item : Runtime_config.effective_access_item) ->
        Digestif.SHA256.(digest_string item.value |> to_hex))
      access.instructions

let item_values items =
  List.map (fun (i : Runtime_config.effective_access_item) -> i.value) items

let profile_id_for_access_key (config : Runtime_config.t) access_key =
  Runtime_config.resolve_room_profile config ~session_key:access_key
  |> Option.map (fun (p : Runtime_config.room_profile) -> p.id)

let access_session_key ?session_key room_id =
  match (session_key, room_id) with
  | Some key, Some rid -> (
      match String.split_on_char ':' key with
      | connector :: _ when connector <> "" -> connector ^ ":" ^ rid
      | _ -> rid)
  | Some key, None -> key
  | None, Some rid -> rid
  | None, None -> "__anonymous__"

let create ~(config : Runtime_config.t) ~work_type ?session_key ?room_id
    ?profile_id ?room_classification ?(room_policy_decision = "") () : t =
  let derived_room_id =
    match room_id with
    | Some _ -> room_id
    | None -> Option.bind session_key room_id_from_session_key
  in
  let resolved_session_key = access_session_key ?session_key derived_room_id in
  let derived_profile_id =
    match profile_id with
    | Some _ -> profile_id
    | None -> profile_id_for_access_key config resolved_session_key
  in
  let access =
    Runtime_config.resolve_effective_access config
      ~session_key:resolved_session_key ()
  in
  {
    id = generate_id ();
    timestamp = Time_util.sql_datetime_utc ();
    config_hash = config_hash config;
    session_key;
    work_type;
    room_id = derived_room_id;
    profile_id = derived_profile_id;
    bundle_sources = extract_bundle_sources access;
    allowed_tools = item_values access.allowed_tools;
    denied_tools = item_values access.denied_tools;
    codebase_grants = item_values access.codebase_grants;
    blocked_codebase_grants = item_values access.blocked_codebase_grants;
    mcp_servers = item_values access.mcp_servers;
    skills = item_values access.skills;
    repositories = item_values access.repositories;
    repo_grants = item_values access.repo_grants;
    blocked_repo_grants = item_values access.blocked_repo_grants;
    domains = item_values access.domains;
    credential_handles = item_values access.credential_handles;
    memory_grants = item_values access.memory_grants;
    budget_refs = item_values access.budget_refs;
    egress_rules_count = List.length access.egress_rules;
    instruction_digests = extract_instruction_digests access;
    redacted_summary = redacted_summary_of_access access;
    room_classification =
      Option.value room_classification ~default:Runtime_config_types.Rm_unknown;
    room_policy_decision;
  }

let sqlite_column_exists ~db ~table_name ~column_name =
  let stmt =
    Sqlite3.prepare db (Printf.sprintf "PRAGMA table_info(%s)" table_name)
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let found = ref false in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        match Sqlite3.column stmt 1 with
        | Sqlite3.Data.TEXT s when s = column_name -> found := true
        | _ -> ()
      done;
      !found)

let ensure_json_column db column_name =
  if not (sqlite_column_exists ~db ~table_name:"access_snapshots" ~column_name)
  then
    let sql =
      Printf.sprintf
        "ALTER TABLE access_snapshots ADD COLUMN %s TEXT NOT NULL DEFAULT '[]'"
        column_name
    in
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc ->
        failwith
          (Printf.sprintf "Access_snapshot schema migration error: %s"
             (Sqlite3.Rc.to_string rc))

let ensure_int_column db column_name ~default =
  if not (sqlite_column_exists ~db ~table_name:"access_snapshots" ~column_name)
  then
    let sql =
      Printf.sprintf
        "ALTER TABLE access_snapshots ADD COLUMN %s INTEGER NOT NULL DEFAULT %d"
        column_name default
    in
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc ->
        failwith
          (Printf.sprintf "Access_snapshot schema migration error: %s"
             (Sqlite3.Rc.to_string rc))

let init_schema db =
  let sql =
    "CREATE TABLE IF NOT EXISTS access_snapshots (\n\
    \  id TEXT PRIMARY KEY,\n\
    \  timestamp TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \  config_hash TEXT NOT NULL,\n\
    \  session_key TEXT,\n\
    \  work_type TEXT NOT NULL,\n\
    \  room_id TEXT,\n\
    \  profile_id TEXT,\n\
    \  bundle_sources_json TEXT NOT NULL DEFAULT '[]',\n\
    \  allowed_tools_json TEXT NOT NULL DEFAULT '[]',\n\
    \  denied_tools_json TEXT NOT NULL DEFAULT '[]',\n\
    \  codebase_grants_json TEXT NOT NULL DEFAULT '[]',\n\
    \  blocked_codebase_grants_json TEXT NOT NULL DEFAULT '[]',\n\
    \  mcp_servers_json TEXT NOT NULL DEFAULT '[]',\n\
    \  skills_json TEXT NOT NULL DEFAULT '[]',\n\
    \  repositories_json TEXT NOT NULL DEFAULT '[]',\n\
    \  repo_grants_json TEXT NOT NULL DEFAULT '[]',\n\
    \  blocked_repo_grants_json TEXT NOT NULL DEFAULT '[]',\n\
    \  domains_json TEXT NOT NULL DEFAULT '[]',\n\
    \  credential_handles_json TEXT NOT NULL DEFAULT '[]',\n\
    \  memory_grants_json TEXT NOT NULL DEFAULT '[]',\n\
    \  budget_refs_json TEXT NOT NULL DEFAULT '[]',\n\
    \  egress_rules_count INTEGER NOT NULL DEFAULT 0,\n\
    \  instruction_digests_json TEXT NOT NULL DEFAULT '[]',\n\
    \  redacted_summary TEXT NOT NULL DEFAULT ''\n\
     )"
  in
  (match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "Access_snapshot schema error: %s"
           (Sqlite3.Rc.to_string rc)));
  List.iter (ensure_json_column db)
    [
      "mcp_servers_json";
      "skills_json";
      "repositories_json";
      "repo_grants_json";
      "blocked_repo_grants_json";
      "domains_json";
      "credential_handles_json";
      "memory_grants_json";
      "budget_refs_json";
    ];
  ensure_int_column db "egress_rules_count" ~default:0;
  ignore (ensure_json_column db "room_classification");
  ensure_json_column db "room_policy_decision"

let list_to_json strings = `List (List.map (fun s -> `String s) strings)

let bundle_source_to_json (src : bundle_source) : Yojson.Safe.t =
  `Assoc
    [
      ("bundle_id", `String src.bundle_id);
      ("layer", `String src.layer);
      ("source_id", `String src.source_id);
    ]

let snapshot_select_columns =
  "id, timestamp, config_hash, session_key, work_type, room_id, profile_id, \
   bundle_sources_json, allowed_tools_json, denied_tools_json, \
   codebase_grants_json, blocked_codebase_grants_json, mcp_servers_json, \
   skills_json, repositories_json, repo_grants_json, blocked_repo_grants_json, \
   domains_json, credential_handles_json, memory_grants_json, \
   budget_refs_json, egress_rules_count, instruction_digests_json, \
   redacted_summary, room_classification, room_policy_decision"

let persist ~(db : Sqlite3.db) (snap : t) =
  let stmt =
    Sqlite3.prepare db
      "INSERT INTO access_snapshots (id, timestamp, config_hash, session_key, \
       work_type, room_id, profile_id, bundle_sources_json, \
       allowed_tools_json, denied_tools_json, codebase_grants_json, \
       blocked_codebase_grants_json, mcp_servers_json, skills_json, \
       repositories_json, repo_grants_json, blocked_repo_grants_json, \
       domains_json, credential_handles_json, memory_grants_json, \
       budget_refs_json, egress_rules_count, instruction_digests_json, \
       redacted_summary, room_classification, room_policy_decision) VALUES (?, \
       ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, \
       ?)"
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
      bind_text 1 snap.id;
      bind_text 2 snap.timestamp;
      bind_text 3 snap.config_hash;
      bind_opt_text 4 snap.session_key;
      bind_text 5 (work_type_to_string snap.work_type);
      bind_opt_text 6 snap.room_id;
      bind_opt_text 7 snap.profile_id;
      bind_text 8
        (Yojson.Safe.to_string
           (`List (List.map bundle_source_to_json snap.bundle_sources)));
      bind_text 9 (Yojson.Safe.to_string (list_to_json snap.allowed_tools));
      bind_text 10 (Yojson.Safe.to_string (list_to_json snap.denied_tools));
      bind_text 11 (Yojson.Safe.to_string (list_to_json snap.codebase_grants));
      bind_text 12
        (Yojson.Safe.to_string (list_to_json snap.blocked_codebase_grants));
      bind_text 13 (Yojson.Safe.to_string (list_to_json snap.mcp_servers));
      bind_text 14 (Yojson.Safe.to_string (list_to_json snap.skills));
      bind_text 15 (Yojson.Safe.to_string (list_to_json snap.repositories));
      bind_text 16 (Yojson.Safe.to_string (list_to_json snap.repo_grants));
      bind_text 17
        (Yojson.Safe.to_string (list_to_json snap.blocked_repo_grants));
      bind_text 18 (Yojson.Safe.to_string (list_to_json snap.domains));
      bind_text 19
        (Yojson.Safe.to_string (list_to_json snap.credential_handles));
      bind_text 20 (Yojson.Safe.to_string (list_to_json snap.memory_grants));
      bind_text 21 (Yojson.Safe.to_string (list_to_json snap.budget_refs));
      ignore
        (Sqlite3.bind stmt 22
           (Sqlite3.Data.INT (Int64.of_int snap.egress_rules_count)));
      bind_text 23
        (Yojson.Safe.to_string (list_to_json snap.instruction_digests));
      bind_text 24 snap.redacted_summary;
      bind_text 25
        (Yojson.Safe.to_string
           (`String (Room_policy.room_scope_to_string snap.room_classification)));
      bind_text 26 snap.room_policy_decision;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "Access_snapshot persist error: %s"
               (Sqlite3.Rc.to_string rc)))

let row_of_stmt stmt : t =
  let text pos =
    match Sqlite3.column stmt pos with Sqlite3.Data.TEXT s -> s | _ -> ""
  in
  let opt_text pos =
    match Sqlite3.column stmt pos with
    | Sqlite3.Data.TEXT s -> Some s
    | Sqlite3.Data.NULL -> None
    | _ -> None
  in
  let json_list pos =
    let raw = text pos in
    try
      match Yojson.Safe.from_string raw with
      | `List items ->
          List.filter_map (function `String s -> Some s | _ -> None) items
      | _ -> []
    with _ -> []
  in
  let json_bundle_sources pos =
    let raw = text pos in
    try
      match Yojson.Safe.from_string raw with
      | `List items ->
          List.filter_map
            (fun item ->
              let open Yojson.Safe.Util in
              try
                Some
                  {
                    bundle_id = item |> member "bundle_id" |> to_string;
                    layer = item |> member "layer" |> to_string;
                    source_id = item |> member "source_id" |> to_string;
                  }
              with _ -> None)
            items
      | _ -> []
    with _ -> []
  in
  {
    id = text 0;
    timestamp = text 1;
    config_hash = text 2;
    session_key = opt_text 3;
    work_type =
      (match work_type_of_string (text 4) with
      | Some wt -> wt
      | None -> Room_turn);
    room_id = opt_text 5;
    profile_id = opt_text 6;
    bundle_sources = json_bundle_sources 7;
    allowed_tools = json_list 8;
    denied_tools = json_list 9;
    codebase_grants = json_list 10;
    blocked_codebase_grants = json_list 11;
    mcp_servers = json_list 12;
    skills = json_list 13;
    repositories = json_list 14;
    repo_grants = json_list 15;
    blocked_repo_grants = json_list 16;
    domains = json_list 17;
    credential_handles = json_list 18;
    memory_grants = json_list 19;
    budget_refs = json_list 20;
    egress_rules_count =
      (match Sqlite3.column stmt 21 with
      | Sqlite3.Data.INT n -> Int64.to_int n
      | _ -> 0);
    instruction_digests = json_list 22;
    redacted_summary = text 23;
    room_classification =
      (let raw = text 24 in
       match Yojson.Safe.from_string raw with
       | `String s -> Room_policy.room_scope_of_string s
       | _ -> Rm_unknown
       | exception _ -> Rm_unknown);
    room_policy_decision = text 25;
  }

let query ~(db : Sqlite3.db) ?work_type ?session_key ?room_id ?config_hash
    ?(limit = 50) () =
  let conditions = ref [] in
  let bindings = ref [] in
  let add_cond cond value =
    conditions := !conditions @ [ cond ];
    bindings := !bindings @ [ value ]
  in
  (match work_type with
  | Some wt ->
      add_cond "work_type = ?" (Sqlite3.Data.TEXT (work_type_to_string wt))
  | None -> ());
  (match session_key with
  | Some key -> add_cond "session_key = ?" (Sqlite3.Data.TEXT key)
  | None -> ());
  (match room_id with
  | Some rid -> add_cond "room_id = ?" (Sqlite3.Data.TEXT rid)
  | None -> ());
  (match config_hash with
  | Some hash -> add_cond "config_hash = ?" (Sqlite3.Data.TEXT hash)
  | None -> ());
  let where =
    match !conditions with
    | [] -> ""
    | conds -> " WHERE " ^ String.concat " AND " conds
  in
  let sql =
    Printf.sprintf
      "SELECT %s FROM access_snapshots%s ORDER BY timestamp DESC LIMIT ?"
      snapshot_select_columns where
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
            results := row_of_stmt stmt :: !results;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "Access_snapshot query error: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ();
      List.rev !results)

let get_by_id ~(db : Sqlite3.db) id =
  let sql =
    Printf.sprintf "SELECT %s FROM access_snapshots WHERE id = ?"
      snapshot_select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (row_of_stmt stmt)
      | Sqlite3.Rc.DONE -> None
      | rc ->
          failwith
            (Printf.sprintf "Access_snapshot get_by_id error: %s"
               (Sqlite3.Rc.to_string rc)))

let to_json (snap : t) : Yojson.Safe.t =
  `Assoc
    [
      ("id", `String snap.id);
      ("timestamp", `String snap.timestamp);
      ("config_hash", `String snap.config_hash);
      ( "session_key",
        match snap.session_key with Some s -> `String s | None -> `Null );
      ("work_type", `String (work_type_to_string snap.work_type));
      ("room_id", match snap.room_id with Some s -> `String s | None -> `Null);
      ( "profile_id",
        match snap.profile_id with Some s -> `String s | None -> `Null );
      ( "bundle_sources",
        `List (List.map bundle_source_to_json snap.bundle_sources) );
      ("allowed_tools", list_to_json snap.allowed_tools);
      ("denied_tools", list_to_json snap.denied_tools);
      ("codebase_grants", list_to_json snap.codebase_grants);
      ("blocked_codebase_grants", list_to_json snap.blocked_codebase_grants);
      ("mcp_servers", list_to_json snap.mcp_servers);
      ("skills", list_to_json snap.skills);
      ("repositories", list_to_json snap.repositories);
      ("repo_grants", list_to_json snap.repo_grants);
      ("blocked_repo_grants", list_to_json snap.blocked_repo_grants);
      ("domains", list_to_json snap.domains);
      ("credential_handles", list_to_json snap.credential_handles);
      ("memory_grants", list_to_json snap.memory_grants);
      ("budget_refs", list_to_json snap.budget_refs);
      ("egress_rules_count", `Int snap.egress_rules_count);
      ("instruction_digests", list_to_json snap.instruction_digests);
      ("redacted_summary", `String snap.redacted_summary);
      ( "room_classification",
        `String (Room_policy.room_scope_to_string snap.room_classification) );
      ("room_policy_decision", `String snap.room_policy_decision);
    ]

let record_for_work ~(db : Sqlite3.db) ~(config : Runtime_config.t) ~work_type
    ?session_key ?room_id ?profile_id ?room_classification ?room_policy_decision
    () =
  let snap =
    create ~config ~work_type ?session_key ?room_id ?profile_id
      ?room_classification ?room_policy_decision ()
  in
  (try persist ~db snap
   with _ ->
     (* Table may not exist yet if schema init hasn't run; silently skip *)
     ());
  snap.id

(** [create_and_persist ~db ~config ~work_type ...] creates a snapshot, persists
    it, and returns the full snapshot record. Used when the caller needs both
    the snapshot ID and the resolved access fields (e.g. to store on the agent
    for snapshot-scoped access during execution). *)
let create_and_persist ~(db : Sqlite3.db) ~(config : Runtime_config.t)
    ~work_type ?session_key ?room_id ?profile_id ?room_classification
    ?room_policy_decision () =
  let snap =
    create ~config ~work_type ?session_key ?room_id ?profile_id
      ?room_classification ?room_policy_decision ()
  in
  (try persist ~db snap with _ -> ());
  snap

(** [tool_denial snap ~tool_name ?equivalence_names] checks whether a tool
    should be denied based on the snapshot's resolved allowed/denied tools.
    Deny-wins over the full equivalence class (canonical + aliases): any deny
    hit denies the class; otherwise a nonempty allowlist admits any allowed
    equivalent. Returns [Some msg] if denied, [None] if allowed. *)
let tool_denial (snap : t) ~tool_name ?(equivalence_names = [ tool_name ]) () :
    string option =
  Tool_authz.denial_message ~canonical:tool_name ~equivalence_names
    ~allowed_tools:snap.allowed_tools ~denied_tools:snap.denied_tools ()

let export_json ~(db : Sqlite3.db) ?(limit = 100) ?work_type ?session_key
    ?room_id ?config_hash ~path () =
  let snaps =
    query ~db ?work_type ?session_key ?room_id ?config_hash ~limit ()
  in
  let oc = open_out path in
  List.iter
    (fun snap ->
      let line = Yojson.Safe.to_string (to_json snap) in
      output_string oc line;
      output_char oc '\n')
    snaps;
  close_out oc;
  List.length snaps
