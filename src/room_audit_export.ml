(** Room governance audit export.

    Produces a comprehensive audit export for a room that includes scope
    snapshot, memory events, GitHub events, delivery events, setup events, and
    policy events with redacted references. Suitable for compliance, governance
    review, and debugging. *)

open Runtime_config_types

(** {1 Types} *)

type event_category =
  | Scope_snapshot
  | Memory_event
  | Github_event
  | Delivery_event
  | Setup_event
  | Policy_event

let category_to_string = function
  | Scope_snapshot -> "scope_snapshot"
  | Memory_event -> "memory"
  | Github_event -> "github"
  | Delivery_event -> "delivery"
  | Setup_event -> "setup"
  | Policy_event -> "policy"

type export_event = {
  category : event_category;
  event : Room_activity_ledger.event;
}

type scope_snapshot_data = {
  room_id : string;
  scope : room_scope;
  connector : string option;
  profile_id : string option;
  profile_status : string option;
  binding_active : bool;
}

type export = {
  room_id : string;
  exported_at : string;
  scope_snapshot : scope_snapshot_data;
  events : export_event list;
  total_count : int;
  category_counts : (string * int) list;
}

(** {1 Event categorization} *)

(** Classify a ledger event into a governance category. Uses prefix matching for
    delivery lifecycle families (teams_delivery_XX, ambient_deliveryXX) to catch
    all states. *)
let categorize_event (event : Room_activity_ledger.event) : event_category =
  let et = event.event_type in
  (* Memory events *)
  match et with
  | "memory_saved" | "memory_corrected" | "memory_forgotten"
  | "memory_hard_purged" | "scope_granted" | "scope_revoked"
  | "team_grant_added" | "team_grant_removed" ->
      Memory_event
  (* GitHub events *)
  | "github_update_delivered" | "github_update_skipped" | "github_update_denied"
    ->
      Github_event
  (* Delivery events -- explicit list *)
  | "delivery_attempt" | "delivery_success" | "delivery_failure" ->
      Delivery_event
  (* Setup/admin events *)
  | "admin_denied" | "room_bound" | "room_unbound" | "profile_created"
  | "profile_deleted" | "profile_updated" ->
      Setup_event
  (* Policy events -- explicit list *)
  | "provider_request" | "provider_response" | "background_task_create"
  | "background_task_start" | "background_task_complete"
  | "background_task_fail" ->
      Policy_event
  | _ ->
      (* Prefix matching for delivery lifecycle families *)
      if
        String.starts_with ~prefix:"teams_delivery_" et
        || String.starts_with ~prefix:"ambient_delivery" et
      then Delivery_event
      else Policy_event

(** {1 Redaction} *)

(** Redact a reference string by keeping only structural prefix. *)
let redact_reference ref_str =
  let len = String.length ref_str in
  if len <= 8 then String.make len '*'
  else
    String.sub ref_str 0 4
    ^ String.make (len - 8) '*'
    ^ String.sub ref_str (len - 4) 4

(** Redact sensitive fields in event metadata. Returns a new JSON with
    references, tokens, and IDs partially redacted. *)
let redact_metadata (metadata : Yojson.Safe.t) : Yojson.Safe.t =
  let redact_string_value key =
    match key with
    (* Credential and auth fields *)
    | "token" | "bearer" | "api_key" | "secret" -> true
    (* Reference/ID fields that leak operational detail *)
    | "reference" | "source_message_id" | "service_url" | "snapshot_id"
    | "delivery_id" | "activity_id" | "message_id" | "tracking_id"
    | "access_snapshot_id" | "item_id" | "scope_key" ->
        true
    | _ -> false
  in
  let rec redact_json = function
    | `Assoc fields ->
        `Assoc
          (List.map
             (fun (key, value) ->
               if redact_string_value key then
                 match value with
                 | `String s -> (key, `String (redact_reference s))
                 | other -> (key, other)
               else (key, redact_json value))
             fields)
    | `List items -> `List (List.map redact_json items)
    | other -> other
  in
  redact_json metadata

(** {1 Scope snapshot} *)

(** Build a scope snapshot for a room from config and policy. *)
let build_scope_snapshot ~(cfg : Runtime_config.t) ~(room_id : string) :
    scope_snapshot_data =
  let scope = Room_policy.derive_scope_from_session_key room_id in
  let binding =
    List.find_opt
      (fun (b : room_profile_binding) -> b.room = room_id)
      cfg.room_profile_bindings
  in
  let profile_id =
    Option.map (fun (b : room_profile_binding) -> b.profile_id) binding
  in
  let binding_active =
    match binding with Some b -> b.active | None -> false
  in
  let profile_status =
    match profile_id with
    | Some pid -> (
        match
          List.find_opt (fun (p : room_profile) -> p.id = pid) cfg.room_profiles
        with
        | Some p -> Some p.status
        | None -> Some "missing")
    | None -> None
  in
  {
    room_id;
    scope;
    connector = None;
    profile_id;
    profile_status;
    binding_active;
  }

(** {1 Export generation} *)

(** [generate ~cfg ~db ~room_id ()] produces a comprehensive governance audit
    export for the given room. Queries the activity ledger for all
    governance-relevant event types and organizes them by category. *)
let generate ~(cfg : Runtime_config.t) ~(db : Sqlite3.db) ~(room_id : string) ()
    : export =
  let scope_snapshot = build_scope_snapshot ~cfg ~room_id in
  let all_events = Room_activity_ledger.query ~db ~room_id () in
  let export_events =
    List.map
      (fun event -> { category = categorize_event event; event })
      all_events
  in
  let category_counts =
    let counts = Hashtbl.create 8 in
    List.iter
      (fun (ee : export_event) ->
        let key = category_to_string ee.category in
        Hashtbl.replace counts key
          (1 + Option.value ~default:0 (Hashtbl.find_opt counts key)))
      export_events;
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) counts []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  {
    room_id;
    exported_at = Room_activity_ledger.timestamp_now ();
    scope_snapshot;
    events = export_events;
    total_count = List.length export_events;
    category_counts;
  }

(** {1 JSON formatting} *)

let scope_snapshot_to_json (snap : scope_snapshot_data) : Yojson.Safe.t =
  let fields =
    [
      ("room_id", `String snap.room_id);
      ("scope", `String (Room_policy.room_scope_to_string snap.scope));
      ("binding_active", `Bool snap.binding_active);
    ]
  in
  let fields =
    match snap.connector with
    | Some c -> ("connector", `String c) :: fields
    | None -> fields
  in
  let fields =
    match snap.profile_id with
    | Some pid -> ("profile_id", `String pid) :: fields
    | None -> fields
  in
  let fields =
    match snap.profile_status with
    | Some ps -> ("profile_status", `String ps) :: fields
    | None -> fields
  in
  `Assoc fields

let export_event_to_json (ee : export_event) : Yojson.Safe.t =
  let base = Room_activity_ledger.event_to_json ee.event in
  match base with
  | `Assoc fields ->
      `Assoc
        (("category", `String (category_to_string ee.category))
        :: ("metadata_redacted", redact_metadata (List.assoc "metadata" fields))
        :: List.filter (fun (k, _) -> k <> "metadata") fields)
  | other -> other

let export_to_json (exp : export) : Yojson.Safe.t =
  `Assoc
    [
      ("room_id", `String exp.room_id);
      ("exported_at", `String exp.exported_at);
      ("scope_snapshot", scope_snapshot_to_json exp.scope_snapshot);
      ("events", `List (List.map export_event_to_json exp.events));
      ("total_count", `Int exp.total_count);
      ( "category_counts",
        `Assoc (List.map (fun (k, v) -> (k, `Int v)) exp.category_counts) );
    ]

let export_to_json_string (exp : export) : string =
  Yojson.Safe.pretty_to_string (export_to_json exp)

(** {1 JSONL formatting} *)

let export_to_jsonl (exp : export) : string =
  (* First line: scope snapshot header *)
  let header =
    `Assoc
      [
        ("type", `String "header");
        ("room_id", `String exp.room_id);
        ("exported_at", `String exp.exported_at);
        ("scope_snapshot", scope_snapshot_to_json exp.scope_snapshot);
        ("total_count", `Int exp.total_count);
        ( "category_counts",
          `Assoc (List.map (fun (k, v) -> (k, `Int v)) exp.category_counts) );
      ]
  in
  let event_lines =
    List.map
      (fun ee -> Yojson.Safe.to_string (export_event_to_json ee))
      exp.events
  in
  String.concat "\n" (Yojson.Safe.to_string header :: event_lines)

(** {1 Text formatting} *)

let format_text (exp : export) : string =
  let open Setup_common in
  let buf = Buffer.create 4096 in
  let add line =
    Buffer.add_string buf line;
    Buffer.add_char buf '\n'
  in
  add (bold "=== Room Governance Audit Export ===");
  add "";
  add (Printf.sprintf "  Room:        %s" exp.room_id);
  add (Printf.sprintf "  Exported at: %s" exp.exported_at);
  add "";
  add (bold "  Scope Snapshot");
  let snap = exp.scope_snapshot in
  add
    (Printf.sprintf "    Scope:          %s"
       (Room_policy.room_scope_to_string snap.scope));
  add
    (Printf.sprintf "    Binding active: %s"
       (if snap.binding_active then "yes" else "no"));
  (match snap.profile_id with
  | Some pid -> add (Printf.sprintf "    Profile:        %s" pid)
  | None -> add "    Profile:        (none)");
  (match snap.profile_status with
  | Some ps -> add (Printf.sprintf "    Profile status: %s" ps)
  | None -> ());
  add "";
  add (bold "  Event Summary");
  List.iter
    (fun (cat, count) -> add (Printf.sprintf "    %-20s %d" cat count))
    exp.category_counts;
  add (Printf.sprintf "    %-20s %d" "TOTAL" exp.total_count);
  add "";
  if exp.events = [] then
    add (dim "  No governance events recorded for this room.")
  else begin
    add (bold "  Events");
    List.iter
      (fun (ee : export_event) ->
        let icon =
          match ee.category with
          | Scope_snapshot -> cyan "SNAP"
          | Memory_event -> green "MEM"
          | Github_event -> yellow "GH"
          | Delivery_event -> cyan "DLVR"
          | Setup_event -> red "SETUP"
          | Policy_event -> dim "POL"
        in
        add
          (Printf.sprintf "  [%s] %s %s by %s" icon ee.event.timestamp
             ee.event.event_type ee.event.actor))
      exp.events
  end;
  Buffer.contents buf

(** {1 Summary for CLI} *)

(** Produce a one-line summary suitable for inclusion in room show/inspect. *)
let summary_line ~(export : export) : string =
  Printf.sprintf "Audit: %d events (%s)" export.total_count
    (String.concat ", "
       (List.map
          (fun (cat, count) -> Printf.sprintf "%s=%d" cat count)
          export.category_counts))
