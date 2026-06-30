(** Safe ambient follow-up delivery.

    Ties together stale-item queries, ambient policy checks, watcher decision
    persistence, budget checks, connector capability gates, and activity ledger
    recording. Only delivers when every safety condition permits it; failures
    are always ledger-visible.

    The actual connector send is injected via [~send_message] so the module
    remains connector-agnostic and testable. *)

(** {1 Types} *)

type delivery_outcome = {
  item : Room_stale_query.stale_item;
  acted : bool;
  skip_reason : Room_watcher_decision.skip_reason option;
  delivery_error : string option;
}
(** Per-item delivery result. [acted=true] means the message was sent.
    [delivery_error] is [Some msg] when delivery was attempted but the connector
    returned an error. *)

(** {1 Formatting} *)

(** Format a concise follow-up message for a stale item. The message is
    connector-agnostic plain text. *)
let format_followup_message (item : Room_stale_query.stale_item) =
  let source_label =
    match item.source with
    | `Background_task -> "background task"
    | `Task_tree -> "task-tree item"
  in
  let age_hours = item.age_seconds /. 3600.0 in
  let age_str =
    if age_hours >= 1.0 then Printf.sprintf "%.1fh" age_hours
    else Printf.sprintf "%.0fm" (item.age_seconds /. 60.0)
  in
  Printf.sprintf "Heads up: %s \"%s\" (id %s) has been %s for %s." source_label
    (if String.length item.title > 80 then String.sub item.title 0 77 ^ "..."
     else item.title)
    item.id item.status age_str

(** {1 Delivery count helpers} *)

(** Count deliveries this hour from the activity ledger for a given room. Looks
    for [ambient_delivery] events with timestamps within the current UTC hour.
*)
let count_deliveries_this_hour ~db ~room_id =
  let now = Unix.gettimeofday () in
  let tm = Unix.gmtime now in
  let hour_start =
    Printf.sprintf "%04d-%02d-%02dT%02d:00:00" (tm.Unix.tm_year + 1900)
      (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour
  in
  let events =
    Room_activity_ledger.query ~db ~room_id ~event_type:"ambient_delivery"
      ~from_timestamp:hour_start ()
  in
  List.length events

let decision_item_id (item : Room_stale_query.stale_item) =
  match item.thread_id with
  | Some thread_id when String.trim thread_id <> "" ->
      Printf.sprintf "%s:%s" thread_id item.id
  | _ -> item.id

let effective_thread_id ~fallback (item : Room_stale_query.stale_item) =
  match item.thread_id with
  | Some thread_id when String.trim thread_id <> "" -> Some thread_id
  | _ -> fallback

let policy_skip_reason = function
  | Ambient_policy.Ambient_not_enabled -> Room_watcher_decision.Policy_denied
  | Ambient_policy.Quiet_hours -> Room_watcher_decision.Quiet_hours
  | Ambient_policy.Rate_limited -> Room_watcher_decision.Rate_limited
  | Ambient_policy.Budget_exceeded -> Room_watcher_decision.Budget_exceeded
  | Ambient_policy.Connector_unsupported ->
      Room_watcher_decision.Connector_unsupported

let connector_capabilities_of_type = function
  | "slack" -> Some Connector_capabilities.slack
  | "discord" -> Some Connector_capabilities.discord
  | "teams" -> Some Connector_capabilities.teams
  | "telegram" -> Some Connector_capabilities.telegram
  | "matrix" -> Some Connector_capabilities.matrix
  | "mattermost" -> Some Connector_capabilities.mattermost
  | "irc" -> Some Connector_capabilities.irc
  | "email" -> Some Connector_capabilities.email
  | "github" -> Some Connector_capabilities.github
  | "signal" -> Some Connector_capabilities.signal
  | "whatsapp" -> Some Connector_capabilities.whatsapp
  | _ -> None

let supports_ambient_history_for_connector = function
  | Some connector_type -> (
      match connector_capabilities_of_type connector_type with
      | Some caps ->
          Connector_capabilities.should_capture_history ~enabled:true caps
      | None -> false)
  | None -> false

let latest_connector_type_for_room ~db ~room_id =
  try
    let sql =
      "SELECT connector_type FROM connector_history WHERE room_id = ? ORDER BY \
       id DESC LIMIT 1"
    in
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.TEXT s when String.trim s <> "" -> Some s
            | _ -> None)
        | _ -> None)
  with _ -> None

let budget_exceeded_for_profile ~db ~profile_id =
  match Memory_core.get_room_profile_by_name ~db ~name:profile_id with
  | None -> false
  | Some profile -> (
      Room_budget.init_schema db;
      match Room_budget.get_profile_budget ~db ~profile_id:profile.id with
      | Some budget -> budget.Room_budget.limit_exceeded
      | None -> false)

(** {1 Core delivery} *)

(** Deliver ambient follow-ups for a list of stale items.

    For each item the pipeline is:
    + Run [Ambient_policy.check_all] — if denied, record a skip decision.
    + Check material-change by comparing fingerprints against the latest watcher
      decision for this item — if identical, suppress.
    + Call [send_message]. If it returns an error, record the failure in the
      activity ledger and the watcher decision.
    + On success, record [ambient_delivery] in the ledger and an [Acted] watcher
      decision.

    Parameters:
    - [~db] the SQLite database handle
    - [~profile] the room profile with ambient config fields
    - [~room_id] the target room identifier
    - [?thread_id] optional fallback thread for thread-scoped delivery
    - [~stale_items] items returned by [Room_stale_query.find_stale]
    - [~hour] current UTC hour (0–23), injectable for testing
    - [~budget_exceeded] whether the room's budget limit is hit
    - [~supports_ambient] whether the connector has ambient_history_capture
    - [~send_message] callback that performs the actual connector delivery;
      receives [~room_id], [?thread_id], [~message] and returns [Ok ()] or
      [Error reason]. Must accept a trailing [()] so the optional [?thread_id]
      is erasable.

    Returns a list of {!delivery_outcome}, one per input item. *)
let deliver_ambient_followups ~db ~profile ~room_id ?thread_id ~stale_items
    ~hour ~budget_exceeded ~supports_ambient ~send_message () =
  let open Lwt.Syntax in
  let deliveries_this_hour = ref (count_deliveries_this_hour ~db ~room_id) in
  let process_one (item : Room_stale_query.stale_item) =
    let watcher_type =
      match item.source with
      | `Background_task -> Room_watcher_decision.Stale_task
      | `Task_tree -> Room_watcher_decision.Stale_task
    in
    let item_source = Room_stale_query.source_to_string item.source in
    let decision_item_id = decision_item_id item in
    let fingerprint =
      Room_watcher_decision.compute_fingerprint ~source:item.source
        ~item_id:decision_item_id ~status:item.status
        ~age_seconds:item.age_seconds
    in
    let policy_result =
      Ambient_policy.check_all ~hour ~deliveries_this_hour:!deliveries_this_hour
        ~budget_exceeded ~supports_ambient profile
    in
    match policy_result with
    | Denied reason ->
        let skip_reason = policy_skip_reason reason in
        ignore
          (Room_watcher_decision.record_if_changed ~db ~room_id ~watcher_type
             ~outcome:Skipped ~skip_reason ~item_source
             ~item_id:decision_item_id ~fingerprint
             ~metadata:
               (`Assoc
                  [
                    ("item_id", `String item.id);
                    ( "skip_reason",
                      `String (Ambient_policy.reason_to_string reason) );
                  ])
             ());
        Lwt.return
          {
            item;
            acted = false;
            skip_reason = Some skip_reason;
            delivery_error = None;
          }
    | Allowed -> (
        let existing =
          Room_watcher_decision.latest_decision ~db ~room_id ~item_source
            ~item_id:decision_item_id
        in
        let is_suppressed =
          match existing with
          | Some prev
            when prev.Room_watcher_decision.fingerprint = fingerprint
                 && prev.Room_watcher_decision.outcome
                    = Room_watcher_decision.Acted ->
              true
          | _ -> false
        in
        if is_suppressed then
          Lwt.return
            {
              item;
              acted = false;
              skip_reason = Some No_material_change;
              delivery_error = None;
            }
        else
          let message = format_followup_message item in
          let delivery_thread_id =
            effective_thread_id ~fallback:thread_id item
          in
          let* result =
            send_message ~room_id ?thread_id:delivery_thread_id ~message ()
          in
          match result with
          | Ok () ->
              incr deliveries_this_hour;
              ignore
                (Room_activity_ledger.append_now ~db ~room_id
                   ~event_type:"ambient_delivery" ~actor:"ambient_watcher"
                   ~metadata:
                     (`Assoc
                        ([
                           ("item_source", `String item_source);
                           ("item_id", `String item.id);
                           ("message_preview", `String message);
                         ]
                        @
                        match delivery_thread_id with
                        | Some tid -> [ ("thread_id", `String tid) ]
                        | None -> [])));
              ignore
                (Room_watcher_decision.record ~db ~room_id ~watcher_type
                   ~outcome:Acted ~item_source ~item_id:decision_item_id
                   ~fingerprint
                   ~metadata:
                     (`Assoc
                        ([
                           ("action", `String "delivered");
                           ("item_id", `String item.id);
                           ("message_preview", `String message);
                         ]
                        @
                        match delivery_thread_id with
                        | Some tid -> [ ("thread_id", `String tid) ]
                        | None -> []))
                   ());
              Lwt.return
                {
                  item;
                  acted = true;
                  skip_reason = None;
                  delivery_error = None;
                }
          | Error err ->
              ignore
                (Room_activity_ledger.append_now ~db ~room_id
                   ~event_type:"ambient_delivery_failed"
                   ~actor:"ambient_watcher"
                   ~metadata:
                     (`Assoc
                        ([
                           ("item_source", `String item_source);
                           ("item_id", `String item.id);
                           ("error", `String err);
                           ("message_preview", `String message);
                         ]
                        @
                        match delivery_thread_id with
                        | Some tid -> [ ("thread_id", `String tid) ]
                        | None -> [])));
              ignore
                (Room_watcher_decision.record ~db ~room_id ~watcher_type
                   ~outcome:Skipped
                   ~skip_reason:Room_watcher_decision.Policy_denied ~item_source
                   ~item_id:decision_item_id ~fingerprint
                   ~metadata:
                     (`Assoc
                        ([
                           ("delivery_error", `String err);
                           ("item_id", `String item.id);
                           ("message_preview", `String message);
                         ]
                        @
                        match delivery_thread_id with
                        | Some tid -> [ ("thread_id", `String tid) ]
                        | None -> []))
                   ());
              Lwt.return
                {
                  item;
                  acted = false;
                  skip_reason = None;
                  delivery_error = Some err;
                })
  in
  Lwt_list.map_s process_one stale_items

let deliver_room_ambient_followups ~db ~profile ~room_id ~stale_after_s
    ?check_room_allowed ~send_message () =
  let open Lwt.Syntax in
  (* B735: pre-check room allowance (e.g. private channel policy) *)
  let* room_allowed =
    match check_room_allowed with
    | Some check -> check ~room_id
    | None -> Lwt.return true
  in
  if not room_allowed then begin
    Logs.info (fun m ->
        m "Ambient delivery skipped for room %s: room not allowed" room_id);
    Lwt.return []
  end
  else
    let stale_items =
      Room_stale_query.find_stale ~db ~stale_after_s ~room_id ()
    in
    if stale_items = [] then Lwt.return []
    else
      let now = Unix.gettimeofday () in
      let hour = (Unix.gmtime now).Unix.tm_hour in
      let connector_type = latest_connector_type_for_room ~db ~room_id in
      let supports_ambient =
        supports_ambient_history_for_connector connector_type
      in
      let budget_exceeded =
        budget_exceeded_for_profile ~db
          ~profile_id:profile.Runtime_config_types.id
      in
      deliver_ambient_followups ~db ~profile ~room_id ~stale_items ~hour
        ~budget_exceeded ~supports_ambient ~send_message ()
