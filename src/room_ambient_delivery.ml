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
    - [~thread_id] optional thread for thread-scoped delivery
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
  let deliveries_this_hour = count_deliveries_this_hour ~db ~room_id in
  let process_one (item : Room_stale_query.stale_item) =
    let watcher_type =
      match item.source with
      | `Background_task -> Room_watcher_decision.Stale_task
      | `Task_tree -> Room_watcher_decision.Stale_task
    in
    let item_source = Room_stale_query.source_to_string item.source in
    let fingerprint =
      Room_watcher_decision.compute_fingerprint ~source:item.source
        ~item_id:item.id ~status:item.status ~age_seconds:item.age_seconds
    in
    (* Step 1: policy gate *)
    let policy_result =
      Ambient_policy.check_all ~hour ~deliveries_this_hour ~budget_exceeded
        ~supports_ambient profile
    in
    match policy_result with
    | Denied reason ->
        let skip_reason =
          match reason with
          | Ambient_policy.Ambient_not_enabled ->
              Room_watcher_decision.Policy_denied
          | Quiet_hours -> Room_watcher_decision.Quiet_hours
          | Rate_limited -> Room_watcher_decision.Rate_limited
          | Budget_exceeded -> Room_watcher_decision.Budget_exceeded
          | Connector_unsupported -> Room_watcher_decision.Connector_unsupported
        in
        ignore
          (Room_watcher_decision.record_if_changed ~db ~room_id ~watcher_type
             ~outcome:Skipped ~skip_reason ~item_source ~item_id:item.id
             ~fingerprint
             ~metadata:
               (`Assoc
                  [
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
        (* Step 2: material-change gate — check latest decision fingerprint *)
        let existing =
          Room_watcher_decision.latest_decision ~db ~room_id ~item_source
            ~item_id:item.id
        in
        let is_suppressed =
          match existing with
          | Some prev when prev.Room_watcher_decision.fingerprint = fingerprint
            ->
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
          (* Step 3: attempt delivery *)
          let message = format_followup_message item in
          let* result = send_message ~room_id ?thread_id ~message () in
          match result with
          | Ok () ->
              (* Record success in ledger *)
              ignore
                (Room_activity_ledger.append_now ~db ~room_id
                   ~event_type:"ambient_delivery" ~actor:"ambient_watcher"
                   ~metadata:
                     (`Assoc
                        [
                          ("item_source", `String item_source);
                          ("item_id", `String item.id);
                          ("message_preview", `String message);
                        ]));
              (* Record Acted decision *)
              ignore
                (Room_watcher_decision.record ~db ~room_id ~watcher_type
                   ~outcome:Acted ~item_source ~item_id:item.id ~fingerprint
                   ~metadata:
                     (`Assoc
                        [
                          ("action", `String "delivered");
                          ("message_preview", `String message);
                        ])
                   ());
              Lwt.return
                {
                  item;
                  acted = true;
                  skip_reason = None;
                  delivery_error = None;
                }
          | Error err ->
              (* Record failure in ledger *)
              ignore
                (Room_activity_ledger.append_now ~db ~room_id
                   ~event_type:"ambient_delivery_failed"
                   ~actor:"ambient_watcher"
                   ~metadata:
                     (`Assoc
                        [
                          ("item_source", `String item_source);
                          ("item_id", `String item.id);
                          ("error", `String err);
                          ("message_preview", `String message);
                        ]));
              (* Record failure in watcher decisions *)
              ignore
                (Room_watcher_decision.record ~db ~room_id ~watcher_type
                   ~outcome:Skipped
                   ~skip_reason:Room_watcher_decision.Policy_denied ~item_source
                   ~item_id:item.id ~fingerprint
                   ~metadata:
                     (`Assoc
                        [
                          ("delivery_error", `String err);
                          ("message_preview", `String message);
                        ])
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
