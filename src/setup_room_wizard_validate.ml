(** Validate audit ledger and connector delivery in the room wizard.

    Simulates a connector delivery flow using an in-memory SQLite database,
    exercises all three audit/visibility subsystems (room activity ledger,
    egress audit, and Teams delivery lifecycle), then displays the resulting
    traces to prove that visibility and accounting are wired correctly.

    The simulation uses a throwaway in-memory DB so no side effects touch the
    production database. *)

(* ── Types ──────────────────────────────────────────────────────── *)

type trace_summary = {
  ledger_events : Room_activity_ledger.event list;
  egress_events : Egress_audit.event list;
  lifecycle_events : Room_activity_ledger.event list;
  delivery_attempt_count : int;
  delivery_success_count : int;
  delivery_failure_count : int;
  egress_allowed_count : int;
  egress_denied_count : int;
  lifecycle_state_count : int;
}

(* ── Simulation ─────────────────────────────────────────────────── *)

(** [simulate_delivery ~connector ~room_id ~profile_id ~task_id] runs a
    simulated delivery flow against an in-memory DB, recording events in all
    three subsystems. Returns a {!trace_summary}. *)
let simulate_delivery ~(connector : string) ~(room_id : string)
    ~(profile_id : string) ~(task_id : int) : trace_summary =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      (* Initialize schemas *)
      Room_activity_ledger.init_schema db;
      Egress_audit.init_schema db;

      let thread_id = "sim-thread-001" in
      let message_id =
        match connector with
        | "teams" -> "sim-activity-abc"
        | "slack" -> "sim-ts-12345"
        | _ -> "sim-msg-001"
      in

      (* 1. Record delivery attempt *)
      let _attempt =
        Room_activity_ledger.record_delivery_attempt ~db ~room_id ~connector
          ~task_id ~thread_id ()
      in

      (* 2. Record egress audit - allowed *)
      Egress_audit.record ~db ~decision:Allowed
        ~host:(Printf.sprintf "%s.api.com" connector)
        ~method_:"POST" ~path:"/messages" ~matched_rule_index:0
        ~session_key:(Printf.sprintf "%s:%s:user-1" connector room_id)
        ~tool_name:"connector_send" ~profile_id
        ~credential_handle_ids:[ Printf.sprintf "%s-bot:prod" connector ]
        ();

      (* 3. Record egress audit - denied (for completeness) *)
      Egress_audit.record ~db ~decision:Denied ~host:"blocked.example.com"
        ~method_:"GET" ~matched_rule_index:(-1)
        ~session_key:(Printf.sprintf "%s:%s:user-1" connector room_id)
        ~tool_name:"http_request" ~profile_id ();

      (* 4. Record Teams delivery lifecycle if connector is teams *)
      let tracking_id_opt =
        if connector = "teams" then begin
          let tid = Teams_delivery_lifecycle.generate_tracking_id () in
          Teams_delivery_lifecycle.record_scheduled ~db ~room_id ~connector
            ~tracking_id:tid ~task_id ~thread_id ();
          Teams_delivery_lifecycle.record_generated ~db ~room_id ~connector
            ~tracking_id:tid ~task_id ~thread_id ();
          Teams_delivery_lifecycle.record_attempted ~db ~room_id ~connector
            ~tracking_id:tid ~task_id ~thread_id ();
          Teams_delivery_lifecycle.record_transport_accepted ~db ~room_id
            ~connector ~tracking_id:tid ~task_id ~thread_id ();
          Teams_delivery_lifecycle.record_message_id_recorded ~db ~room_id
            ~connector ~tracking_id:tid ~task_id ~message_id ~thread_id ();
          Some tid
        end
        else None
      in

      (* 5. Record delivery success *)
      let _success =
        Room_activity_ledger.record_delivery_success ~db ~room_id ~connector
          ~task_id ~message_id ~thread_id ()
      in

      (* 6. Also record a simulated failure for a different task to show
         failure path visibility *)
      let _failure =
        Room_activity_ledger.record_delivery_failure ~db ~room_id ~connector
          ~task_id:(task_id + 1000) ~error:"simulated timeout" ()
      in

      (* Query all traces *)
      let ledger_events = Room_activity_ledger.query ~db ~room_id () in
      let egress_events =
        Egress_audit.query ~db
          ~session_key:(Printf.sprintf "%s:%s:user-1" connector room_id)
          ()
      in
      let lifecycle_events =
        match tracking_id_opt with
        | Some tid ->
            Teams_delivery_lifecycle.query_by_tracking_id ~db ~tracking_id:tid
              ()
        | None -> []
      in

      (* Compute counts *)
      let delivery_attempt_count =
        List.fold_left
          (fun acc (e : Room_activity_ledger.event) ->
            if e.event_type = "delivery_attempt" then acc + 1 else acc)
          0 ledger_events
      in
      let delivery_success_count =
        List.fold_left
          (fun acc (e : Room_activity_ledger.event) ->
            if e.event_type = "delivery_success" then acc + 1 else acc)
          0 ledger_events
      in
      let delivery_failure_count =
        List.fold_left
          (fun acc (e : Room_activity_ledger.event) ->
            if e.event_type = "delivery_failure" then acc + 1 else acc)
          0 ledger_events
      in
      let egress_allowed_count =
        List.fold_left
          (fun acc (e : Egress_audit.event) ->
            if e.decision = Egress_audit.Allowed then acc + 1 else acc)
          0 egress_events
      in
      let egress_denied_count =
        List.fold_left
          (fun acc (e : Egress_audit.event) ->
            if e.decision = Egress_audit.Denied then acc + 1 else acc)
          0 egress_events
      in
      let lifecycle_state_count = List.length lifecycle_events in

      {
        ledger_events;
        egress_events;
        lifecycle_events;
        delivery_attempt_count;
        delivery_success_count;
        delivery_failure_count;
        egress_allowed_count;
        egress_denied_count;
        lifecycle_state_count;
      })

(* ── Display ────────────────────────────────────────────────────── *)

let display_traces ~(connector : string) ~(room_id : string)
    ~(profile_id : string) (summary : trace_summary) =
  let open Setup_common in
  Printf.printf "\n%s\n" (bold "=== Delivery Validation Traces ===");
  Printf.printf "\n";
  Printf.printf "  Connector: %s\n" connector;
  Printf.printf "  Room:      %s\n" room_id;
  Printf.printf "  Profile:   %s\n" profile_id;
  Printf.printf "\n";

  (* Ledger summary *)
  Printf.printf "  %s\n" (bold "Room Activity Ledger");
  Printf.printf "    delivery_attempt:  %d\n" summary.delivery_attempt_count;
  Printf.printf "    delivery_success:  %d\n" summary.delivery_success_count;
  Printf.printf "    delivery_failure:  %d\n" summary.delivery_failure_count;
  Printf.printf "    total events:      %d\n"
    (List.length summary.ledger_events);
  Printf.printf "\n";

  (* Ledger event details *)
  Printf.printf "  %s\n" (bold "Ledger Event Details");
  List.iter
    (fun (e : Room_activity_ledger.event) ->
      let icon =
        match e.event_type with
        | "delivery_attempt" -> cyan "~"
        | "delivery_success" -> green "+"
        | "delivery_failure" -> red "!"
        | _ -> dim "-"
      in
      Printf.printf "    %s %-24s actor=%s ts=%s\n" icon e.event_type e.actor
        e.timestamp)
    summary.ledger_events;
  Printf.printf "\n";

  (* Egress audit summary *)
  Printf.printf "  %s\n" (bold "Egress Audit");
  Printf.printf "    allowed: %d\n" summary.egress_allowed_count;
  Printf.printf "    denied:  %d\n" summary.egress_denied_count;
  Printf.printf "    total:   %d\n" (List.length summary.egress_events);
  Printf.printf "\n";

  (* Egress event details *)
  Printf.printf "  %s\n" (bold "Egress Audit Details");
  List.iter
    (fun (e : Egress_audit.event) ->
      let decision_str =
        match e.decision with
        | Egress_audit.Allowed -> green "ALLOWED"
        | Egress_audit.Denied -> red "DENIED"
      in
      Printf.printf "    [%s] host=%s rule=%d tool=%s ts=%s\n" decision_str
        e.host_redacted e.matched_rule_index
        (Option.value e.tool_name ~default:"-")
        e.timestamp)
    summary.egress_events;
  Printf.printf "\n";

  (* Teams delivery lifecycle (if applicable) *)
  if summary.lifecycle_state_count > 0 then begin
    Printf.printf "  %s\n" (bold "Teams Delivery Lifecycle");
    Printf.printf "    states tracked: %d\n" summary.lifecycle_state_count;
    Printf.printf "\n";
    Printf.printf "  %s\n" (bold "Lifecycle State Details");
    List.iter
      (fun (e : Room_activity_ledger.event) ->
        let state =
          match e.metadata with
          | `Assoc fields -> (
              match List.assoc_opt "lifecycle_state" fields with
              | Some (`String s) -> s
              | _ -> "?")
          | _ -> "?"
        in
        Printf.printf "    %s %s -> %s ts=%s\n" (cyan "~") e.event_type state
          e.timestamp)
      summary.lifecycle_events;
    Printf.printf "\n"
  end
  else begin
    Printf.printf "  %s\n"
      (dim "(No Teams delivery lifecycle events -- connector is not teams)");
    Printf.printf "\n"
  end;

  (* Overall verdict *)
  let has_attempt = summary.delivery_attempt_count > 0 in
  let has_success = summary.delivery_success_count > 0 in
  let has_egress = List.length summary.egress_events > 0 in
  let has_lifecycle =
    connector <> "teams" || summary.lifecycle_state_count > 0
  in
  let all_good = has_attempt && has_success && has_egress && has_lifecycle in
  if all_good then
    Printf.printf "  %s\n"
      (green
         "PASS: All audit/ledger/delivery traces recorded successfully. \
          Visibility confirmed.")
  else begin
    Printf.printf "  %s\n" (red "FAIL: Some traces are missing:");
    if not has_attempt then
      Printf.printf "    %s\n" (red "- No delivery_attempt recorded");
    if not has_success then
      Printf.printf "    %s\n" (red "- No delivery_success recorded");
    if not has_egress then
      Printf.printf "    %s\n" (red "- No egress audit events recorded");
    if not has_lifecycle then
      Printf.printf "    %s\n"
        (red "- No Teams delivery lifecycle events recorded")
  end;
  Printf.printf "\n";
  all_good

(* ── CLI entry point ────────────────────────────────────────────── *)

(** [run ~profile_id ~connector ~room_id ()] simulates a delivery and displays
    traces. Returns a summary string for CLI output. *)
let run ~(profile_id : string) ~(connector : string) ~(room_id : string) () :
    string =
  if profile_id = "" then
    "Error: --profile-id is required for validate-delivery.\n\n\
     Usage: clawq rooms wizard validate-delivery --profile-id ID [--connector \
     C] [--room R]"
  else if room_id = "" then
    "Error: --room is required for validate-delivery.\n\n\
     Usage: clawq rooms wizard validate-delivery --profile-id ID [--connector \
     C] [--room R]"
  else
    let summary =
      simulate_delivery ~connector ~room_id ~profile_id ~task_id:999999
    in
    let passed = display_traces ~connector ~room_id ~profile_id summary in
    if passed then
      Printf.sprintf
        "Validation passed. %d ledger events, %d egress events, %d lifecycle \
         events recorded."
        (List.length summary.ledger_events)
        (List.length summary.egress_events)
        summary.lifecycle_state_count
    else "Validation failed. See trace details above."
