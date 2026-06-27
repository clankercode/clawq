(** Ambient watcher admin inspection surface.

    Aggregates watcher decisions, delivery failures, rate-limit state, and
    quiet-hours information for admin inspection. Scoped by room to avoid
    exposing unauthorized content. *)

open Runtime_config_types

(** {1 Types} *)

type inspection_result = {
  room_id : string;
  profile_name : string option;
  ambient_enabled : bool;
  quiet_hours_range : (int * int) option;
  rate_limit_rph : int;
  recent_decisions : Room_watcher_decision.watcher_decision list;
  decision_summary : Room_watcher_decision.decision_summary;
  delivery_failures : Room_activity_ledger.event list;
  deliveries_this_hour : int;
}

(** {1 Inspection} *)

(** Inspect the ambient watcher state for a room.

    Looks up the room profile from config, queries recent watcher decisions,
    delivery failures from the activity ledger, and computes delivery counts for
    the current hour. All data is scoped to the given [room_id].

    Parameters:
    - [~db] the SQLite database handle
    - [~cfg] the runtime config (for room profiles and bindings)
    - [~room_id] the room to inspect
    - [?decision_limit] max recent decisions to include (default: 20) *)
let inspect ~db ~cfg ~room_id ?(decision_limit = 20) () =
  (* Ensure schemas exist *)
  Room_watcher_decision.init_schema db;
  Room_activity_ledger.init_schema db;
  (* Resolve room profile *)
  let binding =
    List.find_opt
      (fun (b : room_profile_binding) -> b.room = room_id)
      cfg.room_profile_bindings
  in
  let profile =
    match binding with
    | Some b ->
        List.find_opt
          (fun (p : room_profile) -> p.id = b.profile_id)
          cfg.room_profiles
    | None -> None
  in
  let profile_name = Option.map (fun (p : room_profile) -> p.id) profile in
  let ambient_enabled =
    match profile with Some p -> p.ambient_enabled | None -> false
  in
  let quiet_hours_range =
    match profile with
    | Some p when p.ambient_quiet_start <> p.ambient_quiet_end ->
        Some (p.ambient_quiet_start, p.ambient_quiet_end)
    | _ -> None
  in
  let rate_limit_rph =
    match profile with Some p -> p.ambient_rate_limit_rph | None -> 0
  in
  (* Query recent decisions *)
  let recent_decisions =
    Room_watcher_decision.query_by_room ~db ~room_id ~limit:decision_limit ()
  in
  let decision_summary = Room_watcher_decision.summarize ~db ~room_id in
  (* Query delivery failures from ledger *)
  let delivery_failures =
    Room_activity_ledger.query ~db ~room_id
      ~event_type:"ambient_delivery_failed" ()
  in
  (* Count deliveries this hour *)
  let deliveries_this_hour =
    Room_ambient_delivery.count_deliveries_this_hour ~db ~room_id
  in
  {
    room_id;
    profile_name;
    ambient_enabled;
    quiet_hours_range;
    rate_limit_rph;
    recent_decisions;
    decision_summary;
    delivery_failures;
    deliveries_this_hour;
  }

(** {1 Formatting} *)

let format_skip_reason = function
  | Room_watcher_decision.No_material_change ->
      "no material change (suppressed)"
  | Recently_decided -> "recently decided"
  | Policy_denied -> "policy denied"
  | Budget_exceeded -> "budget exceeded"
  | Rate_limited -> "rate limited"
  | Quiet_hours -> "quiet hours"
  | Connector_unsupported -> "connector unsupported"

let format_outcome (d : Room_watcher_decision.watcher_decision) =
  match d.outcome with
  | Acted -> "acted"
  | Skipped -> (
      match d.skip_reason with
      | Some reason -> "skipped: " ^ format_skip_reason reason
      | None -> "skipped")

(** Format an inspection result as a human-readable string. *)
let format_inspection (r : inspection_result) =
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add (Printf.sprintf "Ambient Inspection: %s" r.room_id);
  add (String.make (20 + String.length r.room_id) '=');
  add "";
  (* Profile info *)
  add
    (Printf.sprintf "Profile:          %s"
       (Option.value r.profile_name ~default:"(not bound)"));
  add
    (Printf.sprintf "Ambient enabled:  %s"
       (if r.ambient_enabled then "yes" else "no"));
  (match r.quiet_hours_range with
  | Some (start_h, end_h) ->
      add
        (Printf.sprintf "Quiet hours:      %02d:00 - %02d:00 UTC" start_h end_h)
  | None -> add "Quiet hours:      disabled");
  add
    (Printf.sprintf "Rate limit:       %s"
       (if r.rate_limit_rph <= 0 then "unlimited"
        else Printf.sprintf "%d/hour" r.rate_limit_rph));
  add (Printf.sprintf "Deliveries/hour:  %d" r.deliveries_this_hour);
  add "";
  (* Decision summary *)
  let s = r.decision_summary in
  add "Decision Summary (all time):";
  add (Printf.sprintf "  Total decisions:  %d" s.total_decisions);
  add (Printf.sprintf "  Acted:            %d" s.acted_count);
  add (Printf.sprintf "  Skipped:          %d" s.skipped_count);
  if s.skip_breakdown <> [] then begin
    add "  Skip breakdown:";
    List.iter
      (fun (reason, count) ->
        add (Printf.sprintf "    %-25s %d" (format_skip_reason reason) count))
      s.skip_breakdown
  end;
  add "";
  (* Recent decisions *)
  add
    (Printf.sprintf "Recent Decisions (last %d):"
       (List.length r.recent_decisions));
  if r.recent_decisions = [] then add "  (none)"
  else
    List.iter
      (fun (d : Room_watcher_decision.watcher_decision) ->
        let item_info = Printf.sprintf "%s/%s" d.item_source d.item_id in
        add
          (Printf.sprintf "  %-24s  %-20s  %s" d.timestamp (format_outcome d)
             item_info))
      r.recent_decisions;
  add "";
  (* Delivery failures *)
  add
    (Printf.sprintf "Delivery Failures (%d):" (List.length r.delivery_failures));
  if r.delivery_failures = [] then add "  (none)"
  else
    List.iter
      (fun (e : Room_activity_ledger.event) ->
        let error_msg =
          Yojson.Safe.Util.(member "error" e.metadata |> to_string_option)
          |> Option.value ~default:"(unknown)"
        in
        add (Printf.sprintf "  %s  %s" e.timestamp error_msg))
      r.delivery_failures;
  String.concat "\n" (List.rev !lines)
