include Agent_provider_ledger

let room_budget_profile_id_for_turn ~db ?session_key () =
  match session_key with
  | None -> None
  | Some session_key ->
      profiled_room_candidates ~db ~session_key ()
      |> List.find_map (fun room_id ->
          match Memory.get_room_profile_binding ~db ~room_id with
          | Some binding -> Some binding.profile_id
          | None -> None)

let format_room_budget_exceeded (state : Room_budget.state) =
  Room_budget.budget_exceeded_message_redacted state

let format_room_budget_exceeded_admin (state : Room_budget.state) =
  Printf.sprintf
    "budget exceeded for room profile %d: current usage is %d tokens, %.6f \
     USD, %d turn(s); limits are %d tokens and %.6f USD; period started at %s"
    state.profile_id state.current_usage.total_tokens
    state.current_usage.cost_usd state.current_usage.turns state.token_limit
    state.cost_limit_usd state.period_started_at

let default_room_budget_reservation_tokens = 20

let estimated_room_budget_reservation_tokens ~db ~profile_id ~messages =
  let estimated =
    max 1
      (min default_room_budget_reservation_tokens
         (Provider.estimate_messages_tokens messages))
  in
  match Room_budget.get_profile_budget ~db ~profile_id with
  | Some state -> min estimated state.token_limit
  | None -> estimated

let check_room_budget_before_provider_call ?db ?session_key () =
  match db with
  | None -> ()
  | Some db -> (
      match room_budget_profile_id_for_turn ~db ?session_key () with
      | None -> ()
      | Some profile_id -> (
          match Room_budget.get_profile_budget ~db ~profile_id with
          | Some state when state.limit_exceeded ->
              Logs.warn (fun m ->
                  m "Budget exceeded (admin): %s"
                    (format_room_budget_exceeded_admin state));
              raise (Budget_exceeded (format_room_budget_exceeded state))
          | Some _ | None -> ()))

let reserve_room_budget_before_provider_call ?db ?session_key ~messages () =
  let open Lwt.Syntax in
  match db with
  | None -> Lwt.return (fun () -> ())
  | Some db -> (
      match room_budget_profile_id_for_turn ~db ?session_key () with
      | None -> Lwt.return (fun () -> ())
      | Some profile_id -> (
          let estimated_tokens =
            estimated_room_budget_reservation_tokens ~db ~profile_id ~messages
          in
          let* reservation =
            Room_budget.reserve_profile_budget ~db ~profile_id ~estimated_tokens
              ~estimated_cost_usd:0.0
          in
          match reservation with
          | Ok release ->
              (match Room_budget.check_soft_budget_warning ~db ~profile_id with
              | Some (_state, msg) -> Logs.warn (fun m -> m "%s" msg)
              | None -> ());
              Lwt.return release
          | Error state ->
              Logs.warn (fun m ->
                  m "Budget exceeded (admin): %s"
                    (format_room_budget_exceeded_admin state));
              Lwt.fail (Budget_exceeded (format_room_budget_exceeded state))))
