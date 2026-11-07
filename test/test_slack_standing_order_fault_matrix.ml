open Slack_standing_order_fault_support
open Lwt.Syntax

let outcomes = [ Timeout; Status_500; Crash ]

let no_more_than e n =
  check "unauthorized delivery count stays within stricter bound" true
    (relays e <= n);
  bounds e.server

let fire_fault point outcome after e =
  let* () = pass e in
  if point = Share_lookup then e.server.complete_shares <- false;
  let* () =
    fault_run e point outcome after (fun () ->
        start e;
        let* () = advance e 3. in
        live e;
        until e "named external fault actually reached"
          (fun () -> e.server.hits <> [])
          180)
  in
  no_more_than e 1;
  let saved = one e in
  let attempts = (Option.get (Store.runtime e.db ~fire_id:saved.id)).attempts in
  let before = relays e in
  let* () = restart e in
  eq "server messages survive worker memory and SQLite reopen" before (relays e);
  let* () = advance e 1201. in
  let* () = pass e in
  let* () = advance e 601. in
  let* () = pass e in
  let* () = worker e in
  eq "unproven completion absence stays held; other healthy cases recover"
    (if point = Complete_before_share && not after then 0 else 1)
    (relays e);
  no_more_than e 1;
  let final = Option.get (Store.runtime e.db ~fire_id:saved.id) in
  List.iter
    (fun (a : Store.relay_attempt) ->
      if a.dispatched_at <> None then
        let retained = Option.get (Store.attempt final a.attempt_no) in
        check "dispatch identity retained after real reopen" true
          (retained.uploaded_file_ids = a.uploaded_file_ids
          && retained.dispatched_at = a.dispatched_at))
    attempts;
  eq "fault consumed exactly once" 1 (List.length e.server.hits);
  Printf.eprintf
    "T8_FAULT point=%s outcome=%s after=%b shares=%d attempts=%d reopen=true\n\
     %!"
    (point_name point) (outcome_name outcome) after (relays e)
    (List.length final.attempts);
  Lwt.return_unit

let notice_fault point outcome after e =
  e.server.roots <- [ ended_root ];
  ignore (install_instance e ended_root);
  let* () = fault_run e point outcome after (fun () -> pass e) in
  check "notice fault reached actual post" true (e.server.hits <> []);
  eq "no relay before confirmed notice" 0 (relays e);
  let* () = restart e in
  (* Let all owner and interrupted-unit deadlines expire before adoption.
     This outage path deliberately has no normal-event latency assertion. *)
  let* () = advance e 1201. in
  let* () = pass e in
  let* () = advance e 599. in
  eq "no relay before renewed confirmed hold" 0 (relays e);
  let* () = advance e 2. in
  let* () = pass e in
  let* () = worker e in
  bounds e.server;
  check "notice ambiguous outcome never blindly reposted" true
    (List.length
       (List.filter (fun m -> m.kind = "notice") e.server.state.messages)
    <= 1);
  no_more_than e 1;
  let visible_notice =
    List.exists (fun m -> m.kind = "notice") e.server.state.messages
  in
  if visible_notice then
    eq "confirmed/adopted visible notice eventually releases exactly one share"
      1 (relays e)
  else eq "unprovable notice remains held with zero relay" 0 (relays e);
  Lwt.return_unit

let page_fault point outcome after e =
  e.server.roots <- [ root ];
  let* () = fault_run e point outcome after (fun () -> pass e) in
  check "page fault reached actual HTTP" true (e.server.hits <> []);
  check "incomplete scan cannot certify coverage" false
    (R.covered e.db e.order.id e.order.epoch);
  eq "incomplete scan produces zero relays" 0 (relays e);
  let* () = restart e in
  let* () = advance e 1201. in
  let* () = pass e in
  check "fresh scan recovers only with completed coverage" true
    (R.covered e.db e.order.id e.order.epoch);
  eq "live source never manufactured fire" 0 (List.length (fires e));
  Lwt.return_unit

let retention e =
  H.drain e.server.clock
    (Slack_standing_order_retention.pass ~ctx:e.ctx ~db:e.db ~manager:e.manager
       ~http:(server_http e.server)
       (Option.get (Store.find e.db e.order.id)))

let retention_fault point outcome after e =
  let* () = pass e in
  start e;
  live e;
  let* () =
    until e "actual relay for retention fault" (fun () -> relays e = 1) 59
  in
  let* () = stop () in
  e.server.state <- { e.server.state with deleted = [ "FCANVAS" ] };
  checkpoint e.server;
  let* () =
    fault_run e point outcome after (fun () ->
        let* () = advance e 301. in
        retention e)
  in
  check "retention fault reached actual external side effect" true
    (e.server.hits <> []);
  let* () = restart e in
  let* () = advance e 301. in
  let* () = retention e in
  let* () = advance e 301. in
  let* () = retention e in
  no_more_than e 1;
  let notes =
    List.length
      (List.filter (fun m -> m.kind = "cleanup") e.server.state.messages)
  in
  check "cleanup note never repeated after loss" true (notes <= 1);
  check "source is never a deletion target" true
    (not
       (List.exists
          (fun r ->
            r.method_ = "files.delete"
            && List.assoc_opt "file" r.ids = Some "FCANVAS")
          e.server.calls));
  Lwt.return_unit

let state_role = function
  | T.Detected | Fetching | Uploading -> `Predispatch
  | Posting | Posting_unknown -> `Ambiguous
  | Posted | Acknowledged | Picked_up -> `Retained
  | No_pickup | Not_delivered | No_notes | Unavailable | Skipped_no_transcript
  | Denied | Skipped_inactive | Skipped_cap | Failed ->
      `Terminal

let dispatch_state state = List.mem (state_role state) [ `Ambiguous; `Retained ]
let pre_state state = state_role state = `Predispatch

let capture_pre e state =
  let gate, _ = Lwt.task () and reached = ref false in
  e.server.hook <-
    (fun _ _ ->
      match fires e with
      | [ f ] when f.T.state = state ->
          reached := true;
          gate
      | _ -> Lwt.return_unit);
  start e;
  live e;
  let* () =
    until e
      ("actual transition reached " ^ T.fire_state_to_string state)
      (fun () -> !reached)
      59
  in
  let* () = H.drain e.server.clock (E.stop ()) in
  e.server.hook <- (fun _ _ -> Lwt.return_unit);
  Lwt.return_unit

let public_state state e =
  let* () = pass e in
  let actual =
    List.mem state
      T.[ Detected; Fetching; Uploading; Posting; Posting_unknown; Posted ]
  in
  let* () =
    if actual then crash_state e state
    else (
      start e;
      live e;
      let* () =
        until e "real side effect before retained/terminal projection"
          (fun () -> relays e = 1)
          59
      in
      let* () = stop () in
      let fire = one e in
      (* Terminal-state restart controls retain a real prior delivery. The
       projection isolates restart invariance, not reachability of its cause. *)
      Sql.exec e.db
        "UPDATE slack_standing_order_fires SET \
         state=?,reason=NULL,next_attempt_at=NULL WHERE id=?"
        [ Sql.text (T.fire_state_to_string state); Sql.text fire.id ];
      Lwt.return_unit)
  in
  let fire = one e in
  Alcotest.check S.fire_state_t "exact public state at loss" state fire.state;
  let before = relays e in
  let* () = restart e in
  let other = Sqlite3.db_open e.path in
  Lwt.finalize
    (fun () ->
      check "different handles" true (other != e.db);
      let retained = Option.get (Store.runtime other ~fire_id:fire.id) in
      Alcotest.check S.fire_state_t "second handle sees durable public state"
        state retained.fire.state;
      let* () = pass e in
      let* () = advance e 599. in
      eq "restart never bypasses notice hold" before (relays e);
      let* () = advance e 2. in
      let* () = pass e in
      let* () = worker e in
      no_more_than e 1;
      if not (pre_state state) then
        eq "retained or terminal state cannot authorize new relay" before
          (relays e);
      Printf.eprintf
        "T8_STATE %s provenance=%s shares=%d reopen=true handles=2\n%!"
        (T.fire_state_to_string state)
        (if actual then "actual-process-exit"
         else if dispatch_state state then "retained-projection-control"
         else "terminal-invariance-control")
        (relays e);
      Lwt.return_unit)
    (fun () ->
      ignore (Sqlite3.db_close other);
      Lwt.return_unit)

let phase e phase =
  if List.mem phase [ "idle"; "pause"; "history"; "swept"; "complete" ] then (
    e.server.roots <- [ root ];
    let* () = pass e in
    Sql.exec e.db "UPDATE slack_standing_order_recovery_passes SET phase=?"
      [ Sql.text phase ];
    let* () = restart e in
    check "restart invalidates phase authority" false
      (R.covered e.db e.order.id e.order.epoch);
    let* () = pass e in
    check "all required work repeated before verification" true
      (R.covered e.db e.order.id e.order.epoch);
    eq "no relay from phase control" 0 (relays e);
    Lwt.return_unit)
  else (
    e.server.roots <- [ ended_root ];
    ignore (install_instance e ended_root);
    if phase = "pending" then
      Sql.exec e.db
        "CREATE TEMP TRIGGER fault_pending BEFORE UPDATE OF status ON \
         slack_standing_order_recovery_batches WHEN \
         NEW.status='notice_attempted' BEGIN SELECT \
         RAISE(ABORT,'pending-before-reservation'); END"
        [];
    let* () = pass e in
    let batch = List.hd (C.pending_batches e.db e.order) in
    if phase = "pending" then Sql.exec e.db "DROP TRIGGER fault_pending" []
    else
      Sql.exec e.db
        "UPDATE slack_standing_order_recovery_batches SET status=? WHERE id=?"
        [ Sql.text phase; Sql.text batch.id ];
    let* () = restart e in
    let* () = pass e in
    let* () = advance e 599. in
    eq "phase loss never shortens hold" 0 (relays e);
    let* () = advance e 2. in
    let* () = pass e in
    let* () = worker e in
    bounds e.server;
    if phase = "cancelled" then
      eq "cancelled recovery batch never revives" 0 (relays e);
    no_more_than e 1;
    Lwt.return_unit)

let suite =
  List.concat_map
    (fun point ->
      List.concat_map
        (fun outcome ->
          List.map
            (fun after ->
              let work =
                match point with
                | Allocate | Binary_upload | Complete_before_share
                | Complete_after_share | Share_lookup ->
                    fire_fault point outcome after
                | Notice_before_post | Notice_after_post ->
                    notice_fault point outcome after
                | Pause_page | History_page -> page_fault point outcome after
                | Retention_delete | Retention_note ->
                    retention_fault point outcome after
              in
              case
                (Printf.sprintf "server %s %s %s then real process-state loss"
                   (point_name point) (outcome_name outcome)
                   (if after then "after" else "before"))
                work)
            [ false; true ])
        outcomes)
    points
  @ List.map
      (fun state ->
        case
          ("all_fire_states restart " ^ T.fire_state_to_string state)
          (public_state state))
      T.all_fire_states
  @ List.map
      (fun p ->
        case ("durable recovery phase restart " ^ p) (fun e -> phase e p))
      [
        "idle";
        "pause";
        "history";
        "swept";
        "complete";
        "pending";
        "notice_attempted";
        "notice_unknown";
        "confirmed";
        "released";
        "cancelled";
      ]
