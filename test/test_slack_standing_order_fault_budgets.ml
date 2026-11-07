open Slack_standing_order_fault_support
open Lwt.Syntax

let grant_methods =
  [
    "auth.test";
    "bots.info";
    "conversations.info";
    "conversations.members";
    "users.list";
  ]

let histogram s label =
  let methods =
    List.map (fun r -> r.method_) s.calls |> List.sort_uniq String.compare
  in
  Printf.eprintf "T8_HTTP %s total=%d %s\n%!" label (List.length s.calls)
    (String.concat ","
       (List.map (fun m -> m ^ "=" ^ string_of_int (count s m)) methods))

let ceilings s label table =
  histogram s label;
  List.iter
    (fun (m, n) -> check (label ^ " ceiling " ^ m) true (count s m <= n))
    table;
  check
    (label ^ " zero unlisted actual methods")
    true
    (List.for_all (fun r -> List.mem_assoc r.method_ table) s.calls)

let idle e =
  e.server.roots <- [ root ];
  e.server.calls <- [];
  start e;
  let* () = advance e 602. in
  ceilings e.server "combined-idle-0-300-600"
    (List.map (fun m -> (m, 3)) grant_methods @ [ ("conversations.history", 6) ]);
  check "combined idle <=21 actual HTTP/raw calls" true
    (List.length e.server.calls <= 21);
  List.iter
    (fun m ->
      eq ("three actual periodic validations " ^ m) 3 (count e.server m))
    grant_methods;
  eq "live root never becomes artificial fire" 0 (List.length (fires e));
  Lwt.return_unit

let failing_root e =
  e.server.roots <- [ root ];
  e.server.override <-
    (fun m p ->
      if m = "conversations.history" && get "limit" p = `Int 1 then
        Some (503, obj [])
      else None);
  e.server.calls <- [];
  start e;
  let* () = advance e 602. in
  ceilings e.server "combined-root-failure-0-120-420"
    (List.map (fun m -> (m, 5)) grant_methods @ [ ("conversations.history", 6) ]);
  check "combined root failure <=31 actual calls" true
    (List.length e.server.calls <= 31);
  let roots =
    List.filter
      (fun r ->
        r.method_ = "conversations.history"
        && List.mem_assoc "latest" r.ids
        && List.assoc "latest" r.ids = H.root_ts)
      e.server.calls
  in
  eq "three exact root failures" 3 (List.length roots);
  check "failed page never claims coverage" false
    (R.covered e.db e.order.id e.order.epoch);
  eq "no artificial failed live fire" 0 (List.length (fires e));
  Lwt.return_unit

let parents =
  List.init 100 (fun n ->
      obj
        [
          ("ts", str (Printf.sprintf "17000000%02d.000001" n));
          ("reply_count", `Int 1);
          ("latest_reply", str "1700000099.000001");
        ])

let maintenance e =
  e.server.roots <- parents;
  let* () = pass e in
  eq "initial full gap sweeps all 100 threads" 100
    (count e.server "conversations.replies");
  start e;
  e.server.calls <- [];
  let* () = advance e 301. in
  eq "100 unchanged parents add zero replies" 0
    (count e.server "conversations.replies");
  e.server.roots <-
    List.map (set "latest_reply" (str "1700000900.000001")) parents;
  e.server.calls <- [];
  let* () = advance e 401. in
  eq "100 changed parents add exactly one actual request each" 100
    (count e.server "conversations.replies");
  let replies =
    List.filter (fun r -> r.method_ = "conversations.replies") e.server.calls
  in
  let first = List.hd replies and last = List.hd (List.rev replies) in
  check "100 actual maintenance replies span at least 99 fake seconds" true
    (last.at -. first.at >= 99.);
  Lwt.return_unit

let latency ~backoff e =
  e.server.roots <- parents;
  let* () = pass e in
  start e;
  let* () = advance e 3. in
  let* () =
    if backoff then (
      e.server.ready <- false;
      e.server.roots <- parents @ [ ended_root ];
      ignore (install_instance e ended_root);
      observe e (event_root ended_root);
      until e "real worker enters 300s notes backoff"
        (fun () ->
          List.exists
            (fun (f : T.fire) ->
              match Store.runtime e.db ~fire_id:f.id with
              | Some r ->
                  f.reason = Some "notes_not_ready"
                  && Option.value r.next_attempt_at ~default:0.
                     -. e.server.clock.now
                     > 250.
              | _ -> false)
            (fires e))
        250)
    else Lwt.return_unit
  in
  if backoff then
    check "SQL due excludes backoff before wake" true
      (not
         (List.exists
            (fun (r : Store.fire_runtime) -> r.fire.id = (one e).id)
            (Store.due e.db ~now:e.server.clock.now)));
  e.server.ready <- true;
  e.server.roots <-
    (List.map (set "latest_reply" (str "1700000900.000001")) parents
    @ if backoff then [ ended_root ] else []);
  C.trigger ();
  let* () = advance e 3. in
  let bg_before = count e.server "conversations.replies" in
  check "actual maintenance is unfinished when ready arrives" true
    (bg_before < 200);
  e.server.calls <- [];
  let ready_at = e.server.clock.now in
  e.server.roots <- (e.server.roots @ if backoff then [] else [ ended_root ]);
  ignore (install_instance e ended_root);
  observe e (event_root ended_root);
  observe e (ready_for ended_root |> set "files" (`List []));
  observe e (ready_for ended_root);
  observe e (ready_for ended_root);
  let* () =
    until e "first actual share under 60s amid both running workers"
      (fun () -> relays e = 1)
      59
  in
  check "first healthy share <60 fake seconds" true
    (e.server.clock.now -. ready_at < 60.);
  let live_calls = List.filter (fun r -> r.lane = "live") e.server.calls in
  let all_calls = e.server.calls in
  e.server.calls <- live_calls;
  ceilings e.server
    (if backoff then "B852-live-backoff" else "healthy-live-priority")
    (List.map (fun m -> (m, 10)) grant_methods
    @ [
        ("conversations.history", 4);
        ("conversations.replies", 1);
        ("files.info", 2);
        ("canvases.getContent", 1);
        ("users.info", 2);
        ("files.getUploadURLExternal", 2);
        ("raw_upload", 2);
        ("files.completeUploadExternal", 1);
      ]);
  List.iter
    (fun (m, n) -> eq ("actual live boundary " ^ m) n (count e.server m))
    [
      ("files.info", 2);
      ("canvases.getContent", 1);
      ("files.getUploadURLExternal", 2);
      ("raw_upload", 2);
      ("files.completeUploadExternal", 1);
    ];
  check "actual live total includes raw and is <=65" true
    (List.length live_calls <= 65);
  e.server.calls <- all_calls;
  Printf.eprintf
    "T8_FIRST_SHARE backoff=%b elapsed=%.0f raw=2 total_live=%d\n%!" backoff
    (e.server.clock.now -. ready_at)
    (List.length live_calls);
  Lwt.return_unit

let suite =
  [
    case "both running workers idle actual method ceilings" idle;
    case "both running workers share exact-root failure backoff" failing_root;
    case "all 100 unchanged and changed parents accounted" maintenance;
    case "ordinary fresh first share preempts long maintenance"
      (latency ~backoff:false);
    case "B852 SQL due backoff wake preempts long maintenance"
      (latency ~backoff:true);
  ]

let retry_after e =
  let* () = pass e in
  start e;
  e.server.ready <- false;
  live e;
  let* () =
    until e "real notes-not-ready fetching checkpoint"
      (fun () ->
        List.exists
          (fun (f : T.fire) -> f.reason = Some "notes_not_ready")
          (fires e))
      59
  in
  let fire = one e in
  let* granted =
    H.drain e.server.clock
      (Slack_standing_order_grant.evaluate ~db:e.db
         ~current_config:(fun () -> Session.get_config e.manager)
         ~http:(server_http e.server) ~limiter:(Option.get e.ctx.limiter)
         ~now:e.server.clock.now
         ~stage:
           (Slack_standing_order_grant.Fire
              { fire_id = fire.id; fire_epoch = fire.epoch })
         ~unit_label:"retry-after-fixture"
         (Slack_standing_order_grant.subject_of_order e.order))
  in
  let validated = Result.get_ok granted in
  let once = ref false in
  e.server.override <-
    (fun m _ ->
      if m = "files.info" && not !once then (
        once := true;
        Some (429, obj []))
      else None);
  let* result =
    H.drain e.server.clock
      (validated.call "files.info" [ ("file", str "FCANVAS") ])
  in
  check "actual 429 establishes server cooldown" true
    (result = Error SO.Rate_limited);
  let at = e.server.clock.now in
  e.server.ready <- true;
  observe e (ready_for ended_root);
  let* () = advance e 59. in
  eq "ready hint cannot bypass actual transport cooldown" 0 (relays e);
  let* () =
    until e "ready work resumes after Retry-After and fresh final fences"
      (fun () -> relays e = 1)
      59
  in
  check "cooldown really remained binding" true (e.server.clock.now -. at >= 60.);
  Lwt.return_unit

let suite =
  suite
  @ [
      case "B852 ready wake respects actual 429 transport cooldown" retry_after;
    ]
