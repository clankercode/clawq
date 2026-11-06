module S = Test_slack_standing_order_support
module RS = Test_slack_standing_order_recovery_store
module R = Slack_standing_order_recovery_store
module C = Slack_standing_order_reconcile

let test_startup_closes () =
  let db = RS.fresh () in
  let o = RS.new_order db "so_startup" in
  let gap, owner, coverage = RS.scan db o in
  RS.finish db o gap owner coverage;
  let manager = Session.create ~config:(S.config ()) ~db () in
  let ctx =
    Slack_standing_order_ctx.make
      ~accepting:(fun () -> true)
      ~limit:(fun ~channel_id:_ -> Lwt.return_unit)
      ()
  in
  let old_limiter = !Slack_standing_order_slack.configured_limiter in
  C.configure ~db ~manager ~ctx;
  Alcotest.(check bool)
    "startup invalidates existing coverage" false
    (R.covered db o.id o.epoch);
  Lwt_main.run (C.stop ());
  Slack_standing_order_slack.configure_limiter old_limiter;
  ignore (Sqlite3.db_close db)

let suite = [ S.case "startup closes before any await" test_startup_closes ]

open Test_slack_standing_order_reconcile_support
open Lwt.Syntax

let idle e =
  e.roots := [ root ];
  let started = e.clock.now in
  let check_phase label previous =
    let calls = List.filteri (fun i _ -> i >= previous) !(e.calls) in
    print_histogram label calls;
    List.iter
      (fun m ->
        check
          (label ^ " one " ^ m)
          true
          (List.length (List.filter (fun (_, k, _) -> k = m) calls) <= 1))
      [
        "auth.test";
        "bots.info";
        "conversations.info";
        "conversations.members";
        "users.list";
      ];
    check
      (label ^ " at most two history")
      true
      (List.length
         (List.filter (fun (_, m, _) -> m = "conversations.history") calls)
      <= 2);
    check
      (label ^ " at most seven real HTTP requests")
      true
      (List.length calls <= 7)
  in
  let* () = pass e in
  check_phase "startup-0" 0;
  check "initial complete coverage" true (covered e);
  E.start ~ctx:e.ctx ~db:e.f.db ~manager:e.f.base.manager ~http:e.http ();
  let after_startup = List.length !(e.calls) in
  let* () = advance e.clock (started +. 300.) in
  let* () = pass e in
  check_phase "maintenance-300" after_startup;
  let after_300 = List.length !(e.calls) in
  let* () = advance e.clock (started +. 600.) in
  let* () = pass e in
  check_phase "maintenance-600" after_300;
  List.iter
    (fun m ->
      check ("three fresh validations at most: " ^ m) true (count e m <= 3))
    [
      "auth.test";
      "bots.info";
      "conversations.info";
      "conversations.members";
      "users.list";
    ];
  check "window plus only due exact-root calls" true
    (count e "conversations.history" <= 6);
  List.iter
    (fun m -> eq ("no " ^ m) 0 (count e m))
    [
      "conversations.replies";
      "files.info";
      "canvases.getContent";
      "users.info";
      "files.getUploadURLExternal";
      "files.completeUploadExternal";
      "chat.postMessage";
    ];
  check "daemon ten-minute actual HTTP ceiling" true
    (List.length !(e.calls) <= 21);
  no_other_reads e;
  eq "no artificial live fire" 0
    (List.length
       (Slack_standing_order_store.list_fires e.f.db ~order_id:e.order.id
          ~limit:10));
  Lwt.return_unit

let maintenance e =
  let parents =
    List.init 100 (fun n ->
        obj
          [
            ("ts", str (Printf.sprintf "17000000%02d.000001" n));
            ("reply_count", `Int 1);
            ("latest_reply", str "1700000099.000001");
          ])
  in
  e.roots := parents;
  let* () = pass e in
  check "first full thread sweep covered" true (covered e);
  eq "startup replies once per parent" 100 (count e "conversations.replies");
  e.calls := [];
  let* () = advance e.clock (e.clock.now +. 300.) in
  let* () = pass e in
  check "maintenance does not close verified admission" true (covered e);
  eq "unchanged parents require zero replies" 0
    (count e "conversations.replies");
  e.calls := [];
  e.roots := List.map (set "latest_reply" (str "1700000500.000001")) parents;
  let* () = advance e.clock (e.clock.now +. 300.) in
  let start = e.clock.now in
  let p =
    C.pass_with ~ctx:e.ctx ~http:e.http ~db:e.f.db ~manager:e.f.base.manager
      e.order
  in
  let* () = settle 30 in
  check "long maintenance preserves admission before its waits" true (covered e);
  let* () = drain e.clock p in
  eq "changed parents exactly once over bounded slices" 100
    (count e "conversations.replies");
  check "maintenance really exceeds 99 fake seconds" true
    (e.clock.now -. start >= 99.);
  Lwt.return_unit

let config_publication e =
  let* () = pass e in
  check "covered before direct publication" true (covered e);
  let current = Session.get_config e.f.base.manager in
  Session.update_config e.f.base.manager { current with log = current.log };
  check "direct publication closes synchronously" false (covered e);
  let* () = pass e in
  check "same installation reverified" true (covered e);
  Lwt.return_unit

let pending_pause e =
  let* () = pass e in
  let message =
    obj
      [
        ("type", str "message");
        ("channel", str "C123");
        ("user", str "U456");
        ("ts", str "1700000090.000001");
        ("text", str "pause SO-1");
      ]
  in
  let body = event message in
  !Slack_socket.on_ingress_admit ~body;
  check "pause is closed before membership or ACK await" false (covered e);
  eq "pending identity metadata only" 1
    (Sql.int_query e.f.db
       "SELECT count(*) FROM slack_order_ingress WHERE pause=1 AND applied=0" []);
  !Slack_thread_ingress.on_ingress_applied ~body;
  check "handler completion without decision stays held" false (covered e);
  Lwt.return_unit

let socket_before_start () =
  with_env ~socket:true (fun e ->
      check "socket startup closed" false (covered e);
      !Slack_socket.on_gap ~cause:"hello";
      !Slack_socket.on_connected ();
      let* () = pass e in
      check "hello before worker installation preserved" true (covered e);
      !Slack_socket.on_gap ~cause:"disconnect";
      check "disconnect synchronously invalidates" false (covered e);
      let before = List.length !(e.calls) in
      let* () = pass e in
      eq "offline no discovery HTTP" before (List.length !(e.calls));
      !Slack_socket.on_gap ~cause:"hello";
      !Slack_socket.on_connected ();
      let* () = pass e in
      check "fresh hello coverage recovers" true (covered e);
      Lwt.return_unit)

let suite =
  suite
  @ [
      case "ordinary setup combined daemon actual idle histogram" idle;
      case "full window incremental threads across bounded slices" maintenance;
      case "direct config publication and revalidation" config_publication;
      case "pending pause before ACK blocks admission" pending_pause;
      S.case "hello before worker setup disconnect and offline"
        socket_before_start;
    ]

let lifecycle e =
  with_transport e (fun () ->
      C.start ~db:e.f.db ~manager:e.f.base.manager;
      let* () = advance e.clock (e.clock.now +. 3.) in
      eq "one startup validation" 1 (count e "auth.test");
      let start = e.clock.now in
      let* () = advance e.clock (start +. 297.) in
      eq "one periodic validation at 300" 2 (count e "auth.test");
      let gate, u = Lwt.task () in
      let previous = !(e.answer) in
      (e.answer :=
         fun m p ->
           if m = "auth.test" then
             let* () = gate in
             previous m p
           else previous m p);
      !Slack_socket.on_gap ~cause:"hello";
      !Slack_socket.on_connected ();
      let* () = settle 30 in
      List.iter
        (fun _ ->
          !Slack_socket.on_gap ~cause:"hello";
          !Slack_socket.on_connected ())
        (List.init 20 Fun.id);
      Lwt.wakeup_later u ();
      let* () = advance e.clock (e.clock.now +. 20.) in
      check "storm coalesces to one rerun" true (count e "auth.test" <= 4);
      check "latest generation verified after storm" true (covered e);
      Lwt.return_unit)

let live_latency ?(backoff = false) ?(disconnect_complete = false) e =
  F.set_config e.f (fun cfg ->
      {
        cfg with
        egress =
          {
            cfg.egress with
            default_allowlist =
              {
                Runtime_config.host = "files.slack.com";
                path = Some "/upload/*";
                method_ = Some "POST";
                action = Allow;
                log_policy = No_log;
              }
              :: cfg.egress.default_allowlist;
          };
      });
  let* () = pass e in
  let notes_available = ref (not backoff) in
  let first = ref None and raw_count = ref 0 and allocations = ref 0 in
  let ended_room =
    room |> set "has_ended" (`Bool true) |> set "date_end" (`Int 1700000030)
  in
  let ended_root = root |> set "room" ended_room in
  let ready =
    P.ready
    |> set "thread_ts" (str root_ts)
    |> set "ts" (str "1700000060.000100")
    |> set "channel" (str "C123")
    |> set "type" (str "message")
  in
  let canvas =
    P.canvas
    |> set "huddle_transcript_file_id" (str "FTRANS")
    |> set "shares"
         (obj
            [
              ( "private",
                obj
                  [
                    ( "C123",
                      `List [ P.share root_ts; P.share "1700000060.000100" ] );
                  ] );
            ])
  in
  let transcript =
    P.transcript
    |> set "huddle_transcription"
         (obj
            [
              ("channel_id", str "C123");
              ("date_start", `Int 1700000010);
              ( "lines",
                `List
                  [
                    obj
                      [
                        ("start_time_ms", `Int 1000);
                        ("user_id", str "U123");
                        ("contents", str "one");
                      ];
                    obj
                      [
                        ("start_time_ms", `Int 2000);
                        ("user_id", str "U456");
                        ("contents", str "two");
                      ];
                  ] );
            ])
  in
  let previous = !(e.answer) in
  (e.answer :=
     fun m p ->
       match m with
       | "conversations.history" when P.get "limit" p = `Int 1 ->
           Lwt.return (200, page [ ended_root ])
       | "conversations.replies" when Slack_huddles.field "ts" p = root_ts ->
           Lwt.return
             ( 200,
               page
                 (if !notes_available then [ ended_root; ready ]
                  else [ ended_root ]) )
       | "files.info" ->
           Lwt.return
             ( 200,
               S.ok
                 [
                   ( "file",
                     if Slack_huddles.field "file" p = "FCANVAS" then canvas
                     else transcript );
                 ] )
       | "canvases.getContent" ->
           Lwt.return (200, S.ok [ ("content", str "notes") ])
       | "files.getUploadURLExternal" ->
           incr allocations;
           Lwt.return
             ( 200,
               S.ok
                 [
                   ("file_id", str ("FUPLOAD" ^ string_of_int !allocations));
                   ("upload_url", str "https://files.slack.com/upload/fixture");
                 ] )
       | "files.completeUploadExternal" ->
           first := Some e.clock.now;
           let ts = Printf.sprintf "%.6f" e.clock.now in
           let* () =
             if disconnect_complete then (
               !Slack_socket.on_gap ~cause:"disconnect";
               wait e.clock ~seconds:1.)
             else Lwt.return_unit
           in
           Lwt.return
             ( 200,
               S.ok
                 [
                   ( "files",
                     `List
                       [
                         obj
                           [
                             ( "shares",
                               obj
                                 [
                                   ( "private",
                                     obj
                                       [
                                         ( "C123",
                                           `List [ obj [ ("ts", str ts) ] ] );
                                       ] );
                                 ] );
                           ];
                       ] );
                 ] )
       | _ -> previous m p);
  let upload_http _ =
    incr raw_count;
    Lwt.return
      Slack_standing_order_upload.{ status = 200; headers = []; body = "" }
  in
  E.start ~ctx:e.ctx ~db:e.f.db ~manager:e.f.base.manager ~http:e.http
    ~upload_http ();
  e.roots :=
    List.init 100 (fun n ->
        obj
          [
            ("ts", str (Printf.sprintf "17000000%02d.000001" n));
            ("reply_count", `Int 1);
            ("latest_reply", str "1700000099.000001");
          ]);
  let* () =
    if not backoff then advance e.clock (e.clock.now +. 300.)
    else (
      observe e
        (ended_root
        |> set "channel" (str "C123")
        |> set "type" (str "message")
        |> set "user" (str "USLACKBOT"));
      let rec wait_future n =
        let rows =
          Slack_standing_order_store.list_fires e.f.db ~order_id:e.order.id
            ~limit:10
        in
        let held =
          List.exists
            (fun (f : T.fire) ->
              match Slack_standing_order_store.runtime e.f.db ~fire_id:f.id with
              | Some r ->
                  f.reason = Some "notes_not_ready"
                  && Option.value r.next_attempt_at ~default:0. -. e.clock.now
                     > 250.
              | _ -> false)
            rows
        in
        if held then Lwt.return_unit
        else if n = 0 then
          Alcotest.fail "real worker did not enter 300-second notes backoff"
        else
          let* () = advance e.clock (e.clock.now +. 1.) in
          wait_future (n - 1)
      in
      let* () = wait_future 250 in
      notes_available := true;
      Lwt.return_unit)
  in
  let maintenance =
    C.pass_with ~ctx:e.ctx ~http:e.http ~db:e.f.db ~manager:e.f.base.manager
      e.order
  in
  let* () = advance e.clock (e.clock.now +. 2.) in
  check "maintenance is still running" true (Lwt.is_sleeping maintenance);
  e.calls := [];
  e.classified := [];
  let at = e.clock.now in
  observe e
    (ended_root
    |> set "channel" (str "C123")
    |> set "type" (str "message")
    |> set "user" (str "USLACKBOT"));
  observe e ready;
  observe e ready;
  let rec until_share n =
    if !first <> None then Lwt.return_unit
    else if n = 0 then Alcotest.fail "live share missed 60s"
    else
      let* () = advance e.clock (e.clock.now +. 1.) in
      until_share (n - 1)
  in
  let* () = until_share 60 in
  check "verified content fetched" true (count e "canvases.getContent" = 1);
  eq "two real allocations" 2 !allocations;
  eq "two actual raw uploads" 2 !raw_count;
  eq "one real completion" 1 (count e "files.completeUploadExternal");
  check "first share under a minute during maintenance" true
    (Option.get !first -. at <= 60.);
  List.iter
    (fun m -> check ("live authority bound " ^ m) true (count e m <= 10))
    [
      "auth.test";
      "bots.info";
      "conversations.info";
      "conversations.members";
      "users.list";
    ];
  eq "one ready provenance reply" 1
    (List.length
       (List.filter
          (fun (_, m, p) ->
            m = "conversations.replies" && Slack_huddles.field "ts" p = root_ts)
          !(e.calls)));
  check "independent speaker reads bounded" true (count e "users.info" <= 2);
  eq "no fallback post" 0 (count e "chat.postMessage");
  let live_methods =
    List.filter_map
      (fun (phase, m) -> if phase = "live" then Some m else None)
      !(e.classified)
  in
  let live_count m = List.length (List.filter (( = ) m) live_methods) in
  check "live path history ceiling" true
    (live_count "conversations.history" <= 4);
  check "live path reply ceiling" true (live_count "conversations.replies" <= 1);
  eq "two verified source file reads" 2 (live_count "files.info");
  check "at most 65 actual live HTTP and raw requests" true
    (List.length live_methods + !raw_count <= 65);
  print_histogram
    (if backoff then "B852-notes-backoff-first-share"
     else "healthy-first-share-during-maintenance")
    !(e.calls);
  print_histogram "live-only-to-first-share"
    (List.combine !(e.classified) !(e.calls)
    |> List.filter_map (fun ((phase, _), call) ->
        if phase = "live" then Some call else None));
  Printf.eprintf
    "Task6 live-only requests=%d raw-upload=%d first-share-seconds=%.0f\n%!"
    (List.length live_methods) !raw_count
    (Option.get !first -. at);
  let* () =
    if disconnect_complete then (
      let* () = advance e.clock (e.clock.now +. 2.) in
      let fire =
        List.hd
          (Slack_standing_order_store.list_fires e.f.db ~order_id:e.order.id
             ~limit:10)
      in
      check "pre-gap dispatched completion receipt retained" true
        (fire.relay_ts <> None && fire.dispatched_at <> None);
      eq "one immutable admitted-generation attempt" 1
        (Sql.int_query e.f.db
           "SELECT count(*) FROM slack_order_dispatch_admission WHERE fire_id=?"
           [ Sql.text fire.id ]);
      check "late receipt never opens disconnected admission" false (covered e);
      Lwt.return_unit)
    else Lwt.return_unit
  in
  let stopped = E.stop () in
  let* () = settle 50 in
  check "stop cancels queued limiter waits without advancing time" false
    (Lwt.is_sleeping stopped);
  let* () = stopped in
  let* () = drain e.clock maintenance in
  Lwt.return_unit

let suite =
  suite
  @ [
      case "startup cadence and reconnect storms through start" lifecycle;
      case "live content and upload share under 60s during 100-parent scan"
        (live_latency ~backoff:false);
      case "B852 real 300-second retry wakes during maintenance"
        (live_latency ~backoff:true);
    ]

let root_failures e =
  e.roots := [ root ];
  let previous = !(e.answer) in
  (e.answer :=
     fun m p ->
       if m = "conversations.history" && P.get "limit" p = `Int 1 then
         Lwt.return (503, obj [])
       else previous m p);
  let start = e.clock.now in
  let* () = pass e in
  E.start ~ctx:e.ctx ~db:e.f.db ~manager:e.f.base.manager ~http:e.http ();
  let* () = advance e.clock (start +. 300.) in
  let* () = pass e in
  let* () = advance e.clock (start +. 600.) in
  let* () = pass e in
  let roots =
    List.filter
      (fun (_, m, p) -> m = "conversations.history" && P.get "limit" p = `Int 1)
      !(e.calls)
  in
  eq "only three scheduled failed exact roots" 3 (List.length roots);
  check "failed pass never opens coverage" false (covered e);
  List.iter
    (fun m ->
      check ("combined failure method bound " ^ m) true (count e m <= 5))
    [
      "auth.test";
      "bots.info";
      "conversations.info";
      "conversations.members";
      "users.list";
    ];
  check "combined failure history ceiling" true
    (count e "conversations.history" <= 6);
  check "31 actual HTTP requests maximum" true (List.length !(e.calls) <= 31);
  no_other_reads e;
  print_histogram "combined-root-failure-through-600" !(e.calls);
  check "third failure persists long backoff" true
    (Sql.int_query e.f.db
       "SELECT count(*) FROM slack_order_root_schedule WHERE retry_at>=?"
       [ Sqlite3.Data.FLOAT (start +. 1020.) ]
    = 1);
  Lwt.return_unit

let grant_failure e =
  with_transport e (fun () ->
      let previous = !(e.answer) in
      (e.answer :=
         fun m p ->
           if m = "auth.test" then Lwt.return (503, obj []) else previous m p);
      let start = e.clock.now in
      C.start ~db:e.f.db ~manager:e.f.base.manager;
      let* () = advance e.clock (start +. 600.) in
      check "Grant failures only due retries" true (count e "auth.test" <= 3);
      check "initial authority failure never scans" true
        (List.for_all (fun (_, m, _) -> m = "auth.test") !(e.calls));
      check "failure keeps admission closed" false (covered e);
      Lwt.return_unit)

let gap_hold_pause ?(offline_pause = true) e =
  F.set_config e.f (fun cfg ->
      {
        cfg with
        egress =
          {
            cfg.egress with
            default_allowlist =
              {
                Runtime_config.host = "files.slack.com";
                path = Some "/upload/*";
                method_ = Some "POST";
                action = Allow;
                log_policy = No_log;
              }
              :: cfg.egress.default_allowlist;
          };
      });
  let ended =
    root
    |> set "room"
         (room
         |> set "has_ended" (`Bool true)
         |> set "date_end" (`Int 1700000030))
  in
  let ready = P.ready |> set "thread_ts" (str root_ts) in
  let canvas =
    P.canvas
    |> set "huddle_transcript_file_id" `Null
    |> set "shares"
         (obj
            [
              ( "private",
                obj [ ("C123", `List [ P.share root_ts; P.share P.ready_ts ]) ]
              );
            ])
  in
  e.roots := [ ended ];
  let previous = !(e.answer) in
  (e.answer :=
     fun m p ->
       match m with
       | "conversations.history" when P.get "limit" p = `Int 1 ->
           Lwt.return (200, page [ ended ])
       | "conversations.replies" when Slack_huddles.field "ts" p = root_ts ->
           Lwt.return (200, page [ ended; ready ])
       | "files.info" -> Lwt.return (200, S.ok [ ("file", canvas) ])
       | "canvases.getContent" ->
           Lwt.return (200, S.ok [ ("content", str "private notes") ])
       | "files.getUploadURLExternal" ->
           Lwt.return
             ( 200,
               S.ok
                 [
                   ("file_id", str "FUPLOAD");
                   ("upload_url", str "https://files.slack.com/upload/fixture");
                 ] )
       | "files.completeUploadExternal" ->
           Lwt.return
             ( 200,
               S.ok
                 [
                   ( "files",
                     `List
                       [
                         obj
                           [
                             ( "shares",
                               obj
                                 [
                                   ( "private",
                                     obj
                                       [
                                         ( "C123",
                                           `List
                                             [
                                               obj
                                                 [
                                                   ( "ts",
                                                     str
                                                       (Printf.sprintf "%.6f"
                                                          e.clock.now) );
                                                 ];
                                             ] );
                                       ] );
                                 ] );
                           ];
                       ] );
                 ] )
       | _ -> previous m p);
  let* () = pass e in
  let batch = List.hd (C.pending_batches e.f.db e.order) in
  let release =
    Option.get (Option.get (R.find_batch e.f.db ~batch_id:batch.id)).release_at
  in
  check "real discovery creates held batch" true (release >= e.clock.now +. 600.);
  with_transport e (fun () ->
      C.start ~db:e.f.db ~manager:e.f.base.manager;
      E.start ~ctx:e.ctx ~db:e.f.db ~manager:e.f.base.manager ~http:e.http
        ~upload_http:(fun _ ->
          Lwt.return
            Slack_standing_order_upload.
              { status = 200; headers = []; body = "" })
        ();
      let* () = advance e.clock (release -. 1.) in
      eq "no early catch-up allocation before confirmed full hold" 0
        (count e "files.getUploadURLExternal");
      if not offline_pause then (
        let* () = advance e.clock (release +. 40.) in
        eq "complete missed huddle through sole executor after hold" 1
          (count e "files.completeUploadExternal");
        Lwt.return_unit)
      else (
        !Slack_socket.on_gap ~cause:"disconnect";
        let pause =
          obj
            [
              ("ts", str (Printf.sprintf "%.6f" e.clock.now));
              ("user", str "U456");
              ("text", str "pause SO-1");
            ]
        in
        e.roots := [ pause; ended ];
        let* () = advance e.clock (release +. 30.) in
        eq "offline after hold no allocation" 0
          (count e "files.getUploadURLExternal");
        check "offline admission remains closed" false (covered e);
        !Slack_socket.on_gap ~cause:"hello";
        !Slack_socket.on_connected ();
        let* () = advance e.clock (e.clock.now +. 10.) in
        Alcotest.check S.state_t "offline nonallowlisted pause replayed"
          T.Paused
          (Option.get (Slack_standing_order_store.find e.f.db e.order.id)).state;
        eq "no completion after reconnect replay" 0
          (count e "files.completeUploadExternal");
        Lwt.return_unit))

let suite =
  suite
  @ [
      case "actual root-failure HTTP histogram and shared retry" root_failures;
      case "initial Grant failure prefixes back off" grant_failure;
      case "gap at hold second 599 stays offline and replays pause"
        (gap_hold_pause ~offline_pause:true);
    ]

let unresolved_pause e =
  with_transport e (fun () ->
      let* () = pass e in
      let prior = !(e.answer) in
      (e.answer :=
         fun m p ->
           if m = "conversations.members" then Lwt.return (503, obj [])
           else prior m p);
      let message =
        obj
          [
            ("type", str "message");
            ("channel", str "C123");
            ("user", str "U456");
            ("ts", str "1700000090.000002");
            ("text", str "pause SO-1");
          ]
      in
      let body = event ~id:"Ev-transient-pause" message in
      let* _ =
        drain e.clock
          (Slack.handle_event
             ~config:
               (Option.get (Session.get_config e.f.base.manager).channels.slack)
             ~session_manager:e.f.base.manager body)
      in
      check "transient membership cannot release pause fence" false (covered e);
      eq "unresolved exact pause remains pending" 1
        (Sql.int_query e.f.db
           "SELECT count(*) FROM slack_order_ingress WHERE pause=1 AND \
            applied=0"
           []);
      e.answer := prior;
      e.roots := [ message ];
      let* () = pass e in
      Alcotest.check S.state_t "later sweep commits the pending pause" T.Paused
        (Option.get (Slack_standing_order_store.find e.f.db e.order.id)).state;
      Lwt.return_unit)

let changed_config e =
  let* () = pass e in
  let base = Session.get_config e.f.base.manager in
  let variants =
    [
      ( "disabled",
        fun (sc : Runtime_config.slack_config) ->
          {
            sc with
            standing_orders = { sc.standing_orders with orders_enabled = false };
          } );
      ( "empty targets",
        fun (sc : Runtime_config.slack_config) ->
          {
            sc with
            standing_orders = { sc.standing_orders with allowed_targets = [] };
          } );
      ( "grantor allowlist",
        fun (sc : Runtime_config.slack_config) ->
          { sc with allow_users = [ "UOTHER" ] } );
    ]
  in
  let* () =
    Lwt_list.iter_s
      (fun (name, change) ->
        let cfg = S.with_slack base change in
        Session.update_config e.f.base.manager cfg;
        check (name ^ " closes before await") false (covered e);
        let before = List.length !(e.calls) in
        let* () = pass e in
        eq (name ^ " zero HTTP") before (List.length !(e.calls));
        Session.update_config e.f.base.manager base;
        let* () = pass e in
        Lwt.return_unit)
      variants
  in
  Lwt.return_unit

let late_completion e =
  let gate, u = Lwt.task () in
  let prior = !(e.answer) in
  (e.answer :=
     fun m p ->
       if m = "auth.test" then
         let* () = gate in
         prior m p
       else prior m p);
  let p =
    C.pass_with ~ctx:e.ctx ~http:e.http ~db:e.f.db ~manager:e.f.base.manager
      e.order
  in
  let* () = settle 30 in
  !Slack_socket.on_gap ~cause:"disconnect";
  Lwt.wakeup_later u ();
  let* () = drain e.clock p in
  check "stale initial grant cannot verify offline gap" false (covered e);
  eq "no history after gap in first grant" 0 (count e "conversations.history");
  Lwt.return_unit

let suite =
  suite
  @ [
      case "unresolved ingress pause survives transient membership"
        unresolved_pause;
      case "local policy publication closes before zero-HTTP refusal"
        changed_config;
      case "disconnect while first Grant HTTP awaits invalidates completion"
        late_completion;
    ]

let suite =
  suite @ Test_slack_standing_order_reconcile_edges.suite
  @ Test_slack_standing_order_reconcile_ingress.suite

let suite =
  suite
  @ [
      case "dispatched real upload retains late receipt across disconnect"
        (live_latency ~disconnect_complete:true);
    ]

let suite =
  suite
  @ [
      case
        "actual missed huddle first share follows confirmed full 600-second \
         hold"
        (gap_hold_pause ~offline_pause:false);
    ]

let publication_store_failure e =
  let* () = pass e in
  check "covered before publication store fault" true (covered e);
  let manager = e.f.base.manager in
  let agent, _, _ = Session.get_or_create_locked manager ~key:"slack:C123" in
  let current = Session.get_config manager in
  let next =
    { current with security = { current.security with tools_enabled = false } }
  in
  let sql statement = Slack_standing_order_sql.exec e.f.db statement [] in
  sql
    "CREATE TEMP TRIGGER reject_gap_publication BEFORE UPDATE ON \
     slack_standing_order_recovery_gaps BEGIN SELECT RAISE(ABORT,'injected \
     publication store fault'); END";
  Fun.protect
    ~finally:(fun () -> sql "DROP TRIGGER reject_gap_publication")
    (fun () ->
      let failed =
        try
          Session.update_config manager next;
          false
        with Failure _ -> true
      in
      check "real admission-store failure propagates" true failed;
      check "failed durable closure leaves admission denied" false (covered e);
      check "manager publishes policy despite hook fault" false
        (Session.get_config manager).security.tools_enabled;
      check "existing agent policy remains synchronized" false
        agent.Agent.config.security.tools_enabled);
  let before = List.length !(e.calls) in
  let* () = pass e in
  eq "local denial after failed publication makes zero HTTP" before
    (List.length !(e.calls));
  Lwt.return_unit

let suite =
  suite
  @ [
      case "publication store failure denies and refreshes existing agents"
        publication_store_failure;
    ]
