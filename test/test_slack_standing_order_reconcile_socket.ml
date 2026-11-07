open Test_slack_standing_order_reconcile_support
open Lwt.Syntax

let gap_hold_pause ?(offline_pause = true) ?(stale_click = false) ?instance
    ?predispatch ~disconnect ~reconnect e =
  let root, room, root_ts, ready_ts =
    Option.value instance ~default:(root, room, root_ts, P.ready_ts)
  in
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
  let* () = reconnect () in
  let ended =
    root
    |> set "room"
         (room
         |> set "has_ended" (`Bool true)
         |> set "date_end"
              (`Int
                 ((match P.get "date_start" room with
                    | `Int n -> n
                    | _ -> 1700000010)
                 + 20)))
  in
  let canvas_id = Slack_huddles.field "transcript_file_id" room in
  let ready =
    P.ready
    |> set "thread_ts" (str root_ts)
    |> set "ts" (str ready_ts)
    |> set "files" (`List [ obj [ ("id", str canvas_id) ] ])
  in
  let canvas =
    P.canvas
    |> set "id" (str canvas_id)
    |> set "huddle_transcript_file_id" `Null
    |> set "shares"
         (obj
            [
              ( "private",
                obj [ ("C123", `List [ P.share root_ts; P.share ready_ts ]) ] );
            ])
  in
  e.roots := [ ended ];
  let original =
    Option.map
      (fun state ->
        let start =
          match P.get "date_start" room with
          | `Int n -> string_of_int n
          | _ -> "1700000010"
        in
        let f =
          S.fire e.f.db e.order ~room:"R123" ~date_start:start ~state ()
        in
        Sql.exec e.f.db
          "UPDATE slack_standing_order_fires SET root_ts=? WHERE id=?"
          Sql.[ text root_ts; text f.id ];
        f.id)
      predispatch
  in
  let previous = !(e.answer) in
  (e.answer :=
     fun m p ->
       match m with
       | "conversations.history"
         when P.get "limit" p = `Int 1
              && Slack_huddles.field "oldest" p = root_ts ->
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
  let batch =
    match C.pending_batches e.f.db e.order with
    | b :: _ -> b
    | [] ->
        Alcotest.failf
          "real discovered batch absent: recovery=%s calls=%d fires=%d"
          (Yojson.Safe.to_string (R.status_json e.f.db ~order_id:e.order.id))
          (List.length !(e.calls))
          (List.length
             (Slack_standing_order_store.list_fires e.f.db ~order_id:e.order.id
                ~limit:100))
  in
  let release =
    Option.get (Option.get (R.find_batch e.f.db ~batch_id:batch.id)).release_at
  in
  check "real discovery creates held batch" true (release >= e.clock.now +. 600.);
  if stale_click then
    with_transport e (fun () ->
        let blocked, wake = Lwt.task () and entered = ref false in
        let before = !(e.answer) in
        (e.answer :=
           fun m p ->
             if m = "conversations.members" then (
               entered := true;
               let* () = blocked in
               before m p)
             else before m p);
        let click =
          F.click e.f ~action_id:Slack_standing_order_render.action_pause
            ~card_id:batch.id
            ~message_ts:(Option.get batch.notice_ts)
            ~user:"U456" ()
        in
        let* () = advance e.clock (e.clock.now +. 3.) in
        check "C-created notice click reaches actual member wait" true !entered;
        let paused =
          Slack_standing_order_store.deactivate e.f.db ~order_id:e.order.id
            (Slack_standing_order_store.Pause "U123") ~now:e.clock.now
          |> Result.get_ok
        in
        let resumed =
          Slack_standing_order_store.resume e.f.db ~order_id:e.order.id
            ~expected_epoch:paused.after.epoch
            ~active_since:(Printf.sprintf "%.6f" e.clock.now)
            ~bot_user_id:"UBOT" ~bot_id:"BOWN" ~now:e.clock.now
          |> Result.get_ok
        in
        Lwt.wakeup_later wake ();
        let* _ = drain e.clock click in
        let current =
          Option.get (Slack_standing_order_store.find e.f.db e.order.id)
        in
        check "old reconciler-created notice cannot pause resumed order" true
          (current.state = T.Active && current.epoch = resumed.epoch);
        eq "stale click never creates relay" 0
          (count e "files.completeUploadExternal");
        Lwt.return_unit)
  else
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
        let* () = disconnect () in
        let* () = advance e.clock (release +. 30.) in
        eq "both controls remain held offline after 600" 0
          (count e "files.getUploadURLExternal");
        check "offline continuity denied" false (covered e);
        if not offline_pause then (
          let* () = reconnect () in
          let* () = advance e.clock (e.clock.now +. 40.) in
          eq "complete missed huddle through sole executor after hold" 1
            (count e "files.completeUploadExternal");
          Option.iter
            (fun id ->
              let f =
                Option.get
                  (Slack_standing_order_store.runtime e.f.db ~fire_id:id)
              in
              check "pre-dispatch startup row retains original fire identity"
                true (f.fire.relay_ts <> None);
              eq "one retained attempt and no third relay" 1
                (List.length f.attempts))
            original;
          Lwt.return_unit)
        else
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
          let* () = reconnect () in
          let* () = advance e.clock (e.clock.now +. 10.) in
          Alcotest.check S.state_t "offline nonallowlisted pause replayed"
            T.Paused
            (Option.get (Slack_standing_order_store.find e.f.db e.order.id))
              .state;
          eq "no completion after reconnect replay" 0
            (count e "files.completeUploadExternal");
          Lwt.return_unit)

let gap_case ?(offline_pause = true) ?predispatch e =
  (* Explicit Socket mode is installed before configure; this is ordinary
     connector ingress, with its second connection unavailable while offline. *)
  with_transport e (fun () ->
      let first = Test_slack_socket.make_socket ()
      and second = Test_slack_socket.make_socket () in
      let online, u = Lwt.task () in
      let connects = ref 0
      and disconnected = ref false
      and attached = ref false in
      let second_transport =
        {
          second.transport with
          on_message =
            (fun cb ->
              attached := true;
              second.transport.on_message cb);
        }
      in
      let connector =
        Slack_socket.run
          ~sleep:(fun seconds -> wait e.clock ~seconds)
          ~now:(fun () -> e.clock.now)
          ~connect:(fun () ->
            incr connects;
            if !connects = 1 then Lwt.return first.transport
            else
              let+ () = Lwt.protected online in
              second_transport)
          ~handle_event:(fun body ->
            let+ _ =
              Slack.handle_event
                ~config:
                  (Option.get
                     (Session.get_config e.f.base.manager).channels.slack)
                ~session_manager:e.f.base.manager body
            in
            ())
          ~on_health:(fun _ -> ())
          ()
      in
      Lwt.finalize
        (fun () ->
          let* () = settle 30 in
          let* () = first.emit {|{"type":"hello"}|} in
          let disconnect () =
            disconnected := true;
            let* () = first.emit {|{"type":"disconnect"}|} in
            settle 30
          in
          let reconnect () =
            let before = List.length !(e.calls) in
            if !disconnected && Lwt.is_sleeping online then
              Lwt.wakeup_later u ();
            let rec await_connection n =
              if (not !disconnected) || !attached then settle 40
              else if n = 0 then
                Alcotest.fail "second actual socket never connected"
              else
                let* () = advance e.clock (e.clock.now +. 1.) in
                await_connection (n - 1)
            in
            let* () = await_connection 60 in
            check "TCP reconnect alone never verifies coverage" false
              (covered e);
            eq "no TCP-triggered Slack pass" before (List.length !(e.calls));
            let socket = if !disconnected then second else first in
            let* () = socket.emit {|{"type":"hello"}|} in
            check "new hello still requires full replay" false (covered e);
            eq "no relay at hello frontier" 0
              (count e "files.getUploadURLExternal");
            Lwt.return_unit
          in
          gap_hold_pause ~offline_pause ?predispatch ~disconnect ~reconnect e)
        (fun () -> Slack_standing_order_resolution_lease.cancel connector))
