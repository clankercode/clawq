(* This test kills the worker process after Slack's real fixture share but
   before its completion receipt. The parent has never owned a worker Session. *)
open Slack_standing_order_fault_support
open Lwt.Syntax

let process_loss () =
  with_server (fun server ->
      let metadata = Filename.temp_file "fault-process-metadata" ".bin" in
      let child = Unix.fork () in
      if child = 0 then (
        Random.self_init ();
        try
          let () =
            with_env ~server (fun e ->
                let ch = open_out_bin metadata in
                Marshal.to_channel ch
                  (e.path, Session.get_config e.manager, e.order)
                  [];
                close_out ch;
                e.server.hook <-
                  (fun m _ ->
                    if m = "shared" then Unix._exit 0 else Lwt.return_unit);
                let* () = pass e in
                start e;
                live e;
                let* () = advance e 59. in
                Lwt.return_unit)
          in
          Unix._exit 3
        with _ -> Unix._exit 4)
      else
        let _, status = Unix.waitpid [] child in
        check
          "separate worker process exits at persisted external share boundary"
          true (status = Unix.WEXITED 0);
        let ch = open_in_bin metadata in
        let path, config, (order : T.order) = Marshal.from_channel ch in
        close_in ch;
        reload server;
        server.instances <-
          List.filter_map (fun m -> m.instance) server.state.messages;
        server.roots <- [ ended_root ];
        server.clock.now <- S.now +. 250.;
        eq "parent independently loads exactly one server-side share" 1
          (relay_count server ~order_id:order.id ~room_id:"R123"
             ~date_start:"1700000010");
        let db = Sqlite3.db_open path in
        let old = !SO.configured_limiter in
        let manager = Session.create ~config ~db () in
        let accepting = ref true in
        let e =
          {
            server;
            path;
            db;
            manager;
            order;
            ctx = context server accepting;
            accepting;
            closed = false;
          }
        in
        (Store.clock := fun () -> server.clock.now);
        Fun.protect
          ~finally:(fun () ->
            Lwt_main.run (stop ());
            SO.configure_limiter old;
            ignore (Sqlite3.db_close db);
            Sys.remove path;
            Sys.remove metadata)
          (fun () ->
            let fire = one e in
            Alcotest.check S.fire_state_t
              "process died before completion receipt" T.Posting fire.state;
            check "no local fabricated receipt" true (fire.relay_ts = None);
            Lwt_main.run
              (transport server (fun () ->
                   configure e;
                   let* () = pass e in
                   let* () = worker e in
                   Lwt.return_unit));
            eq "actual process loss does not create a second share" 1 (relays e);
            check "new process adopts the already posted relay" true
              ((one e).relay_ts <> None);
            bounds server))

let suite =
  [
    S.case
      "actual process exit after independent Slack share survives parent \
       takeover"
      process_loss;
  ]
