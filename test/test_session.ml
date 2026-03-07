let query_single_text_option db sql =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None)
      | _ -> None)

let test_reset_clears_active_session_and_history () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  let agent = Agent.create ~config () in
  Hashtbl.replace mgr.sessions "s1" (agent, Lwt_mutex.create (), ref None);
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"hello");
  Lwt_main.run (Session.reset mgr ~key:"s1");
  Alcotest.(check bool)
    "session entry removed" false
    (Hashtbl.mem mgr.sessions "s1");
  Alcotest.(check int)
    "history cleared" 0
    (List.length (Memory.load_history ~db ~session_key:"s1"))

let test_reset_waits_for_session_lock () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  let mutex = Lwt_mutex.create () in
  let agent = Agent.create ~config () in
  Hashtbl.replace mgr.sessions "s1" (agent, mutex, ref None);
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"hello");
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let reset_p = Session.reset mgr ~key:"s1" in
     let* () = Lwt.pause () in
     Alcotest.(check bool)
       "session still present while locked" true
       (Hashtbl.mem mgr.sessions "s1");
     Lwt_mutex.unlock mutex;
     reset_p);
  Alcotest.(check bool)
    "session removed after unlock" false
    (Hashtbl.mem mgr.sessions "s1");
  Alcotest.(check int)
    "history cleared after unlock" 0
    (List.length (Memory.load_history ~db ~session_key:"s1"))

let test_same_key_restore_from_db_on_first_create () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"hello");
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"assistant" ~content:"hi");
  Lwt_main.run
    (Session.with_session_lock mgr ~key:"s1" (fun agent _interrupt ->
         Alcotest.(check int)
           "history restored for same key" 2
           (List.length agent.Agent.history);
         Lwt.return_unit));
  Alcotest.(check bool)
    "session created for key" true
    (Hashtbl.mem mgr.sessions "s1")

let test_reset_then_same_key_create_is_fresh () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"hello");
  Lwt_main.run
    (Session.with_session_lock mgr ~key:"s1" (fun _ _ -> Lwt.return_unit));
  Lwt_main.run (Session.reset mgr ~key:"s1");
  Lwt_main.run
    (Session.with_session_lock mgr ~key:"s1" (fun agent _ ->
         Alcotest.(check int)
           "same key recreated with empty history after reset" 0
           (List.length agent.Agent.history);
         Lwt.return_unit))

let test_reset_clears_pending_session_state () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  Session.record_agent_turn mgr ~key:"slack:c1:u1" ~channel:"slack"
    ~channel_id:"c1" ();
  Lwt_main.run (Session.reset mgr ~key:"slack:c1:u1");
  Alcotest.(check int)
    "pending session state cleared" 0
    (List.length
       (Session.load_pending_agent_sessions mgr ~max_age_seconds:3600))

let test_new_key_create_is_empty () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  Lwt_main.run
    (Session.with_session_lock mgr ~key:"fresh" (fun agent _ ->
         Alcotest.(check int)
           "new key starts empty" 0
           (List.length agent.Agent.history);
         Lwt.return_unit))

let test_record_agent_turn_persists_channel_metadata () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  Session.record_agent_turn mgr ~key:"discord:chan:user" ~channel:"discord"
    ~channel_id:"chan" ();
  Alcotest.(check (option string))
    "turn stored as agent" (Some "agent")
    (query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'discord:chan:user'");
  Alcotest.(check (option string))
    "channel stored" (Some "discord")
    (query_single_text_option db
       "SELECT channel FROM session_state WHERE session_key = \
        'discord:chan:user'");
  Alcotest.(check (option string))
    "channel id stored" (Some "chan")
    (query_single_text_option db
       "SELECT channel_id FROM session_state WHERE session_key = \
        'discord:chan:user'")

let test_mark_response_sent_updates_session_state () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  Session.record_agent_turn mgr ~key:"telegram:42:user" ~channel:"telegram"
    ~channel_id:"42" ();
  Session.mark_response_sent mgr ~key:"telegram:42:user";
  Alcotest.(check (option string))
    "turn reset to user" (Some "user")
    (query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'telegram:42:user'");
  Alcotest.(check bool)
    "response timestamp set" true
    (query_single_text_option db
       "SELECT response_sent_at FROM session_state WHERE session_key = \
        'telegram:42:user'"
    <> None)

let test_pending_turn_persists_user_message_before_response () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  Lwt_main.run
    (Session.with_session_lock mgr ~key:"web:s1" (fun agent _ ->
         let open Lwt.Syntax in
         let history_before = List.length agent.Agent.history in
         let* () =
           Agent.prepare_turn_history agent ~user_message:"hello" ~db ()
         in
         Session.persist_new_messages mgr ~key:"web:s1" ~history_before agent;
         Lwt.return_unit));
  let history = Memory.load_history ~db ~session_key:"web:s1" in
  Alcotest.(check int) "user message persisted" 1 (List.length history);
  Alcotest.(check string) "persisted role" "user" (List.hd history).role;
  Alcotest.(check string) "persisted content" "hello" (List.hd history).content

let test_load_pending_agent_sessions_reads_manager_db () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  Session.record_agent_turn mgr ~key:"slack:c1:u1" ~channel:"slack"
    ~channel_id:"c1" ();
  Session.record_agent_turn mgr ~key:"slack:c2:u2" ~channel:"slack"
    ~channel_id:"c2" ();
  Session.mark_response_sent mgr ~key:"slack:c2:u2";
  let pending = Session.load_pending_agent_sessions mgr ~max_age_seconds:3600 in
  Alcotest.(check int) "one pending session" 1 (List.length pending);
  Alcotest.(check (triple string (option string) (option string)))
    "pending session preserved"
    ("slack:c1:u1", Some "slack", Some "c1")
    (List.hd pending)

let test_turn_returns_restart_message_while_draining () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  Lwt_main.run (Session.start_draining mgr);
  let response =
    Lwt_main.run (Session.turn mgr ~key:"web:s" ~message:"hello" ())
  in
  Alcotest.(check string)
    "restart message returned" Session.draining_message response;
  Alcotest.(check bool)
    "no session created while draining" false
    (Hashtbl.mem mgr.sessions "web:s");
  Lwt_main.run (Session.stop_draining mgr)

let test_with_registered_notifier_sends_warning () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let received = ref [] in
  Lwt_main.run
    (Session.with_registered_notifier mgr ~key:"telegram:1:u"
       ~notify:(fun text ->
         received := text :: !received;
         Lwt.return_unit)
       (fun () -> Session.notify_channel_sessions mgr "Restarting soon"));
  Alcotest.(check (list string))
    "warning delivered" [ "Restarting soon" ] (List.rev !received)

let test_turn_uses_special_command_handler () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let progress = ref [] in
  Session.set_special_command_handler mgr (fun ~key ~message ~send_progress ->
      let open Lwt.Syntax in
      Alcotest.(check string) "handler key" "web:update" key;
      Alcotest.(check string) "handler message" "/update" message;
      let* () =
        match send_progress with
        | Some send -> send "Starting update..."
        | None -> Lwt.return_unit
      in
      Lwt.return_some "Build complete. Sending restart signal...");
  let response =
    Lwt_main.run
      (Session.with_registered_notifier mgr ~key:"web:update"
         ~notify:(fun text ->
           progress := text :: !progress;
           Lwt.return_unit)
         (fun () -> Session.turn mgr ~key:"web:update" ~message:"/update" ()))
  in
  Alcotest.(check string)
    "special command response" "Build complete. Sending restart signal..."
    response;
  Alcotest.(check (list string))
    "progress forwarded" [ "Starting update..." ] (List.rev !progress)

let test_turn_stream_uses_special_command_handler () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let chunks = ref [] in
  let response =
    Lwt_main.run
      (let on_chunk chunk =
         chunks := chunk :: !chunks;
         Lwt.return_unit
       in
       Session.set_special_command_handler mgr
         (fun ~key ~message ~send_progress ->
           let open Lwt.Syntax in
           Alcotest.(check string) "stream handler key" "web:update" key;
           Alcotest.(check string) "stream handler message" "/update" message;
           let* () =
             match send_progress with
             | Some send -> send "Starting update..."
             | None -> Lwt.return_unit
           in
           Lwt.return_some "Build complete. Sending restart signal...");
       Session.turn_stream mgr ~key:"web:update" ~message:"/update" ~on_chunk ())
  in
  Alcotest.(check string)
    "stream special command response"
    "Build complete. Sending restart signal..." response;
  Alcotest.(check (list string))
    "stream deltas"
    [ "Starting update...\n"; "Build complete. Sending restart signal..." ]
    (List.rev_map
       (function
         | Provider.Delta text -> text | Provider.Done -> "[DONE]" | _ -> "")
       (List.filter
          (function Provider.Delta _ | Provider.Done -> true | _ -> false)
          !chunks)
    |> List.filter (fun text -> text <> "[DONE]"));
  Alcotest.(check bool)
    "stream done sent" true
    (List.exists (function Provider.Done -> true | _ -> false) !chunks)

let suite =
  [
    Alcotest.test_case "reset clears active session and history" `Quick
      test_reset_clears_active_session_and_history;
    Alcotest.test_case "reset waits for session lock" `Quick
      test_reset_waits_for_session_lock;
    Alcotest.test_case "same key restore from db on first create" `Quick
      test_same_key_restore_from_db_on_first_create;
    Alcotest.test_case "reset then same key create is fresh" `Quick
      test_reset_then_same_key_create_is_fresh;
    Alcotest.test_case "reset clears pending session state" `Quick
      test_reset_clears_pending_session_state;
    Alcotest.test_case "new key create is empty" `Quick
      test_new_key_create_is_empty;
    Alcotest.test_case "record agent turn persists channel metadata" `Quick
      test_record_agent_turn_persists_channel_metadata;
    Alcotest.test_case "mark response sent updates session state" `Quick
      test_mark_response_sent_updates_session_state;
    Alcotest.test_case "pending turn persists user message before response"
      `Quick test_pending_turn_persists_user_message_before_response;
    Alcotest.test_case "load pending agent sessions reads manager db" `Quick
      test_load_pending_agent_sessions_reads_manager_db;
    Alcotest.test_case "turn returns restart message while draining" `Quick
      test_turn_returns_restart_message_while_draining;
    Alcotest.test_case "registered notifier sends warning" `Quick
      test_with_registered_notifier_sends_warning;
    Alcotest.test_case "turn uses special command handler" `Quick
      test_turn_uses_special_command_handler;
    Alcotest.test_case "turn stream uses special command handler" `Quick
      test_turn_stream_uses_special_command_handler;
  ]
