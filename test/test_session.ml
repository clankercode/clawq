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

let string_contains haystack needle =
  let hay_len = String.length haystack and needle_len = String.length needle in
  let rec loop i =
    if i + needle_len > hay_len then false
    else if String.sub haystack i needle_len = needle then true
    else loop (i + 1)
  in
  needle_len = 0 || loop 0

let free_port () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close sock)
    (fun () ->
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname sock with
      | Unix.ADDR_INET (_, port) -> port
      | Unix.ADDR_UNIX _ -> Alcotest.fail "expected inet socket")

let make_fake_provider_config base_url : Runtime_config.provider_config =
  {
    api_key = "test-key";
    kind = None;
    base_url = Some base_url;
    default_model = Some "fake-model";
    project_id = None;
    location = None;
    service_account_json = None;
    thinking_budget_tokens = None;
    oai_thinking_style = "none";
    codex_oauth = None;
  }

let with_fake_chat_provider f =
  let port = free_port () in
  let callback _conn req body =
    let open Lwt.Syntax in
    let* body_text = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Safe.from_string body_text in
    let open Yojson.Safe.Util in
    let messages = json |> member "messages" |> to_list in
    let user_messages =
      messages
      |> List.filter_map (fun msg ->
          try
            if msg |> member "role" |> to_string = "user" then
              Some (msg |> member "content" |> to_string)
            else None
          with _ -> None)
    in
    let latest = match List.rev user_messages with x :: _ -> x | [] -> "" in
    let response_text = "reply:" ^ latest in
    let stream = try json |> member "stream" |> to_bool with _ -> false in
    match
      (Cohttp.Request.meth req, Uri.path (Cohttp.Request.uri req), stream)
    with
    | `POST, "/chat/completions", false ->
        let response_body =
          Yojson.Safe.to_string
            (`Assoc
               [
                 ("id", `String "cmpl_fake");
                 ("object", `String "chat.completion");
                 ("model", `String "fake-model");
                 ( "choices",
                   `List
                     [
                       `Assoc
                         [
                           ("index", `Int 0);
                           ( "message",
                             `Assoc
                               [
                                 ("role", `String "assistant");
                                 ("content", `String response_text);
                               ] );
                           ("finish_reason", `String "stop");
                         ];
                     ] );
                 ( "usage",
                   `Assoc
                     [
                       ("prompt_tokens", `Int 1); ("completion_tokens", `Int 1);
                     ] );
               ])
        in
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:response_body ()
    | `POST, "/chat/completions", true ->
        let stream_body, push = Lwt_stream.create () in
        let chunk =
          Yojson.Safe.to_string
            (`Assoc
               [
                 ("model", `String "fake-model");
                 ( "choices",
                   `List
                     [
                       `Assoc
                         [
                           ("index", `Int 0);
                           ( "delta",
                             `Assoc [ ("content", `String response_text) ] );
                         ];
                     ] );
               ])
        in
        push (Some ("data: " ^ chunk ^ "\n\n"));
        push (Some "data: [DONE]\n\n");
        push None;
        let headers =
          Cohttp.Header.of_list [ ("Content-Type", "text/event-stream") ]
        in
        Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
          ~body:(Cohttp_lwt.Body.of_stream stream_body)
          ()
    | _ -> Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"" ()
  in
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  Lwt.async (fun () -> Lwt.pick [ server; stop ]);
  Fun.protect
    ~finally:(fun () -> Lwt.wakeup_later stopper ())
    (fun () ->
      let config =
        {
          Runtime_config.default with
          default_provider = Some "fake";
          providers =
            [
              ( "fake",
                make_fake_provider_config
                  (Printf.sprintf "http://127.0.0.1:%d" port) );
            ];
          prompt =
            { Runtime_config.default.prompt with dynamic_enabled = false };
          security =
            { Runtime_config.default.security with tools_enabled = false };
          agent_defaults =
            {
              Runtime_config.default.agent_defaults with
              primary_model = "fake-model";
              show_thinking = false;
              show_tool_calls = false;
            };
        }
      in
      f config)

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

let test_reset_session_idempotent () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  Lwt_main.run
    (Session.with_session_lock mgr ~key:"web:s1" (fun agent _ ->
         let open Lwt.Syntax in
         let history_before = List.length agent.Agent.history in
         let* _compacted =
           Agent.prepare_turn_history agent ~user_message:"hello" ~db ()
         in
         Session.persist_new_messages mgr ~key:"web:s1" ~history_before agent;
         Lwt.return_unit));
  Session.record_agent_turn mgr ~key:"web:s1" ~channel:"web" ~channel_id:"s1" ();
  Lwt_main.run (Session.reset mgr ~key:"web:s1");
  Alcotest.(check bool)
    "session removed after first reset" false
    (Hashtbl.mem mgr.sessions "web:s1");
  Alcotest.(check int)
    "persisted history cleared after first reset" 0
    (List.length (Memory.load_history ~db ~session_key:"web:s1"));
  Alcotest.(check int)
    "pending session state cleared after first reset" 0
    (List.length
       (Session.load_pending_agent_sessions mgr ~max_age_seconds:3600));
  Lwt_main.run (Session.reset mgr ~key:"web:s1");
  Alcotest.(check bool)
    "session still absent after second reset" false
    (Hashtbl.mem mgr.sessions "web:s1");
  Alcotest.(check int)
    "persisted history still cleared after second reset" 0
    (List.length (Memory.load_history ~db ~session_key:"web:s1"));
  Alcotest.(check int)
    "pending session state still cleared after second reset" 0
    (List.length
       (Session.load_pending_agent_sessions mgr ~max_age_seconds:3600));
  Lwt_main.run
    (Session.with_session_lock mgr ~key:"web:s1" (fun agent _ ->
         Alcotest.(check int)
           "same key recreates empty after repeated reset" 0
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

let test_get_or_create_preserves_other_in_memory_session () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  Lwt_main.run
    (Session.with_session_lock mgr ~key:"s1" (fun agent _ ->
         agent.Agent.history <-
           [ Provider.make_message ~role:"user" ~content:"keep me" ];
         Lwt.return_unit));
  Lwt_main.run
    (Session.with_session_lock mgr ~key:"s2" (fun agent _ ->
         agent.Agent.history <-
           [ Provider.make_message ~role:"user" ~content:"other session" ];
         Lwt.return_unit));
  match Hashtbl.find_opt mgr.sessions "s1" with
  | None -> Alcotest.fail "expected first session to remain cached"
  | Some (agent, _, _) ->
      Alcotest.(check int)
        "other session preserved" 1
        (List.length agent.Agent.history);
      Alcotest.(check string)
        "other session content preserved" "keep me"
        (List.hd agent.Agent.history).content

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
         let* _compacted =
           Agent.prepare_turn_history agent ~user_message:"hello" ~db ()
         in
         Session.persist_new_messages mgr ~key:"web:s1" ~history_before agent;
         Lwt.return_unit));
  let history = Memory.load_history ~db ~session_key:"web:s1" in
  Alcotest.(check int) "user message persisted" 1 (List.length history);
  Alcotest.(check string) "persisted role" "user" (List.hd history).role;
  Alcotest.(check string) "persisted content" "hello" (List.hd history).content

let test_store_message_isolated () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  let persist_message key message =
    Lwt_main.run
      (Session.with_session_lock mgr ~key (fun agent _ ->
           let open Lwt.Syntax in
           let history_before = List.length agent.Agent.history in
           let* _compacted =
             Agent.prepare_turn_history agent ~user_message:message ~db ()
           in
           Session.persist_new_messages mgr ~key ~history_before agent;
           Lwt.return_unit))
  in
  persist_message "web:s1" "alpha";
  persist_message "web:s2" "beta";
  let s2_history_before = Memory.load_history ~db ~session_key:"web:s2" in
  let s2_cached_before =
    match Hashtbl.find_opt mgr.sessions "web:s2" with
    | Some (agent, _, _) ->
        List.map (fun msg -> msg.Provider.content) agent.Agent.history
    | None -> Alcotest.fail "expected second session to be cached"
  in
  persist_message "web:s1" "alpha followup";
  let s1_history = Memory.load_history ~db ~session_key:"web:s1" in
  let s2_history_after = Memory.load_history ~db ~session_key:"web:s2" in
  let s2_cached_after =
    match Hashtbl.find_opt mgr.sessions "web:s2" with
    | Some (agent, _, _) ->
        List.map (fun msg -> msg.Provider.content) agent.Agent.history
    | None -> Alcotest.fail "expected second session cache to remain present"
  in
  Alcotest.(check (list string))
    "mutated key persists new history"
    [ "alpha"; "alpha followup" ]
    (List.map (fun msg -> msg.Provider.content) s1_history);
  Alcotest.(check (list string))
    "other key persisted history unchanged"
    (List.map (fun msg -> msg.Provider.content) s2_history_before)
    (List.map (fun msg -> msg.Provider.content) s2_history_after);
  Alcotest.(check (list string))
    "other key cached session state unchanged" s2_cached_before s2_cached_after

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

let test_interrupt_resumable_channel_sessions_targets_supported_channels () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let telegram_interrupt = ref None in
  let slack_interrupt = ref None in
  let web_interrupt = ref None in
  Hashtbl.replace mgr.sessions "telegram:1:u"
    (Agent.create ~config (), Lwt_mutex.create (), telegram_interrupt);
  Hashtbl.replace mgr.sessions "slack:c1:u"
    (Agent.create ~config (), Lwt_mutex.create (), slack_interrupt);
  Hashtbl.replace mgr.sessions "web:s1"
    (Agent.create ~config (), Lwt_mutex.create (), web_interrupt);
  Session.register_channel_notifier mgr ~key:"telegram:1:u" (fun _ ->
      Lwt.return_unit);
  Session.register_channel_notifier mgr ~key:"slack:c1:u" (fun _ ->
      Lwt.return_unit);
  Session.register_channel_notifier mgr ~key:"web:s1" (fun _ -> Lwt.return_unit);
  Lwt_main.run (Session.interrupt_resumable_channel_sessions mgr);
  Alcotest.(check (option string))
    "telegram interrupted" (Some Agent.restart_interrupt_token)
    !telegram_interrupt;
  Alcotest.(check (option string))
    "slack interrupted" (Some Agent.restart_interrupt_token) !slack_interrupt;
  Alcotest.(check (option string)) "web untouched" None !web_interrupt

let test_take_response_deferred_clears_marker () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  Session.set_response_deferred mgr ~key:"telegram:1:u";
  Alcotest.(check bool)
    "first read deferred" true
    (Session.take_response_deferred mgr ~key:"telegram:1:u");
  Alcotest.(check bool)
    "second read cleared" false
    (Session.take_response_deferred mgr ~key:"telegram:1:u")

let test_clear_response_deferred_removes_marker () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  Session.set_response_deferred mgr ~key:"telegram:2:u";
  Session.clear_response_deferred mgr ~key:"telegram:2:u";
  Alcotest.(check bool)
    "marker removed" false
    (Session.take_response_deferred mgr ~key:"telegram:2:u")

let queued_message ?channel_name ?channel_type ?sender_id ?sender_name ?channel
    ?channel_id message =
  {
    Session.message;
    attachments = [];
    channel_name;
    channel_type;
    sender_id;
    sender_name;
    channel;
    channel_id;
  }

let test_enqueue_message_if_busy_marks_interrupt_and_preserves_message () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  Hashtbl.replace mgr.sessions "telegram:1:u"
    (Agent.create ~config (), mutex, interrupt);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     Session.register_channel_notifier mgr ~key:"telegram:1:u" (fun _ ->
         Lwt.return_unit);
     let* queued =
       Session.enqueue_message_if_busy mgr ~key:"telegram:1:u"
         (queued_message ~channel:"telegram" ~channel_id:"1" "hello later")
     in
     Alcotest.(check bool) "message queued" true queued;
     Alcotest.(check (option string))
       "interrupt marked" (Some "[queued inbound message]") !interrupt;
     Lwt.return_unit);
  match Session.take_next_queued_message mgr ~key:"telegram:1:u" with
  | None -> Alcotest.fail "expected queued message"
  | Some queued ->
      Alcotest.(check string)
        "queued content preserved" "hello later" queued.Session.message;
      Alcotest.(check (option string))
        "queue emptied" None
        (Option.map
           (fun msg -> msg.Session.message)
           (Session.take_next_queued_message mgr ~key:"telegram:1:u"))

let test_drain_queued_messages_sends_followup_response () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let sent = ref [] in
      Lwt_main.run
        (Session.with_registered_notifier mgr ~key:"telegram:1:u"
           ~notify:(fun text ->
             sent := text :: !sent;
             Lwt.return_unit)
           (fun () ->
             Session.with_session_lock mgr ~key:"telegram:1:u"
               (fun agent interrupt ->
                 let open Lwt.Syntax in
                 let* queued =
                   Session.enqueue_message_if_busy mgr ~key:"telegram:1:u"
                     (queued_message ~channel_name:"telegram" ~channel_type:"dm"
                        ~channel:"telegram" ~channel_id:"1" "please continue")
                 in
                 Alcotest.(check bool) "busy queue succeeds" true queued;
                 Session.drain_queued_messages mgr ~key:"telegram:1:u" agent
                   interrupt)));
      Alcotest.(check int) "followup sent once" 1 (List.length !sent);
      Alcotest.(check bool)
        "followup produced provider reply" true
        (String.starts_with ~prefix:"reply:" (List.hd !sent));
      Alcotest.(check bool)
        "queue drained" true
        (Session.take_next_queued_message mgr ~key:"telegram:1:u" = None))

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

let test_turn_stream_emits_compaction_notice () =
  with_fake_chat_provider (fun config ->
      let config =
        {
          config with
          memory = { config.memory with max_messages_per_session = 21 };
        }
      in
      let mgr = Session.create ~config () in
      Lwt_main.run
        (Session.with_session_lock mgr ~key:"web:s-compact" (fun agent _ ->
             for i = 1 to 25 do
               agent.Agent.history <-
                 Provider.make_message ~role:"user"
                   ~content:(Printf.sprintf "seed message %02d" i)
                 :: agent.Agent.history
             done;
             Lwt.return_unit));
      let chunks = ref [] in
      ignore
        (Lwt_main.run
           (Session.turn_stream mgr ~key:"web:s-compact" ~message:"hello"
              ~on_chunk:(fun chunk ->
                chunks := chunk :: !chunks;
                Lwt.return_unit)
              ()));
      let deltas =
        List.rev_map
          (function
            | Provider.Delta text -> text | Provider.Done -> "[DONE]" | _ -> "")
          (List.filter
             (function Provider.Delta _ | Provider.Done -> true | _ -> false)
             !chunks)
      in
      let output = String.concat "" deltas in
      Alcotest.(check bool)
        "compaction notice text present" true
        (string_contains output Session.compaction_notice))

let test_bang_message_interrupts_before_lock_and_turns_normally () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let mutex = Lwt_mutex.create () in
      let interrupt = ref None in
      let agent = Agent.create ~config () in
      Hashtbl.replace mgr.sessions "web:s1" (agent, mutex, interrupt);
      Lwt_main.run
        (let open Lwt.Syntax in
         let* () = Lwt_mutex.lock mutex in
         let turn_p = Session.turn mgr ~key:"web:s1" ~message:"!hello" () in
         let* () = Lwt.pause () in
         Alcotest.(check (option string))
           "interrupt delivered before lock release" (Some "hello") !interrupt;
         Lwt_mutex.unlock mutex;
         let* response = turn_p in
         Alcotest.(check string)
           "bang message processed as normal turn" "reply:hello" response;
         Lwt.return_unit);
      let history = Memory.load_history ~db ~session_key:"web:s1" in
      Alcotest.(check (list string))
        "history keeps normalized user message and assistant reply"
        [ "hello"; "reply:hello" ]
        (List.map (fun msg -> msg.Provider.content) history))

let test_bang_message_turn_stream_processes_normally () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let mutex = Lwt_mutex.create () in
      let interrupt = ref None in
      let agent = Agent.create ~config () in
      let chunks = ref [] in
      Hashtbl.replace mgr.sessions "web:s2" (agent, mutex, interrupt);
      Lwt_main.run
        (let open Lwt.Syntax in
         let on_chunk chunk =
           chunks := chunk :: !chunks;
           Lwt.return_unit
         in
         let* () = Lwt_mutex.lock mutex in
         let turn_p =
           Session.turn_stream mgr ~key:"web:s2" ~message:"!hello" ~on_chunk ()
         in
         let* () = Lwt.pause () in
         Alcotest.(check (option string))
           "stream interrupt delivered before lock release" (Some "hello")
           !interrupt;
         Lwt_mutex.unlock mutex;
         let* response = turn_p in
         Alcotest.(check string)
           "stream bang message processed as normal turn" "reply:hello" response;
         Lwt.return_unit);
      Alcotest.(check (list string))
        "stream emits provider response, not raw stripped message"
        [ "reply:hello" ]
        (List.rev_map
           (function
             | Provider.Delta text -> text | Provider.Done -> "[DONE]" | _ -> "")
           (List.filter
              (function Provider.Delta _ | Provider.Done -> true | _ -> false)
              !chunks)
        |> List.filter (fun text -> text <> "[DONE]"));
      let history = Memory.load_history ~db ~session_key:"web:s2" in
      Alcotest.(check (list string))
        "stream history keeps normalized user message and assistant reply"
        [ "hello"; "reply:hello" ]
        (List.map (fun msg -> msg.Provider.content) history))

let test_empty_bang_message_becomes_interrupted_message () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let response =
        Lwt_main.run (Session.turn mgr ~key:"web:s3" ~message:"!" ())
      in
      Alcotest.(check string)
        "empty bang becomes interrupted marker" "reply:[interrupted]" response;
      let history = Memory.load_history ~db ~session_key:"web:s3" in
      Alcotest.(check (list string))
        "empty bang persists normalized message"
        [ "[interrupted]"; "reply:[interrupted]" ]
        (List.map (fun msg -> msg.Provider.content) history))

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
    Alcotest.test_case "reset session idempotent" `Quick
      test_reset_session_idempotent;
    Alcotest.test_case "reset clears pending session state" `Quick
      test_reset_clears_pending_session_state;
    Alcotest.test_case "new key create is empty" `Quick
      test_new_key_create_is_empty;
    Alcotest.test_case "get_or_create preserves other in-memory session" `Quick
      test_get_or_create_preserves_other_in_memory_session;
    Alcotest.test_case "record agent turn persists channel metadata" `Quick
      test_record_agent_turn_persists_channel_metadata;
    Alcotest.test_case "mark response sent updates session state" `Quick
      test_mark_response_sent_updates_session_state;
    Alcotest.test_case "pending turn persists user message before response"
      `Quick test_pending_turn_persists_user_message_before_response;
    Alcotest.test_case "store_message isolated" `Quick
      test_store_message_isolated;
    Alcotest.test_case "load pending agent sessions reads manager db" `Quick
      test_load_pending_agent_sessions_reads_manager_db;
    Alcotest.test_case "turn returns restart message while draining" `Quick
      test_turn_returns_restart_message_while_draining;
    Alcotest.test_case "registered notifier sends warning" `Quick
      test_with_registered_notifier_sends_warning;
    Alcotest.test_case
      "interrupt resumable channel sessions targets supported channels" `Quick
      test_interrupt_resumable_channel_sessions_targets_supported_channels;
    Alcotest.test_case "take response deferred clears marker" `Quick
      test_take_response_deferred_clears_marker;
    Alcotest.test_case "clear response deferred removes marker" `Quick
      test_clear_response_deferred_removes_marker;
    Alcotest.test_case "clear response deferred removes marker" `Quick
      test_clear_response_deferred_removes_marker;
    Alcotest.test_case
      "enqueue message if busy marks interrupt and preserves message" `Quick
      test_enqueue_message_if_busy_marks_interrupt_and_preserves_message;
    Alcotest.test_case "drain queued messages sends followup response" `Quick
      test_drain_queued_messages_sends_followup_response;
    Alcotest.test_case "turn uses special command handler" `Quick
      test_turn_uses_special_command_handler;
    Alcotest.test_case "turn stream uses special command handler" `Quick
      test_turn_stream_uses_special_command_handler;
    Alcotest.test_case "turn stream emits compaction notice" `Quick
      test_turn_stream_emits_compaction_notice;
    Alcotest.test_case "bang message interrupts before lock and turns normally"
      `Quick test_bang_message_interrupts_before_lock_and_turns_normally;
    Alcotest.test_case "bang message turn stream processes normally" `Quick
      test_bang_message_turn_stream_processes_normally;
    Alcotest.test_case "empty bang message becomes interrupted message" `Quick
      test_empty_bang_message_becomes_interrupted_message;
  ]
