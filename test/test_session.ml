let with_captured_logs f =
  let messages = ref [] in
  let reporter =
    {
      Logs.report =
        (fun _src level ~over k msgf ->
          let k _ =
            over ();
            k ()
          in
          msgf @@ fun ?header ?tags:_ fmt ->
          Format.kasprintf
            (fun msg ->
              let level = Logs.level_to_string (Some level) in
              let full =
                match header with
                | Some h when h <> "" ->
                    Printf.sprintf "[%s] %s" level (h ^ " " ^ msg)
                | _ -> Printf.sprintf "[%s] %s" level msg
              in
              messages := full :: !messages;
              k ())
            fmt);
    }
  in
  let prev = Logs.reporter () in
  Logs.set_reporter reporter;
  Fun.protect
    ~finally:(fun () -> Logs.set_reporter prev)
    (fun () ->
      let result = f () in
      (result, List.rev !messages))

let mock_status_notifier () =
  let sent = ref [] in
  let edited = ref [] in
  let notifier : Status_message.notifier =
    {
      send =
        (fun ?parse_mode:_ text ->
          sent := text :: !sent;
          Lwt.return "msg-1");
      edit =
        (fun id ?parse_mode:_ text ->
          edited := (id, text) :: !edited;
          Lwt.return None);
      delete = (fun _id -> Lwt.return_unit);
    }
  in
  (notifier, sent, edited)

let rec remove_path path =
  try
    if Sys.is_directory path then begin
      Array.iter
        (fun name -> remove_path (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
    end
    else Sys.remove path
  with Sys_error _ | Unix.Unix_error _ -> ()

let with_temp_workspace f =
  let base = Filename.get_temp_dir_name () in
  let workspace = Filename.temp_file ~temp_dir:base "clawq_ws_" "" in
  Sys.remove workspace;
  Unix.mkdir workspace 0o755;
  Fun.protect (fun () -> f workspace) ~finally:(fun () -> remove_path workspace)

let make_fake_provider_config base_url : Runtime_config.provider_config =
  {
    Runtime_config.default_provider_config with
    api_key = "test-key";
    base_url = Some base_url;
    default_model = Some "fake-model";
  }

let with_fake_chat_provider ?response_for_user f =
  let port = Test_helpers.free_port () in
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
    let response_text =
      match response_for_user with
      | Some f -> f latest
      | None -> "reply:" ^ latest
    in
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

let make_openai_capture_config () =
  let provider =
    {
      Runtime_config.default_provider_config with
      api_key = "test-key";
      kind = Some "openai";
      default_model = Some "gpt-test";
    }
  in
  {
    Runtime_config.default with
    providers = [ ("openai", provider) ];
    prompt = { Runtime_config.default.prompt with dynamic_enabled = false };
    security = { Runtime_config.default.security with tools_enabled = false };
    agent_defaults =
      {
        Runtime_config.default.agent_defaults with
        primary_model = "openai:gpt-test";
        show_thinking = false;
        show_tool_calls = false;
      };
    memory = { Runtime_config.default.memory with search_enabled = true };
  }

let with_captured_native_provider f =
  let captured = ref [] in
  let prev_complete = !Provider.native_complete in
  Fun.protect
    (fun () ->
      Provider.register_native_complete Provider.OpenAICompat
        (fun
          ~config:_ ~provider:_ ~model ~messages ?tools:_ ?session_key:_ () ->
          captured := messages;
          Lwt.return
            (Provider.Text
               {
                 content = "ok";
                 model;
                 usage = None;
                 provider_response_items_json = None;
                 thinking = None;
               }));
      f (make_openai_capture_config ()) captured)
    ~finally:(fun () -> Provider.native_complete := prev_complete)

let with_tool_capture_native_provider f =
  let tool_calls = ref 0 in
  let prev_complete = !Provider.native_complete in
  Fun.protect
    (fun () ->
      Provider.register_native_complete Provider.OpenAICompat
        (fun
          ~config:_ ~provider:_ ~model ~messages:_ ?tools ?session_key:_ () ->
          (match tools with Some _ -> incr tool_calls | None -> ());
          Lwt.return
            (Provider.Text
               {
                 content = "ok";
                 model;
                 usage = None;
                 provider_response_items_json = None;
                 thinking = None;
               }));
      f (make_openai_capture_config ()) tool_calls)
    ~finally:(fun () -> Provider.native_complete := prev_complete)

let all_message_content messages =
  messages
  |> List.map (fun (m : Provider.message) -> m.content)
  |> String.concat "\n"

let with_delayed_chat_provider ?(delay_s = 0.05) ?response_for_user f =
  let port = Test_helpers.free_port () in
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
    let response_text =
      match response_for_user with
      | Some g -> g latest
      | None -> "reply:" ^ latest
    in
    let stream = try json |> member "stream" |> to_bool with _ -> false in
    let* () = Lwt_unix.sleep delay_s in
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

type fake_provider_reply =
  | Fake_text of string
  | Fake_tool_calls of (string * string * string) list

let with_fake_openai_provider ~handle_request f =
  let port = Test_helpers.free_port () in
  let callback _conn req body =
    let open Lwt.Syntax in
    let* body_text = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Safe.from_string body_text in
    let open Yojson.Safe.Util in
    let messages = json |> member "messages" |> to_list in
    let stream = try json |> member "stream" |> to_bool with _ -> false in
    match
      ( Cohttp.Request.meth req,
        Uri.path (Cohttp.Request.uri req),
        handle_request ~stream ~messages ~json )
    with
    | `POST, "/chat/completions", Fake_text response_text when not stream ->
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
    | `POST, "/chat/completions", Fake_tool_calls calls when not stream ->
        let tool_calls_json =
          `List
            (List.map
               (fun (id, name, arguments) ->
                 `Assoc
                   [
                     ("id", `String id);
                     ("type", `String "function");
                     ( "function",
                       `Assoc
                         [
                           ("name", `String name);
                           ("arguments", `String arguments);
                         ] );
                   ])
               calls)
        in
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
                                 ("content", `String "");
                                 ("tool_calls", tool_calls_json);
                               ] );
                           ("finish_reason", `String "tool_calls");
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
    | `POST, "/chat/completions", Fake_text response_text ->
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
    | `POST, "/chat/completions", Fake_tool_calls calls ->
        let stream_body, push = Lwt_stream.create () in
        (* Emit tool_calls in SSE format *)
        List.iteri
          (fun idx (id, name, arguments) ->
            let start_chunk =
              Yojson.Safe.to_string
                (`Assoc
                   [
                     ("model", `String "fake-model");
                     ( "choices",
                       `List
                         [
                           `Assoc
                             [
                               ( "delta",
                                 `Assoc
                                   [
                                     ( "tool_calls",
                                       `List
                                         [
                                           `Assoc
                                             [
                                               ("index", `Int idx);
                                               ("id", `String id);
                                               ( "function",
                                                 `Assoc
                                                   [
                                                     ("name", `String name);
                                                     ( "arguments",
                                                       `String arguments );
                                                   ] );
                                             ];
                                         ] );
                                   ] );
                             ];
                         ] );
                   ])
            in
            push (Some ("data: " ^ start_chunk ^ "\n\n")))
          calls;
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
  ignore (Lwt_main.run (Session.reset mgr ~key:"s1"));
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
  ignore
    (Lwt_main.run
       (let open Lwt.Syntax in
        let* () = Lwt_mutex.lock mutex in
        let reset_p = Session.reset mgr ~key:"s1" in
        let* () = Lwt.pause () in
        (* F2: Phase 1 clears DB but keeps the session in the hashtable to
           prevent new session creation during the Phase 2 wait. *)
        Alcotest.(check bool)
          "session still present after phase 1" true
          (Hashtbl.mem mgr.sessions "s1");
        (* Phase 2 is waiting on the per-session mutex *)
        Alcotest.(check bool)
          "reset still sleeping (phase 2 blocked)" true
          (Lwt.is_sleeping reset_p);
        Lwt_mutex.unlock mutex;
        reset_p));
  Alcotest.(check int)
    "history cleared after unlock" 0
    (List.length (Memory.load_history ~db ~session_key:"s1"))

let test_with_session_lock_relocks_when_session_replaced () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "web:replaced-lock" in
  Lwt_main.run
    (Session.with_session_lock mgr ~key (fun _agent _interrupt ->
         Lwt.return_unit));
  let old_mutex =
    match Hashtbl.find_opt mgr.Session_core.sessions key with
    | Some (_, mutex, _) -> mutex
    | None -> Alcotest.fail "expected initial session"
  in
  let replacement_agent = Agent.create ~config () in
  let replacement_mutex = Lwt_mutex.create () in
  let replacement_interrupt = ref None in
  let ran = ref false in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock old_mutex in
     let lock_p =
       Session.with_session_lock mgr ~key (fun agent _interrupt ->
           ran := true;
           Alcotest.(check bool)
             "uses replacement agent" true
             (agent == replacement_agent);
           Lwt.return_unit)
     in
     let* () = Lwt.pause () in
     let* () = Lwt_mutex.lock replacement_mutex in
     Hashtbl.replace mgr.Session_core.sessions key
       (replacement_agent, replacement_mutex, replacement_interrupt);
     Lwt_mutex.unlock old_mutex;
     let* () = Lwt.pause () in
     Alcotest.(check bool)
       "does not run while replacement mutex is locked" false !ran;
     Lwt_mutex.unlock replacement_mutex;
     lock_p);
  Alcotest.(check bool) "ran after replacement unlock" true !ran

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
  ignore (Lwt_main.run (Session.reset mgr ~key:"s1"));
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
  ignore (Lwt_main.run (Session.reset mgr ~key:"web:s1"));
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
  ignore (Lwt_main.run (Session.reset mgr ~key:"web:s1"));
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
  ignore (Lwt_main.run (Session.reset mgr ~key:"slack:c1:u1"));
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
    (Test_helpers.query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'discord:chan:user'");
  Alcotest.(check (option string))
    "channel stored" (Some "discord")
    (Test_helpers.query_single_text_option db
       "SELECT channel FROM session_state WHERE session_key = \
        'discord:chan:user'");
  Alcotest.(check (option string))
    "channel id stored" (Some "chan")
    (Test_helpers.query_single_text_option db
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
    (Test_helpers.query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'telegram:42:user'");
  Alcotest.(check bool)
    "response timestamp set" true
    (Test_helpers.query_single_text_option db
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

let test_profiled_room_session_turn_skips_unscoped_memory_context () =
  with_captured_native_provider (fun config captured ->
      let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
      let profile_id =
        Memory.insert_room_profile ~db ~name:"channel-a-profile"
      in
      Memory.upsert_room_profile_binding ~db ~room_id:"C_A" ~profile_id;
      Memory.store_message ~db ~session_key:"slack:C_A:user"
        (Provider.make_message ~role:"user" ~content:"channel A local context");
      Memory.store_message ~db ~session_key:"slack:C_B:user"
        (Provider.make_message ~role:"user"
           ~content:"CHANNEL-B-SECRET leak-token only belongs to channel B");
      Memory.store_core ~db ~key:"global-core-secret"
        ~content:"GLOBAL-CORE-SECRET must not be injected" ();
      let mgr = Session.create ~config ~db () in
      let response =
        Lwt_main.run
          (Session.turn mgr ~key:"slack:C_A:user" ~message:"leak-token"
             ~channel:"slack" ~channel_id:"C_A" ())
      in
      Alcotest.(check string) "response" "ok" response;
      let prompt = all_message_content !captured in
      Alcotest.(check bool)
        "cross-channel search result absent" false
        (Test_helpers.string_contains prompt "CHANNEL-B-SECRET");
      Alcotest.(check bool)
        "global core memory absent" false
        (Test_helpers.string_contains prompt "GLOBAL-CORE-SECRET"))

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

let test_spawn_postmortem_agent_circuit_breaker_blocks_duplicates () =
  let mgr = Session.create ~config:Runtime_config.default () in
  let launches = ref [] in
  let prev = !Session.spawn_postmortem_agent_fn in
  Fun.protect
    ~finally:(fun () -> Session.spawn_postmortem_agent_fn := prev)
    (fun () ->
      (Session.spawn_postmortem_agent_fn :=
         fun _mgr ~stuck_history:_ ~session_key ~reason ?db:_ () ->
           launches := (session_key, reason) :: !launches;
           Lwt.return_unit);
      (* B612: same-pattern repeats are suppressed by the breaker. Distinct
         patterns get their own postmortem (see
         test_spawn_postmortem_agent_distinct_patterns_each_launch). *)
      Lwt_main.run
        (Session.spawn_postmortem_agent mgr ~stuck_history:[]
           ~session_key:"web:stuck"
           ~reason:
             "RepeatedToolCall: \"shell_exec\" called 4 times with same args"
           ());
      Lwt_main.run
        (Session.spawn_postmortem_agent mgr ~stuck_history:[]
           ~session_key:"web:stuck"
           ~reason:
             "RepeatedToolCall: \"shell_exec\" called 9 times with same args"
           ());
      Alcotest.(check int)
        "same-pattern repeat suppressed" 1 (List.length !launches);
      Alcotest.(check (list string))
        "first launch preserved by session key" [ "web:stuck" ]
        (List.map fst (List.rev !launches)))

(* B609: when postmortem.enabled=false in config, spawn_postmortem_agent must
   not mutate the circuit-breaker table and must not call the inner
   spawn_postmortem_agent_fn. Disabled means truly inert. *)
let test_spawn_postmortem_agent_disabled_short_circuits_without_side_effects ()
    =
  let config =
    {
      Runtime_config.default with
      postmortem = { Runtime_config.default.postmortem with enabled = false };
    }
  in
  let mgr = Session.create ~config () in
  let launches = ref [] in
  let prev = !Session.spawn_postmortem_agent_fn in
  Fun.protect
    ~finally:(fun () -> Session.spawn_postmortem_agent_fn := prev)
    (fun () ->
      (Session.spawn_postmortem_agent_fn :=
         fun _mgr ~stuck_history:_ ~session_key ~reason ?db:_ () ->
           launches := (session_key, reason) :: !launches;
           Lwt.return_unit);
      Lwt_main.run
        (Session.spawn_postmortem_agent mgr ~stuck_history:[]
           ~session_key:"web:stuck" ~reason:"loop-1" ());
      Alcotest.(check int)
        "no inner spawn when disabled" 0 (List.length !launches);
      Alcotest.(check int)
        "circuit breaker untouched when disabled" 0
        (Hashtbl.length mgr.Session_core.postmortem_circuit_breakers))

(* B612: a postmortem launched for one stuck pattern should NOT suppress a
   later launch for a distinct stuck pattern in the same root session.
   Previously the breaker was keyed by root_session_key alone, so the
   second pattern was permanently silenced. *)
let test_spawn_postmortem_agent_distinct_patterns_each_launch () =
  let mgr = Session.create ~config:Runtime_config.default () in
  let launches = ref [] in
  let prev = !Session.spawn_postmortem_agent_fn in
  Fun.protect
    ~finally:(fun () -> Session.spawn_postmortem_agent_fn := prev)
    (fun () ->
      (Session.spawn_postmortem_agent_fn :=
         fun _mgr ~stuck_history:_ ~session_key ~reason ?db:_ () ->
           launches := (session_key, reason) :: !launches;
           Lwt.return_unit);
      Lwt_main.run
        (Session.spawn_postmortem_agent mgr ~stuck_history:[]
           ~session_key:"web:stuck"
           ~reason:
             "RepeatedToolCall: \"shell_exec\" called 4 times with same args"
           ());
      Lwt_main.run
        (Session.spawn_postmortem_agent mgr ~stuck_history:[]
           ~session_key:"web:stuck"
           ~reason:
             "RepeatedToolCall: \"file_read\" called 4 times with same args"
           ());
      Lwt_main.run
        (Session.spawn_postmortem_agent mgr ~stuck_history:[]
           ~session_key:"web:stuck"
           ~reason:"ConsecutiveErrors: 5 consecutive tool errors from \"grep\""
           ());
      Alcotest.(check int)
        "three distinct patterns each got a postmortem" 3
        (List.length !launches);
      (* Same pattern repeating still suppressed *)
      Lwt_main.run
        (Session.spawn_postmortem_agent mgr ~stuck_history:[]
           ~session_key:"web:stuck"
           ~reason:
             "RepeatedToolCall: \"shell_exec\" called 9 times with same args"
           ());
      Alcotest.(check int)
        "repeat of shell_exec RepeatedToolCall pattern suppressed" 3
        (List.length !launches))

let test_spawn_postmortem_agent_circuit_breaker_blocks_recursive_postmortems ()
    =
  let mgr = Session.create ~config:Runtime_config.default () in
  let launches = ref [] in
  let prev = !Session.spawn_postmortem_agent_fn in
  Fun.protect
    ~finally:(fun () -> Session.spawn_postmortem_agent_fn := prev)
    (fun () ->
      (Session.spawn_postmortem_agent_fn :=
         fun _mgr ~stuck_history:_ ~session_key ~reason ?db:_ () ->
           launches := (session_key, reason) :: !launches;
           Lwt.return_unit);
      Lwt_main.run
        (Session.spawn_postmortem_agent mgr ~stuck_history:[]
           ~session_key:"__postmortem_web:stuck" ~reason:"still-looping" ());
      Alcotest.(check int)
        "recursive postmortem launch suppressed" 0 (List.length !launches))

let test_postmortem_session_starts_fresh_not_restored () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  let key = "__postmortem_web:stuck@1234567890" in
  Memory.store_message ~db ~session_key:key
    (Provider.make_message ~role:"user" ~content:"old postmortem msg");
  Memory.store_message ~db ~session_key:key
    (Provider.make_message ~role:"assistant" ~content:"old postmortem reply");
  Alcotest.(check int)
    "pre-existing history in DB" 2
    (List.length (Memory.load_history ~db ~session_key:key));
  Lwt_main.run
    (Session.with_session_lock mgr ~key (fun agent _interrupt ->
         Alcotest.(check int)
           "postmortem session starts with empty history" 0
           (List.length agent.Agent.history);
         Lwt.return_unit));
  (* Old history remains in DB — postmortem history is preserved *)
  Alcotest.(check int)
    "postmortem history preserved in DB" 2
    (List.length (Memory.load_history ~db ~session_key:key))

let test_live_activity_tracks_nested_scopes () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "telegram:live:u" in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* initial = Session.current_live_activity mgr ~key in
     Alcotest.(check bool) "starts inactive" false initial.Session.active;
     let started_p =
       Session.wait_for_live_activity_change mgr ~key
         ~after_generation:initial.generation
     in
     let* started_generation =
       Session.with_live_activity mgr ~key (fun () ->
           let* started = started_p in
           Alcotest.(check bool) "becomes active" true started.Session.active;
           let* outer_snapshot = Session.current_live_activity mgr ~key in
           Alcotest.(check bool) "outer scope active" true outer_snapshot.active;
           let nested_change_p =
             let* _ =
               Session.wait_for_live_activity_change mgr ~key
                 ~after_generation:started.generation
             in
             Lwt.return_true
           in
           let* () =
             Session.with_live_activity mgr ~key (fun () ->
                 let* inner_snapshot = Session.current_live_activity mgr ~key in
                 Alcotest.(check bool)
                   "inner scope active" true inner_snapshot.active;
                 let* changed =
                   Lwt.pick
                     [
                       nested_change_p;
                       (let* () = Lwt_unix.sleep 0.02 in
                        Lwt.return_false);
                     ]
                 in
                 Alcotest.(check bool)
                   "nested scope does not emit transition" false changed;
                 Lwt.return_unit)
           in
           let* still_active = Session.current_live_activity mgr ~key in
           Alcotest.(check bool)
             "still active after inner scope" true still_active.active;
           Lwt.return started.generation)
     in
     let* stopped =
       Session.wait_for_live_activity_change mgr ~key
         ~after_generation:started_generation
     in
     Alcotest.(check bool) "inactive after outer scope" false stopped.active;
     Lwt.return_unit)

let test_turn_marks_special_command_phase_as_live_activity () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let active_during_handler = ref false in
  Session.set_special_command_handler mgr
    (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
      let open Lwt.Syntax in
      let* snapshot = Session.current_live_activity mgr ~key in
      active_during_handler := snapshot.Session.active;
      Lwt.return_some "Build complete. Sending restart signal...");
  let response =
    Lwt_main.run (Session.turn mgr ~key:"web:update" ~message:"/update" ())
  in
  Alcotest.(check string)
    "special command response" "Build complete. Sending restart signal..."
    response;
  Alcotest.(check bool)
    "special handler sees live activity" true !active_during_handler;
  let final_snapshot =
    Lwt_main.run (Session.current_live_activity mgr ~key:"web:update")
  in
  Alcotest.(check bool)
    "live activity cleared after turn" false final_snapshot.Session.active

let test_drain_queued_messages_marks_live_activity () =
  with_fake_chat_provider (fun config ->
      let mgr = Session.create ~config () in
      let key = "telegram:1:u" in
      let active_during_progress = ref false in
      Session.register_channel_notifier mgr ~key (fun _ -> Lwt.return_unit);
      Lwt_main.run
        (Session.with_session_lock mgr ~key (fun agent interrupt ->
             let open Lwt.Syntax in
             let* queued =
               Session.enqueue_message_if_busy mgr ~key
                 {
                   Session.message = "queued work";
                   content_parts = [];
                   attachments = [];
                   channel_name = None;
                   channel_type = None;
                   sender_id = None;
                   sender_name = None;
                   user_group = None;
                   channel = Some "telegram";
                   channel_id = Some "1";
                   message_id = None;
                   inbound_queue_id = None;
                   bang = false;
                   deferred_followup = false;
                   snapshot_work_type = None;
                   has_external_users = false;
                 }
                            in
             Alcotest.(check bool) "message queued" true queued;
             let on_drain_progress : Session.drain_progress =
               {
                 before_turn =
                   (fun _msg_id ->
                     let* snapshot = Session.current_live_activity mgr ~key in
                     active_during_progress := snapshot.Session.active;
                     Lwt.return_unit);
                 after_turn = (fun _msg_id -> Lwt.return_unit);
                 after_all = (fun () -> Lwt.return_unit);
               }
             in
             let* () =
               Session.drain_queued_messages mgr ~key agent interrupt
                 ~on_drain_progress ()
             in
             let* final_snapshot = Session.current_live_activity mgr ~key in
             Alcotest.(check bool)
               "live activity cleared after drain" false final_snapshot.active;
             Lwt.return_unit));
      Alcotest.(check bool)
        "drain progress sees live activity" true !active_during_progress)

let queued_message ?channel_name ?channel_type ?sender_id ?sender_name
    ?user_group ?channel ?channel_id ?message_id ?(deferred_followup = false)
    message =
  {
    Session.message;
    content_parts = [];
    attachments = [];
    channel_name;
    channel_type;
    sender_id;
    sender_name;
    user_group;
    channel;
    channel_id;
    message_id;
    inbound_queue_id = None;
    bang = false;
    deferred_followup;
    snapshot_work_type = None;
    has_external_users = false;
  }

let stop_interrupt_token_for_test = Agent.stop_interrupt_token

let check_pending_question_cancelled mgr ~key promise =
  let open Lwt.Syntax in
  let* () = Lwt.pause () in
  Alcotest.(check bool)
    "pending question removed" false
    (Session.has_pending_question mgr ~key);
  (match Lwt.state promise with
  | Lwt.Return raw ->
      Alcotest.(check string)
        "pending question cancelled" Session.question_cancelled_sentinel raw
  | Lwt.Sleep -> Alcotest.fail "expected pending question cancellation"
  | Lwt.Fail exn ->
      Alcotest.fail
        (Printf.sprintf "pending question failed: %s" (Printexc.to_string exn)));
  Lwt.return_unit

let test_enqueue_admin_stop_if_busy_does_not_run_interrupt_finalizer () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "telegram:1:u" in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  let finalizer_calls = ref 0 in
  Hashtbl.replace mgr.sessions key (Agent.create ~config (), mutex, interrupt);
  Session.register_interrupt_finalizer mgr ~key (fun () ->
      incr finalizer_calls;
      Lwt.return_unit);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let* queued =
       Session.enqueue_message_if_busy mgr ~key
         (queued_message ~channel:"telegram" ~channel_id:"1" ~user_group:"admin"
            "stop")
     in
     Alcotest.(check bool) "admin stop handled" true queued;
     Alcotest.(check (option string))
       "stop interrupt marked" (Some stop_interrupt_token_for_test) !interrupt;
     Alcotest.(check int) "admin stop finalizer not run" 0 !finalizer_calls;
     let* queued =
       Session.enqueue_message_if_busy mgr ~key
         (queued_message ~channel:"telegram" ~channel_id:"1" "follow-up")
     in
     Alcotest.(check bool) "follow-up queued" true queued;
     Alcotest.(check int) "queued follow-up finalizer run" 1 !finalizer_calls;
     Lwt.return_unit)

let test_stop_busy_session_if_admin_stop_does_not_run_interrupt_finalizer () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "telegram:1:u" in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  let finalizer_calls = ref 0 in
  Hashtbl.replace mgr.sessions key (Agent.create ~config (), mutex, interrupt);
  Session.register_interrupt_finalizer mgr ~key (fun () ->
      incr finalizer_calls;
      Lwt.return_unit);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let* stopped =
       Session.stop_busy_session_if_admin_stop mgr ~key ~message:"/stop"
         ~user_group:"admin" ()
     in
     Alcotest.(check bool) "admin stop handled" true stopped;
     Alcotest.(check (option string))
       "stop interrupt marked" (Some stop_interrupt_token_for_test) !interrupt;
     Alcotest.(check int) "admin stop finalizer not run" 0 !finalizer_calls;
     Lwt.return_unit)

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
       "interrupt marked" (Some Agent.queued_message_interrupt_token) !interrupt;
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

let test_midturn_injection_pulls_steering_before_followups () =
  let mgr = Session.create ~config:Runtime_config.default () in
  let key = "telegram:1:u" in
  Hashtbl.replace mgr.queued_messages key
    [
      queued_message "x";
      queued_message ~deferred_followup:true "y";
      queued_message "z";
      queued_message ~deferred_followup:true "w";
    ];
  let injected = Session.take_all_queued_messages_for_injection mgr ~key in
  Alcotest.(check (list string))
    "only steering messages are injected" [ "x"; "z" ]
    (List.map (fun (msg : Session.queued_message) -> msg.message) injected);
  Alcotest.(check (list string))
    "followups remain queued in order" [ "y"; "w" ]
    (List.filter_map
       (fun _ ->
         Option.map
           (fun (msg : Session.queued_message) -> msg.message)
           (Session.take_next_queued_message mgr ~key))
       [ (); () ])

let test_metadata_free_user_inject_counts_as_steering () =
  let mgr = Session.create ~config:Runtime_config.default () in
  let key = "telegram:1:u" in
  Hashtbl.replace mgr.queued_messages key [ queued_message "please steer" ];
  Alcotest.(check bool)
    "metadata-free user injection steers" true
    (Lwt_main.run (Session.has_queued_steering_message mgr ~key));
  Hashtbl.replace mgr.queued_messages key
    [ queued_message "[bg #12 started: Codex repo=/tmp/repo branch=main]" ];
  Alcotest.(check bool)
    "background lifecycle message does not steer" false
    (Lwt_main.run (Session.has_queued_steering_message mgr ~key))

let test_append_followup_updates_last_queued_followup () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "telegram:1:u" in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  Hashtbl.replace mgr.sessions key (Agent.create ~config (), mutex, interrupt);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let* first =
       Session.enqueue_followup_if_busy mgr ~key
         (queued_message ~channel:"telegram" ~channel_id:"1" "first")
     in
     (match first with
     | `Queued -> ()
     | `Idle -> Alcotest.fail "expected first followup to queue");
     let* appended =
       Session.append_followup_if_busy mgr ~key
         (queued_message ~channel:"telegram" ~channel_id:"1" "second")
     in
     (match appended with
     | `Appended -> ()
     | `Queued | `Idle -> Alcotest.fail "expected append to update followup");
     Lwt.return_unit);
  match Session.take_next_queued_message mgr ~key with
  | None -> Alcotest.fail "expected queued followup"
  | Some queued ->
      Alcotest.(check bool)
        "queued item is deferred followup" true queued.deferred_followup;
      Alcotest.(check string)
        "append separated by blank line" "first\n\nsecond" queued.message

let test_append_followup_without_existing_queues_new_followup () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "telegram:1:u" in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  Hashtbl.replace mgr.sessions key (Agent.create ~config (), mutex, interrupt);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let* result =
       Session.append_followup_if_busy mgr ~key
         (queued_message ~channel:"telegram" ~channel_id:"1" "first")
     in
     (match result with
     | `Queued -> ()
     | `Appended | `Idle ->
         Alcotest.fail "expected append fallback to queue followup");
     Lwt.return_unit);
  match Session.take_next_queued_message mgr ~key with
  | None -> Alcotest.fail "expected queued followup"
  | Some queued ->
      Alcotest.(check bool)
        "fallback queued item is deferred followup" true
        queued.deferred_followup;
      Alcotest.(check string) "content preserved" "first" queued.message

let test_append_followup_updates_sqlite_payload () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:1:u" in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  Hashtbl.replace mgr.sessions key (Agent.create ~config (), mutex, interrupt);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let* first =
       Session.enqueue_followup_if_busy mgr ~key
         (queued_message ~channel:"telegram" ~channel_id:"1" "first")
     in
     (match first with
     | `Queued -> ()
     | `Idle -> Alcotest.fail "expected first followup to queue");
     let* appended =
       Session.append_followup_if_busy mgr ~key
         (queued_message ~channel:"telegram" ~channel_id:"1" "second")
     in
     (match appended with
     | `Appended -> ()
     | `Queued | `Idle -> Alcotest.fail "expected sqlite append");
     Lwt.return_unit);
  match Memory.queue_list ~db ~session_key:key with
  | [ row ] ->
      let json = Yojson.Safe.from_string row.Memory.payload_json in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "sqlite payload updated" "first\n\nsecond"
        (json |> member "message" |> to_string);
      Alcotest.(check bool)
        "sqlite payload marks deferred followup" true
        (json |> member "deferred_followup" |> to_bool)
  | rows -> Alcotest.failf "expected one queued row, got %d" (List.length rows)

let test_enqueue_message_if_busy_queues_without_channel_notifier () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  Hashtbl.replace mgr.sessions "github:owner/repo:pr:1"
    (Agent.create ~config (), mutex, interrupt);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let* queued =
       Session.enqueue_message_if_busy mgr ~key:"github:owner/repo:pr:1"
         (queued_message ~channel:"github" ~channel_id:"owner/repo:pr:1"
            "task result injected")
     in
     Alcotest.(check bool) "message queued without notifier" true queued;
     Alcotest.(check (option string))
       "interrupt marked" (Some Agent.queued_message_interrupt_token) !interrupt;
     Lwt.return_unit);
  match Session.take_next_queued_message mgr ~key:"github:owner/repo:pr:1" with
  | None -> Alcotest.fail "expected queued message"
  | Some queued ->
      Alcotest.(check string)
        "queued content preserved" "task result injected" queued.Session.message

let test_enqueue_admin_stop_if_busy_sets_stop_interrupt_without_queue () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  Hashtbl.replace mgr.sessions "telegram:1:u"
    (Agent.create ~config (), mutex, interrupt);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let* queued =
       Session.enqueue_message_if_busy mgr ~key:"telegram:1:u"
         (queued_message ~channel:"telegram" ~channel_id:"1" ~user_group:"admin"
            " stop ")
     in
     Alcotest.(check bool) "stop handled as busy message" true queued;
     Alcotest.(check (option string))
       "stop interrupt marked" (Some stop_interrupt_token_for_test) !interrupt;
     Lwt.return_unit);
  Alcotest.(check (option string))
    "stop message not stored in queue" None
    (Option.map
       (fun msg -> msg.Session.message)
       (Session.take_next_queued_message mgr ~key:"telegram:1:u"))

let test_turn_admin_stop_on_queueable_busy_session_returns_queued_sentinel () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  Hashtbl.replace mgr.sessions "telegram:1:u"
    (Agent.create ~config (), mutex, interrupt);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let* response =
       Session.turn mgr ~key:"telegram:1:u" ~message:"/stop" ~user_group:"admin"
         ~channel:"telegram" ~channel_id:"1" ()
     in
     Alcotest.(check string)
       "queueable stop uses queued sentinel" Session.queued_message_response
       response;
     Alcotest.(check (option string))
       "stop interrupt marked" (Some stop_interrupt_token_for_test) !interrupt;
     Lwt.return_unit);
  Alcotest.(check (option string))
    "stop message not stored in queue" None
    (Option.map
       (fun msg -> msg.Session.message)
       (Session.take_next_queued_message mgr ~key:"telegram:1:u"))

let test_enqueue_admin_stop_if_busy_cancels_pending_question () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "telegram:1:u" in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  Hashtbl.replace mgr.sessions key (Agent.create ~config (), mutex, interrupt);
  let promise, _resolver = Session.register_pending_question mgr ~key in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let* queued =
       Session.enqueue_message_if_busy mgr ~key
         (queued_message ~channel:"telegram" ~channel_id:"1" ~user_group:"admin"
            "/stop")
     in
     Alcotest.(check bool) "stop handled as busy message" true queued;
     Alcotest.(check (option string))
       "stop interrupt marked" (Some stop_interrupt_token_for_test) !interrupt;
     check_pending_question_cancelled mgr ~key promise)

let test_stop_busy_session_if_admin_stop_cancels_pending_question () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "telegram:1:u" in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  Hashtbl.replace mgr.sessions key (Agent.create ~config (), mutex, interrupt);
  let promise, _resolver = Session.register_pending_question mgr ~key in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let* stopped =
       Session.stop_busy_session_if_admin_stop mgr ~key ~message:"stop"
         ~user_group:"admin" ()
     in
     Alcotest.(check bool) "busy admin stop handled" true stopped;
     Alcotest.(check (option string))
       "stop interrupt marked" (Some stop_interrupt_token_for_test) !interrupt;
     check_pending_question_cancelled mgr ~key promise)

let test_turn_admin_bang_stop_if_busy_queues_followup () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "telegram:1:u" in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  Hashtbl.replace mgr.sessions key (Agent.create ~config (), mutex, interrupt);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let* response =
       Session.turn mgr ~key ~message:"!stop" ~user_group:"admin"
         ~channel:"telegram" ~channel_id:"1" ()
     in
     Alcotest.(check string)
       "bang stop still queues follow-up" Session.queued_message_response
       response;
     Alcotest.(check (option string))
       "bang interrupt remains normalized content" (Some "stop") !interrupt;
     Lwt.return_unit);
  match Session.take_next_queued_message mgr ~key with
  | None -> Alcotest.fail "expected queued bang stop follow-up"
  | Some queued ->
      Alcotest.(check string)
        "queued bang stop content" "stop" queued.Session.message

let test_enqueue_admin_bang_stop_raw_message_queues_followup () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "telegram:1:u" in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  Hashtbl.replace mgr.sessions key (Agent.create ~config (), mutex, interrupt);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Session.set_interrupt_if_present mgr ~key "stop" in
     let* () = Lwt_mutex.lock mutex in
     let* queued =
       Session.enqueue_message_if_busy mgr ~key ~raw_message:"!stop"
         (queued_message ~channel:"telegram" ~channel_id:"1" ~user_group:"admin"
            "stop")
     in
     Alcotest.(check bool) "raw bang stop queues follow-up" true queued;
     Alcotest.(check (option string))
       "normalized bang interrupt preserved" (Some "stop") !interrupt;
     Lwt.return_unit);
  match Session.take_next_queued_message mgr ~key with
  | None -> Alcotest.fail "expected queued raw bang stop follow-up"
  | Some queued ->
      Alcotest.(check string)
        "queued raw bang stop content" "stop" queued.Session.message;
      Alcotest.(check bool) "queued raw bang marker preserved" true queued.bang

let test_enqueue_guest_stop_if_busy_queues_normally () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let interrupt = ref None in
  let mutex = Lwt_mutex.create () in
  Hashtbl.replace mgr.sessions "telegram:1:u"
    (Agent.create ~config (), mutex, interrupt);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let* queued =
       Session.enqueue_message_if_busy mgr ~key:"telegram:1:u"
         (queued_message ~channel:"telegram" ~channel_id:"1" ~user_group:"guest"
            "stop")
     in
     Alcotest.(check bool) "guest stop queued" true queued;
     Alcotest.(check (option string))
       "queued interrupt marked" (Some Agent.queued_message_interrupt_token)
       !interrupt;
     Lwt.return_unit);
  match Session.take_next_queued_message mgr ~key:"telegram:1:u" with
  | None -> Alcotest.fail "expected queued guest stop"
  | Some queued ->
      Alcotest.(check string)
        "queued guest stop content" "stop" queued.Session.message

let test_take_all_queued_messages_for_injection_handles_admin_stop () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "telegram:1:u" in
  let interrupt = ref None in
  Hashtbl.replace mgr.queued_messages key
    [
      queued_message ~channel:"telegram" ~channel_id:"1" ~user_group:"admin"
        "/stop";
      queued_message ~channel:"telegram" ~channel_id:"1" "please continue";
    ];
  let msgs =
    Session.take_all_queued_messages_for_injection ~interrupt mgr ~key
  in
  Alcotest.(check (option string))
    "stop interrupt marked" (Some stop_interrupt_token_for_test) !interrupt;
  Alcotest.(check (list string))
    "admin stop not injected" [ "please continue" ]
    (List.map (fun (msg : Session.queued_message) -> msg.message) msgs)

let test_drain_queued_messages_handles_queued_admin_stop () =
  with_fake_chat_provider (fun config ->
      let mgr = Session.create ~config () in
      let key = "telegram:1:u" in
      let sent = ref [] in
      Lwt_main.run
        (Session.with_registered_notifier mgr ~key
           ~notify:(fun text ->
             sent := text :: !sent;
             Lwt.return_unit)
           (fun () ->
             Session.with_session_lock mgr ~key (fun agent interrupt ->
                 Hashtbl.replace mgr.queued_messages key
                   [
                     queued_message ~channel:"telegram" ~channel_id:"1"
                       ~user_group:"admin" "stop";
                     queued_message ~channel:"telegram" ~channel_id:"1"
                       "please continue";
                   ];
                 let open Lwt.Syntax in
                 let* () =
                   Session.drain_queued_messages mgr ~key agent interrupt ()
                 in
                 Alcotest.(check (option string))
                   "stop interrupt marked" (Some stop_interrupt_token_for_test)
                   !interrupt;
                 Lwt.return_unit)));
      Alcotest.(check int) "no response sent" 0 (List.length !sent);
      match Session.take_next_queued_message mgr ~key with
      | None -> Alcotest.fail "expected later message to remain queued"
      | Some queued ->
          Alcotest.(check string)
            "later message remains queued" "please continue" queued.message)

let test_drain_queued_messages_preserves_admin_bang_followup () =
  with_fake_chat_provider (fun config ->
      let mgr = Session.create ~config () in
      let key = "telegram:1:u" in
      let sent = ref [] in
      Lwt_main.run
        (Session.with_registered_notifier mgr ~key
           ~notify:(fun text ->
             sent := text :: !sent;
             Lwt.return_unit)
           (fun () ->
             Session.with_session_lock mgr ~key (fun agent interrupt ->
                 Hashtbl.replace mgr.queued_messages key
                   [
                     {
                       (queued_message ~channel:"telegram" ~channel_id:"1"
                          ~user_group:"admin" "stop")
                       with
                       bang = true;
                     };
                   ];
                 Session.drain_queued_messages mgr ~key agent interrupt ())));
      Alcotest.(check int) "followup sent once" 1 (List.length !sent);
      Alcotest.(check bool)
        "followup produced provider reply" true
        (String.starts_with ~prefix:"reply:" (List.hd !sent)))

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
                   interrupt ())));
      Alcotest.(check int) "followup sent once" 1 (List.length !sent);
      Alcotest.(check bool)
        "followup produced provider reply" true
        (String.starts_with ~prefix:"reply:" (List.hd !sent));
      Alcotest.(check bool)
        "queue drained" true
        (Session.take_next_queued_message mgr ~key:"telegram:1:u" = None))

let test_drain_deferred_followup_sends_plain_message () =
  let seen_user_message = ref None in
  let response_for_user message =
    seen_user_message := Some message;
    "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      let mgr = Session.create ~config () in
      let key = "telegram:1:u" in
      let sent = ref [] in
      Lwt_main.run
        (Session.with_registered_notifier mgr ~key
           ~notify:(fun text ->
             sent := text :: !sent;
             Lwt.return_unit)
           (fun () ->
             Session.with_session_lock mgr ~key (fun agent interrupt ->
                 Hashtbl.replace mgr.queued_messages key
                   [ queued_message ~deferred_followup:true "please continue" ];
                 Session.drain_queued_messages mgr ~key agent interrupt ())));
      Alcotest.(check (option string))
        "deferred followup delivered as plain user message"
        (Some "please continue") !seen_user_message;
      Alcotest.(check int) "followup sent once" 1 (List.length !sent))

let test_deferred_turn_queues_if_lock_lost () =
  with_fake_chat_provider (fun config ->
      let mgr = Session.create ~config () in
      let key = "telegram:1:u" in
      let response =
        Lwt_main.run
          (Session.with_session_lock mgr ~key (fun _agent _interrupt ->
               Session.turn mgr ~key ~message:"after this turn"
                 ~deferred_if_busy:true ()))
      in
      Alcotest.(check string)
        "busy fallback suppresses immediate response"
        Session.queued_message_response response;
      match Session.take_next_queued_message mgr ~key with
      | Some queued ->
          Alcotest.(check string)
            "queued message" "after this turn" queued.message;
          Alcotest.(check bool)
            "queued as deferred followup" true queued.deferred_followup
      | None -> Alcotest.fail "expected deferred followup to be queued")

let test_idle_followup_drains_later_busy_followup () =
  let mgr_ref = ref None in
  let queued_second = ref false in
  let response_for_user message =
    if
      Test_helpers.string_contains message "first followup"
      && not !queued_second
    then begin
      queued_second := true;
      match !mgr_ref with
      | Some (mgr : Session.t) ->
          Hashtbl.replace mgr.queued_messages "test:c:u"
            [ queued_message ~deferred_followup:true "second followup" ]
      | None -> Alcotest.fail "session manager not initialized"
    end;
    "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      let mgr = Session.create ~config () in
      mgr_ref := Some mgr;
      let sent = ref [] in
      Lwt_main.run
        (Connector_dispatch.dispatch_followup ~session_mgr:mgr ~key:"test:c:u"
           ~connector_name:"test" ~channel_id:"c" ~user_id:"u" ~is_admin:true
           ~send_reply:(fun text ->
             sent := text :: !sent;
             Lwt.return_unit)
           (Slash_commands.FollowupQueue "first followup"));
      Alcotest.(check bool)
        "second followup queued during turn" true !queued_second;
      let sent = List.rev !sent in
      Alcotest.(check int) "two followup responses sent" 2 (List.length sent);
      let find_idx needle =
        List.find_index
          (fun text -> Test_helpers.string_contains text needle)
          sent
      in
      Alcotest.(check (option int))
        "first followup response index" (Some 0)
        (find_idx "first followup");
      Alcotest.(check (option int))
        "second followup response index" (Some 1)
        (find_idx "second followup");
      Alcotest.(check bool)
        "queue empty after drain" true
        (Session.take_next_queued_message mgr ~key:"test:c:u" = None))

let test_turn_uses_special_command_handler () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let progress = ref [] in
  Session.set_special_command_handler mgr
    (fun ~key ~message ~send_progress ~interrupt_check:_ ->
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
         (fun ~key ~message ~send_progress ~interrupt_check:_ ->
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

let test_turn_stream_skill_load_notifies_registered_notifier_once () =
  with_fake_chat_provider (fun config ->
      let mgr = Session.create ~config () in
      let key = "telegram:skill-load" in
      let notifications = ref [] in
      let chunks = ref [] in
      let skill_injection = "[Skill: stream-skill]\nUse stream skill" in
      let on_chunk chunk =
        chunks := chunk :: !chunks;
        Lwt.return_unit
      in
      let run_once message =
        Session.turn_stream mgr ~key ~message
          ~skill_injections:[ skill_injection ] ~on_chunk ()
      in
      Lwt_main.run
        (Session.with_registered_notifier mgr ~key
           ~notify:(fun text ->
             notifications := text :: !notifications;
             Lwt.return_unit)
           (fun () ->
             let open Lwt.Syntax in
             let* _ = run_once "first" in
             let* () = Lwt.pause () in
             Alcotest.(check (list string))
               "first load notification"
               [ "Loaded skill: stream-skill" ]
               (List.rev !notifications);
             let* _ = run_once "repeat" in
             let* () = Lwt.pause () in
             Lwt.return_unit));
      Alcotest.(check (list string))
        "repeat does not notify again"
        [ "Loaded skill: stream-skill" ]
        (List.rev !notifications);
      let deltas =
        List.filter_map
          (function Provider.Delta text -> Some text | _ -> None)
          (List.rev !chunks)
      in
      Alcotest.(check bool)
        "registered notifier suppresses stream delta notification" false
        (List.exists
           (fun text ->
             Test_helpers.string_contains text "Loaded skill: stream-skill")
           deltas))

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
        (Test_helpers.string_contains output "Compacting conversation history"))

let test_session_debug_toggle_persists () =
  let db = Memory.init ~db_path:":memory:" () in
  let mgr = Session.create ~config:Runtime_config.default ~db () in
  let key = "web:debug" in
  Alcotest.(check bool)
    "debug defaults off" false
    (Session.session_debug_enabled mgr ~key);
  Alcotest.(check (result unit string))
    "debug set on" (Ok ())
    (Session.set_session_debug mgr ~key ~enabled:true);
  Alcotest.(check bool)
    "debug enabled" true
    (Session.session_debug_enabled mgr ~key);
  Alcotest.(check string)
    "debug status on" "Session web:debug: debug = on"
    (Session.session_debug_status_text mgr ~key);
  Alcotest.(check (result unit string))
    "debug set off" (Ok ())
    (Session.set_session_debug mgr ~key ~enabled:false);
  Alcotest.(check bool)
    "debug disabled" false
    (Session.session_debug_enabled mgr ~key)

let test_session_debug_formats_llm_call_summary () =
  let line =
    Session_debug.format_llm_call
      {
        Session_debug.provider = "fake";
        model = "fake-model";
        duration_s = 1.234;
        usage = Some (100, 20, 25);
        tool_call_count = 2;
      }
  in
  Alcotest.(check bool)
    "mentions provider/model" true
    (Test_helpers.string_contains line "provider=fake"
    && Test_helpers.string_contains line "model=fake-model");
  Alcotest.(check bool)
    "mentions duration" true
    (Test_helpers.string_contains line "duration=1.23s");
  Alcotest.(check bool)
    "mentions total tokens" true
    (Test_helpers.string_contains line "tokens=120");
  Alcotest.(check bool)
    "mentions prompt tokens" true
    (Test_helpers.string_contains line "prompt=100");
  Alcotest.(check bool)
    "mentions reasoning-inclusive output tokens" true
    (Test_helpers.string_contains line "output+reasoning=20");
  Alcotest.(check bool)
    "mentions cached tokens and percent" true
    (Test_helpers.string_contains line "cached=25"
    && Test_helpers.string_contains line "cached_pct=25%");
  Alcotest.(check bool)
    "mentions tool calls" true
    (Test_helpers.string_contains line "tool_calls=2")

let test_turn_sends_debug_summary_after_llm_call () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let key = "web:debug-summary" in
      let sent = ref [] in
      Session.register_channel_notifier mgr ~key (fun text ->
          sent := text :: !sent;
          Lwt.return_unit);
      Alcotest.(check (result unit string))
        "debug set on" (Ok ())
        (Session.set_session_debug mgr ~key ~enabled:true);
      let response = Lwt_main.run (Session.turn mgr ~key ~message:"hello" ()) in
      Alcotest.(check string) "response" "reply:hello" response;
      let messages = List.rev !sent in
      let debug_lines =
        List.filter
          (fun text -> Test_helpers.string_contains text "debug: llm")
          messages
      in
      Alcotest.(check int) "one debug line" 1 (List.length debug_lines);
      let line = List.hd debug_lines in
      Alcotest.(check bool)
        "summary includes provider/model" true
        (Test_helpers.string_contains line "provider=fake"
        && Test_helpers.string_contains line "model=fake-model");
      Alcotest.(check bool)
        "summary includes duration" true
        (Test_helpers.string_contains line "duration=");
      Alcotest.(check bool)
        "summary includes usage" true
        (Test_helpers.string_contains line "tokens=2"
        && Test_helpers.string_contains line "prompt=1"
        && Test_helpers.string_contains line "output+reasoning=1");
      Alcotest.(check bool)
        "summary includes tool calls" true
        (Test_helpers.string_contains line "tool_calls=0"))

let test_agent_mention_sends_debug_summary_for_parent_session () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let key = "web:agent-mention-debug" in
      let sent = ref [] in
      Session.register_channel_notifier mgr ~key (fun text ->
          sent := text :: !sent;
          Lwt.return_unit);
      Alcotest.(check (result unit string))
        "debug set on" (Ok ())
        (Session.set_session_debug mgr ~key ~enabled:true);
      let response =
        Lwt_main.run
          (Session.turn mgr ~key ~message:"@debugger investigate this" ())
      in
      Alcotest.(check string) "response" "reply:investigate this" response;
      let messages = List.rev !sent in
      let debug_lines =
        List.filter
          (fun text -> Test_helpers.string_contains text "debug: llm")
          messages
      in
      Alcotest.(check int) "one debug line" 1 (List.length debug_lines);
      Alcotest.(check bool)
        "summary includes provider/model" true
        (Test_helpers.string_contains (List.hd debug_lines) "provider=fake"
        && Test_helpers.string_contains (List.hd debug_lines) "model=fake-model"
        ))

let test_agent_invoke_sends_debug_summary_for_parent_session () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let key = "web:agent-invoke-debug" in
      let sent = ref [] in
      Alcotest.(check (result unit string))
        "debug set on" (Ok ())
        (Session.set_session_debug mgr ~key ~enabled:true);
      let waiter, resolver = Lwt.wait () in
      Session.agent_invoke_turn mgr ~parent_key:key ~agent_name:"debugger"
        ~prompt:"investigate this"
        ~debug_notify:(fun text ->
          sent := text :: !sent;
          Lwt.return_unit)
        ~send_reply:(fun text ->
          sent := text :: !sent;
          Lwt.wakeup_later resolver text;
          Lwt.return_unit)
        ();
      let response = Lwt_main.run waiter in
      Alcotest.(check string) "response" "reply:investigate this" response;
      let messages = List.rev !sent in
      let debug_lines =
        List.filter
          (fun text -> Test_helpers.string_contains text "debug: llm")
          messages
      in
      Alcotest.(check int) "one debug line" 1 (List.length debug_lines);
      Alcotest.(check bool)
        "summary includes provider/model" true
        (Test_helpers.string_contains (List.hd debug_lines) "provider=fake"
        && Test_helpers.string_contains (List.hd debug_lines) "model=fake-model"
        ))

let test_delegate_sends_debug_summary_for_parent_session () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let key = "web:delegate-debug" in
      let sent = ref [] in
      Alcotest.(check (result unit string))
        "debug set on" (Ok ())
        (Session.set_session_debug mgr ~key ~enabled:true);
      let waiter, resolver = Lwt.wait () in
      Session.delegate_turn mgr ~parent_key:key ~prompt:"investigate this"
        ~debug_notify:(fun text ->
          sent := text :: !sent;
          Lwt.return_unit)
        ~send_reply:(fun text ->
          sent := text :: !sent;
          Lwt.wakeup_later resolver text;
          Lwt.return_unit)
        ();
      let response = Lwt_main.run waiter in
      Alcotest.(check string) "response" "reply:investigate this" response;
      let messages = List.rev !sent in
      let debug_lines =
        List.filter
          (fun text -> Test_helpers.string_contains text "debug: llm")
          messages
      in
      Alcotest.(check int) "one debug line" 1 (List.length debug_lines);
      Alcotest.(check bool)
        "summary includes provider/model" true
        (Test_helpers.string_contains (List.hd debug_lines) "provider=fake"
        && Test_helpers.string_contains (List.hd debug_lines) "model=fake-model"
        ))

let test_fork_and_sends_debug_summary_for_parent_session () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let key = "web:fork-debug" in
      let sent = ref [] in
      Alcotest.(check (result unit string))
        "debug set on" (Ok ())
        (Session.set_session_debug mgr ~key ~enabled:true);
      let waiter, resolver = Lwt.wait () in
      Session.fork_and_run mgr ~parent_key:key ~prompt:"investigate this"
        ~debug_notify:(fun text ->
          sent := text :: !sent;
          Lwt.return_unit)
        ~send_reply:(fun text ->
          sent := text :: !sent;
          Lwt.wakeup_later resolver text;
          Lwt.return_unit)
        ();
      let response = Lwt_main.run waiter in
      Alcotest.(check bool)
        "response comes from fake provider" true
        (Test_helpers.string_contains response "reply:");
      let messages = List.rev !sent in
      let debug_lines =
        List.filter
          (fun text -> Test_helpers.string_contains text "debug: llm")
          messages
      in
      Alcotest.(check int) "one debug line" 1 (List.length debug_lines);
      Alcotest.(check bool)
        "summary includes provider/model" true
        (Test_helpers.string_contains (List.hd debug_lines) "provider=fake"
        && Test_helpers.string_contains (List.hd debug_lines) "model=fake-model"
        ))

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

let make_fake_tool name invocations =
  {
    Tool.name;
    description = "fake tool " ^ name;
    parameters_schema = `Assoc [];
    invoke =
      (fun ?context:_ _args ->
        invocations := name :: !invocations;
        Lwt.return ("result:" ^ name));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let make_tool_call ~id ~name =
  { Provider.id; function_name = name; arguments = "{}" }

let room_profile_tool_config ?(allowed_tools = []) ?(denied_tools = []) () =
  {
    Runtime_config.default with
    room_profiles =
      [
        {
          Runtime_config.id = "profile-a";
          display_name = None;
          model = "openai:gpt-5.4";
          system_prompt = "";
          max_tool_iterations = 10;
          status = "active";
          allowed_tools;
          denied_tools;
          access_bundle_ids = [];
          ambient_enabled = false;
          ambient_quiet_start = 23;
          ambient_quiet_end = 8;
          ambient_rate_limit_rph = 0;
        };
      ];
    room_profile_bindings =
      [
        { Runtime_config.profile_id = "profile-a"; room = "C_A"; active = true };
      ];
  }

let test_room_profile_tool_allowlist_allows_listed_tool () =
  let config = room_profile_tool_config ~allowed_tools:[ "tool_a" ] () in
  let invocations = ref [] in
  let registry = Tool_registry.create () in
  List.iter
    (fun n -> Tool_registry.register registry (make_fake_tool n invocations))
    [ "tool_a"; "tool_b" ];
  let agent = Agent.create ~config ~tool_registry:registry () in
  Lwt_main.run
    (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
       ~session_key:(Some "web:C_A")
       [ make_tool_call ~id:"tc1" ~name:"tool_a" ]);
  Alcotest.(check (list string))
    "allowed tool invoked" [ "tool_a" ] (List.rev !invocations);
  Alcotest.(check (list string))
    "allowed tool result" [ "result:tool_a" ]
    (List.map (fun (msg : Provider.message) -> msg.content) agent.history)

let test_room_profile_tool_denylist_blocks_denied_tool () =
  let config = room_profile_tool_config ~denied_tools:[ "tool_b" ] () in
  let invocations = ref [] in
  let registry = Tool_registry.create () in
  List.iter
    (fun n -> Tool_registry.register registry (make_fake_tool n invocations))
    [ "tool_a"; "tool_b" ];
  let agent = Agent.create ~config ~tool_registry:registry () in
  Lwt_main.run
    (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
       ~session_key:(Some "web:C_A")
       [ make_tool_call ~id:"tc1" ~name:"tool_b" ]);
  Alcotest.(check (list string)) "denied tool not invoked" [] !invocations;
  match agent.history with
  | [ msg ] ->
      Alcotest.(check bool)
        "denied result is not permitted" true
        (Test_helpers.string_contains msg.content "not permitted")
  | _ -> Alcotest.fail "expected one denied tool result"

let test_room_profile_tool_allowlist_blocks_unlisted_tool () =
  let config = room_profile_tool_config ~allowed_tools:[ "tool_a" ] () in
  let invocations = ref [] in
  let registry = Tool_registry.create () in
  List.iter
    (fun n -> Tool_registry.register registry (make_fake_tool n invocations))
    [ "tool_a"; "tool_b" ];
  let agent = Agent.create ~config ~tool_registry:registry () in
  Lwt_main.run
    (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
       ~session_key:(Some "web:C_A")
       [ make_tool_call ~id:"tc1" ~name:"tool_b" ]);
  Alcotest.(check (list string)) "unlisted tool not invoked" [] !invocations;
  match agent.history with
  | [ msg ] ->
      Alcotest.(check bool)
        "unlisted result is not permitted" true
        (Test_helpers.string_contains msg.content "not permitted");
      Alcotest.(check bool)
        "unlisted result explains allowlist" true
        (Test_helpers.string_contains msg.content "allowed_tools")
  | _ -> Alcotest.fail "expected one unlisted tool result"

let test_room_profile_tool_grants_block_user_bash_command () =
  let config = room_profile_tool_config ~denied_tools:[ "shell_exec" ] () in
  let result =
    Lwt_main.run
      (Slash_commands_bash.run_bash_command ~config ~session_key:"web:C_A"
         "echo should-not-run")
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "user slash bash denied as not permitted" true
        (Test_helpers.string_contains msg "not permitted")
  | Ok _ -> Alcotest.fail "expected denied user-initiated shell_exec"

let test_interrupt_skips_tool_calls_in_batch () =
  let config = Runtime_config.default in
  let invocations = ref [] in
  let registry = Tool_registry.create () in
  List.iter
    (fun n -> Tool_registry.register registry (make_fake_tool n invocations))
    [ "tool_a"; "tool_b"; "tool_c" ];
  let agent = Agent.create ~config ~tool_registry:registry () in
  let interrupt_check () = Some "user interrupted" in
  let calls =
    [
      make_tool_call ~id:"tc1" ~name:"tool_a";
      make_tool_call ~id:"tc2" ~name:"tool_b";
      make_tool_call ~id:"tc3" ~name:"tool_c";
    ]
  in
  Lwt_main.run
    (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
       ~session_key:None ~interrupt_check calls);
  Alcotest.(check (list string)) "no tools invoked" [] (List.rev !invocations);
  Alcotest.(check int)
    "three tool results in history" 3
    (List.length agent.history);
  List.iter
    (fun (msg : Provider.message) ->
      Alcotest.(check string)
        "skipped content" "[skipped: interrupted by user]" msg.content)
    agent.history

let test_interrupt_skips_remaining_tools_stream () =
  let config = Runtime_config.default in
  let invocations = ref [] in
  let registry = Tool_registry.create () in
  List.iter
    (fun n -> Tool_registry.register registry (make_fake_tool n invocations))
    [ "tool_a"; "tool_b"; "tool_c" ];
  let agent = Agent.create ~config ~tool_registry:registry () in
  let interrupt_check () = Some "user interrupted" in
  let chunks = ref [] in
  let on_chunk chunk =
    chunks := chunk :: !chunks;
    Lwt.return_unit
  in
  let calls =
    [
      make_tool_call ~id:"tc1" ~name:"tool_a";
      make_tool_call ~id:"tc2" ~name:"tool_b";
      make_tool_call ~id:"tc3" ~name:"tool_c";
    ]
  in
  Lwt_main.run
    (Agent.execute_tool_calls_stream agent ~db:None ~audit_enabled:false
       ~session_key:None ~interrupt_check ~on_chunk calls);
  Alcotest.(check (list string)) "no tools invoked" [] (List.rev !invocations);
  Alcotest.(check int)
    "three tool results in history" 3
    (List.length agent.history);
  let tool_results =
    List.filter_map
      (fun chunk ->
        match chunk with
        | Provider.ToolResult { result; _ } -> Some result
        | _ -> None)
      (List.rev !chunks)
  in
  Alcotest.(check (list string))
    "all three tool results are skipped"
    [
      "[skipped: interrupted by user]";
      "[skipped: interrupted by user]";
      "[skipped: interrupted by user]";
    ]
    tool_results

let test_interrupt_latch_skips_after_first_tool () =
  let config = Runtime_config.default in
  let invocations = ref [] in
  let call_count = ref 0 in
  let registry = Tool_registry.create () in
  List.iter
    (fun n -> Tool_registry.register registry (make_fake_tool n invocations))
    [ "tool_a"; "tool_b"; "tool_c" ];
  let agent = Agent.create ~config ~tool_registry:registry () in
  let interrupt_check () =
    incr call_count;
    if !call_count >= 2 then Some "interrupted" else None
  in
  let calls =
    [
      make_tool_call ~id:"tc1" ~name:"tool_a";
      make_tool_call ~id:"tc2" ~name:"tool_b";
      make_tool_call ~id:"tc3" ~name:"tool_c";
    ]
  in
  Lwt_main.run
    (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
       ~session_key:None ~interrupt_check calls);
  let results =
    List.map (fun (msg : Provider.message) -> msg.content) agent.history
  in
  let skipped =
    List.filter (fun c -> c = "[skipped: interrupted by user]") results
  in
  Alcotest.(check bool)
    "at least one tool skipped" true
    (List.length skipped >= 1);
  Alcotest.(check int)
    "three tool results in history" 3
    (List.length agent.history)

let tool_enabled_config (config : Runtime_config.t) =
  {
    config with
    security = { config.security with tools_enabled = true };
    agent_defaults = { config.agent_defaults with max_tool_iterations = 5 };
  }

let test_agent_turn_stop_interrupt_returns_stopped_by_admin () =
  let request_count = ref 0 in
  let handle_request ~stream:_ ~messages:_ ~json:_ =
    incr request_count;
    if !request_count = 1 then Fake_tool_calls [ ("tc_1", "tool_a", "{}") ]
    else Fake_text "continued after stop"
  in
  with_fake_openai_provider ~handle_request (fun base_config ->
      let config = tool_enabled_config base_config in
      let invocations = ref [] in
      let registry = Tool_registry.create () in
      Tool_registry.register registry (make_fake_tool "tool_a" invocations);
      let agent = Agent.create ~config ~tool_registry:registry () in
      let interrupt_check () = Some stop_interrupt_token_for_test in
      let response =
        Lwt_main.run
          (Agent.turn agent ~user_message:"do work" ~interrupt_check ())
      in
      Alcotest.(check string) "stop response" "Stopped by admin." response;
      Alcotest.(check (list string)) "tool not invoked" [] !invocations;
      Alcotest.(check int) "no follow-up provider call" 1 !request_count)

let test_agent_turn_stream_stop_interrupt_returns_stopped_by_admin () =
  let request_count = ref 0 in
  let handle_request ~stream:_ ~messages:_ ~json:_ =
    incr request_count;
    if !request_count = 1 then Fake_tool_calls [ ("tc_1", "tool_a", "{}") ]
    else Fake_text "continued after stop"
  in
  with_fake_openai_provider ~handle_request (fun base_config ->
      let config = tool_enabled_config base_config in
      let invocations = ref [] in
      let registry = Tool_registry.create () in
      Tool_registry.register registry (make_fake_tool "tool_a" invocations);
      let agent = Agent.create ~config ~tool_registry:registry () in
      let chunks = ref [] in
      let interrupt_check () = Some stop_interrupt_token_for_test in
      let response =
        Lwt_main.run
          (Agent.turn_stream agent ~user_message:"do work" ~interrupt_check
             ~on_chunk:(fun chunk ->
               chunks := chunk :: !chunks;
               Lwt.return_unit)
             ())
      in
      let deltas =
        List.filter_map
          (function Provider.Delta text -> Some text | _ -> None)
          (List.rev !chunks)
      in
      Alcotest.(check string)
        "stream stop response" "Stopped by admin." response;
      Alcotest.(check bool)
        "stream emits stopped response" true
        (List.mem "Stopped by admin." deltas);
      Alcotest.(check (list string)) "stream tool not invoked" [] !invocations;
      Alcotest.(check int) "stream no follow-up provider call" 1 !request_count)

let test_with_registered_notifier_restores_previous () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  Lwt_main.run
    (Session.with_registered_notifier mgr ~key:"telegram:1:u"
       ~notify:(fun _ -> Lwt.return_unit)
       (fun () ->
         let open Lwt.Syntax in
         let* () =
           Session.with_registered_notifier mgr ~key:"telegram:1:u"
             ~notify:(fun _ -> Lwt.return_unit)
             (fun () -> Lwt.return_unit)
         in
         Alcotest.(check bool)
           "notifier restored" true
           (Session.find_registered_notifier mgr ~key:"telegram:1:u" <> None);
         Lwt.return_unit));
  Alcotest.(check bool)
    "notifier removed after outer scope" true
    (Session.find_registered_notifier mgr ~key:"telegram:1:u" = None)

let test_with_registered_notifier_preserves_factory_and_capabilities () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "teams:conv1:u" in
  Session.register_connector_capabilities mgr ~key Connector_capabilities.teams;
  Session.register_status_message_factory mgr ~key (fun () ->
      let notifier, _, _ = mock_status_notifier () in
      Status_message.create ~notifier ~parse_mode:"Teams" ());
  Lwt_main.run
    (Session.with_registered_notifier mgr ~key
       ~notify:(fun _ -> Lwt.return_unit)
       (fun () -> Lwt.return_unit));
  Alcotest.(check bool)
    "capabilities preserved after cleanup" true
    (Option.is_some (Session.find_connector_capabilities mgr ~key));
  Alcotest.(check bool)
    "channel notifier removed after cleanup" true
    (Option.is_none (Session.find_registered_notifier mgr ~key))

let test_consolidated_status_on_chunk_hides_thinking_when_disabled () =
  let notifier, sent, edited = mock_status_notifier () in
  let sm =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  let thinking =
    "this hidden chain of thought is long enough to force a send"
  in
  let agent_defaults =
    {
      Runtime_config.default.agent_defaults with
      show_thinking = false;
      show_tool_calls = true;
    }
  in
  let handler =
    Status_update.make_handler ~strategy:Consolidated
      ~notifier_factory:(Some (fun () -> sm))
      ~notify:(fun _ -> Lwt.return_unit)
      ~agent_defaults ~parse_mode:"Markdown" ()
  in
  Lwt_main.run (handler.on_chunk (Provider.ThinkingDelta thinking));
  Alcotest.(check string)
    "thinking buffer remains empty" "" (handler.get_thinking ());
  Alcotest.(check string)
    "status render stays empty" "" (Status_message.render sm);
  Alcotest.(check int) "no status message sent" 0 (List.length !sent);
  Alcotest.(check int) "no status message edited" 0 (List.length !edited)

let test_consolidated_status_on_chunk_shows_thinking_when_enabled () =
  let notifier, sent, _edited = mock_status_notifier () in
  let sm =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  let thinking = "visible plan for the user" in
  let agent_defaults =
    {
      Runtime_config.default.agent_defaults with
      show_thinking = true;
      show_tool_calls = true;
    }
  in
  let handler =
    Status_update.make_handler ~strategy:Consolidated
      ~notifier_factory:(Some (fun () -> sm))
      ~notify:(fun _ -> Lwt.return_unit)
      ~agent_defaults ~parse_mode:"Markdown" ()
  in
  Lwt_main.run (handler.on_chunk (Provider.ThinkingDelta thinking));
  Alcotest.(check string)
    "thinking buffer captures text" thinking (handler.get_thinking ());
  Alcotest.(check bool)
    "status render includes thinking" true
    (Test_helpers.string_contains (Status_message.render sm) thinking);
  Alcotest.(check bool) "status message emitted" true (List.length !sent >= 1)

let test_drain_works_after_concurrent_notifier_registration () =
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
                 let* () =
                   Session.with_registered_notifier mgr ~key:"telegram:1:u"
                     ~notify:(fun _ -> Lwt.return_unit)
                     (fun () ->
                       let* _queued =
                         Session.enqueue_message_if_busy mgr ~key:"telegram:1:u"
                           (queued_message ~channel:"telegram" ~channel_id:"1"
                              "queued msg")
                       in
                       Lwt.return_unit)
                 in
                 Session.drain_queued_messages mgr ~key:"telegram:1:u" agent
                   interrupt ())));
      Alcotest.(check int)
        "queued message drained and sent" 1 (List.length !sent))

let test_queued_interrupt_does_not_skip_tools () =
  let config = Runtime_config.default in
  let invocations = ref [] in
  let registry = Tool_registry.create () in
  List.iter
    (fun n -> Tool_registry.register registry (make_fake_tool n invocations))
    [ "tool_a"; "tool_b"; "tool_c" ];
  let agent = Agent.create ~config ~tool_registry:registry () in
  let interrupt_check () = Some Agent.queued_message_interrupt_token in
  let calls =
    [
      make_tool_call ~id:"tc1" ~name:"tool_a";
      make_tool_call ~id:"tc2" ~name:"tool_b";
      make_tool_call ~id:"tc3" ~name:"tool_c";
    ]
  in
  Lwt_main.run
    (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
       ~session_key:None ~interrupt_check calls);
  Alcotest.(check (list string))
    "all tools invoked"
    [ "tool_a"; "tool_b"; "tool_c" ]
    (List.rev !invocations);
  Alcotest.(check int)
    "three tool results in history" 3
    (List.length agent.history);
  List.iter
    (fun (msg : Provider.message) ->
      Alcotest.(check bool)
        "not skipped" true
        (not (Test_helpers.string_contains msg.content "[skipped")))
    agent.history

let test_mid_turn_injection_adds_to_history () =
  let config = Runtime_config.default in
  let invocations = ref [] in
  let registry = Tool_registry.create () in
  List.iter
    (fun n -> Tool_registry.register registry (make_fake_tool n invocations))
    [ "tool_a" ];
  let agent = Agent.create ~config ~tool_registry:registry () in
  let injected = ref false in
  let inject_messages () =
    if not !injected then begin
      injected := true;
      [ "A new message arrived...\n\nNew message:\nhey there" ]
    end
    else []
  in
  agent.history <-
    [
      {
        Provider.role = "assistant";
        content = "";
        content_parts = [];
        tool_calls = [ make_tool_call ~id:"tc1" ~name:"tool_a" ];
        tool_call_id = None;
        name = None;
        provider_response_items_json = None;
        thinking = None;
        is_error = false;
      };
    ];
  Lwt_main.run
    (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
       ~session_key:None
       [ make_tool_call ~id:"tc1" ~name:"tool_a" ]);
  (match inject_messages () with
  | msgs ->
      List.iter
        (fun msg ->
          agent.history <-
            Provider.make_message ~role:"user" ~content:msg :: agent.history)
        msgs);
  let has_injected =
    List.exists
      (fun (m : Provider.message) ->
        m.role = "user" && Test_helpers.string_contains m.content "hey there")
      agent.history
  in
  Alcotest.(check bool) "injected message in history" true has_injected

let test_restore_sanitizes_orphaned_tool_results () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"web:s1"
    (Provider.make_tool_result ~tool_call_id:"tc_missing" ~name:"file_read"
       ~content:"orphan");
  Memory.store_message ~db ~session_key:"web:s1"
    (Provider.make_message ~role:"user" ~content:"hello");
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  Lwt_main.run
    (Session.with_session_lock mgr ~key:"web:s1" (fun agent _interrupt ->
         Alcotest.(check int)
           "orphan removed from restored history" 1
           (List.length agent.Agent.history);
         Alcotest.(check string)
           "user message kept" "user"
           (List.hd agent.Agent.history).Provider.role;
         Lwt.return_unit));
  let persisted = Memory.load_history ~db ~session_key:"web:s1" in
  Alcotest.(check int) "sanitized history persisted" 1 (List.length persisted)

let test_active_doc_write_persists_workspace_refresh_event () =
  with_temp_workspace (fun workspace ->
      let db = Memory.init ~db_path:":memory:" () in
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let mgr = Session.create ~config ~db () in
      let registry = Tool_registry.create () in
      Tool_registry.register registry
        (Tools_builtin.doc_write ~workspace ~workspace_files:[ "AGENTS.md" ]);
      let agent = Agent.create ~config ~tool_registry:registry () in
      let history_before = List.length agent.Agent.history in
      let call =
        {
          Provider.id = "tc-doc-write";
          function_name = "doc_write";
          arguments =
            Yojson.Safe.to_string
              (`Assoc
                 [
                   ("filename", `String "AGENTS.md");
                   ("content", `String "updated guidance");
                 ]);
        }
      in
      Lwt_main.run
        (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
           ~session_key:(Some "web:s1") [ call ]);
      Session.persist_new_messages mgr ~key:"web:s1" ~history_before agent;
      let persisted = Memory.load_history ~db ~session_key:"web:s1" in
      Alcotest.(check int)
        "tool result and refresh persisted" 2 (List.length persisted);
      Alcotest.(check string)
        "tool result role first" "tool" (List.nth persisted 0).Provider.role;
      Alcotest.(check string)
        "refresh role second" "event" (List.nth persisted 1).Provider.role;
      Alcotest.(check bool)
        "refresh mentions file" true
        (Test_helpers.string_contains (List.nth persisted 1).Provider.content
           "AGENTS.md"))

let test_active_file_write_persists_workspace_refresh_event () =
  with_temp_workspace (fun workspace ->
      let db = Memory.init ~db_path:":memory:" () in
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let mgr = Session.create ~config ~db () in
      let registry = Tool_registry.create () in
      Tool_registry.register registry
        (Tools_builtin.file_write ~workspace ~workspace_only:true
           ~extra_allowed_paths:[]);
      let agent = Agent.create ~config ~tool_registry:registry () in
      let history_before = List.length agent.Agent.history in
      let call =
        {
          Provider.id = "tc-file-write";
          function_name = "file_write";
          arguments =
            Yojson.Safe.to_string
              (`Assoc
                 [
                   ("path", `String "AGENTS.md");
                   ("content", `String "updated guidance");
                 ]);
        }
      in
      Lwt_main.run
        (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
           ~session_key:(Some "web:s1") [ call ]);
      Session.persist_new_messages mgr ~key:"web:s1" ~history_before agent;
      let persisted = Memory.load_history ~db ~session_key:"web:s1" in
      Alcotest.(check int)
        "tool result and refresh persisted for file_write" 2
        (List.length persisted);
      Alcotest.(check string)
        "refresh role second" "event" (List.nth persisted 1).Provider.role;
      Alcotest.(check bool)
        "refresh mentions file" true
        (Test_helpers.string_contains (List.nth persisted 1).Provider.content
           "AGENTS.md"))

let test_active_file_write_with_equivalent_path_persists_workspace_refresh_event
    () =
  with_temp_workspace (fun workspace ->
      let db = Memory.init ~db_path:":memory:" () in
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let mgr = Session.create ~config ~db () in
      let registry = Tool_registry.create () in
      Tool_registry.register registry
        (Tools_builtin.file_write ~workspace ~workspace_only:true
           ~extra_allowed_paths:[]);
      let agent = Agent.create ~config ~tool_registry:registry () in
      let history_before = List.length agent.Agent.history in
      let call =
        {
          Provider.id = "tc-file-write-dot";
          function_name = "file_write";
          arguments =
            Yojson.Safe.to_string
              (`Assoc
                 [
                   ("path", `String "./AGENTS.md");
                   ("content", `String "updated guidance");
                 ]);
        }
      in
      Lwt_main.run
        (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
           ~session_key:(Some "web:s1") [ call ]);
      Session.persist_new_messages mgr ~key:"web:s1" ~history_before agent;
      let persisted = Memory.load_history ~db ~session_key:"web:s1" in
      Alcotest.(check int)
        "tool result and refresh persisted for equivalent file_write path" 2
        (List.length persisted);
      Alcotest.(check string)
        "refresh role second" "event" (List.nth persisted 1).Provider.role;
      Alcotest.(check bool)
        "refresh mentions normalized file" true
        (Test_helpers.string_contains (List.nth persisted 1).Provider.content
           "AGENTS.md"))

let test_shell_exec_persists_workspace_refresh_event_for_active_file_update () =
  with_temp_workspace (fun workspace ->
      let db = Memory.init ~db_path:":memory:" () in
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let mgr = Session.create ~config ~db () in
      let registry = Tool_registry.create () in
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:true ()
      in
      Tool_registry.register registry
        (Tools_builtin.shell_exec ~workspace ~workspace_only:true
           ~allowed_commands:[ "touch" ] ~extra_allowed_paths:[] ~sandbox);
      let agent = Agent.create ~config ~tool_registry:registry () in
      let history_before = List.length agent.Agent.history in
      let call =
        {
          Provider.id = "tc-shell-touch";
          function_name = "shell_exec";
          arguments =
            Yojson.Safe.to_string
              (`Assoc [ ("command", `String "touch AGENTS.md") ]);
        }
      in
      Lwt_main.run
        (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
           ~session_key:(Some "web:s1") [ call ]);
      Session.persist_new_messages mgr ~key:"web:s1" ~history_before agent;
      let persisted = Memory.load_history ~db ~session_key:"web:s1" in
      Alcotest.(check int)
        "tool result and refresh persisted for shell active file update" 2
        (List.length persisted);
      Alcotest.(check string)
        "refresh role second" "event" (List.nth persisted 1).Provider.role;
      Alcotest.(check bool)
        "refresh mentions shell-updated file" true
        (Test_helpers.string_contains (List.nth persisted 1).Provider.content
           "AGENTS.md"))

let test_git_operations_checkout_persists_workspace_refresh_event () =
  with_temp_workspace (fun workspace ->
      Test_helpers.run_command_or_fail ~label:"git init"
        (Printf.sprintf "git -C %s init -q" (Filename.quote workspace));
      Test_helpers.run_command_or_fail ~label:"git config email"
        (Printf.sprintf "git -C %s config user.email test@example.com"
           (Filename.quote workspace));
      Test_helpers.run_command_or_fail ~label:"git config name"
        (Printf.sprintf "git -C %s config user.name Test"
           (Filename.quote workspace));
      let agents_path = Filename.concat workspace "AGENTS.md" in
      let oc = open_out agents_path in
      output_string oc "base guidance\n";
      close_out oc;
      Test_helpers.run_command_or_fail ~label:"git add"
        (Printf.sprintf "git -C %s add AGENTS.md" (Filename.quote workspace));
      Test_helpers.run_command_or_fail ~label:"git commit"
        (Printf.sprintf "git -C %s commit -q -m init" (Filename.quote workspace));
      let oc = open_out agents_path in
      output_string oc "changed guidance\n";
      close_out oc;
      let db = Memory.init ~db_path:":memory:" () in
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let mgr = Session.create ~config ~db () in
      let registry = Tool_registry.create () in
      Tool_registry.register registry (Tools_builtin.git_operations ~workspace);
      let agent = Agent.create ~config ~tool_registry:registry () in
      let history_before = List.length agent.Agent.history in
      let call =
        {
          Provider.id = "tc-git-checkout";
          function_name = "git_operations";
          arguments =
            Yojson.Safe.to_string
              (`Assoc
                 [
                   ("operation", `String "checkout");
                   ("paths", `List [ `String "AGENTS.md" ]);
                 ]);
        }
      in
      Lwt_main.run
        (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
           ~session_key:(Some "web:s1") [ call ]);
      Session.persist_new_messages mgr ~key:"web:s1" ~history_before agent;
      let persisted = Memory.load_history ~db ~session_key:"web:s1" in
      Alcotest.(check int)
        "tool result and refresh persisted for git checkout active file update"
        2 (List.length persisted);
      Alcotest.(check string)
        "refresh role second" "event" (List.nth persisted 1).Provider.role;
      Alcotest.(check bool)
        "refresh mentions git-updated file" true
        (Test_helpers.string_contains (List.nth persisted 1).Provider.content
           "AGENTS.md"))

let test_batched_active_workspace_updates_attribute_refresh_per_tool_call () =
  with_temp_workspace (fun workspace ->
      let db = Memory.init ~db_path:":memory:" () in
      let prompt =
        {
          Runtime_config.default.prompt with
          workspace_files = [ "AGENTS.md"; "CLAUDE.md" ];
        }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let mgr = Session.create ~config ~db () in
      let registry = Tool_registry.create () in
      Tool_registry.register registry
        (Tools_builtin.file_write ~workspace ~workspace_only:true
           ~extra_allowed_paths:[]);
      let agent = Agent.create ~config ~tool_registry:registry () in
      let history_before = List.length agent.Agent.history in
      let calls =
        [
          {
            Provider.id = "tc-file-write-agents";
            function_name = "file_write";
            arguments =
              Yojson.Safe.to_string
                (`Assoc
                   [
                     ("path", `String "AGENTS.md");
                     ("content", `String "agents guidance");
                   ]);
          };
          {
            Provider.id = "tc-file-write-claude";
            function_name = "file_write";
            arguments =
              Yojson.Safe.to_string
                (`Assoc
                   [
                     ("path", `String "CLAUDE.md");
                     ("content", `String "claude guidance");
                   ]);
          };
        ]
      in
      Lwt_main.run
        (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
           ~session_key:(Some "web:s1") calls);
      Session.persist_new_messages mgr ~key:"web:s1" ~history_before agent;
      let persisted = Memory.load_history ~db ~session_key:"web:s1" in
      (* B607 changed execute_tool_calls to run tool invocations in parallel
         via Lwt_list.map_p. Per-tool workspace-refresh attribution is now
         best-effort: when two tools modify two different active workspace
         files in the same batch, each tool's refresh event may list both
         files (the after-state capture races with the sibling tool's
         write). The total set of file changes is still surfaced to the
         model; only the per-tool attribution is less precise. *)
      Alcotest.(check int)
        "two tool results and two refresh events persisted" 4
        (List.length persisted);
      let refresh1 = (List.nth persisted 1).Provider.content in
      let refresh2 = (List.nth persisted 3).Provider.content in
      let mentions_both s =
        Test_helpers.string_contains s "AGENTS.md"
        && Test_helpers.string_contains s "CLAUDE.md"
      in
      let mentions_one s =
        Test_helpers.string_contains s "AGENTS.md"
        || Test_helpers.string_contains s "CLAUDE.md"
      in
      Alcotest.(check bool)
        "first refresh mentions at least one active file" true
        (mentions_one refresh1);
      Alcotest.(check bool)
        "second refresh mentions at least one active file" true
        (mentions_one refresh2);
      let combined_mentions_both =
        mentions_both refresh1 || mentions_both refresh2
        || mentions_one refresh1 && mentions_one refresh2
           && Test_helpers.string_contains (refresh1 ^ refresh2) "AGENTS.md"
           && Test_helpers.string_contains (refresh1 ^ refresh2) "CLAUDE.md"
      in
      Alcotest.(check bool)
        "both AGENTS.md and CLAUDE.md surfaced across the two refresh events"
        true combined_mentions_both)

let test_workspace_refresh_event_does_not_enter_live_prompt () =
  with_temp_workspace (fun workspace ->
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let registry = Tool_registry.create () in
      Tool_registry.register registry
        (Tools_builtin.doc_write ~workspace ~workspace_files:[ "AGENTS.md" ]);
      let agent = Agent.create ~config ~tool_registry:registry () in
      let call =
        {
          Provider.id = "tc-doc-write";
          function_name = "doc_write";
          arguments =
            Yojson.Safe.to_string
              (`Assoc
                 [
                   ("filename", `String "AGENTS.md");
                   ("content", `String "updated guidance");
                 ]);
        }
      in
      Lwt_main.run
        (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
           ~session_key:(Some "web:s1") [ call ]);
      let msgs = Agent.build_messages agent in
      let system_msg =
        List.find (fun (msg : Provider.message) -> msg.role = "system") msgs
      in
      Alcotest.(check bool)
        "refresh marker not injected into live prompt" false
        (Test_helpers.string_contains system_msg.content
           "workspace context refreshed after active workspace file update");
      Alcotest.(check bool)
        "event role omitted from live messages" false
        (List.exists (fun (msg : Provider.message) -> msg.role = "event") msgs))

let test_non_active_doc_write_does_not_persist_workspace_refresh_event () =
  with_temp_workspace (fun workspace ->
      let db = Memory.init ~db_path:":memory:" () in
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let mgr = Session.create ~config ~db () in
      let registry = Tool_registry.create () in
      Tool_registry.register registry
        (Tools_builtin.doc_write ~workspace ~workspace_files:[ "AGENTS.md" ]);
      let agent = Agent.create ~config ~tool_registry:registry () in
      let history_before = List.length agent.Agent.history in
      let call =
        {
          Provider.id = "tc-doc-write-other";
          function_name = "doc_write";
          arguments =
            Yojson.Safe.to_string
              (`Assoc
                 [
                   ("filename", `String "NOTES.md");
                   ("content", `String "scratchpad");
                 ]);
        }
      in
      Lwt_main.run
        (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
           ~session_key:(Some "web:s1") [ call ]);
      Session.persist_new_messages mgr ~key:"web:s1" ~history_before agent;
      let persisted = Memory.load_history ~db ~session_key:"web:s1" in
      Alcotest.(check int)
        "only tool result persisted" 1 (List.length persisted);
      Alcotest.(check string)
        "only tool result role" "tool" (List.hd persisted).Provider.role)

let test_turn_notifier_surfaces_workspace_refresh_event_for_tool_update () =
  with_temp_workspace (fun workspace ->
      let db = Memory.init ~db_path:":memory:" () in
      let notifications = ref [] in
      let secret = "VERY SECRET AGENTS CONTENT" in
      let request_count = ref 0 in
      with_fake_openai_provider
        ~handle_request:(fun ~stream:_ ~messages:_ ~json:_ ->
          if !request_count = 0 then begin
            incr request_count;
            Fake_tool_calls
              [
                ( "call_doc_write",
                  "doc_write",
                  Yojson.Safe.to_string
                    (`Assoc
                       [
                         ("filename", `String "AGENTS.md");
                         ("content", `String secret);
                       ]) );
              ]
          end
          else Fake_text "done")
        (fun base_config ->
          let prompt =
            {
              base_config.prompt with
              dynamic_enabled = true;
              include_tools_section = false;
              include_safety_section = false;
              include_runtime_section = false;
              include_datetime_section = false;
              include_autonomy_section = false;
              workspace_files = [ "AGENTS.md" ];
            }
          in
          let config =
            {
              base_config with
              workspace;
              prompt;
              security = { base_config.security with tools_enabled = true };
            }
          in
          let registry = Tool_registry.create () in
          Tool_registry.register registry
            (Tools_builtin.doc_write ~workspace ~workspace_files:[ "AGENTS.md" ]);
          let mgr = Session.create ~config ~tool_registry:registry ~db () in
          let response =
            Lwt_main.run
              (Session.with_registered_notifier mgr ~key:"telegram:1:u"
                 ~notify:(fun text ->
                   notifications := text :: !notifications;
                   Lwt.return_unit)
                 (fun () ->
                   Session.turn mgr ~key:"telegram:1:u" ~message:"refresh" ()))
          in
          Alcotest.(check string) "final response" "done" response;
          Alcotest.(check (list string))
            "refresh notice delivered to notifier"
            [
              "workspace context refreshed after active workspace file update: \
               AGENTS.md";
            ]
            (List.rev !notifications);
          Alcotest.(check bool)
            "refresh notice does not leak prompt contents" false
            (List.exists
               (fun text -> Test_helpers.string_contains text secret)
               !notifications);
          let persisted = Memory.load_history ~db ~session_key:"telegram:1:u" in
          Alcotest.(check bool)
            "event persisted in session history" true
            (List.exists
               (fun (msg : Provider.message) ->
                 msg.role = "event"
                 && Test_helpers.string_contains msg.content "AGENTS.md")
               persisted)))

let test_turn_stream_detects_external_workspace_edit_on_next_turn () =
  with_temp_workspace (fun workspace ->
      let agents_path = Filename.concat workspace "AGENTS.md" in
      let write_file path content =
        let oc = open_out path in
        Fun.protect
          (fun () -> output_string oc content)
          ~finally:(fun () -> close_out_noerr oc)
      in
      let original = "ORIGINAL AGENTS CONTENT" in
      let updated = "UPDATED SECRET AGENTS CONTENT" in
      write_file agents_path original;
      let db = Memory.init ~db_path:":memory:" () in
      let seen_system_prompts = ref [] in
      with_fake_openai_provider
        ~handle_request:(fun ~stream:_ ~messages ~json:_ ->
          let open Yojson.Safe.Util in
          let system_prompt =
            List.find_map
              (fun msg ->
                try
                  if msg |> member "role" |> to_string = "system" then
                    Some (msg |> member "content" |> to_string)
                  else None
                with _ -> None)
              messages
            |> Option.value ~default:""
          in
          seen_system_prompts := system_prompt :: !seen_system_prompts;
          Fake_text "ok")
        (fun base_config ->
          let prompt =
            {
              base_config.prompt with
              dynamic_enabled = true;
              include_tools_section = false;
              include_safety_section = false;
              include_runtime_section = false;
              include_datetime_section = false;
              include_autonomy_section = false;
              workspace_files = [ "AGENTS.md" ];
            }
          in
          let config = { base_config with workspace; prompt } in
          let mgr = Session.create ~config ~db () in
          let run_turn manager message =
            let chunks = ref [] in
            let push_chunk chunk =
              chunks := chunk :: !chunks;
              Lwt.return_unit
            in
            let push_text text =
              chunks := Provider.Delta text :: !chunks;
              Lwt.return_unit
            in
            let response =
              Lwt_main.run
                (Session.with_registered_notifier manager ~key:"web:s-external"
                   ~notify:push_text (fun () ->
                     Session.turn_stream manager ~key:"web:s-external" ~message
                       ~on_chunk:push_chunk ()))
            in
            (response, List.rev !chunks)
          in
          let first_response, first_chunks = run_turn mgr "first" in
          Alcotest.(check string) "first response" "ok" first_response;
          Alcotest.(check bool)
            "first turn streams assistant delta" true
            (List.exists
               (function Provider.Delta "ok" -> true | _ -> false)
               first_chunks);
          write_file agents_path updated;
          let second_response, second_chunks = run_turn mgr "second" in
          Alcotest.(check string) "second response" "ok" second_response;
          Alcotest.(check bool)
            "second turn streams refresh notice" true
            (List.exists
               (function
                 | Provider.Delta text ->
                     Test_helpers.string_contains text
                       "workspace context refreshed after active workspace \
                        file update: AGENTS.md"
                 | _ -> false)
               second_chunks);
          Alcotest.(check bool)
            "second turn streams assistant delta" true
            (List.exists
               (function Provider.Delta "ok" -> true | _ -> false)
               second_chunks);
          let prompts = List.rev !seen_system_prompts in
          Alcotest.(check int) "two prompts observed" 2 (List.length prompts);
          Alcotest.(check bool)
            "first prompt used original workspace content" true
            (Test_helpers.string_contains (List.nth prompts 0) original);
          Alcotest.(check bool)
            "second prompt used updated workspace content" true
            (Test_helpers.string_contains (List.nth prompts 1) updated);
          Alcotest.(check bool)
            "visible refresh notice does not leak updated contents" false
            (List.exists
               (function
                 | Provider.Delta text ->
                     Test_helpers.string_contains text updated
                 | _ -> false)
               second_chunks);
          let persisted =
            Memory.load_history ~db ~session_key:"web:s-external"
          in
          Alcotest.(check bool)
            "external edit refresh event persisted" true
            (List.exists
               (fun (msg : Provider.message) ->
                 msg.role = "event"
                 && Test_helpers.string_contains msg.content
                      "workspace context refreshed after active workspace file \
                       update: AGENTS.md")
               persisted)))

let test_restored_session_detects_external_workspace_edit_on_next_turn () =
  with_temp_workspace (fun workspace ->
      let agents_path = Filename.concat workspace "AGENTS.md" in
      let write_file path content =
        let oc = open_out path in
        Fun.protect
          (fun () -> output_string oc content)
          ~finally:(fun () -> close_out_noerr oc)
      in
      let original = "RESTORE ORIGINAL AGENTS CONTENT" in
      let updated = "RESTORE UPDATED SECRET AGENTS CONTENT" in
      write_file agents_path original;
      let db = Memory.init ~db_path:":memory:" () in
      let seen_system_prompts = ref [] in
      with_fake_openai_provider
        ~handle_request:(fun ~stream:_ ~messages ~json:_ ->
          let open Yojson.Safe.Util in
          let system_prompt =
            List.find_map
              (fun msg ->
                try
                  if msg |> member "role" |> to_string = "system" then
                    Some (msg |> member "content" |> to_string)
                  else None
                with _ -> None)
              messages
            |> Option.value ~default:""
          in
          seen_system_prompts := system_prompt :: !seen_system_prompts;
          Fake_text "ok")
        (fun base_config ->
          let prompt =
            {
              base_config.prompt with
              dynamic_enabled = true;
              include_tools_section = false;
              include_safety_section = false;
              include_runtime_section = false;
              include_datetime_section = false;
              include_autonomy_section = false;
              workspace_files = [ "AGENTS.md" ];
            }
          in
          let config = { base_config with workspace; prompt } in
          let run_turn manager message =
            let chunks = ref [] in
            let push_chunk chunk =
              chunks := chunk :: !chunks;
              Lwt.return_unit
            in
            let push_text text =
              chunks := Provider.Delta text :: !chunks;
              Lwt.return_unit
            in
            let response =
              Lwt_main.run
                (Session.with_registered_notifier manager ~key:"web:s-restore"
                   ~notify:push_text (fun () ->
                     Session.turn_stream manager ~key:"web:s-restore" ~message
                       ~on_chunk:push_chunk ()))
            in
            (response, List.rev !chunks)
          in
          let first_mgr = Session.create ~config ~db () in
          let first_response, first_chunks = run_turn first_mgr "first" in
          Alcotest.(check string) "first response" "ok" first_response;
          Alcotest.(check bool)
            "first turn streams assistant delta" true
            (List.exists
               (function Provider.Delta "ok" -> true | _ -> false)
               first_chunks);
          write_file agents_path updated;
          let restored_mgr = Session.create ~config ~db () in
          let second_response, second_chunks = run_turn restored_mgr "second" in
          Alcotest.(check string) "second response" "ok" second_response;
          Alcotest.(check bool)
            "restored turn streams refresh notice" true
            (List.exists
               (function
                 | Provider.Delta text ->
                     Test_helpers.string_contains text
                       "workspace context refreshed after active workspace \
                        file update: AGENTS.md"
                 | _ -> false)
               second_chunks);
          Alcotest.(check bool)
            "restored turn streams assistant delta" true
            (List.exists
               (function Provider.Delta "ok" -> true | _ -> false)
               second_chunks);
          let prompts = List.rev !seen_system_prompts in
          Alcotest.(check int)
            "two prompts observed across restore" 2 (List.length prompts);
          Alcotest.(check bool)
            "first prompt used original workspace content" true
            (Test_helpers.string_contains (List.nth prompts 0) original);
          Alcotest.(check bool)
            "restored prompt used updated workspace content" true
            (Test_helpers.string_contains (List.nth prompts 1) updated);
          Alcotest.(check bool)
            "restored refresh notice does not leak updated contents" false
            (List.exists
               (function
                 | Provider.Delta text ->
                     Test_helpers.string_contains text updated
                 | _ -> false)
               second_chunks);
          let persisted =
            Memory.load_history ~db ~session_key:"web:s-restore"
          in
          let refresh_events =
            List.filter
              (fun (msg : Provider.message) ->
                msg.role = "event"
                && Test_helpers.string_contains msg.content
                     "workspace context refreshed after active workspace file \
                      update: AGENTS.md")
              persisted
          in
          Alcotest.(check int)
            "restored external edit refresh event persisted once" 1
            (List.length refresh_events)))

let test_external_edit_detected_via_note_function () =
  with_temp_workspace (fun workspace ->
      let agents_path = Filename.concat workspace "AGENTS.md" in
      let write_file path content =
        let oc = open_out path in
        Fun.protect
          (fun () -> output_string oc content)
          ~finally:(fun () -> close_out_noerr oc)
      in
      write_file agents_path "original content";
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let agent = Agent.create ~config () in
      let first = Agent.note_external_workspace_refresh_if_needed agent in
      Alcotest.(check bool)
        "no change on first call (baseline already captured at create)" true
        (Option.is_none first);
      write_file agents_path "externally modified content";
      let second = Agent.note_external_workspace_refresh_if_needed agent in
      Alcotest.(check bool)
        "external edit detected" true (Option.is_some second);
      let event_msg = Option.get second in
      Alcotest.(check string) "event role" "event" event_msg.Provider.role;
      Alcotest.(check bool)
        "event mentions AGENTS.md" true
        (Test_helpers.string_contains event_msg.content "AGENTS.md");
      Alcotest.(check bool)
        "event in agent history" true
        (List.exists
           (fun (msg : Provider.message) ->
             msg.role = "event"
             && Test_helpers.string_contains msg.content "AGENTS.md")
           agent.history))

let test_external_edit_event_filtered_from_build_messages () =
  with_temp_workspace (fun workspace ->
      let agents_path = Filename.concat workspace "AGENTS.md" in
      let write_file path content =
        let oc = open_out path in
        Fun.protect
          (fun () -> output_string oc content)
          ~finally:(fun () -> close_out_noerr oc)
      in
      write_file agents_path "original";
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let agent = Agent.create ~config () in
      write_file agents_path "externally updated";
      let _ = Agent.note_external_workspace_refresh_if_needed agent in
      let msgs = Agent.build_messages agent in
      Alcotest.(check bool)
        "no event role in build_messages output" false
        (List.exists (fun (msg : Provider.message) -> msg.role = "event") msgs))

let test_no_false_positive_when_files_unchanged () =
  with_temp_workspace (fun workspace ->
      let agents_path = Filename.concat workspace "AGENTS.md" in
      let write_file path content =
        let oc = open_out path in
        Fun.protect
          (fun () -> output_string oc content)
          ~finally:(fun () -> close_out_noerr oc)
      in
      write_file agents_path "stable content";
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let agent = Agent.create ~config () in
      let first = Agent.note_external_workspace_refresh_if_needed agent in
      Alcotest.(check bool)
        "no change on first call" true (Option.is_none first);
      let second = Agent.note_external_workspace_refresh_if_needed agent in
      Alcotest.(check bool)
        "no change on second call" true (Option.is_none second);
      Alcotest.(check bool)
        "no event messages in history" false
        (List.exists
           (fun (msg : Provider.message) -> msg.role = "event")
           agent.history))

let test_external_file_deletion_detected () =
  with_temp_workspace (fun workspace ->
      let agents_path = Filename.concat workspace "AGENTS.md" in
      let write_file path content =
        let oc = open_out path in
        Fun.protect
          (fun () -> output_string oc content)
          ~finally:(fun () -> close_out_noerr oc)
      in
      write_file agents_path "content that will be deleted";
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let agent = Agent.create ~config () in
      let first = Agent.note_external_workspace_refresh_if_needed agent in
      Alcotest.(check bool) "no change initially" true (Option.is_none first);
      Sys.remove agents_path;
      let second = Agent.note_external_workspace_refresh_if_needed agent in
      Alcotest.(check bool) "deletion detected" true (Option.is_some second);
      let event_msg = Option.get second in
      Alcotest.(check bool)
        "deletion event mentions AGENTS.md" true
        (Test_helpers.string_contains event_msg.content "AGENTS.md"))

let test_tool_write_then_external_edit_no_double_report () =
  with_temp_workspace (fun workspace ->
      let agents_path = Filename.concat workspace "AGENTS.md" in
      let write_file path content =
        let oc = open_out path in
        Fun.protect
          (fun () -> output_string oc content)
          ~finally:(fun () -> close_out_noerr oc)
      in
      write_file agents_path "original";
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let registry = Tool_registry.create () in
      Tool_registry.register registry
        (Tools_builtin.doc_write ~workspace ~workspace_files:[ "AGENTS.md" ]);
      let agent = Agent.create ~config ~tool_registry:registry () in
      let call =
        {
          Provider.id = "tc-doc-write-tool";
          function_name = "doc_write";
          arguments =
            Yojson.Safe.to_string
              (`Assoc
                 [
                   ("filename", `String "AGENTS.md");
                   ("content", `String "tool-written content");
                 ]);
        }
      in
      Lwt_main.run
        (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
           ~session_key:(Some "web:s1") [ call ]);
      let after_tool = Agent.note_external_workspace_refresh_if_needed agent in
      Alcotest.(check bool)
        "no external change detected right after tool write" true
        (Option.is_none after_tool);
      write_file agents_path "externally modified after tool write";
      let after_external =
        Agent.note_external_workspace_refresh_if_needed agent
      in
      Alcotest.(check bool)
        "external edit after tool write detected" true
        (Option.is_some after_external);
      let event_count =
        List.length
          (List.filter
             (fun (msg : Provider.message) -> msg.role = "event")
             agent.history)
      in
      Alcotest.(check int)
        "exactly one workspace refresh event from tool write + one from \
         external edit"
        2 event_count)

let test_autonomous_continuation_stays_idle_disarms () =
  let continuation_calls = ref 0 in
  let response_for_user message =
    if String.starts_with ~prefix:Session.autonomous_continuation_prompt message
    then (
      incr continuation_calls;
      "STAY_IDLE")
    else "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      let mgr = Session.create ~config () in
      let key = "__main__" in
      Lwt_main.run
        (Session.schedule_autonomous_continuation ~delay:0.02 mgr ~key);
      let state = Hashtbl.find mgr.Session.continuation_checks key in
      Alcotest.(check int) "continuation prompt sent once" 1 !continuation_calls;
      Alcotest.(check bool) "disarmed after STAY_IDLE" true state.disarmed)

let test_autonomous_continuation_resets_after_work () =
  let continuation_calls = ref 0 in
  let response_for_user message =
    if String.starts_with ~prefix:Session.autonomous_continuation_prompt message
    then (
      incr continuation_calls;
      if !continuation_calls < 2 then "keep_working" else "STAY_IDLE")
    else "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      let mgr = Session.create ~config () in
      let key = "__main__" in
      Lwt_main.run
        (Session.schedule_autonomous_continuation ~delay:0.02 mgr ~key);
      let state = Hashtbl.find mgr.Session.continuation_checks key in
      Alcotest.(check int)
        "two continuation prompts before idling" 2 !continuation_calls;
      Alcotest.(check bool)
        "disarmed after second STAY_IDLE" true state.disarmed)

let test_autonomous_continuation_delivers_via_on_response () =
  let continuation_calls = ref 0 in
  let delivered = ref [] in
  let response_for_user message =
    if String.starts_with ~prefix:Session.autonomous_continuation_prompt message
    then (
      incr continuation_calls;
      if !continuation_calls < 2 then "I'm still working on things"
      else "STAY_IDLE")
    else "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      let mgr = Session.create ~config () in
      let key = "telegram:42:7" in
      let on_response text =
        delivered := text :: !delivered;
        Lwt.return_unit
      in
      Lwt_main.run
        (Session.schedule_autonomous_continuation ~delay:0.02 ~on_response mgr
           ~key);
      Alcotest.(check int)
        "two continuation prompts before idling" 2 !continuation_calls;
      Alcotest.(check int)
        "on_response called once (not for STAY_IDLE)" 1 (List.length !delivered);
      Alcotest.(check string)
        "delivered response content" "I'm still working on things"
        (List.hd !delivered))

let test_autonomous_continuation_rearms_after_side_turn () =
  let continuation_calls = ref 0 in
  let response_for_user message =
    if String.starts_with ~prefix:Session.autonomous_continuation_prompt message
    then (
      incr continuation_calls;
      "STAY_IDLE")
    else "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      let mgr = Session.create ~config () in
      let key = "telegram:42:side" in
      let state = Session.continuation_state mgr ~key in
      state.Session.disarmed <- true;
      Lwt_main.run
        (let open Lwt.Syntax in
         let* _ =
           Session.turn mgr ~key ~message:"brief aside about a new bug" ()
         in
         Session.schedule_autonomous_continuation ~delay:0.02 mgr ~key);
      let state = Session.continuation_state mgr ~key in
      Alcotest.(check int)
        "continuation prompt reran after side turn" 1 !continuation_calls;
      Alcotest.(check bool)
        "disarmed again after stay idle response" true state.disarmed)

let test_autonomous_continuation_is_cancellable_by_new_turn () =
  let continuation_calls = ref 0 in
  let response_for_user message =
    if String.starts_with ~prefix:Session.autonomous_continuation_prompt message
    then (
      incr continuation_calls;
      "keep_working")
    else "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      let mgr = Session.create ~config () in
      let key = "__main__" in
      Lwt_main.run
        (let open Lwt.Syntax in
         let continuation_p =
           Session.schedule_autonomous_continuation ~delay:0.2 mgr ~key
         in
         let* () = Lwt_unix.sleep 0.05 in
         let* _ = Session.turn mgr ~key ~message:"manual nudge" () in
         continuation_p);
      Alcotest.(check int)
        "continuation prompt suppressed" 0 !continuation_calls)

let test_autonomous_continuation_is_session_scoped () =
  let continuation_a = ref 0 in
  let continuation_b = ref 0 in
  let response_for_user message =
    if String.starts_with ~prefix:Session.autonomous_continuation_prompt message
    then
      if String.contains message 'A' then (
        incr continuation_a;
        "STAY_IDLE")
      else (
        incr continuation_b;
        "keep_working")
    else "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      let mgr = Session.create ~config () in
      let key_a = "telegram:1:A" in
      let key_b = "telegram:2:B" in
      Lwt_main.run
        (let open Lwt.Syntax in
         let a =
           Session.schedule_autonomous_continuation ~delay:0.02 mgr ~key:key_a
         in
         let b =
           Session.schedule_autonomous_continuation ~delay:0.02 mgr ~key:key_b
         in
         let* () = a in
         let* () = Session.cancel_autonomous_continuation mgr ~key:key_b in
         b);
      let state_a = Hashtbl.find mgr.Session.continuation_checks key_a in
      let state_b = Hashtbl.find mgr.Session.continuation_checks key_b in
      Alcotest.(check int) "session A continuation fired once" 1 !continuation_a;
      Alcotest.(check int)
        "session B continuation fired once before cancel" 1 !continuation_b;
      Alcotest.(check bool)
        "session A disarmed independently" true state_a.disarmed;
      Alcotest.(check bool) "session B remains armed" false state_b.disarmed)

let test_autonomous_continuation_sends_visible_injection () =
  let continuation_calls = ref 0 in
  let notified = ref [] in
  let response_for_user message =
    if String.starts_with ~prefix:Session.autonomous_continuation_prompt message
    then (
      incr continuation_calls;
      "STAY_IDLE")
    else "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      let config =
        {
          config with
          agent_defaults =
            { config.agent_defaults with send_continuation_checkin = true };
        }
      in
      let mgr = Session.create ~config () in
      let key = "telegram:42:7" in
      Session.register_channel_notifier mgr ~key (fun text ->
          notified := text :: !notified;
          Lwt.return_unit);
      Lwt_main.run
        (Session.schedule_autonomous_continuation ~delay:0.02 mgr ~key);
      Alcotest.(check int) "continuation prompt sent once" 1 !continuation_calls;
      let labeled =
        List.find_opt
          (fun text ->
            String.starts_with ~prefix:"[automatic continuation check-in]" text)
          !notified
      in
      Alcotest.(check bool)
        "labeled injection present in notifier output" true
        (Option.is_some labeled);
      match labeled with
      | Some msg ->
          Alcotest.(check bool)
            "injection contains continuation prompt" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string Session.autonomous_continuation_prompt)
                    msg 0);
               true
             with Not_found -> false)
      | None -> ())

let test_autonomous_continuation_suppresses_checkin_by_default () =
  let continuation_calls = ref 0 in
  let notified = ref [] in
  let response_for_user message =
    if String.starts_with ~prefix:Session.autonomous_continuation_prompt message
    then (
      incr continuation_calls;
      "STAY_IDLE")
    else "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      (* default send_continuation_checkin = false *)
      let mgr = Session.create ~config () in
      let key = "telegram:42:8" in
      Session.register_channel_notifier mgr ~key (fun text ->
          notified := text :: !notified;
          Lwt.return_unit);
      Lwt_main.run
        (Session.schedule_autonomous_continuation ~delay:0.02 mgr ~key);
      Alcotest.(check int) "continuation prompt sent once" 1 !continuation_calls;
      let labeled =
        List.find_opt
          (fun text ->
            String.starts_with ~prefix:"[automatic continuation check-in]" text)
          !notified
      in
      Alcotest.(check bool)
        "no labeled injection when send_continuation_checkin=false" true
        (Option.is_none labeled))

let test_autonomous_continuation_suppresses_all_output_when_checkin_disabled ()
    =
  let continuation_calls = ref 0 in
  let notified = ref [] in
  let response_for_user message =
    if String.starts_with ~prefix:Session.autonomous_continuation_prompt message
    then (
      incr continuation_calls;
      "STAY_IDLE")
    else "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      (* send_continuation_checkin = false (default) *)
      let mgr = Session.create ~config () in
      let key = "telegram:42:9" in
      Session.register_channel_notifier mgr ~key (fun text ->
          notified := text :: !notified;
          Lwt.return_unit);
      Lwt_main.run
        (Session.schedule_autonomous_continuation ~delay:0.02 mgr ~key);
      Alcotest.(check int) "continuation prompt sent once" 1 !continuation_calls;
      Alcotest.(check (list string))
        "no notifications at all when checkin disabled" [] !notified;
      (* Verify notifier was restored after the turn *)
      Alcotest.(check bool)
        "notifier restored after suppressed turn" true
        (Option.is_some (Session.find_registered_notifier mgr ~key)))

let test_autonomous_continuation_preserves_history_when_checkin_disabled () =
  let continuation_calls = ref 0 in
  let response_for_user message =
    if String.starts_with ~prefix:Session.autonomous_continuation_prompt message
    then (
      incr continuation_calls;
      "STAY_IDLE")
    else "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let key = "telegram:42:10" in
      Session.register_channel_notifier mgr ~key (fun _text -> Lwt.return_unit);
      Lwt_main.run
        (Session.schedule_autonomous_continuation ~delay:0.02 mgr ~key);
      Alcotest.(check int) "continuation prompt sent once" 1 !continuation_calls;
      (* Verify the continuation prompt and STAY_IDLE response are in the
         persisted message history *)
      let messages = Memory.load_raw_history ~db ~session_key:key in
      let user_msgs =
        List.filter (fun (m : Memory.raw_message) -> m.role = "user") messages
      in
      let assistant_msgs =
        List.filter
          (fun (m : Memory.raw_message) -> m.role = "assistant")
          messages
      in
      Alcotest.(check bool)
        "continuation prompt persisted as user message" true
        (List.exists
           (fun (m : Memory.raw_message) ->
             String.starts_with ~prefix:Session.autonomous_continuation_prompt
               m.content)
           user_msgs);
      Alcotest.(check bool)
        "STAY_IDLE response persisted as assistant message" true
        (List.exists
           (fun (m : Memory.raw_message) ->
             String.trim m.content = Session.autonomous_stay_idle_message)
           assistant_msgs))

let test_autonomous_continuation_disabled_by_config () =
  let continuation_calls = ref 0 in
  let response_for_user message =
    if String.starts_with ~prefix:Session.autonomous_continuation_prompt message
    then (
      incr continuation_calls;
      "STAY_IDLE")
    else "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      let config =
        {
          config with
          agent_defaults =
            {
              config.agent_defaults with
              autonomous_continuation_enabled = false;
            };
        }
      in
      let mgr = Session.create ~config () in
      let key = "__main__" in
      Lwt_main.run
        (Session.schedule_autonomous_continuation ~delay:0.02 mgr ~key);
      Alcotest.(check int)
        "no continuation prompt when disabled" 0 !continuation_calls;
      Alcotest.(check bool)
        "no continuation state created"
        (not (Hashtbl.mem mgr.Session.continuation_checks key))
        true)

let test_autonomous_continuation_uses_config_delay () =
  let continuation_calls = ref 0 in
  let response_for_user message =
    if String.starts_with ~prefix:Session.autonomous_continuation_prompt message
    then (
      incr continuation_calls;
      "STAY_IDLE")
    else "reply:" ^ message
  in
  with_fake_chat_provider ~response_for_user (fun config ->
      let config =
        {
          config with
          agent_defaults =
            { config.agent_defaults with autonomous_continuation_delay = 0.02 };
        }
      in
      let mgr = Session.create ~config () in
      let key = "telegram:42:delay_cfg" in
      Lwt_main.run (Session.schedule_autonomous_continuation mgr ~key);
      Alcotest.(check int)
        "continuation prompt fired using config delay" 1 !continuation_calls)

let test_drain_queued_messages_drains_all_pending_without_relock () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  let notified = ref [] in
  let key = "telegram:1:1" in
  Session.register_channel_notifier session_manager ~key (fun text ->
      notified := text :: !notified;
      Lwt.return_unit);
  let agent = Agent.create ~config () in
  let interrupt = ref None in
  let mkq msg =
    {
      Session.message = msg;
      content_parts = [];
      attachments = [];
      channel_name = None;
      channel_type = None;
      sender_id = None;
      sender_name = None;
      user_group = None;
      channel = Some "telegram";
      channel_id = Some "1";
      message_id = None;
      inbound_queue_id = None;
      bang = false;
      deferred_followup = false;
      snapshot_work_type = None;
      has_external_users = false;
    }
  in
  ignore
    (Session.enqueue_message_if_busy session_manager ~key (mkq "one")
    |> Lwt_main.run);
  ignore
    (Session.enqueue_message_if_busy session_manager ~key (mkq "two")
    |> Lwt_main.run);
  ignore
    (Session.enqueue_message_if_busy session_manager ~key (mkq "three")
    |> Lwt_main.run);
  Lwt_main.run
    (Session.drain_queued_messages session_manager ~key agent interrupt ());
  Alcotest.(check int) "all queued messages notified" 3 (List.length !notified)

let test_drain_progress_callbacks_fire_for_queued_messages () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let sent = ref [] in
      let before_count = ref 0 in
      let after_all_count = ref 0 in
      let after_turn_count = ref 0 in
      let on_drain_progress : Session.drain_progress =
        {
          before_turn =
            (fun _msg_id ->
              incr before_count;
              Lwt.return_unit);
          after_turn =
            (fun _msg_id ->
              incr after_turn_count;
              Lwt.return_unit);
          after_all =
            (fun () ->
              incr after_all_count;
              Lwt.return_unit);
        }
      in
      Lwt_main.run
        (Session.with_registered_notifier mgr ~key:"telegram:1:u"
           ~notify:(fun text ->
             sent := text :: !sent;
             Lwt.return_unit)
           (fun () ->
             Session.with_session_lock mgr ~key:"telegram:1:u"
               (fun agent interrupt ->
                 let open Lwt.Syntax in
                 let* q1 =
                   Session.enqueue_message_if_busy mgr ~key:"telegram:1:u"
                     (queued_message ~channel_name:"telegram" ~channel_type:"dm"
                        ~channel:"telegram" ~channel_id:"1" "msg one")
                 in
                 let* q2 =
                   Session.enqueue_message_if_busy mgr ~key:"telegram:1:u"
                     (queued_message ~channel_name:"telegram" ~channel_type:"dm"
                        ~channel:"telegram" ~channel_id:"1" "msg two")
                 in
                 Alcotest.(check bool) "q1 enqueued" true q1;
                 Alcotest.(check bool) "q2 enqueued" true q2;
                 Session.drain_queued_messages mgr ~key:"telegram:1:u" agent
                   interrupt ~on_drain_progress ())));
      Alcotest.(check int) "before_turn called per queued msg" 2 !before_count;
      Alcotest.(check int)
        "after_turn called per queued msg" 2 !after_turn_count;
      Alcotest.(check int) "after_all called once" 1 !after_all_count;
      Alcotest.(check int) "all messages notified" 2 (List.length !sent))

let test_drain_progress_not_called_when_no_queued_messages () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let before_count = ref 0 in
      let after_turn_count = ref 0 in
      let after_all_count = ref 0 in
      let on_drain_progress : Session.drain_progress =
        {
          before_turn =
            (fun _msg_id ->
              incr before_count;
              Lwt.return_unit);
          after_turn =
            (fun _msg_id ->
              incr after_turn_count;
              Lwt.return_unit);
          after_all =
            (fun () ->
              incr after_all_count;
              Lwt.return_unit);
        }
      in
      Lwt_main.run
        (Session.with_registered_notifier mgr ~key:"telegram:1:u"
           ~notify:(fun _text -> Lwt.return_unit)
           (fun () ->
             Session.with_session_lock mgr ~key:"telegram:1:u"
               (fun agent interrupt ->
                 Session.drain_queued_messages mgr ~key:"telegram:1:u" agent
                   interrupt ~on_drain_progress ())));
      Alcotest.(check int) "before_turn not called" 0 !before_count;
      Alcotest.(check int) "after_turn not called" 0 !after_turn_count;
      Alcotest.(check int) "after_all not called" 0 !after_all_count)

let test_before_drain_fires_before_drain_notifier () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let order = ref [] in
      Lwt_main.run
        (Session.with_registered_notifier mgr ~key:"telegram:1:u"
           ~notify:(fun text ->
             order := ("drain:" ^ text) :: !order;
             Lwt.return_unit)
           (fun () ->
             Session.with_session_lock mgr ~key:"telegram:1:u"
               (fun agent interrupt ->
                 let open Lwt.Syntax in
                 let* queued =
                   Session.enqueue_message_if_busy mgr ~key:"telegram:1:u"
                     (queued_message ~channel_name:"telegram" ~channel_type:"dm"
                        ~channel:"telegram" ~channel_id:"1" "queued msg")
                 in
                 Alcotest.(check bool) "enqueued" true queued;
                 let before_drain response =
                   order := ("before_drain:" ^ response) :: !order;
                   Lwt.return_unit
                 in
                 let* response = Agent.turn agent ~user_message:"hello" () in
                 let* () = before_drain response in
                 let* () =
                   Session.drain_queued_messages mgr ~key:"telegram:1:u" agent
                     interrupt ()
                 in
                 Lwt.return_unit)));
      let order_list = List.rev !order in
      Alcotest.(check int) "two events recorded" 2 (List.length order_list);
      Alcotest.(check bool)
        "before_drain fires first" true
        (match order_list with
        | [ a; b ] ->
            String.starts_with ~prefix:"before_drain:" a
            && String.starts_with ~prefix:"drain:" b
        | _ -> false))

let test_session_restore_no_false_positive_workspace_refresh () =
  with_temp_workspace (fun workspace ->
      let db = Memory.init ~db_path:":memory:" () in
      let ws_file = Filename.concat workspace "AGENTS.md" in
      let oc = open_out ws_file in
      output_string oc "original content";
      close_out oc;
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      Memory.store_message ~db ~session_key:"web:s1"
        (Provider.make_message ~role:"user" ~content:"hello");
      Memory.store_message ~db ~session_key:"web:s1"
        (Provider.make_message ~role:"assistant" ~content:"hi back");
      let mgr = Session.create ~config ~db () in
      Lwt_main.run
        (Session.with_session_lock mgr ~key:"web:s1" (fun agent _interrupt ->
             Alcotest.(check int)
               "history restored" 2
               (List.length agent.Agent.history);
             let has_event =
               List.exists
                 (fun (msg : Provider.message) -> msg.role = "event")
                 agent.Agent.history
             in
             Alcotest.(check bool)
               "no false positive event after restore" false has_event;
             Lwt.return_unit)))

let test_session_restore_detects_external_edit_on_next_turn () =
  with_temp_workspace (fun workspace ->
      let db = Memory.init ~db_path:":memory:" () in
      let ws_file = Filename.concat workspace "AGENTS.md" in
      let oc = open_out ws_file in
      output_string oc "original content";
      close_out oc;
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      Memory.store_message ~db ~session_key:"web:s1"
        (Provider.make_message ~role:"user" ~content:"hello");
      let mgr = Session.create ~config ~db () in
      Lwt_main.run
        (Session.with_session_lock mgr ~key:"web:s1" (fun agent _interrupt ->
             let oc = open_out ws_file in
             output_string oc "externally changed";
             close_out oc;
             let open Lwt.Syntax in
             let* _compacted =
               Agent.prepare_turn_history agent ~user_message:"check" ()
             in
             let has_event =
               List.exists
                 (fun (msg : Provider.message) ->
                   msg.role = "event"
                   && Test_helpers.string_contains msg.content "AGENTS.md")
                 agent.Agent.history
             in
             Alcotest.(check bool)
               "external edit detected on next turn" true has_event;
             Lwt.return_unit)))

let test_fresh_session_no_false_positive_on_first_turn () =
  with_temp_workspace (fun workspace ->
      let ws_file = Filename.concat workspace "AGENTS.md" in
      let oc = open_out ws_file in
      output_string oc "initial content";
      close_out oc;
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let agent = Agent.create ~config () in
      Lwt_main.run
        (let open Lwt.Syntax in
         let* _compacted =
           Agent.prepare_turn_history agent ~user_message:"hello" ()
         in
         let has_event =
           List.exists
             (fun (msg : Provider.message) -> msg.role = "event")
             agent.Agent.history
         in
         Alcotest.(check bool) "no false positive on first turn" false has_event;
         Lwt.return_unit))

let test_workspace_change_not_re_reported_on_subsequent_turn () =
  with_temp_workspace (fun workspace ->
      let ws_file = Filename.concat workspace "AGENTS.md" in
      let oc = open_out ws_file in
      output_string oc "original content";
      close_out oc;
      let prompt =
        { Runtime_config.default.prompt with workspace_files = [ "AGENTS.md" ] }
      in
      let config = { Runtime_config.default with workspace; prompt } in
      let agent = Agent.create ~config () in
      let oc = open_out ws_file in
      output_string oc "changed content";
      close_out oc;
      Lwt_main.run
        (let open Lwt.Syntax in
         let* _compacted =
           Agent.prepare_turn_history agent ~user_message:"first" ()
         in
         let event_count_after_first =
           List.length
             (List.filter
                (fun (msg : Provider.message) -> msg.role = "event")
                agent.Agent.history)
         in
         Alcotest.(check int)
           "one event after first turn" 1 event_count_after_first;
         let* _compacted =
           Agent.prepare_turn_history agent ~user_message:"second" ()
         in
         let event_count_after_second =
           List.length
             (List.filter
                (fun (msg : Provider.message) -> msg.role = "event")
                agent.Agent.history)
         in
         Alcotest.(check int)
           "still one event after second turn (not re-reported)" 1
           event_count_after_second;
         Lwt.return_unit))

let test_compact_loads_session_from_db_when_not_in_memory () =
  (* Simulate daemon restart: session has history in DB but the in-memory
     sessions hashtable is empty. Session.compact should lazily load the
     session from DB via get_or_create_locked rather than returning
     "Session not found". *)
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  (* Store enough messages in DB to exceed compaction_keep_recent (20) *)
  for i = 1 to 25 do
    Memory.store_message ~db ~session_key:"telegram:42:user1"
      (Provider.make_message ~role:"user"
         ~content:(Printf.sprintf "msg %02d" i))
  done;
  (* Create a fresh manager (simulating daemon restart — sessions hashtable
     is empty even though DB has history) *)
  let mgr = Session.create ~config ~db () in
  Alcotest.(check bool)
    "session not in memory before compact" false
    (Hashtbl.mem mgr.sessions "telegram:42:user1");
  let result = Lwt_main.run (Session.compact mgr ~key:"telegram:42:user1" ()) in
  (* Before the fix, this returned Error "Session not found".
     Now it should load the session from DB and return Ok _. *)
  match result with
  | Ok _ ->
      Alcotest.(check bool)
        "session loaded into memory after compact" true
        (Hashtbl.mem mgr.sessions "telegram:42:user1")
  | Error msg ->
      Alcotest.fail (Printf.sprintf "compact should not fail: %s" msg)

let seed_compactable_history ~db ~session_key =
  for i = 1 to 25 do
    Memory.store_message ~db ~session_key
      (Provider.make_message ~role:"user"
         ~content:(Printf.sprintf "msg %02d" i))
  done

let test_compact_profiled_room_skips_pre_compaction_flush () =
  with_tool_capture_native_provider (fun config tool_calls ->
      let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
      let profile_id =
        Memory.insert_room_profile ~db ~name:"channel-a-profile"
      in
      Memory.upsert_room_profile_binding ~db ~room_id:"C_A" ~profile_id;
      let key = "slack:C_A" in
      seed_compactable_history ~db ~session_key:key;
      let mgr = Session.create ~config ~db () in
      let result = Lwt_main.run (Session.compact mgr ~key ()) in
      (match result with
      | Ok true -> ()
      | Ok false -> Alcotest.fail "compact should compact seeded history"
      | Error msg ->
          Alcotest.fail (Printf.sprintf "compact should not fail: %s" msg));
      Alcotest.(check int)
        "pre-compaction memory flush tool calls" 0 !tool_calls)

let test_compact_releases_session_lock_while_summarizing () =
  with_delayed_chat_provider ~delay_s:0.05 (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let key = "telegram:42:user1" in
      seed_compactable_history ~db ~session_key:key;
      let acquired_during_compact =
        Lwt_main.run
          (let open Lwt.Syntax in
           let compact_p = Session.compact mgr ~key () in
           let* () = Lwt_unix.sleep 0.01 in
           let* acquired =
             Lwt.pick
               [
                 (let* () =
                    Session.with_session_lock mgr ~key (fun _agent _interrupt ->
                        Lwt.return_unit)
                  in
                  Lwt.return_true);
                 (let* () = Lwt_unix.sleep 0.02 in
                  Lwt.return_false);
               ]
           in
           let* result = compact_p in
           match result with
           | Ok true -> Lwt.return acquired
           | Ok false -> Alcotest.fail "expected compaction to run"
           | Error msg ->
               Alcotest.fail (Printf.sprintf "compact should not fail: %s" msg))
      in
      Alcotest.(check bool)
        "session lock can be re-acquired during compact execution" true
        acquired_during_compact)

let test_compact_preserves_new_messages_arriving_midflight () =
  with_delayed_chat_provider ~delay_s:0.05 (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let key = "telegram:42:user1" in
      let injected = "arrived during compact" in
      seed_compactable_history ~db ~session_key:key;
      Lwt_main.run
        (let open Lwt.Syntax in
         let compact_p = Session.compact mgr ~key () in
         let* () = Lwt_unix.sleep 0.01 in
         let* () =
           Session.with_session_lock mgr ~key (fun agent _interrupt ->
               agent.Agent.history <-
                 Provider.make_message ~role:"user" ~content:injected
                 :: agent.Agent.history;
               Lwt.return_unit)
         in
         let* result = compact_p in
         match result with
         | Ok true -> Lwt.return_unit
         | Ok false -> Alcotest.fail "expected compaction to apply"
         | Error msg ->
             Alcotest.fail (Printf.sprintf "compact should not fail: %s" msg));
      Lwt_main.run
        (Session.with_session_lock mgr ~key (fun agent _interrupt ->
             let newest =
               match agent.Agent.history with
               | msg :: _ -> msg.Provider.content
               | [] -> Alcotest.fail "expected compacted history"
             in
             Alcotest.(check string)
               "new message preserved at head of history" injected newest;
             let persisted =
               Memory.load_history ~db ~session_key:key
               |> List.map (fun msg -> msg.Provider.content)
             in
             Alcotest.(check bool)
               "new message also persisted" true
               (List.mem injected persisted);
             Lwt.return_unit)))

let test_compact_skips_apply_after_reset_midflight () =
  with_delayed_chat_provider ~delay_s:0.05 (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let key = "telegram:42:user1" in
      seed_compactable_history ~db ~session_key:key;
      let result =
        Lwt_main.run
          (let open Lwt.Syntax in
           let compact_p = Session.compact mgr ~key () in
           let* () = Lwt_unix.sleep 0.01 in
           let* _active_bg_tasks = Session.reset mgr ~key in
           compact_p)
      in
      (match result with
      | Ok false -> ()
      | Ok true -> Alcotest.fail "expected compact apply to be skipped"
      | Error msg ->
          Alcotest.fail (Printf.sprintf "compact should not fail: %s" msg));
      Alcotest.(check int)
        "reset keeps persisted history cleared" 0
        (List.length (Memory.load_history ~db ~session_key:key)))

let test_turn_stream_forwards_tool_events_to_on_chunk () =
  let call_count = ref 0 in
  let handle_request ~stream ~messages:_ ~json:_ =
    incr call_count;
    if !call_count = 1 then
      (* First call: return tool call *)
      if stream then
        Fake_tool_calls [ ("tc_1", "test_tool", {|{"arg":"val"}|}) ]
      else Fake_tool_calls [ ("tc_1", "test_tool", {|{"arg":"val"}|}) ]
    else Fake_text "done"
  in
  with_fake_openai_provider ~handle_request (fun config ->
      let config =
        {
          config with
          security = { config.security with tools_enabled = true };
          agent_defaults =
            {
              config.agent_defaults with
              show_tool_calls = true;
              max_tool_iterations = 5;
            };
        }
      in
      let registry = Tool_registry.create () in
      let tool : Tool.t =
        {
          name = "test_tool";
          description = "A test tool";
          parameters_schema = `Assoc [];
          invoke = (fun ?context:_ _ -> Lwt.return "tool result ok");
          invoke_stream = None;
          risk_level = Tool.Low;
          deferred = false;
        }
      in
      Tool_registry.register registry tool;
      let mgr = Session.create ~config () in
      (* Pre-create agent with tool registry *)
      let agent = Agent.create ~config ~tool_registry:registry () in
      Hashtbl.replace mgr.sessions "web:test-tc"
        (agent, Lwt_mutex.create (), ref None);
      let chunks = ref [] in
      let _response =
        Lwt_main.run
          (Session.turn_stream mgr ~key:"web:test-tc" ~message:"use the tool"
             ~on_chunk:(fun chunk ->
               chunks := chunk :: !chunks;
               Lwt.return_unit)
             ())
      in
      let tool_starts =
        List.filter
          (function Provider.ToolStart _ -> true | _ -> false)
          (List.rev !chunks)
      in
      let tool_results =
        List.filter
          (function Provider.ToolResult _ -> true | _ -> false)
          (List.rev !chunks)
      in
      Alcotest.(check int)
        "at least one ToolStart event" 1 (List.length tool_starts);
      Alcotest.(check int)
        "at least one ToolResult event" 1 (List.length tool_results);
      (* Verify ToolStart has correct name *)
      (match tool_starts with
      | Provider.ToolStart { name; _ } :: _ ->
          Alcotest.(check string) "ToolStart name" "test_tool" name
      | _ -> Alcotest.fail "expected ToolStart event");
      (* Verify ToolResult has result content *)
      match tool_results with
      | Provider.ToolResult { result; _ } :: _ ->
          Alcotest.(check bool)
            "ToolResult has content" true
            (String.length result > 0)
      | _ -> Alcotest.fail "expected ToolResult event")

let test_non_vision_model_filters_images () =
  let config =
    {
      Runtime_config.default with
      agent_defaults =
        {
          Runtime_config.default.agent_defaults with
          primary_model = "gpt-5.3-codex-spark";
        };
    }
  in
  let content_parts =
    [
      Provider.Image_base64 { data = "img1"; media_type = "image/jpeg" };
      Provider.Text "some text";
      Provider.Image_base64 { data = "img2"; media_type = "image/png" };
    ]
  in
  let filtered = Agent.filter_content_parts_for_model config content_parts in
  Alcotest.(check int) "only text remains" 1 (List.length filtered);
  match filtered with
  | [ Provider.Text txt ] ->
      Alcotest.(check string) "text preserved" "some text" txt
  | _ -> Alcotest.fail "expected only text content"

let test_vision_model_keeps_images () =
  let config =
    {
      Runtime_config.default with
      agent_defaults =
        {
          Runtime_config.default.agent_defaults with
          primary_model = "claude-opus-4-6";
        };
    }
  in
  let content_parts =
    [
      Provider.Image_base64 { data = "img1"; media_type = "image/jpeg" };
      Provider.Text "some text";
      Provider.Image_base64 { data = "img2"; media_type = "image/png" };
    ]
  in
  let filtered = Agent.filter_content_parts_for_model config content_parts in
  Alcotest.(check int) "all parts kept" 3 (List.length filtered)

let test_runtime_context_block_ignores_prompt_toggles () =
  let config =
    {
      Runtime_config.default with
      prompt =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = false;
          include_runtime_section = false;
          include_datetime_section = false;
        };
    }
  in
  let session_manager = Session.create ~config () in
  let output =
    Lwt_main.run (Session.runtime_context_block session_manager ~key:"__main__")
  in
  Alcotest.(check bool)
    "includes runtime header" true
    (Test_helpers.string_contains output "[Runtime context for this turn only]");
  Alcotest.(check bool)
    "includes session id" true
    (Test_helpers.string_contains output "Session id: __main__");
  Alcotest.(check bool)
    "includes main session flag" true
    (Test_helpers.string_contains output "Main session: yes");
  Alcotest.(check bool)
    "includes workspace" true
    (Test_helpers.string_contains output "Effective workspace:");
  (* Git fields only appear when running inside a git repo *)
  (match Prompt_builder.find_git_root_and_dir (Sys.getcwd ()) with
  | Some _ ->
      Alcotest.(check bool)
        "includes git field" true
        (Test_helpers.string_contains output "Git branch:"
        || Test_helpers.string_contains output "Git repo root:")
  | None -> ());
  Alcotest.(check bool)
    "includes timezone" true
    (Test_helpers.string_contains output "Local timezone:");
  Alcotest.(check bool)
    "includes shell policy" true
    (Test_helpers.string_contains output "Shell policy:");
  Alcotest.(check bool)
    "includes daemon uptime" true
    (Test_helpers.string_contains output "Daemon uptime:");
  Alcotest.(check bool)
    "includes context usage" true
    (Test_helpers.string_contains output "Context usage:");
  Alcotest.(check bool)
    "includes background tasks" true
    (Test_helpers.string_contains output "Background tasks:")

let test_enqueue_message_if_busy_persists_to_sqlite () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:99:testuser" in
  Session.register_channel_notifier mgr ~key (fun _ -> Lwt.return_unit);
  Lwt_main.run
    (Session.with_session_lock mgr ~key (fun _agent _interrupt ->
         let open Lwt.Syntax in
         let* queued =
           Session.enqueue_message_if_busy mgr ~key
             (queued_message ~channel_name:"telegram" ~channel:"telegram"
                ~channel_id:"99" "durable test")
         in
         Alcotest.(check bool) "message was queued" true queued;
         Alcotest.(check int)
           "1 row persisted to inbound_queue" 1
           (Memory.queue_count ~db ~session_key:key);
         Lwt.return_unit))

let test_drain_queued_messages_deletes_sqlite_row () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let key = "telegram:99:testuser" in
      Lwt_main.run
        (Session.with_registered_notifier mgr ~key
           ~notify:(fun _ -> Lwt.return_unit)
           (fun () ->
             Session.with_session_lock mgr ~key (fun agent interrupt ->
                 let open Lwt.Syntax in
                 let* queued =
                   Session.enqueue_message_if_busy mgr ~key
                     (queued_message ~channel_name:"telegram"
                        ~channel:"telegram" ~channel_id:"99" "drain me")
                 in
                 Alcotest.(check bool) "message queued" true queued;
                 Alcotest.(check int)
                   "1 row before drain" 1
                   (Memory.queue_count ~db ~session_key:key);
                 let* () =
                   Session.drain_queued_messages mgr ~key agent interrupt ()
                 in
                 Alcotest.(check int)
                   "0 rows after drain" 0
                   (Memory.queue_count ~db ~session_key:key);
                 Lwt.return_unit))))

let test_drain_preserves_messages_without_notifier () =
  with_fake_chat_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let mgr = Session.create ~config ~db () in
      let key = "telegram:99:testuser" in
      Lwt_main.run
        (Session.with_session_lock mgr ~key (fun agent interrupt ->
             let open Lwt.Syntax in
             (* Register notifier so enqueue succeeds *)
             Session.register_channel_notifier mgr ~key (fun _ ->
                 Lwt.return_unit);
             let* queued =
               Session.enqueue_message_if_busy mgr ~key
                 (queued_message ~channel_name:"telegram" ~channel:"telegram"
                    ~channel_id:"99" "preserve me")
             in
             Alcotest.(check bool) "message queued" true queued;
             Alcotest.(check int)
               "1 sqlite row before drain" 1
               (Memory.queue_count ~db ~session_key:key);
             (* Remove notifier to simulate suppressed output *)
             Hashtbl.remove mgr.channel_notifiers key;
             let* () =
               Session.drain_queued_messages mgr ~key agent interrupt ()
             in
             (* Message should be preserved, not dropped *)
             (match Session.take_next_queued_message mgr ~key with
             | None -> Alcotest.fail "expected message to be preserved in queue"
             | Some msg ->
                 Alcotest.(check string)
                   "preserved message content" "preserve me" msg.message);
             (* SQLite row should NOT be deleted *)
             Alcotest.(check int)
               "sqlite row preserved" 1
               (Memory.queue_count ~db ~session_key:key);
             Lwt.return_unit)))

let test_with_session_lock_warns_on_slow_mutex_acquisition () =
  let mgr =
    Session.create ~config:Runtime_config.default
      ~db:(Memory.init ~db_path:":memory:" ())
      ()
  in
  let key = "web:slow-lock" in
  let agent = Agent.create ~config:Runtime_config.default () in
  let mutex = Lwt_mutex.create () in
  let interrupt = ref None in
  Hashtbl.replace mgr.sessions key (agent, mutex, interrupt);
  let seen =
    Lwt_main.run
      (let open Lwt.Syntax in
       let* () = Lwt_mutex.lock mutex in
       let waiter =
         Lwt_preemptive.detach
           (fun () ->
             with_captured_logs (fun () ->
                 Lwt_main.run
                   (Session.with_session_lock ~session_warn_timeout:0.01
                      ~session_fatal_timeout:0.2 mgr ~key (fun _ _ ->
                        Lwt.return_unit))))
           ()
       in
       let* () = Lwt_unix.sleep 0.03 in
       Lwt_mutex.unlock mutex;
       let* (), logs = waiter in
       Lwt.return logs)
  in
  let joined = String.concat "\n" seen in
  Alcotest.(check bool)
    "warns on slow acquisition" true
    (String.contains joined 'M' && String.contains joined 's');
  Alcotest.(check bool)
    "mentions slow retry" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "Mutex acquisition slow")
            joined 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "mentions eventual acquisition" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "Mutex acquired after extended wait")
            joined 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "mentions session label" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "session_mutex/with_session_lock[web:slow-lock]")
            joined 0);
       true
     with Not_found -> false)

let test_history_delta_tokens_includes_tool_results () =
  let large_content = String.make 2400 'x' in
  let assistant =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls =
        [
          {
            Provider.id = "tc-file-write";
            function_name = "file_write";
            arguments =
              Yojson.Safe.to_string
                (`Assoc
                   [
                     ("path", `String "AGENTS.md");
                     ("content", `String large_content);
                   ]);
          };
        ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
      is_error = false;
    }
  in
  let tool_result =
    Provider.make_tool_result ~tool_call_id:"tc-file-write" ~name:"file_write"
      ~content:"Wrote AGENTS.md"
  in
  let refresh =
    Provider.make_message ~role:"event"
      ~content:
        "[workspace context refreshed after active workspace file update: \
         AGENTS.md]"
  in
  let prior_user =
    Provider.make_message ~role:"user" ~content:"update AGENTS"
  in
  let history = [ refresh; tool_result; assistant; prior_user ] in
  let estimated =
    Agent.estimate_history_delta_tokens ~history
      ~previous_request_history_len:(Some 1) ~current_request_history_len:4
  in
  let expected_assistant = Agent.estimate_message_tokens assistant in
  let expected_tool = Agent.estimate_message_tokens tool_result in
  let expected = expected_assistant + expected_tool in
  Alcotest.(check (option int))
    "sums assistant + tool result tokens (excludes event)" (Some expected)
    estimated;
  Alcotest.(check bool)
    "delta token estimate is meaningfully large" true (expected > 100)

let test_history_delta_tokens_tool_only () =
  let tool_result =
    Provider.make_tool_result ~tool_call_id:"tc-read" ~name:"file_read"
      ~content:"file contents here with enough text to be meaningful"
  in
  let prior_user =
    Provider.make_message ~role:"user" ~content:"read the file"
  in
  let history = [ tool_result; prior_user ] in
  let estimated =
    Agent.estimate_history_delta_tokens ~history
      ~previous_request_history_len:(Some 1) ~current_request_history_len:2
  in
  let expected = Agent.estimate_message_tokens tool_result in
  Alcotest.(check (option int))
    "counts tool-only delta" (Some expected) estimated;
  Alcotest.(check bool) "tool result tokens > 0" true (expected > 0)

let suite =
  [
    Alcotest.test_case "reset clears active session and history" `Quick
      test_reset_clears_active_session_and_history;
    Alcotest.test_case "reset waits for session lock" `Quick
      test_reset_waits_for_session_lock;
    Alcotest.test_case "with_session_lock relocks replaced session" `Quick
      test_with_session_lock_relocks_when_session_replaced;
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
    Alcotest.test_case
      "profiled room session turn skips unscoped memory context" `Quick
      test_profiled_room_session_turn_skips_unscoped_memory_context;
    Alcotest.test_case "room profile tool allowlist allows listed tool" `Quick
      test_room_profile_tool_allowlist_allows_listed_tool;
    Alcotest.test_case "room profile tool denylist blocks denied tool" `Quick
      test_room_profile_tool_denylist_blocks_denied_tool;
    Alcotest.test_case "room profile tool allowlist blocks unlisted tool" `Quick
      test_room_profile_tool_allowlist_blocks_unlisted_tool;
    Alcotest.test_case "room profile tool grants block user bash command" `Quick
      test_room_profile_tool_grants_block_user_bash_command;
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
    Alcotest.test_case "live activity tracks nested scopes" `Quick
      test_live_activity_tracks_nested_scopes;
    Alcotest.test_case "turn marks special command phase as live activity"
      `Quick test_turn_marks_special_command_phase_as_live_activity;
    Alcotest.test_case "drain queued messages marks live activity" `Quick
      test_drain_queued_messages_marks_live_activity;
    Alcotest.test_case "clear response deferred removes marker" `Quick
      test_clear_response_deferred_removes_marker;
    Alcotest.test_case
      "postmortem circuit breaker suppresses duplicate launches" `Quick
      test_spawn_postmortem_agent_circuit_breaker_blocks_duplicates;
    Alcotest.test_case
      "postmortem circuit breaker suppresses recursive postmortems" `Quick
      test_spawn_postmortem_agent_circuit_breaker_blocks_recursive_postmortems;
    Alcotest.test_case
      "B609: postmortem disabled short-circuits without side effects" `Quick
      test_spawn_postmortem_agent_disabled_short_circuits_without_side_effects;
    Alcotest.test_case
      "B612: distinct stuck patterns in same root each get a postmortem" `Quick
      test_spawn_postmortem_agent_distinct_patterns_each_launch;
    Alcotest.test_case "postmortem session starts fresh, not restored from DB"
      `Quick test_postmortem_session_starts_fresh_not_restored;
    Alcotest.test_case
      "enqueue message if busy marks interrupt and preserves message" `Quick
      test_enqueue_message_if_busy_marks_interrupt_and_preserves_message;
    Alcotest.test_case
      "mid-turn injection pulls steering before deferred followups" `Quick
      test_midturn_injection_pulls_steering_before_followups;
    Alcotest.test_case "metadata-free user inject counts as steering" `Quick
      test_metadata_free_user_inject_counts_as_steering;
    Alcotest.test_case "append followup updates last queued followup" `Quick
      test_append_followup_updates_last_queued_followup;
    Alcotest.test_case "append followup without existing queues new followup"
      `Quick test_append_followup_without_existing_queues_new_followup;
    Alcotest.test_case "append followup updates sqlite payload" `Quick
      test_append_followup_updates_sqlite_payload;
    Alcotest.test_case "enqueue message if busy queues without channel notifier"
      `Quick test_enqueue_message_if_busy_queues_without_channel_notifier;
    Alcotest.test_case
      "enqueue admin stop if busy does not run interrupt finalizer" `Quick
      test_enqueue_admin_stop_if_busy_does_not_run_interrupt_finalizer;
    Alcotest.test_case
      "enqueue admin stop if busy sets stop interrupt without queue" `Quick
      test_enqueue_admin_stop_if_busy_sets_stop_interrupt_without_queue;
    Alcotest.test_case "enqueue admin stop if busy cancels pending question"
      `Quick test_enqueue_admin_stop_if_busy_cancels_pending_question;
    Alcotest.test_case
      "stop busy session if admin stop cancels pending question" `Quick
      test_stop_busy_session_if_admin_stop_cancels_pending_question;
    Alcotest.test_case
      "stop busy session if admin stop does not run interrupt finalizer" `Quick
      test_stop_busy_session_if_admin_stop_does_not_run_interrupt_finalizer;
    Alcotest.test_case
      "turn admin stop on queueable busy session returns queued sentinel" `Quick
      test_turn_admin_stop_on_queueable_busy_session_returns_queued_sentinel;
    Alcotest.test_case "turn admin bang stop if busy queues follow-up" `Quick
      test_turn_admin_bang_stop_if_busy_queues_followup;
    Alcotest.test_case
      "enqueue admin bang stop with raw message queues follow-up" `Quick
      test_enqueue_admin_bang_stop_raw_message_queues_followup;
    Alcotest.test_case "enqueue guest stop if busy queues normally" `Quick
      test_enqueue_guest_stop_if_busy_queues_normally;
    Alcotest.test_case "queued admin stop is filtered during mid-turn injection"
      `Quick test_take_all_queued_messages_for_injection_handles_admin_stop;
    Alcotest.test_case "queued admin stop halts drain without running turn"
      `Quick test_drain_queued_messages_handles_queued_admin_stop;
    Alcotest.test_case "queued admin bang stop still drains as followup" `Quick
      test_drain_queued_messages_preserves_admin_bang_followup;
    Alcotest.test_case "drain queued messages sends followup response" `Quick
      test_drain_queued_messages_sends_followup_response;
    Alcotest.test_case "deferred followup drains as plain message" `Quick
      test_drain_deferred_followup_sends_plain_message;
    Alcotest.test_case "deferred turn queues if idle lock race is lost" `Quick
      test_deferred_turn_queues_if_lock_lost;
    Alcotest.test_case "idle followup drains later busy followup" `Quick
      test_idle_followup_drains_later_busy_followup;
    Alcotest.test_case "turn uses special command handler" `Quick
      test_turn_uses_special_command_handler;
    Alcotest.test_case "turn stream uses special command handler" `Quick
      test_turn_stream_uses_special_command_handler;
    Alcotest.test_case "turn stream notifies registered skill loads once" `Quick
      test_turn_stream_skill_load_notifies_registered_notifier_once;
    Alcotest.test_case "turn stream emits compaction notice" `Quick
      test_turn_stream_emits_compaction_notice;
    Alcotest.test_case "session debug toggle persists" `Quick
      test_session_debug_toggle_persists;
    Alcotest.test_case "session debug formats llm call summary" `Quick
      test_session_debug_formats_llm_call_summary;
    Alcotest.test_case "turn sends debug summary after llm call" `Quick
      test_turn_sends_debug_summary_after_llm_call;
    Alcotest.test_case "@agent sends debug summary for parent session" `Quick
      test_agent_mention_sends_debug_summary_for_parent_session;
    Alcotest.test_case "agent invoke sends debug summary for parent session"
      `Quick test_agent_invoke_sends_debug_summary_for_parent_session;
    Alcotest.test_case "delegate sends debug summary for parent session" `Quick
      test_delegate_sends_debug_summary_for_parent_session;
    Alcotest.test_case "fork_and sends debug summary for parent session" `Quick
      test_fork_and_sends_debug_summary_for_parent_session;
    Alcotest.test_case
      "autonomous continuation prompt can disarm with STAY_IDLE" `Quick
      test_autonomous_continuation_stays_idle_disarms;
    Alcotest.test_case "autonomous continuation can resume then disarm" `Quick
      test_autonomous_continuation_resets_after_work;
    Alcotest.test_case
      "autonomous continuation delivers response via on_response callback"
      `Quick test_autonomous_continuation_delivers_via_on_response;
    Alcotest.test_case
      "autonomous continuation is cancellable by new turn activity" `Quick
      test_autonomous_continuation_is_cancellable_by_new_turn;
    Alcotest.test_case
      "autonomous continuation sends visible injection to notifier" `Quick
      test_autonomous_continuation_sends_visible_injection;
    Alcotest.test_case "autonomous continuation suppresses check-in by default"
      `Quick test_autonomous_continuation_suppresses_checkin_by_default;
    Alcotest.test_case
      "autonomous continuation suppresses all output when checkin disabled"
      `Quick
      test_autonomous_continuation_suppresses_all_output_when_checkin_disabled;
    Alcotest.test_case
      "autonomous continuation preserves history when checkin disabled" `Quick
      test_autonomous_continuation_preserves_history_when_checkin_disabled;
    Alcotest.test_case
      "autonomous continuation disabled by config returns immediately" `Quick
      test_autonomous_continuation_disabled_by_config;
    Alcotest.test_case "autonomous continuation uses config delay" `Quick
      test_autonomous_continuation_uses_config_delay;
    Alcotest.test_case "bang message interrupts before lock and turns normally"
      `Quick test_bang_message_interrupts_before_lock_and_turns_normally;
    Alcotest.test_case "bang message turn stream processes normally" `Quick
      test_bang_message_turn_stream_processes_normally;
    Alcotest.test_case "empty bang message becomes interrupted message" `Quick
      test_empty_bang_message_becomes_interrupted_message;
    Alcotest.test_case "interrupt skips tool calls in batch" `Quick
      test_interrupt_skips_tool_calls_in_batch;
    Alcotest.test_case "interrupt skips remaining tools stream" `Quick
      test_interrupt_skips_remaining_tools_stream;
    Alcotest.test_case "interrupt latch skips after first tool" `Quick
      test_interrupt_latch_skips_after_first_tool;
    Alcotest.test_case "agent turn stop interrupt returns stopped by admin"
      `Quick test_agent_turn_stop_interrupt_returns_stopped_by_admin;
    Alcotest.test_case
      "agent turn stream stop interrupt returns stopped by admin" `Quick
      test_agent_turn_stream_stop_interrupt_returns_stopped_by_admin;
    Alcotest.test_case "notifier restores previous after nested registration"
      `Quick test_with_registered_notifier_restores_previous;
    Alcotest.test_case "notifier cleanup preserves factory and capabilities"
      `Quick test_with_registered_notifier_preserves_factory_and_capabilities;
    Alcotest.test_case "consolidated status hides thinking when disabled" `Quick
      test_consolidated_status_on_chunk_hides_thinking_when_disabled;
    Alcotest.test_case "consolidated status shows thinking when enabled" `Quick
      test_consolidated_status_on_chunk_shows_thinking_when_enabled;
    Alcotest.test_case "drain works after concurrent notifier registration"
      `Quick test_drain_works_after_concurrent_notifier_registration;
    Alcotest.test_case "queued interrupt does not skip tools" `Quick
      test_queued_interrupt_does_not_skip_tools;
    Alcotest.test_case "mid-turn injection adds to history" `Quick
      test_mid_turn_injection_adds_to_history;
    Alcotest.test_case "restore sanitizes orphaned tool results" `Quick
      test_restore_sanitizes_orphaned_tool_results;
    Alcotest.test_case "active doc_write persists workspace refresh event"
      `Quick test_active_doc_write_persists_workspace_refresh_event;
    Alcotest.test_case "active file_write persists workspace refresh event"
      `Quick test_active_file_write_persists_workspace_refresh_event;
    Alcotest.test_case
      "active file_write equivalent path persists workspace refresh event"
      `Quick
      test_active_file_write_with_equivalent_path_persists_workspace_refresh_event;
    Alcotest.test_case
      "shell_exec persists workspace refresh event for active file update"
      `Quick
      test_shell_exec_persists_workspace_refresh_event_for_active_file_update;
    Alcotest.test_case
      "git checkout persists workspace refresh event for active file update"
      `Quick test_git_operations_checkout_persists_workspace_refresh_event;
    Alcotest.test_case
      "batched active workspace updates attribute refresh per tool call" `Quick
      test_batched_active_workspace_updates_attribute_refresh_per_tool_call;
    Alcotest.test_case
      "turn notifier surfaces workspace refresh event for tool update" `Quick
      test_turn_notifier_surfaces_workspace_refresh_event_for_tool_update;
    Alcotest.test_case
      "turn stream detects external workspace edit on next turn" `Quick
      test_turn_stream_detects_external_workspace_edit_on_next_turn;
    Alcotest.test_case
      "restored session detects external workspace edit on next turn" `Quick
      test_restored_session_detects_external_workspace_edit_on_next_turn;
    Alcotest.test_case "external edit detected via note function" `Quick
      test_external_edit_detected_via_note_function;
    Alcotest.test_case "external edit event filtered from build_messages" `Quick
      test_external_edit_event_filtered_from_build_messages;
    Alcotest.test_case "no false positive when files unchanged" `Quick
      test_no_false_positive_when_files_unchanged;
    Alcotest.test_case "external file deletion detected" `Quick
      test_external_file_deletion_detected;
    Alcotest.test_case "tool write then external edit no double report" `Quick
      test_tool_write_then_external_edit_no_double_report;
    Alcotest.test_case
      "non-active doc_write does not persist workspace refresh event" `Quick
      test_non_active_doc_write_does_not_persist_workspace_refresh_event;
    Alcotest.test_case "workspace refresh event does not enter live prompt"
      `Quick test_workspace_refresh_event_does_not_enter_live_prompt;
    Alcotest.test_case "history delta tokens includes tool results" `Quick
      test_history_delta_tokens_includes_tool_results;
    Alcotest.test_case "history delta tokens tool only" `Quick
      test_history_delta_tokens_tool_only;
    Alcotest.test_case "drain progress callbacks fire for queued messages"
      `Quick test_drain_progress_callbacks_fire_for_queued_messages;
    Alcotest.test_case "drain progress not called when no queued messages"
      `Quick test_drain_progress_not_called_when_no_queued_messages;
    Alcotest.test_case "before_drain fires before drain notifier" `Quick
      test_before_drain_fires_before_drain_notifier;
    Alcotest.test_case "session restore no false positive workspace refresh"
      `Quick test_session_restore_no_false_positive_workspace_refresh;
    Alcotest.test_case "session restore detects external edit on next turn"
      `Quick test_session_restore_detects_external_edit_on_next_turn;
    Alcotest.test_case "fresh session no false positive on first turn" `Quick
      test_fresh_session_no_false_positive_on_first_turn;
    Alcotest.test_case "workspace change not re-reported on subsequent turn"
      `Quick test_workspace_change_not_re_reported_on_subsequent_turn;
    Alcotest.test_case "compact loads session from db when not in memory" `Quick
      test_compact_loads_session_from_db_when_not_in_memory;
    Alcotest.test_case "compact profiled room skips pre-compaction flush" `Quick
      test_compact_profiled_room_skips_pre_compaction_flush;
    Alcotest.test_case "compact releases session lock while summarizing" `Quick
      test_compact_releases_session_lock_while_summarizing;
    Alcotest.test_case "compact preserves new messages arriving midflight"
      `Quick test_compact_preserves_new_messages_arriving_midflight;
    Alcotest.test_case "compact skips apply after reset midflight" `Quick
      test_compact_skips_apply_after_reset_midflight;
    Alcotest.test_case "turn_stream forwards tool events to on_chunk" `Quick
      test_turn_stream_forwards_tool_events_to_on_chunk;
    Alcotest.test_case "non-vision model filters images" `Quick
      test_non_vision_model_filters_images;
    Alcotest.test_case "vision model keeps images" `Quick
      test_vision_model_keeps_images;
    Alcotest.test_case "runtime context block ignores prompt toggles" `Quick
      test_runtime_context_block_ignores_prompt_toggles;
    Alcotest.test_case
      "enqueue_message_if_busy persists to sqlite when db available" `Quick
      test_enqueue_message_if_busy_persists_to_sqlite;
    Alcotest.test_case
      "drain_queued_messages deletes sqlite row after successful turn" `Quick
      test_drain_queued_messages_deletes_sqlite_row;
    Alcotest.test_case "drain preserves messages without notifier" `Quick
      test_drain_preserves_messages_without_notifier;
    Alcotest.test_case "sanitize_session_key replaces forward slash" `Quick
      (fun () ->
        Alcotest.(check string)
          "slash" "web:foo_bar"
          (Session.sanitize_session_key "web:foo/bar"));
    Alcotest.test_case "sanitize_session_key replaces backslash" `Quick
      (fun () ->
        Alcotest.(check string)
          "backslash" "web:foo_bar"
          (Session.sanitize_session_key "web:foo\\bar"));
    Alcotest.test_case "sanitize_session_key replaces null byte" `Quick
      (fun () ->
        Alcotest.(check string)
          "null" "web:foo_bar"
          (Session.sanitize_session_key "web:foo\x00bar"));
    Alcotest.test_case "sanitize_session_key replaces double dots" `Quick
      (fun () ->
        Alcotest.(check string)
          "dotdot" "web:__secret"
          (Session.sanitize_session_key "web:..secret"));
    Alcotest.test_case "sanitize_session_key preserves normal keys" `Quick
      (fun () ->
        Alcotest.(check string)
          "normal" "discord:123:456"
          (Session.sanitize_session_key "discord:123:456"));
    Alcotest.test_case "sanitize_session_key preserves colons" `Quick (fun () ->
        Alcotest.(check string)
          "colons" "telegram:42:user"
          (Session.sanitize_session_key "telegram:42:user"));
    Alcotest.test_case "sanitize_session_key handles mixed unsafe chars" `Quick
      (fun () ->
        Alcotest.(check string)
          "mixed" "web:_________etc_passwd"
          (Session.sanitize_session_key "web:../../../etc/passwd"));
  ]
