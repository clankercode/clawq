let test_require_pairing_blocks_chat () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  let req =
    Cohttp.Request.make ~meth:`POST (Uri.of_string "http://127.0.0.1/chat")
  in
  let body = Cohttp_lwt.Body.of_string {|{"session_id":"s","message":"hi"}|} in
  let resp, _body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:true
         ~auth_token:None (Obj.magic ()) req body)
  in
  Alcotest.(check int)
    "forbidden" 403
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp))

let test_chat_requires_session_id () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  let req =
    Cohttp.Request.make ~meth:`POST (Uri.of_string "http://127.0.0.1/chat")
  in
  let body = Cohttp_lwt.Body.of_string {|{"message":"hi"}|} in
  let resp, _body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:None (Obj.magic ()) req body)
  in
  Alcotest.(check int)
    "bad request" 400
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp))

let test_require_pairing_blocks_chat_stream () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  let req =
    Cohttp.Request.make ~meth:`POST
      (Uri.of_string "http://127.0.0.1/chat/stream")
  in
  let body = Cohttp_lwt.Body.of_string {|{"session_id":"s","message":"hi"}|} in
  let resp, _body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:true
         ~auth_token:None (Obj.magic ()) req body)
  in
  Alcotest.(check int)
    "forbidden" 403
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp))

let test_chat_rejects_missing_auth_token () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  let req =
    Cohttp.Request.make ~meth:`POST (Uri.of_string "http://127.0.0.1/chat")
  in
  let body = Cohttp_lwt.Body.of_string {|{"session_id":"s","message":"hi"}|} in
  let resp, _body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:(Some "secret") (Obj.magic ()) req body)
  in
  Alcotest.(check int)
    "unauthorized" 401
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp))

let body_string body = Lwt_main.run (Cohttp_lwt.Body.to_string body)

let test_chat_runtime_ctx_returns_runtime_context () =
  let config =
    {
      Runtime_config.default with
      prompt = { Runtime_config.default.prompt with dynamic_enabled = false };
    }
  in
  let session_manager = Session.create ~config () in
  let req =
    Cohttp.Request.make ~meth:`POST (Uri.of_string "http://127.0.0.1/chat")
  in
  let body =
    Cohttp_lwt.Body.of_string {|{"session_id":"s","message":"/runtime-ctx"}|}
  in
  let resp, body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:None (Obj.magic ()) req body)
  in
  Alcotest.(check int)
    "ok" 200
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
  let payload = Yojson.Safe.from_string (body_string body) in
  let open Yojson.Safe.Util in
  let response = payload |> member "response" |> to_string in
  let contains needle =
    try
      ignore (Str.search_forward (Str.regexp_string needle) response 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "runtime header present" true
    (contains "[Runtime context for this turn only]");
  Alcotest.(check bool) "session id present" true (contains "Session id: web:s")

let test_session_inject_rejects_missing_auth_token () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  let req =
    Cohttp.Request.make ~meth:`POST
      (Uri.of_string "http://127.0.0.1/session/inject")
  in
  let body =
    Cohttp_lwt.Body.of_string
      {|{"session_key":"telegram:1:u","message":"hello"}|}
  in
  let resp, _body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:(Some "secret") (Obj.magic ()) req body)
  in
  Alcotest.(check int)
    "unauthorized" 401
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp))

let test_session_inject_uses_session_turn () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  Session.set_special_command_handler session_manager
    (fun ~key ~message ~send_progress:_ ~interrupt_check:_ ->
      if key = "telegram:1:u" && message = "hello" then
        Lwt.return_some "processed live"
      else Lwt.return_none);
  let headers = Cohttp.Header.of_list [ ("Authorization", "Bearer secret") ] in
  let req =
    Cohttp.Request.make ~headers ~meth:`POST
      (Uri.of_string "http://127.0.0.1/session/inject")
  in
  let body =
    Cohttp_lwt.Body.of_string
      {|{"session_key":"telegram:1:u","message":"hello"}|}
  in
  let resp, body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:(Some "secret") (Obj.magic ()) req body)
  in
  Alcotest.(check int)
    "ok" 200
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
  let payload = Yojson.Safe.from_string (body_string body) in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "not queued" false
    (payload |> member "queued" |> to_bool);
  Alcotest.(check string)
    "handler response preserved" "processed live"
    (payload |> member "response" |> to_string)

let test_daemon_update_rejects_missing_auth_token () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  let req =
    Cohttp.Request.make ~meth:`POST
      (Uri.of_string "http://127.0.0.1/daemon/update")
  in
  let body = Cohttp_lwt.Body.of_string {|{"mode":"git"}|} in
  let resp, _body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:(Some "secret")
         ~daemon_run_update_command:(fun ~mode:_ ~send_progress:_ () ->
           Lwt.return "ok")
         (Obj.magic ()) req body)
  in
  Alcotest.(check int)
    "unauthorized" 401
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp))

let test_daemon_update_uses_run_update_command () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  let seen_mode = ref None in
  let headers = Cohttp.Header.of_list [ ("Authorization", "Bearer secret") ] in
  let req =
    Cohttp.Request.make ~headers ~meth:`POST
      (Uri.of_string "http://127.0.0.1/daemon/update")
  in
  let body = Cohttp_lwt.Body.of_string {|{"mode":"git"}|} in
  let resp, body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:(Some "secret")
         ~daemon_run_update_command:(fun ~mode ~send_progress () ->
           let open Lwt.Syntax in
           seen_mode := Some mode;
           let* () = send_progress "Starting update..." in
           let* () = send_progress "Running: git pull" in
           Lwt.return "Build complete. Sending restart signal...")
         (Obj.magic ()) req body)
  in
  Alcotest.(check int)
    "ok" 200
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
  Alcotest.(check (option string))
    "mode forwarded" (Some "git")
    (Option.map Update_tool.string_of_update_mode !seen_mode);
  let payload = Yojson.Safe.from_string (body_string body) in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "result preserved" "Build complete. Sending restart signal..."
    (payload |> member "result" |> to_string);
  Alcotest.(check (list string))
    "progress preserved"
    [ "Starting update..."; "Running: git pull" ]
    (payload |> member "progress" |> to_list |> List.map to_string)

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

let fast_fail_config =
  {
    Runtime_config.default with
    providers =
      [
        ( "test",
          {
            Runtime_config.default_provider_config with
            api_key = "fake";
            base_url = Some "http://127.0.0.1:1";
            default_model = Some "test-model";
          } );
      ];
    default_provider = Some "test";
    resilience =
      {
        Runtime_config.default.resilience with
        timeout_s = 1.0;
        max_retries = 0;
        base_delay_s = 0.0;
      };
  }

let test_chat_error_marks_response_sent () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = fast_fail_config in
  let session_manager = Session.create ~config ~db () in
  let req =
    Cohttp.Request.make ~meth:`POST (Uri.of_string "http://127.0.0.1/chat")
  in
  let body = Cohttp_lwt.Body.of_string {|{"session_id":"s","message":"hi"}|} in
  let resp, _body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:None (Obj.magic ()) req body)
  in
  Alcotest.(check int)
    "server error" 500
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
  Alcotest.(check (option string))
    "pending turn cleared" (Some "user")
    (query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'web:s'");
  Alcotest.(check bool)
    "response timestamp set" true
    (query_single_text_option db
       "SELECT response_sent_at FROM session_state WHERE session_key = 'web:s'"
    <> None)

let test_chat_stream_error_marks_response_sent () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = fast_fail_config in
  let session_manager = Session.create ~config ~db () in
  let req =
    Cohttp.Request.make ~meth:`POST
      (Uri.of_string "http://127.0.0.1/chat/stream")
  in
  let body = Cohttp_lwt.Body.of_string {|{"session_id":"s","message":"hi"}|} in
  let resp, body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:None (Obj.magic ()) req body)
  in
  Alcotest.(check int)
    "ok" 200
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
  let payload = body_string body in
  Alcotest.(check bool)
    "sse contains error" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string {|"type":"error"|}) payload 0);
       true
     with Not_found -> false);
  Alcotest.(check (option string))
    "pending stream turn cleared" (Some "user")
    (query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'web:s'");
  Alcotest.(check bool)
    "stream response timestamp set" true
    (query_single_text_option db
       "SELECT response_sent_at FROM session_state WHERE session_key = 'web:s'"
    <> None)

let test_commands_route () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  let req =
    Cohttp.Request.make ~meth:`GET (Uri.of_string "http://127.0.0.1/commands")
  in
  let resp, body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:None (Obj.magic ()) req Cohttp_lwt.Body.empty)
  in
  Alcotest.(check int)
    "ok" 200
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
  let payload = Yojson.Safe.from_string (body_string body) in
  let open Yojson.Safe.Util in
  let commands = payload |> to_list in
  Alcotest.(check bool) "has commands" true (List.length commands > 0);
  Alcotest.(check string)
    "first command has name" "start"
    (List.hd commands |> member "name" |> to_string)

let test_ui_version_route () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  let req =
    Cohttp.Request.make ~meth:`GET (Uri.of_string "http://127.0.0.1/ui-version")
  in
  let resp, body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:None (Obj.magic ()) req Cohttp_lwt.Body.empty)
  in
  Alcotest.(check int)
    "ok" 200
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
  let payload = Yojson.Safe.from_string (body_string body) in
  let version = Yojson.Safe.Util.(payload |> member "version" |> to_string) in
  Alcotest.(check string)
    "matches asset version" Chat_ui_assets.ui_version version

let test_json_of_stream_event_variants () =
  let open Yojson.Safe.Util in
  let check_type expected event =
    Alcotest.(check string)
      "event type" expected
      (Http_server.json_of_stream_event event |> member "type" |> to_string)
  in
  check_type "thinking_delta" (Provider.ThinkingDelta "plan");
  check_type "tool_start"
    (Provider.ToolStart { id = "tc_1"; name = "shell_exec"; arguments = "{}" });
  check_type "tool_output_delta"
    (Provider.ToolOutputDelta { id = "tc_1"; chunk = "hello\n" });
  let tool_result_json =
    Http_server.json_of_stream_event
      (Provider.ToolResult
         { id = "tc_1"; name = "shell_exec"; result = "ok"; is_error = false })
  in
  Alcotest.(check string)
    "tool result type" "tool_result"
    (tool_result_json |> member "type" |> to_string);
  Alcotest.(check bool)
    "tool result success" false
    (tool_result_json |> member "is_error" |> to_bool)

let suite =
  [
    Alcotest.test_case "require_pairing blocks /chat" `Quick
      test_require_pairing_blocks_chat;
    Alcotest.test_case "chat requires session_id" `Quick
      test_chat_requires_session_id;
    Alcotest.test_case "require_pairing blocks /chat/stream" `Quick
      test_require_pairing_blocks_chat_stream;
    Alcotest.test_case "chat rejects missing auth token" `Quick
      test_chat_rejects_missing_auth_token;
    Alcotest.test_case "chat runtime ctx returns runtime context" `Quick
      test_chat_runtime_ctx_returns_runtime_context;
    Alcotest.test_case "session inject rejects missing auth token" `Quick
      test_session_inject_rejects_missing_auth_token;
    Alcotest.test_case "session inject uses session turn" `Quick
      test_session_inject_uses_session_turn;
    Alcotest.test_case "daemon update rejects missing auth token" `Quick
      test_daemon_update_rejects_missing_auth_token;
    Alcotest.test_case "daemon update uses run_update_command" `Quick
      test_daemon_update_uses_run_update_command;
    Alcotest.test_case "chat error marks response sent" `Quick
      test_chat_error_marks_response_sent;
    Alcotest.test_case "chat stream error marks response sent" `Quick
      test_chat_stream_error_marks_response_sent;
    Alcotest.test_case "commands route" `Quick test_commands_route;
    Alcotest.test_case "ui-version route" `Quick test_ui_version_route;
    Alcotest.test_case "json of stream event variants" `Quick
      test_json_of_stream_event_variants;
  ]
