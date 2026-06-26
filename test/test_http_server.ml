let body_string body = Lwt_main.run (Cohttp_lwt.Body.to_string body)

let contains_str haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

let insert_cached_model ?(deprecated = false) ?(unavailable = false) db
    ~provider ~model_id =
  ignore
    (Model_discovery.upsert_model_rich ~db ~provider ~model_id
       ~display_name:(Some model_id) ~context_window:(Some 123000)
       ~supports_vision:false ~supports_tools:true ~supports_thinking:false
       ~deprecated ~unavailable ~source:"provider-api" ())

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

let test_models_route_includes_db_only_models_and_filters_availability () =
  let db = Memory.init ~db_path:":memory:" () in
  insert_cached_model db ~provider:"dbprov" ~model_id:"fresh-model";
  insert_cached_model ~unavailable:true db ~provider:"dbprov"
    ~model_id:"disabled-model";
  let session_manager = Session.create ~config:Runtime_config.default ~db () in
  let request uri =
    let req = Cohttp.Request.make ~meth:`GET (Uri.of_string uri) in
    let resp, body =
      Lwt_main.run
        (Http_server.handler ~session_manager ~require_pairing:false
           ~auth_token:None (Obj.magic ()) req Cohttp_lwt.Body.empty)
    in
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp), body_string body)
  in
  let status_default, body_default =
    request "http://127.0.0.1/models?provider=dbprov"
  in
  let status_all, body_all =
    request "http://127.0.0.1/models?provider=dbprov&availability=all"
  in
  let status_unavailable, body_unavailable =
    request "http://127.0.0.1/models?provider=dbprov&availability=unavailable"
  in
  Alcotest.(check int) "default status" 200 status_default;
  Alcotest.(check int) "all status" 200 status_all;
  Alcotest.(check int) "unavailable status" 200 status_unavailable;
  Alcotest.(check bool)
    "default includes available DB-only model" true
    (contains_str body_default "\"id\":\"fresh-model\"");
  Alcotest.(check bool)
    "default excludes unavailable DB-only model" true
    (not (contains_str body_default "disabled-model"));
  Alcotest.(check bool)
    "all includes unavailable DB-only model" true
    (contains_str body_all "disabled-model");
  Alcotest.(check bool)
    "unavailable includes unavailable DB-only model" true
    (contains_str body_unavailable "disabled-model");
  Alcotest.(check bool)
    "unavailable excludes available DB-only model" true
    (not (contains_str body_unavailable "fresh-model"))

let chat_stream_response ~session_manager message =
  let req =
    Cohttp.Request.make ~meth:`POST
      (Uri.of_string "http://127.0.0.1/chat/stream")
  in
  let body =
    Cohttp_lwt.Body.of_string
      (Printf.sprintf {|{"session_id":"s","message":%s}|}
         (Yojson.Safe.to_string (`String message)))
  in
  let resp, body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:None (Obj.magic ()) req body)
  in
  let body = body_string body in
  let status = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
  if status <> 200 then Alcotest.failf "expected 200, got %d: %s" status body;
  body

let test_chat_model_set_force_rejects_unavailable_cached_model () =
  let db = Memory.init ~db_path:":memory:" () in
  insert_cached_model ~unavailable:true db ~provider:"dbprov"
    ~model_id:"disabled-model";
  let session_manager = Session.create ~config:Runtime_config.default ~db () in
  let response =
    chat_stream_response ~session_manager
      "/model set-force dbprov:disabled-model"
  in
  Alcotest.(check bool)
    "rejects unavailable cached model" true
    (contains_str response "marked unavailable");
  Alcotest.(check (option string))
    "does not persist session override" None
    (Memory.get_session_model_override ~db ~session_key:"web:s")

let with_temp_clawq_home_for_model_test f =
  let base = Filename.temp_file "clawq-home" ".tmp" in
  Sys.remove base;
  Unix.mkdir base 0o755;
  let previous = Sys.getenv_opt "CLAWQ_HOME" in
  Unix.putenv "CLAWQ_HOME" base;
  Fun.protect
    ~finally:(fun () ->
      (match previous with
      | Some v -> Unix.putenv "CLAWQ_HOME" v
      | None -> Unix.putenv "CLAWQ_HOME" "");
      ignore (Sys.command (Printf.sprintf "rm -rf %S" base)))
    (fun () -> f base)

let test_chat_model_set_default_rejects_deprecated_cached_model () =
  with_temp_clawq_home_for_model_test (fun _home ->
      let db = Memory.init ~db_path:":memory:" () in
      insert_cached_model ~deprecated:true db ~provider:"dbprov"
        ~model_id:"old-model";
      let session_manager =
        Session.create ~config:Runtime_config.default ~db ()
      in
      let response =
        chat_stream_response ~session_manager
          "/model set-default dbprov:old-model"
      in
      Alcotest.(check bool)
        "rejects deprecated cached model" true
        (contains_str response "marked deprecated");
      Alcotest.(check bool)
        "does not report default update" false
        (contains_str response "Default model set to:"))

let debug_lines payload =
  match Yojson.Safe.Util.member "debug" payload with
  | `List lines ->
      List.filter_map (function `String line -> Some line | _ -> None) lines
  | _ -> []

let check_debug_summary label payload =
  Alcotest.(check bool)
    label true
    (List.exists
       (fun line -> contains_str line "debug: llm provider=fake")
       (debug_lines payload))

let with_env key value f =
  let previous = Sys.getenv_opt key in
  (match value with Some v -> Unix.putenv key v | None -> Unix.putenv key "");
  Fun.protect f ~finally:(fun () ->
      match previous with
      | Some v -> Unix.putenv key v
      | None -> Unix.putenv key "")

let with_temp_clawq_home f =
  let base = Filename.temp_file "clawq-home" ".tmp" in
  Sys.remove base;
  Unix.mkdir base 0o755;
  with_env "CLAWQ_HOME" (Some base) (fun () ->
      Fun.protect
        ~finally:(fun () ->
          ignore (Sys.command (Printf.sprintf "rm -rf %S" base)))
        (fun () -> f base))

let make_fake_provider_config base_url : Runtime_config.provider_config =
  {
    Runtime_config.default_provider_config with
    api_key = "test-key";
    base_url = Some base_url;
    default_model = Some "fake-model";
  }

let with_text_provider f =
  let port = free_port () in
  let callback _conn _req body =
    let open Lwt.Syntax in
    let* _ = Cohttp_lwt.Body.to_string body in
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
                             ("content", `String "Summary of conversation.");
                           ] );
                       ("finish_reason", `String "stop");
                     ];
                 ] );
             ( "usage",
               `Assoc
                 [ ("prompt_tokens", `Int 1); ("completion_tokens", `Int 1) ] );
           ])
    in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:response_body ()
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

let compute_github_signature ~secret ~body =
  "sha256=" ^ Digestif.SHA256.(hmac_string ~key:secret body |> to_hex)

let with_fake_github_api callback f =
  let port = free_port () in
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  Lwt.async (fun () -> Lwt.pick [ server; stop ]);
  Fun.protect
    ~finally:(fun () -> Lwt.wakeup_later stopper ())
    (fun () -> f (Printf.sprintf "http://127.0.0.1:%d" port))

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
  Alcotest.(check bool)
    "runtime header present" true
    (contains_str response "[Runtime context for this turn only]");
  Alcotest.(check bool)
    "session id present" true
    (contains_str response "Session id: web:s")

let test_chat_costs_returns_cost_summary () =
  Test_helpers.with_memory_db (fun db ->
      Memory.init_request_stats_schema db;
      Request_stats.record ~db ~session_key:"web:s" ~provider:"openai"
        ~model:"gpt-5.4" ~prompt_tokens:1500 ~completion_tokens:250
        ~cost_usd:0.15 ~added_prompt_tokens:900 ();
      let session_manager =
        Session.create ~config:Runtime_config.default ~db ()
      in
      let req =
        Cohttp.Request.make ~meth:`POST (Uri.of_string "http://127.0.0.1/chat")
      in
      let body =
        Cohttp_lwt.Body.of_string {|{"session_id":"s","message":"/costs"}|}
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
      Alcotest.(check bool)
        "has summary heading" true
        (contains_str response "Cost Summary");
      Alcotest.(check bool)
        "has all time row" true
        (contains_str response "All time"))

let make_dummy_tool name description =
  {
    Tool.name;
    description;
    parameters_schema = `Assoc [];
    invoke = (fun ?context:_ _ -> Lwt.return "ok");
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let test_chat_help_returns_plain_help () =
  let session_manager = Session.create ~config:Runtime_config.default () in
  let req =
    Cohttp.Request.make ~meth:`POST (Uri.of_string "http://127.0.0.1/chat")
  in
  let body =
    Cohttp_lwt.Body.of_string {|{"session_id":"s","message":"/help"}|}
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
  Alcotest.(check bool)
    "help header present" true
    (contains_str response "Available commands:");
  Alcotest.(check bool)
    "no markdown table" false
    (contains_str response "| Command | Description |")

let test_chat_tools_returns_plain_tool_list () =
  let registry = Tool_registry.create () in
  Tool_registry.register registry (make_dummy_tool "file_read" "Read a file");
  let session_manager =
    Session.create ~config:Runtime_config.default ~tool_registry:registry ()
  in
  let req =
    Cohttp.Request.make ~meth:`POST (Uri.of_string "http://127.0.0.1/chat")
  in
  let body =
    Cohttp_lwt.Body.of_string {|{"session_id":"s","message":"/tools"}|}
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
  Alcotest.(check bool)
    "tools header present" true
    (contains_str response "Tools (1)");
  Alcotest.(check bool)
    "tool entry present" true
    (contains_str response "file_read")

let test_chat_model_show_returns_formatted_model_summary () =
  let session_manager = Session.create ~config:Runtime_config.default () in
  let req =
    Cohttp.Request.make ~meth:`POST (Uri.of_string "http://127.0.0.1/chat")
  in
  let body =
    Cohttp_lwt.Body.of_string {|{"session_id":"s","message":"/model"}|}
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
  Alcotest.(check bool)
    "model heading present" true
    (contains_str response "Current:")

let test_chat_usage_returns_usage_summary () =
  Test_helpers.with_memory_db (fun db ->
      Memory.init_request_stats_schema db;
      Request_stats.record ~db ~session_key:"web:s" ~provider:"openai"
        ~model:"gpt-5.4" ~prompt_tokens:1500 ~completion_tokens:250
        ~cost_usd:0.15 ~added_prompt_tokens:900 ();
      let session_manager =
        Session.create ~config:Runtime_config.default ~db ()
      in
      let req =
        Cohttp.Request.make ~meth:`POST (Uri.of_string "http://127.0.0.1/chat")
      in
      let body =
        Cohttp_lwt.Body.of_string {|{"session_id":"s","message":"/usage"}|}
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
      Alcotest.(check bool)
        "has summary heading" true
        (contains_str response "Usage Summary");
      Alcotest.(check bool)
        "has all time row" true
        (contains_str response "All time"))

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
    (Test_helpers.query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'web:s'");
  Alcotest.(check bool)
    "response timestamp set" true
    (Test_helpers.query_single_text_option db
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
    (contains_str payload {|"type":"error"|});
  Alcotest.(check (option string))
    "pending stream turn cleared" (Some "user")
    (Test_helpers.query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'web:s'");
  Alcotest.(check bool)
    "stream response timestamp set" true
    (Test_helpers.query_single_text_option db
       "SELECT response_sent_at FROM session_state WHERE session_key = 'web:s'"
    <> None)

let test_chat_stream_compact_sends_debug_summary () =
  with_text_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let session_manager = Session.create ~config ~db () in
      let key = "web:s" in
      for i = 1 to 25 do
        Memory.store_message ~db ~session_key:key
          (Provider.make_message ~role:"user"
             ~content:(Printf.sprintf "msg %02d" i))
      done;
      Alcotest.(check (result unit string))
        "debug set on" (Ok ())
        (Session.set_session_debug session_manager ~key ~enabled:true);
      let req =
        Cohttp.Request.make ~meth:`POST
          (Uri.of_string "http://127.0.0.1/chat/stream")
      in
      let body =
        Cohttp_lwt.Body.of_string {|{"session_id":"s","message":"/compact"}|}
      in
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
        "sse contains compact result" true
        (contains_str payload "Session history compacted");
      Alcotest.(check bool)
        "sse contains debug summary" true
        (contains_str payload "debug: llm provider=fake"))

let test_chat_stream_debate_sends_debug_summary () =
  with_text_provider (fun config ->
      let config =
        {
          config with
          debate =
            {
              config.debate with
              default_models = [ "fake:fake-model" ];
              judge_model = "fake:fake-model";
            };
        }
      in
      let db = Memory.init ~db_path:":memory:" () in
      Debate.init_schema db;
      let session_manager = Session.create ~config ~db () in
      let key = "web:s" in
      Alcotest.(check (result unit string))
        "debug set on" (Ok ())
        (Session.set_session_debug session_manager ~key ~enabled:true);
      let req =
        Cohttp.Request.make ~meth:`POST
          (Uri.of_string "http://127.0.0.1/chat/stream")
      in
      let body =
        Cohttp_lwt.Body.of_string
          {|{"session_id":"s","message":"/debate should debug"}|}
      in
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
        "sse contains debate result" true
        (contains_str payload "Debate Results");
      Alcotest.(check bool)
        "sse contains debate debug summary" true
        (contains_str payload "debug: llm provider=fake"))

let test_chat_json_sends_debug_summary () =
  with_text_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let session_manager = Session.create ~config ~db () in
      let key = "web:s" in
      Alcotest.(check (result unit string))
        "debug set on" (Ok ())
        (Session.set_session_debug session_manager ~key ~enabled:true);
      let req =
        Cohttp.Request.make ~meth:`POST (Uri.of_string "http://127.0.0.1/chat")
      in
      let body =
        Cohttp_lwt.Body.of_string {|{"session_id":"s","message":"hi"}|}
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
      Alcotest.(check string)
        "response" "Summary of conversation."
        (payload |> member "response" |> to_string);
      check_debug_summary "json contains debug summary" payload)

let test_chat_json_debate_sends_debug_summary () =
  with_text_provider (fun config ->
      let config =
        {
          config with
          debate =
            {
              config.debate with
              default_models = [ "fake:fake-model" ];
              judge_model = "fake:fake-model";
            };
        }
      in
      let db = Memory.init ~db_path:":memory:" () in
      Debate.init_schema db;
      let session_manager = Session.create ~config ~db () in
      let key = "web:s" in
      Alcotest.(check (result unit string))
        "debug set on" (Ok ())
        (Session.set_session_debug session_manager ~key ~enabled:true);
      let req =
        Cohttp.Request.make ~meth:`POST (Uri.of_string "http://127.0.0.1/chat")
      in
      let body =
        Cohttp_lwt.Body.of_string
          {|{"session_id":"s","message":"/debate should debug"}|}
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
      Alcotest.(check bool)
        "debate response" true
        (contains_str response "Debate Results");
      check_debug_summary "json debate contains debug summary" payload)

let test_session_compact_json_sends_debug_summary () =
  with_text_provider (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let session_manager = Session.create ~config ~db () in
      let key = "web:s" in
      for i = 1 to 25 do
        Memory.store_message ~db ~session_key:key
          (Provider.make_message ~role:"user"
             ~content:(Printf.sprintf "msg %02d" i))
      done;
      Alcotest.(check (result unit string))
        "debug set on" (Ok ())
        (Session.set_session_debug session_manager ~key ~enabled:true);
      let req =
        Cohttp.Request.make ~meth:`POST
          (Uri.of_string "http://127.0.0.1/session/compact")
      in
      let body = Cohttp_lwt.Body.of_string {|{"session_key":"web:s"}|} in
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
      Alcotest.(check bool)
        "compacted" true
        (payload |> member "compacted" |> to_bool);
      check_debug_summary "compact json contains debug summary" payload)

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
    "first command has name" "help"
    (List.hd commands |> member "name" |> to_string);
  Alcotest.(check bool)
    "command has priority field" true
    (List.hd commands |> member "priority" |> to_int > 0)

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

let test_github_webhook_routes_to_session_and_posts_reply () =
  let webhook_secret = "webhook-secret" in
  let payload =
    {|{"action":"created","issue":{"number":42,"title":"Webhook test","pull_request":{"url":"https://api.github.com/repos/acme/backend/pulls/42"}},"comment":{"id":9001,"user":{"login":"octocat"},"body":"/clawq review routing","html_url":"https://github.com/acme/backend/pull/42#issuecomment-9001"},"repository":{"name":"backend","owner":{"login":"acme"}}}|}
  in
  let signature =
    compute_github_signature ~secret:webhook_secret ~body:payload
  in
  let seen_key = ref None in
  let seen_message = ref None in
  let seen_post_path = ref None in
  let seen_post_body = ref None in
  let callback _conn req body =
    let open Lwt.Syntax in
    let* body_text = Cohttp_lwt.Body.to_string body in
    seen_post_path := Some (Uri.path (Cohttp.Request.uri req));
    seen_post_body := Some body_text;
    Cohttp_lwt_unix.Server.respond_string ~status:`Created ~body:"{}" ()
  in
  with_fake_github_api callback (fun github_api_base ->
      with_env "CLAWQ_GITHUB_API_BASE" (Some github_api_base) (fun () ->
          let db = Memory.init ~db_path:":memory:" () in
          let config = Runtime_config.default in
          let session_manager = Session.create ~config ~db () in
          Session.set_special_command_handler session_manager
            (fun ~key ~message ~send_progress:_ ~interrupt_check:_ ->
              seen_key := Some key;
              seen_message := Some message;
              Lwt.return_some "stubbed GitHub reply");
          let github_config : Runtime_config.github_config =
            {
              auth = Runtime_config.GithubPat "ghp_test12345";
              repos =
                [
                  {
                    Runtime_config.name = "acme/backend";
                    webhook_secret;
                    webhook_path = "/github/webhook/backend";
                    agent_name = None;
                    allow_users = [ "octocat" ];
                    react_to = [ "issue_comment" ];
                    include_pr_files = false;
                  };
                ];
              default_model = None;
            }
          in
          let headers =
            Cohttp.Header.of_list
              [
                ("X-GitHub-Event", "issue_comment");
                ("X-Hub-Signature-256", signature);
              ]
          in
          let req =
            Cohttp.Request.make ~headers ~meth:`POST
              (Uri.of_string "http://127.0.0.1/github/webhook/backend")
          in
          let resp, body =
            Lwt_main.run
              (Http_server.handler ~session_manager ~require_pairing:false
                 ~auth_token:None ~github_config
                 ~github_api_limiter:
                   (Rate_limiter.create ~rate_per_minute:60
                      ~burst_multiplier:1.0)
                 (Obj.magic ()) req
                 (Cohttp_lwt.Body.of_string payload))
          in
          Alcotest.(check int)
            "ok" 200
            (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
          let json = Yojson.Safe.from_string (body_string body) in
          let open Yojson.Safe.Util in
          Alcotest.(check string)
            "status" "accepted"
            (json |> member "status" |> to_string);
          (* Processing is async; wait for side effects *)
          Lwt_main.run (Lwt_unix.sleep 0.1);
          Alcotest.(check (option string))
            "session key" (Some "github:acme/backend:pr:42") !seen_key;
          Alcotest.(check bool)
            "message includes github context" true
            (match !seen_message with
            | Some message -> contains_str message "## GitHub Context"
            | None -> false);
          Alcotest.(check bool)
            "message includes command" true
            (match !seen_message with
            | Some message -> contains_str message "review routing"
            | None -> false);
          Alcotest.(check (option string))
            "comment endpoint" (Some "/repos/acme/backend/issues/42/comments")
            !seen_post_path;
          Alcotest.(check bool)
            "reply body formatted for github" true
            (match !seen_post_body with
            | Some body ->
                contains_str body "> /clawq review routing"
                && contains_str body "stubbed GitHub reply"
            | None -> false)))

let test_github_pr_synchronize_reuses_pr_session () =
  let webhook_secret = "webhook-secret" in
  let payload =
    {|{"action":"synchronize","number":42,"pull_request":{"number":42,"title":"Webhook test","body":"/clawq review latest push","state":"open","html_url":"https://github.com/acme/backend/pull/42","user":{"login":"alice"},"base":{"ref":"main"},"head":{"ref":"feature"}},"repository":{"name":"backend","owner":{"login":"acme"}}}|}
  in
  let signature =
    compute_github_signature ~secret:webhook_secret ~body:payload
  in
  let seen_key = ref None in
  let seen_message = ref None in
  let seen_post_path = ref None in
  let seen_post_body = ref None in
  let callback _conn req body =
    let open Lwt.Syntax in
    let* body_text = Cohttp_lwt.Body.to_string body in
    seen_post_path := Some (Uri.path (Cohttp.Request.uri req));
    seen_post_body := Some body_text;
    Cohttp_lwt_unix.Server.respond_string ~status:`Created ~body:"{}" ()
  in
  with_fake_github_api callback (fun github_api_base ->
      with_env "CLAWQ_GITHUB_API_BASE" (Some github_api_base) (fun () ->
          let db = Memory.init ~db_path:":memory:" () in
          let config = Runtime_config.default in
          let session_manager = Session.create ~config ~db () in
          Session.set_special_command_handler session_manager
            (fun ~key ~message ~send_progress:_ ~interrupt_check:_ ->
              seen_key := Some key;
              seen_message := Some message;
              Lwt.return_some "sync reply");
          let github_config : Runtime_config.github_config =
            {
              auth = Runtime_config.GithubPat "ghp_test12345";
              repos =
                [
                  {
                    Runtime_config.name = "acme/backend";
                    webhook_secret;
                    webhook_path = "/github/webhook/backend";
                    agent_name = None;
                    allow_users = [ "alice" ];
                    react_to = [ "pull_request" ];
                    include_pr_files = false;
                  };
                ];
              default_model = None;
            }
          in
          let headers =
            Cohttp.Header.of_list
              [
                ("X-GitHub-Event", "pull_request");
                ("X-Hub-Signature-256", signature);
              ]
          in
          let req =
            Cohttp.Request.make ~headers ~meth:`POST
              (Uri.of_string "http://127.0.0.1/github/webhook/backend")
          in
          let resp, body =
            Lwt_main.run
              (Http_server.handler ~session_manager ~require_pairing:false
                 ~auth_token:None ~github_config
                 ~github_api_limiter:
                   (Rate_limiter.create ~rate_per_minute:60
                      ~burst_multiplier:1.0)
                 (Obj.magic ()) req
                 (Cohttp_lwt.Body.of_string payload))
          in
          Alcotest.(check int)
            "ok" 200
            (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
          let json = Yojson.Safe.from_string (body_string body) in
          let open Yojson.Safe.Util in
          Alcotest.(check string)
            "status" "accepted"
            (json |> member "status" |> to_string);
          (* Processing is async; wait for side effects *)
          Lwt_main.run (Lwt_unix.sleep 0.1);
          Alcotest.(check (option string))
            "session key" (Some "github:acme/backend:pr:42") !seen_key;
          Alcotest.(check bool)
            "message includes github context" true
            (match !seen_message with
            | Some message ->
                contains_str message "## GitHub Context"
                && contains_str message "review latest push"
            | None -> false);
          Alcotest.(check (option string))
            "comment endpoint" (Some "/repos/acme/backend/issues/42/comments")
            !seen_post_path;
          Alcotest.(check bool)
            "reply body formatted for github" true
            (match !seen_post_body with
            | Some body ->
                contains_str body "> /clawq review latest push"
                && contains_str body "sync reply"
            | None -> false)))

let test_github_workflow_job_hook_routes_to_session () =
  with_temp_clawq_home (fun home ->
      let hooks_dir = Filename.concat home "workspace/gh-hooks" in
      Workspace_scaffold.ensure_dir hooks_dir;
      let hook_path = Filename.concat hooks_dir "workflow_job.md" in
      let oc = open_out hook_path in
      output_string oc
        {|---
name: investigate-ci-failure
repo: acme/backend
event: workflow_job
match:
  status: completed
  conclusion: failure
---
Investigate this failed CI job for {{repo}} on {{branch}}.
Payload: {{payload_path}}
|};
      close_out oc;
      let webhook_secret = "webhook-secret" in
      let payload =
        {|{"action":"completed","workflow_job":{"id":77,"run_id":55,"name":"test","status":"completed","conclusion":"failure","head_branch":"main","head_sha":"abc123","html_url":"https://github.com/acme/backend/actions/runs/55/job/77"},"repository":{"name":"backend","owner":{"login":"acme"},"full_name":"acme/backend"},"sender":{"login":"github-actions[bot]"}}|}
      in
      let signature =
        compute_github_signature ~secret:webhook_secret ~body:payload
      in
      let seen_key = ref None in
      let seen_message = ref None in
      let config = Runtime_config.default in
      let db = Memory.init ~db_path:":memory:" () in
      let session_manager = Session.create ~config ~db () in
      Session.set_special_command_handler session_manager
        (fun ~key ~message ~send_progress:_ ~interrupt_check:_ ->
          seen_key := Some key;
          seen_message := Some message;
          Lwt.return_some "workflow hook reply");
      let github_config : Runtime_config.github_config =
        {
          auth = Runtime_config.GithubPat "ghp_test12345";
          repos =
            [
              {
                Runtime_config.name = "acme/backend";
                webhook_secret;
                webhook_path = "/github/webhook/backend";
                agent_name = None;
                allow_users = [ "octocat" ];
                react_to = [ "workflow_job" ];
                include_pr_files = false;
              };
            ];
          default_model = None;
        }
      in
      let headers =
        Cohttp.Header.of_list
          [
            ("X-GitHub-Event", "workflow_job");
            ("X-Hub-Signature-256", signature);
            ("X-GitHub-Delivery", "workflow-job-delivery");
          ]
      in
      let req =
        Cohttp.Request.make ~headers ~meth:`POST
          (Uri.of_string "http://127.0.0.1/github/webhook/backend")
      in
      let resp, body =
        Lwt_main.run
          (Http_server.handler ~session_manager ~require_pairing:false
             ~auth_token:None ~github_config
             ~github_api_limiter:
               (Rate_limiter.create ~rate_per_minute:60 ~burst_multiplier:1.0)
             (Obj.magic ()) req
             (Cohttp_lwt.Body.of_string payload))
      in
      Alcotest.(check int)
        "ok" 200
        (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
      let json = Yojson.Safe.from_string (body_string body) in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "status" "accepted"
        (json |> member "status" |> to_string);
      (* Processing is async; wait for side effects *)
      Lwt_main.run (Lwt_unix.sleep 0.1);
      Alcotest.(check (option string))
        "session key" (Some "github:acme/backend:workflow_run:55") !seen_key;
      Alcotest.(check bool)
        "message mentions failure" true
        (match !seen_message with
        | Some message ->
            contains_str message "Investigate this failed CI job"
            && contains_str message "Raw Webhook Payload"
        | None -> false))

let test_github_webhook_accepts_repo_case_mismatch () =
  let webhook_secret = "webhook-secret" in
  let payload =
    {|{"action":"completed","workflow_job":{"id":77,"run_id":55,"name":"test","status":"completed","conclusion":"failure"},"repository":{"name":"backend","owner":{"login":"AcMe"},"full_name":"AcMe/Backend"},"sender":{"login":"github-actions[bot]"}}|}
  in
  let signature =
    compute_github_signature ~secret:webhook_secret ~body:payload
  in
  let seen_key = ref None in
  let config = Runtime_config.default in
  let db = Memory.init ~db_path:":memory:" () in
  let session_manager = Session.create ~config ~db () in
  Session.set_special_command_handler session_manager
    (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
      seen_key := Some key;
      Lwt.return_some "accepted");
  let github_config : Runtime_config.github_config =
    {
      auth = Runtime_config.GithubPat "ghp_test12345";
      repos =
        [
          {
            Runtime_config.name = "acme/backend";
            webhook_secret;
            webhook_path = "/github/webhook/backend";
            agent_name = None;
            allow_users = [ "octocat" ];
            react_to = [ "workflow_job" ];
            include_pr_files = false;
          };
        ];
      default_model = None;
    }
  in
  let headers =
    Cohttp.Header.of_list
      [
        ("X-GitHub-Event", "workflow_job");
        ("X-Hub-Signature-256", signature);
        ("X-GitHub-Delivery", "repo-case-mismatch-delivery");
      ]
  in
  let req =
    Cohttp.Request.make ~headers ~meth:`POST
      (Uri.of_string "http://127.0.0.1/github/webhook/backend")
  in
  let resp, body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:None ~github_config
         ~github_api_limiter:
           (Rate_limiter.create ~rate_per_minute:60 ~burst_multiplier:1.0)
         (Obj.magic ()) req
         (Cohttp_lwt.Body.of_string payload))
  in
  Alcotest.(check int)
    "ok" 200
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
  let json = Yojson.Safe.from_string (body_string body) in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "status" "accepted"
    (json |> member "status" |> to_string);
  (* Async processing handles the event internally *)
  Lwt_main.run (Lwt_unix.sleep 0.05);
  Alcotest.(check (option string))
    "no session key for bare event" None !seen_key

let test_github_webhook_rejects_repo_mismatch () =
  with_temp_clawq_home (fun home ->
      let hooks_dir = Filename.concat home "workspace/gh-hooks" in
      Workspace_scaffold.ensure_dir hooks_dir;
      let hook_path = Filename.concat hooks_dir "workflow_job.md" in
      let oc = open_out hook_path in
      output_string oc
        {|---
name: wrong-repo
repo: evil/other
event: workflow_job
---
This should never run.
|};
      close_out oc;
      let webhook_secret = "webhook-secret" in
      let payload =
        {|{"action":"completed","workflow_job":{"id":77,"run_id":55,"name":"test","status":"completed","conclusion":"failure"},"repository":{"name":"other","owner":{"login":"evil"},"full_name":"evil/other"},"sender":{"login":"github-actions[bot]"}}|}
      in
      let signature =
        compute_github_signature ~secret:webhook_secret ~body:payload
      in
      let seen_key = ref None in
      let config = Runtime_config.default in
      let db = Memory.init ~db_path:":memory:" () in
      let session_manager = Session.create ~config ~db () in
      Session.set_special_command_handler session_manager
        (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
          seen_key := Some key;
          Lwt.return_some "unexpected");
      let github_config : Runtime_config.github_config =
        {
          auth = Runtime_config.GithubPat "ghp_test12345";
          repos =
            [
              {
                Runtime_config.name = "acme/backend";
                webhook_secret;
                webhook_path = "/github/webhook/backend";
                agent_name = None;
                allow_users = [ "octocat" ];
                react_to = [ "workflow_job" ];
                include_pr_files = false;
              };
            ];
          default_model = None;
        }
      in
      let headers =
        Cohttp.Header.of_list
          [
            ("X-GitHub-Event", "workflow_job");
            ("X-Hub-Signature-256", signature);
            ("X-GitHub-Delivery", "repo-mismatch-delivery");
          ]
      in
      let req =
        Cohttp.Request.make ~headers ~meth:`POST
          (Uri.of_string "http://127.0.0.1/github/webhook/backend")
      in
      let resp, body =
        Lwt_main.run
          (Http_server.handler ~session_manager ~require_pairing:false
             ~auth_token:None ~github_config
             ~github_api_limiter:
               (Rate_limiter.create ~rate_per_minute:60 ~burst_multiplier:1.0)
             (Obj.magic ()) req
             (Cohttp_lwt.Body.of_string payload))
      in
      Alcotest.(check int)
        "ok" 200
        (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
      let json = Yojson.Safe.from_string (body_string body) in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "status" "accepted"
        (json |> member "status" |> to_string);
      (* Async processing handles the repo mismatch internally *)
      Lwt_main.run (Lwt_unix.sleep 0.05);
      Alcotest.(check (option string)) "no session" None !seen_key)

let test_github_webhook_reaction_and_placeholder_edit () =
  let webhook_secret = "webhook-secret" in
  let payload =
    {|{"action":"created","issue":{"number":42,"title":"Webhook test","pull_request":{"url":"https://api.github.com/repos/acme/backend/pulls/42"}},"comment":{"id":9001,"user":{"login":"octocat"},"body":"/clawq review routing","html_url":"https://github.com/acme/backend/pull/42#issuecomment-9001"},"repository":{"name":"backend","owner":{"login":"acme"}}}|}
  in
  let signature =
    compute_github_signature ~secret:webhook_secret ~body:payload
  in
  let seen_key = ref None in
  let api_calls = ref [] in
  let callback _conn req body =
    let open Lwt.Syntax in
    let* body_text = Cohttp_lwt.Body.to_string body in
    let meth = Cohttp.Code.string_of_method (Cohttp.Request.meth req) in
    let path = Uri.path (Cohttp.Request.uri req) in
    api_calls := (meth, path, body_text) :: !api_calls;
    (* Return {"id": 12345} for POST to comments so placeholder path works *)
    if
      Cohttp.Request.meth req = `POST
      && contains_str path "/comments"
      && not (contains_str path "/reactions")
    then
      Cohttp_lwt_unix.Server.respond_string ~status:`Created
        ~body:{|{"id":12345}|} ()
    else Cohttp_lwt_unix.Server.respond_string ~status:`Created ~body:"{}" ()
  in
  with_fake_github_api callback (fun github_api_base ->
      with_env "CLAWQ_GITHUB_API_BASE" (Some github_api_base) (fun () ->
          let db = Memory.init ~db_path:":memory:" () in
          let config = Runtime_config.default in
          let session_manager = Session.create ~config ~db () in
          Session.set_special_command_handler session_manager
            (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
              seen_key := Some key;
              Lwt.return_some "final agent response");
          let github_config : Runtime_config.github_config =
            {
              auth = Runtime_config.GithubPat "ghp_test12345";
              repos =
                [
                  {
                    Runtime_config.name = "acme/backend";
                    webhook_secret;
                    webhook_path = "/github/webhook/backend";
                    agent_name = None;
                    allow_users = [ "octocat" ];
                    react_to = [ "issue_comment" ];
                    include_pr_files = false;
                  };
                ];
              default_model = None;
            }
          in
          let headers =
            Cohttp.Header.of_list
              [
                ("X-GitHub-Event", "issue_comment");
                ("X-Hub-Signature-256", signature);
              ]
          in
          let req =
            Cohttp.Request.make ~headers ~meth:`POST
              (Uri.of_string "http://127.0.0.1/github/webhook/backend")
          in
          let resp, body =
            Lwt_main.run
              (Http_server.handler ~session_manager ~require_pairing:false
                 ~auth_token:None ~github_config
                 ~github_api_limiter:
                   (Rate_limiter.create ~rate_per_minute:60
                      ~burst_multiplier:1.0)
                 (Obj.magic ()) req
                 (Cohttp_lwt.Body.of_string payload))
          in
          Alcotest.(check int)
            "ok" 200
            (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
          let json = Yojson.Safe.from_string (body_string body) in
          let open Yojson.Safe.Util in
          Alcotest.(check string)
            "status" "accepted"
            (json |> member "status" |> to_string);
          (* Processing is async; wait for side effects *)
          Lwt_main.run (Lwt_unix.sleep 0.2);
          Alcotest.(check (option string))
            "session key" (Some "github:acme/backend:pr:42") !seen_key;
          let calls = List.rev !api_calls in
          (* AC #4: Verify eyes reaction was posted *)
          let reaction_calls =
            List.filter
              (fun (meth, path, body) ->
                meth = "POST"
                && contains_str path "/reactions"
                && contains_str body {|"content":"eyes"|})
              calls
          in
          Alcotest.(check bool)
            "eyes reaction posted" true
            (List.length reaction_calls > 0);
          Alcotest.(check bool)
            "reaction on correct comment" true
            (List.exists
               (fun (_, path, _) -> contains_str path "/issues/comments/9001/")
               reaction_calls);
          (* AC #5: Verify placeholder comment was posted *)
          let placeholder_calls =
            List.filter
              (fun (meth, path, body) ->
                meth = "POST"
                && contains_str path "/issues/42/comments"
                && (not (contains_str path "/reactions"))
                && contains_str body "Working on it")
              calls
          in
          Alcotest.(check bool)
            "placeholder comment posted" true
            (List.length placeholder_calls > 0);
          (* AC #5: Verify final response was PATCHed to the placeholder *)
          let edit_calls =
            List.filter
              (fun (meth, path, body) ->
                meth = "PATCH"
                && contains_str path "/issues/comments/12345"
                && contains_str body "final agent response")
              calls
          in
          Alcotest.(check bool)
            "final response edited into placeholder" true
            (List.length edit_calls > 0)))

let test_github_webhook_rejects_ambiguous_path () =
  let webhook_secret = "webhook-secret" in
  let payload =
    {|{"action":"completed","workflow_job":{"id":77,"run_id":55,"name":"test","status":"completed","conclusion":"failure"},"repository":{"name":"backend","owner":{"login":"acme"},"full_name":"acme/backend"},"sender":{"login":"github-actions[bot]"}}|}
  in
  let signature =
    compute_github_signature ~secret:webhook_secret ~body:payload
  in
  let config = Runtime_config.default in
  let db = Memory.init ~db_path:":memory:" () in
  let session_manager = Session.create ~config ~db () in
  let github_config : Runtime_config.github_config =
    {
      auth = Runtime_config.GithubPat "ghp_test12345";
      repos =
        [
          {
            Runtime_config.name = "acme/backend";
            webhook_secret;
            webhook_path = "/github/webhook/shared";
            agent_name = None;
            allow_users = [ "octocat" ];
            react_to = [ "workflow_job" ];
            include_pr_files = false;
          };
          {
            Runtime_config.name = "acme/frontend";
            webhook_secret = "other-secret";
            webhook_path = "/github/webhook/shared";
            agent_name = None;
            allow_users = [ "octocat" ];
            react_to = [ "workflow_job" ];
            include_pr_files = false;
          };
        ];
      default_model = None;
    }
  in
  let headers =
    Cohttp.Header.of_list
      [
        ("X-GitHub-Event", "workflow_job");
        ("X-Hub-Signature-256", signature);
        ("X-GitHub-Delivery", "ambiguous-path-delivery");
      ]
  in
  let req =
    Cohttp.Request.make ~headers ~meth:`POST
      (Uri.of_string "http://127.0.0.1/github/webhook/shared")
  in
  let resp, body =
    Lwt_main.run
      (Http_server.handler ~session_manager ~require_pairing:false
         ~auth_token:None ~github_config
         ~github_api_limiter:
           (Rate_limiter.create ~rate_per_minute:60 ~burst_multiplier:1.0)
         (Obj.magic ()) req
         (Cohttp_lwt.Body.of_string payload))
  in
  Alcotest.(check int)
    "conflict" 409
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
  Alcotest.(check bool)
    "body mentions ambiguity" true
    (contains_str (body_string body) "ambiguous")

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
    Alcotest.test_case "GET /models includes DB-only models and filters" `Quick
      test_models_route_includes_db_only_models_and_filters_availability;
    Alcotest.test_case "chat model set-force rejects unavailable cached model"
      `Quick test_chat_model_set_force_rejects_unavailable_cached_model;
    Alcotest.test_case "chat model set-default rejects deprecated cached model"
      `Quick test_chat_model_set_default_rejects_deprecated_cached_model;
    Alcotest.test_case "chat runtime ctx returns runtime context" `Quick
      test_chat_runtime_ctx_returns_runtime_context;
    Alcotest.test_case "chat help returns plain help" `Quick
      test_chat_help_returns_plain_help;
    Alcotest.test_case "chat costs returns cost summary" `Quick
      test_chat_costs_returns_cost_summary;
    Alcotest.test_case "chat tools returns plain tool list" `Quick
      test_chat_tools_returns_plain_tool_list;
    Alcotest.test_case "chat model show returns formatted model summary" `Quick
      test_chat_model_show_returns_formatted_model_summary;
    Alcotest.test_case "chat usage returns usage summary" `Quick
      test_chat_usage_returns_usage_summary;
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
    Alcotest.test_case "chat stream compact sends debug summary" `Quick
      test_chat_stream_compact_sends_debug_summary;
    Alcotest.test_case "chat stream debate sends debug summary" `Quick
      test_chat_stream_debate_sends_debug_summary;
    Alcotest.test_case "chat json sends debug summary" `Quick
      test_chat_json_sends_debug_summary;
    Alcotest.test_case "chat json debate sends debug summary" `Quick
      test_chat_json_debate_sends_debug_summary;
    Alcotest.test_case "session compact json sends debug summary" `Quick
      test_session_compact_json_sends_debug_summary;
    Alcotest.test_case "commands route" `Quick test_commands_route;
    Alcotest.test_case "ui-version route" `Quick test_ui_version_route;
    Alcotest.test_case "json of stream event variants" `Quick
      test_json_of_stream_event_variants;
    Alcotest.test_case "github webhook routes to session and posts reply" `Quick
      test_github_webhook_routes_to_session_and_posts_reply;
    Alcotest.test_case "github pull_request synchronize reuses PR session"
      `Quick test_github_pr_synchronize_reuses_pr_session;
    Alcotest.test_case "github workflow_job hook routes to session" `Quick
      test_github_workflow_job_hook_routes_to_session;
    Alcotest.test_case "github webhook rejects repo mismatch" `Quick
      test_github_webhook_rejects_repo_mismatch;
    Alcotest.test_case "github webhook accepts repo case mismatch" `Quick
      test_github_webhook_accepts_repo_case_mismatch;
    Alcotest.test_case "github webhook reaction and placeholder edit" `Quick
      test_github_webhook_reaction_and_placeholder_edit;
    Alcotest.test_case "github webhook rejects ambiguous path" `Quick
      test_github_webhook_rejects_ambiguous_path;
  ]
