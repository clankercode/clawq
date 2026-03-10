let dummy_tool =
  {
    Tool.name = "echo";
    description = "Echo text";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc [ ("text", `Assoc [ ("type", `String "string") ]) ] );
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let text = try args |> member "text" |> to_string with _ -> "" in
        Lwt.return ("echo: " ^ text));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let with_hung_stdio_process f =
  let process =
    Lwt_process.open_process_full
      ("", [| "/bin/sh"; "-c"; "trap '' TERM; while :; do :; done" |])
  in
  let stderr_drain =
    Lwt.catch
      (fun () -> Mcp_client.drain_channel process#stderr)
      (fun _ -> Lwt.return_unit)
  in
  Lwt.async (fun () -> stderr_drain);
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run
        (let open Lwt.Syntax in
         let _ =
           Lwt.catch
             (fun () ->
               process#kill Sys.sigkill;
               Lwt.return_unit)
             (fun _ -> Lwt.return_unit)
         in
         let* _ =
           Lwt.catch
             (fun () -> process#status)
             (fun _ -> Lwt.return (Unix.WEXITED 0))
         in
         Lwt.return_unit))
    (fun () -> f process stderr_drain)

let test_initialize () =
  let registry = Tool_registry.create () in
  let req =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int 1);
        ("method", `String "initialize");
      ]
  in
  let resp = Lwt_main.run (Mcp_server.handle_request ~registry req) in
  match resp with
  | None -> Alcotest.fail "expected response"
  | Some json ->
      let open Yojson.Safe.Util in
      let version =
        json |> member "result" |> member "protocolVersion" |> to_string
      in
      Alcotest.(check string) "protocol version" "2024-11-05" version

let test_tools_list () =
  let registry = Tool_registry.create () in
  Tool_registry.register registry dummy_tool;
  let req =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int 2);
        ("method", `String "tools/list");
      ]
  in
  let resp = Lwt_main.run (Mcp_server.handle_request ~registry req) in
  match resp with
  | None -> Alcotest.fail "expected response"
  | Some json ->
      let open Yojson.Safe.Util in
      let tools = json |> member "result" |> member "tools" |> to_list in
      Alcotest.(check int) "one tool" 1 (List.length tools);
      let name = List.hd tools |> member "name" |> to_string in
      Alcotest.(check string) "tool name" "echo" name

let test_tools_call () =
  let registry = Tool_registry.create () in
  Tool_registry.register registry dummy_tool;
  let req =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int 3);
        ("method", `String "tools/call");
        ( "params",
          `Assoc
            [
              ("name", `String "echo");
              ("arguments", `Assoc [ ("text", `String "hi") ]);
            ] );
      ]
  in
  let resp = Lwt_main.run (Mcp_server.handle_request ~registry req) in
  match resp with
  | None -> Alcotest.fail "expected response"
  | Some json ->
      let open Yojson.Safe.Util in
      let text =
        json |> member "result" |> member "content" |> index 0 |> member "text"
        |> to_string
      in
      Alcotest.(check string) "tool output" "echo: hi" text

let test_unknown_tool () =
  let registry = Tool_registry.create () in
  let req =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int 4);
        ("method", `String "tools/call");
        ("params", `Assoc [ ("name", `String "missing") ]);
      ]
  in
  let resp = Lwt_main.run (Mcp_server.handle_request ~registry req) in
  match resp with
  | None -> Alcotest.fail "expected error response"
  | Some json ->
      let open Yojson.Safe.Util in
      let code = json |> member "error" |> member "code" |> to_int in
      Alcotest.(check int) "method not found" (-32601) code

let test_notification_no_id () =
  let registry = Tool_registry.create () in
  Tool_registry.register registry dummy_tool;
  let req =
    `Assoc [ ("jsonrpc", `String "2.0"); ("method", `String "tools/list") ]
  in
  let resp = Lwt_main.run (Mcp_server.handle_request ~registry req) in
  Alcotest.(check bool) "no response for notification" true (resp = None)

let test_parse_stdio_config () =
  let json =
    `Assoc
      [
        ("name", `String "local");
        ("command", `String "uvx");
        ("args", `List [ `String "mcp-server"; `String "--stdio" ]);
        ("env", `Assoc [ ("TOKEN", `String "abc") ]);
      ]
  in
  match Mcp_client.server_config_of_json json with
  | Error msg -> Alcotest.fail msg
  | Ok cfg ->
      Alcotest.(check string) "name" "local" cfg.name;
      Alcotest.(check string) "command" "uvx" cfg.command;
      Alcotest.(check (list string)) "args" [ "mcp-server"; "--stdio" ] cfg.args;
      Alcotest.(check (list (pair string string)))
        "env"
        [ ("TOKEN", "abc") ]
        cfg.env

let test_parse_http_config () =
  let json =
    `Assoc
      [
        ("name", `String "remote");
        ("url", `String "https://mcp.example.test/rpc");
        ("headers", `Assoc [ ("Authorization", `String "Bearer token") ]);
      ]
  in
  match Mcp_client.server_config_of_json json with
  | Error msg -> Alcotest.fail msg
  | Ok cfg ->
      Alcotest.(check string) "name" "remote" cfg.name;
      Alcotest.(check string)
        "url stored in command" "https://mcp.example.test/rpc" cfg.command;
      Alcotest.(check (list string)) "args empty" [] cfg.args;
      Alcotest.(check (list (pair string string)))
        "headers stored in env"
        [ ("Authorization", "Bearer token") ]
        cfg.env

let test_remote_http_requests () =
  let requests = ref [] in
  let fake_post ~url ~headers ~body =
    requests := (url, headers, Yojson.Safe.from_string body) :: !requests;
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_string body in
    let method_ = json |> member "method" |> to_string in
    match method_ with
    | "initialize" ->
        Lwt.return
          ( 200,
            Yojson.Safe.to_string
              (`Assoc
                 [
                   ("jsonrpc", `String "2.0");
                   ("id", `Int 1);
                   ( "result",
                     `Assoc [ ("protocolVersion", `String "2024-11-05") ] );
                 ]),
            "application/json" )
    | "notifications/initialized" -> Lwt.return (202, "", "application/json")
    | "tools/list" ->
        Lwt.return
          ( 200,
            Yojson.Safe.to_string
              (`Assoc
                 [
                   ("jsonrpc", `String "2.0");
                   ("id", `Int 2);
                   ( "result",
                     `Assoc
                       [
                         ( "tools",
                           `List
                             [
                               `Assoc
                                 [
                                   ("name", `String "remote_echo");
                                   ("description", `String "Remote echo");
                                   ( "inputSchema",
                                     `Assoc [ ("type", `String "object") ] );
                                 ];
                             ] );
                       ] );
                 ]),
            "application/json" )
    | "tools/call" ->
        let text =
          json |> member "params" |> member "arguments" |> member "text"
          |> to_string
        in
        Lwt.return
          ( 200,
            Yojson.Safe.to_string
              (`Assoc
                 [
                   ("jsonrpc", `String "2.0");
                   ("id", `Int 3);
                   ( "result",
                     `Assoc
                       [
                         ( "content",
                           `List
                             [
                               `Assoc
                                 [
                                   ("type", `String "text");
                                   ("text", `String ("remote: " ^ text));
                                 ];
                             ] );
                       ] );
                 ]),
            "application/json" )
    | other -> Alcotest.failf "unexpected method %s" other
  in
  let cfg =
    {
      Mcp_client.name = "remote";
      command = "https://mcp.example.test/rpc";
      args = [];
      env = [ ("Authorization", "Bearer token") ];
    }
  in
  let client = Lwt_main.run (Mcp_client.connect ~http_post:fake_post cfg) in
  let tools = Mcp_client.discovered_tools client in
  Alcotest.(check int) "one remote tool" 1 (List.length tools);
  let tool = List.hd tools in
  let result =
    Lwt_main.run (tool.Tool.invoke (`Assoc [ ("text", `String "hi") ]))
  in
  Alcotest.(check string) "remote tool output" "remote: hi" result;
  let requests = List.rev !requests in
  Alcotest.(check int) "initialize, notify, list, call" 4 (List.length requests);
  let url, headers, first_json = List.hd requests in
  Alcotest.(check string) "request url" "https://mcp.example.test/rpc" url;
  Alcotest.(check (list (pair string string)))
    "request headers"
    [ ("Authorization", "Bearer token") ]
    headers;
  let open Yojson.Safe.Util in
  let first_method = first_json |> member "method" |> to_string in
  Alcotest.(check string) "first method" "initialize" first_method

let test_startup_timeout () =
  let never_post ~url:_ ~headers:_ ~body:_ = fst (Lwt.wait ()) in
  let cfg =
    {
      Mcp_client.name = "remote";
      command = "https://mcp.example.test/rpc";
      args = [];
      env = [];
    }
  in
  match
    Lwt.catch
      (fun () ->
        let open Lwt.Syntax in
        let* _ =
          Mcp_client.connect ~startup_timeout_s:0.01 ~http_post:never_post cfg
        in
        Lwt.return (Ok ()))
      (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
    |> Lwt_main.run
  with
  | Ok () -> Alcotest.fail "expected timeout"
  | Error msg ->
      Alcotest.(check bool)
        "timeout mentions startup" true
        (String.starts_with
           ~prefix:"Failure(\"MCP client: HTTP startup timed out" msg)

let test_stdio_startup_timeout_cleanup_is_bounded () =
  let cfg =
    {
      Mcp_client.name = "local";
      command = "/bin/sh";
      args = [ "-c"; "trap '' TERM; while :; do :; done" ];
      env = [];
    }
  in
  let started_at = Unix.gettimeofday () in
  let result =
    Lwt.catch
      (fun () ->
        let open Lwt.Syntax in
        let* _ = Mcp_client.connect ~startup_timeout_s:0.01 cfg in
        Lwt.return (Ok ()))
      (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
    |> Lwt_main.run
  in
  let elapsed_s = Unix.gettimeofday () -. started_at in
  (match result with
  | Ok () -> Alcotest.fail "expected timeout"
  | Error msg ->
      Alcotest.(check bool)
        "timeout mentions startup handshake" true
        (String.starts_with
           ~prefix:"Failure(\"MCP client: startup handshake timed out" msg));
  Alcotest.(check bool) "cleanup stays bounded" true (elapsed_s < 1.0)

let test_disconnect_cleanup_is_bounded () =
  with_hung_stdio_process (fun process stderr_drain ->
      let client =
        {
          Mcp_client.config =
            { name = "local"; command = "/bin/sh"; args = []; env = [] };
          transport = Mcp_client.Stdio { process; stderr_drain };
          next_id = 1;
          discovered = [];
        }
      in
      let started_at = Unix.gettimeofday () in
      Lwt_main.run (Mcp_client.disconnect client);
      let elapsed_s = Unix.gettimeofday () -. started_at in
      Alcotest.(check bool) "disconnect stays bounded" true (elapsed_s < 1.0))

let test_remote_http_sse_response () =
  (* Simulate a Streamable HTTP server that responds with text/event-stream
     using multiple events and multi-line data framing. *)
  let sse_post ~url:_ ~headers:_ ~body =
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_string body in
    let method_ = json |> member "method" |> to_string in
    let id_opt = try Some (json |> member "id" |> to_int) with _ -> None in
    match method_ with
    | "initialize" ->
        let id = Option.value ~default:1 id_opt in
        let resp =
          "event: ping\n" ^ "data: {\"kind\":\"ping\"}\n\n"
          ^ Printf.sprintf
              "data: {\"jsonrpc\":\"2.0\",\n\
              \ data: \"id\":%d,\n\
              \ data: \"result\":{\"protocolVersion\":\"2024-11-05\"}}\n\n"
              id
        in
        Lwt.return (200, resp, "text/event-stream")
    | "notifications/initialized" -> Lwt.return (202, "", "application/json")
    | "tools/list" ->
        let id = Option.value ~default:2 id_opt in
        let resp =
          "event: ping\n" ^ "data: {\"kind\":\"ping\"}\n\n"
          ^ Printf.sprintf
              "data: {\"jsonrpc\":\"2.0\",\n\
              \ data: \"id\":%d,\n\
              \ data: \
               \"result\":{\"tools\":[{\"name\":\"sse_tool\",\"description\":\"SSE \
               tool\",\"inputSchema\":{\"type\":\"object\"}}]}}\n\n"
              id
        in
        Lwt.return (200, resp, "text/event-stream")
    | other -> Alcotest.failf "unexpected method %s" other
  in
  let cfg =
    {
      Mcp_client.name = "sse-remote";
      command = "https://api.example.test/mcp";
      args = [];
      env = [];
    }
  in
  let client = Lwt_main.run (Mcp_client.connect ~http_post:sse_post cfg) in
  let tools = Mcp_client.discovered_tools client in
  Alcotest.(check int) "one sse tool" 1 (List.length tools);
  let tool = List.hd tools in
  Alcotest.(check string) "sse tool name" "sse_tool" tool.Tool.name

let test_remote_http_redirect_fails () =
  let redirect_post ~url:_ ~headers:_ ~body:_ =
    Lwt.return (302, "moved", "text/plain")
  in
  let cfg =
    {
      Mcp_client.name = "redirect";
      command = "https://api.example.test/mcp";
      args = [];
      env = [];
    }
  in
  match
    Lwt.catch
      (fun () ->
        let open Lwt.Syntax in
        let* _ = Mcp_client.connect ~http_post:redirect_post cfg in
        Lwt.return (Ok ()))
      (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
    |> Lwt_main.run
  with
  | Ok () -> Alcotest.fail "expected redirect failure"
  | Error msg ->
      Alcotest.(check bool)
        "redirect mentions HTTP status" true
        (String.starts_with ~prefix:"Failure(\"MCP client: HTTP 302" msg)

let suite =
  [
    Alcotest.test_case "initialize" `Quick test_initialize;
    Alcotest.test_case "tools/list" `Quick test_tools_list;
    Alcotest.test_case "tools/call" `Quick test_tools_call;
    Alcotest.test_case "unknown tool" `Quick test_unknown_tool;
    Alcotest.test_case "notification no id" `Quick test_notification_no_id;
    Alcotest.test_case "parse stdio config" `Quick test_parse_stdio_config;
    Alcotest.test_case "parse http config" `Quick test_parse_http_config;
    Alcotest.test_case "remote http requests" `Quick test_remote_http_requests;
    Alcotest.test_case "remote http sse response" `Quick
      test_remote_http_sse_response;
    Alcotest.test_case "remote http redirect fails" `Quick
      test_remote_http_redirect_fails;
    Alcotest.test_case "startup timeout" `Quick test_startup_timeout;
    Alcotest.test_case "stdio startup timeout cleanup bounded" `Quick
      test_stdio_startup_timeout_cleanup_is_bounded;
    Alcotest.test_case "disconnect cleanup bounded" `Quick
      test_disconnect_cleanup_is_bounded;
  ]
