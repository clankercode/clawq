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

let suite =
  [
    Alcotest.test_case "initialize" `Quick test_initialize;
    Alcotest.test_case "tools/list" `Quick test_tools_list;
    Alcotest.test_case "tools/call" `Quick test_tools_call;
    Alcotest.test_case "unknown tool" `Quick test_unknown_tool;
    Alcotest.test_case "notification no id" `Quick test_notification_no_id;
  ]
