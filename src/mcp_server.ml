let server_info =
  `Assoc
    [
      ("name", `String "clawq");
      ("version", `String "0.1.0");
    ]

let capabilities = `Assoc [ ("tools", `Assoc []) ]

let tool_to_mcp_json (t : Tool.t) =
  `Assoc
    [
      ("name", `String t.name);
      ("description", `String t.description);
      ("inputSchema", t.parameters_schema);
    ]

let tools_from_registry (registry : Tool_registry.t) =
  List.map tool_to_mcp_json registry.tools

let jsonrpc_response ~id result =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", id);
      ("result", result);
    ]

let jsonrpc_error ~id ~code ~message =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", id);
      ("error", `Assoc [ ("code", `Int code); ("message", `String message) ]);
    ]

let tool_result ~content ~is_error =
  `Assoc
    [
      ( "content",
        `List
          [ `Assoc [ ("type", `String "text"); ("text", `String content) ] ] );
      ("isError", `Bool is_error);
    ]

let handle_request ~(registry : Tool_registry.t) json =
  let open Yojson.Safe.Util in
  let id =
    try
      let id_json = json |> member "id" in
      if id_json = `Null then None else Some id_json
    with _ -> None
  in
  let method_ = try json |> member "method" |> to_string with _ -> "" in
  let params = try json |> member "params" with _ -> `Null in
  match (id, method_) with
  | None, _ ->
    (* Notification — no response *)
    Lwt.return_none
  | Some id, "initialize" ->
    let result =
      `Assoc
        [
          ("protocolVersion", `String "2024-11-05");
          ("serverInfo", server_info);
          ("capabilities", capabilities);
        ]
    in
    Lwt.return_some (jsonrpc_response ~id result)
  | Some id, "tools/list" ->
    let tools = tools_from_registry registry in
    let result = `Assoc [ ("tools", `List tools) ] in
    Lwt.return_some (jsonrpc_response ~id result)
  | Some id, "tools/call" -> (
    let open Lwt.Syntax in
    let tool_name =
      try params |> member "name" |> to_string with _ -> ""
    in
    match Tool_registry.find registry tool_name with
    | None ->
      Lwt.return_some
        (jsonrpc_error ~id ~code:(-32601)
           ~message:(Printf.sprintf "Unknown tool: %s" tool_name))
    | Some tool ->
      let arguments =
        try params |> member "arguments"
        with _ -> `Assoc []
      in
      let* result =
        Lwt.catch
          (fun () ->
            let* output = tool.invoke arguments in
            Lwt.return (tool_result ~content:output ~is_error:false))
          (fun exn ->
            Lwt.return
              (tool_result ~content:(Printexc.to_string exn) ~is_error:true))
      in
      Lwt.return_some (jsonrpc_response ~id result))
  | Some id, _ ->
    Lwt.return_some
      (jsonrpc_error ~id ~code:(-32601)
         ~message:(Printf.sprintf "Unknown method: %s" method_))

let run ~(registry : Tool_registry.t) () =
  let open Lwt.Syntax in
  let starts_with_ci ~prefix s =
    let p = String.lowercase_ascii prefix in
    let v = String.lowercase_ascii s in
    String.length v >= String.length p
    && String.sub v 0 (String.length p) = p
  in
  let parse_content_length line =
    match String.index_opt line ':' with
    | None -> None
    | Some i ->
      let n = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
      int_of_string_opt n
  in
  let rec read_until_blank () =
    let* line = Lwt_io.read_line_opt Lwt_io.stdin in
    match line with
    | None -> Lwt.return_unit
    | Some l ->
      if String.trim l = "" then Lwt.return_unit
      else read_until_blank ()
  in
  let rec read_message () =
    let* first = Lwt_io.read_line_opt Lwt_io.stdin in
    match first with
    | None -> Lwt.return_none
    | Some line ->
      let trimmed = String.trim line in
      if trimmed = "" then
        read_message ()
      else if starts_with_ci ~prefix:"Content-Length:" trimmed then
        (match parse_content_length trimmed with
         | None -> Lwt.return_none
         | Some len ->
           let* () = read_until_blank () in
           let* body = Lwt_io.read ~count:len Lwt_io.stdin in
           if String.length body = len then Lwt.return_some body else Lwt.return_none)
      else
        Lwt.return_some line
  in
  let write_message json =
    let body = Yojson.Safe.to_string json in
    let framed =
      Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body
    in
    let* () = Lwt_io.write Lwt_io.stdout framed in
    Lwt_io.flush Lwt_io.stdout
  in
  let rec loop () =
    let* msg = read_message () in
    match msg with
    | None -> Lwt.return_unit
    | Some line -> (
      let json =
        try Ok (Yojson.Safe.from_string line) with exn -> Error exn
      in
      match json with
      | Error _ ->
        let err =
          jsonrpc_error ~id:`Null ~code:(-32700) ~message:"Parse error"
        in
        let* () = write_message err in
        loop ()
      | Ok json ->
        let* response = handle_request ~registry json in
        let* () =
          match response with
          | None -> Lwt.return_unit
          | Some resp -> write_message resp
        in
        loop ())
  in
  loop ()
