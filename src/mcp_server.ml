let server_info =
  `Assoc
    [
      ("name", `String "clawq");
      ("version", `String "0.1.0");
    ]

let capabilities = `Assoc [ ("tools", `Assoc []) ]

let transcribe_schema =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ( "file_path",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "Path to the audio file to transcribe");
                ] );
            ( "language",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "Language code (e.g. 'en')");
                ] );
          ] );
      ("required", `List [ `String "file_path" ]);
    ]

let tools =
  [
    `Assoc
      [
        ("name", `String "transcribe");
        ( "description",
          `String "Transcribe an audio file to text using speech-to-text" );
        ("inputSchema", transcribe_schema);
      ];
  ]

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

let handle_transcribe ~(config : Runtime_config.t) params =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let file_path =
    try params |> member "arguments" |> member "file_path" |> to_string
    with _ -> ""
  in
  if file_path = "" then
    Lwt.return (tool_result ~content:"file_path is required" ~is_error:true)
  else
    Lwt.catch
      (fun () ->
        let data =
          let ic = open_in_bin file_path in
          let n = in_channel_length ic in
          let buf = Bytes.create n in
          really_input ic buf 0 n;
          close_in ic;
          Bytes.to_string buf
        in
        let filename = Filename.basename file_path in
        let content_type = Stt.content_type_of_ext filename in
        let* result =
          Stt.transcribe ~config ~audio_data:data ~filename ~content_type ()
        in
        Lwt.return (tool_result ~content:result.text ~is_error:false))
      (fun exn ->
        Lwt.return
          (tool_result ~content:(Printexc.to_string exn) ~is_error:true))

let handle_request ~(config : Runtime_config.t) json =
  let open Yojson.Safe.Util in
  let id = try Some (json |> member "id") with _ -> None in
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
    let result = `Assoc [ ("tools", `List tools) ] in
    Lwt.return_some (jsonrpc_response ~id result)
  | Some id, "tools/call" -> (
    let open Lwt.Syntax in
    let tool_name =
      try params |> member "name" |> to_string with _ -> ""
    in
    match tool_name with
    | "transcribe" ->
      let* result = handle_transcribe ~config params in
      Lwt.return_some (jsonrpc_response ~id result)
    | _ ->
      Lwt.return_some
        (jsonrpc_error ~id ~code:(-32601)
           ~message:(Printf.sprintf "Unknown tool: %s" tool_name)))
  | Some id, _ ->
    Lwt.return_some
      (jsonrpc_error ~id ~code:(-32601)
         ~message:(Printf.sprintf "Unknown method: %s" method_))

let run ~(config : Runtime_config.t) () =
  let open Lwt.Syntax in
  let rec loop () =
    let* line =
      Lwt.catch
        (fun () ->
          let* l = Lwt_io.read_line Lwt_io.stdin in
          Lwt.return_some l)
        (fun _ -> Lwt.return_none)
    in
    match line with
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
        let* () =
          Lwt_io.write_line Lwt_io.stdout (Yojson.Safe.to_string err)
        in
        loop ()
      | Ok json ->
        let* response = handle_request ~config json in
        let* () =
          match response with
          | None -> Lwt.return_unit
          | Some resp ->
            Lwt_io.write_line Lwt_io.stdout (Yojson.Safe.to_string resp)
        in
        loop ())
  in
  loop ()
