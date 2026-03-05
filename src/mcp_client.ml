type server_config = {
  name : string;
  command : string;
  args : string list;
  env : (string * string) list;
}

type t = {
  config : server_config;
  process : Lwt_process.process_full;
  mutable next_id : int;
  mutable discovered : Tool.t list;
}

let frame_message json =
  let body = Yojson.Safe.to_string json in
  Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body

let starts_with_ci ~prefix s =
  let p = String.lowercase_ascii prefix in
  let v = String.lowercase_ascii s in
  String.length v >= String.length p && String.sub v 0 (String.length p) = p

let parse_content_length line =
  match String.index_opt line ':' with
  | None -> None
  | Some i ->
      let n =
        String.trim (String.sub line (i + 1) (String.length line - i - 1))
      in
      int_of_string_opt n

let read_message ic =
  let open Lwt.Syntax in
  let rec read_until_blank () =
    let* line = Lwt_io.read_line_opt ic in
    match line with
    | None -> Lwt.return_unit
    | Some l ->
        if String.trim l = "" then Lwt.return_unit else read_until_blank ()
  in
  let rec try_read () =
    let* first = Lwt_io.read_line_opt ic in
    match first with
    | None -> Lwt.return_none
    | Some line ->
        let trimmed = String.trim line in
        if trimmed = "" then try_read ()
        else if starts_with_ci ~prefix:"Content-Length:" trimmed then
          match parse_content_length trimmed with
          | None -> Lwt.return_none
          | Some len ->
              let* () = read_until_blank () in
              let* body = Lwt_io.read ~count:len ic in
              if String.length body = len then Lwt.return_some body
              else Lwt.return_none
        else Lwt.return_some line
  in
  try_read ()

let write_message oc json =
  let open Lwt.Syntax in
  let framed = frame_message json in
  let* () = Lwt_io.write oc framed in
  Lwt_io.flush oc

let send_request t ~method_ ~params =
  let open Lwt.Syntax in
  let id = t.next_id in
  t.next_id <- id + 1;
  let json =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int id);
        ("method", `String method_);
        ("params", params);
      ]
  in
  let* () = write_message t.process#stdin json in
  let* msg = read_message t.process#stdout in
  match msg with
  | None -> Lwt.fail_with ("MCP client: no response for " ^ method_)
  | Some body -> (
      match Yojson.Safe.from_string body with
      | json -> Lwt.return json
      | exception exn ->
          Lwt.fail_with
            ("MCP client: failed to parse response: " ^ Printexc.to_string exn))

let tool_of_mcp_definition ~client (tool_json : Yojson.Safe.t) : Tool.t option =
  let open Yojson.Safe.Util in
  try
    let name = tool_json |> member "name" |> to_string in
    let description =
      try tool_json |> member "description" |> to_string with _ -> ""
    in
    let parameters_schema =
      try tool_json |> member "inputSchema" with _ -> `Assoc []
    in
    let invoke args =
      let open Lwt.Syntax in
      let* resp =
        send_request client ~method_:"tools/call"
          ~params:(`Assoc [ ("name", `String name); ("arguments", args) ])
      in
      let result = try resp |> member "result" with _ -> `Null in
      let content =
        try
          let content_list = result |> member "content" |> to_list in
          List.filter_map
            (fun item ->
              try Some (item |> member "text" |> to_string) with _ -> None)
            content_list
          |> String.concat "\n"
        with _ -> Yojson.Safe.to_string result
      in
      let is_error =
        try result |> member "isError" |> to_bool with _ -> false
      in
      if is_error then Lwt.return ("Error: " ^ content) else Lwt.return content
    in
    Some
      {
        Tool.name;
        description;
        parameters_schema;
        invoke;
        risk_level = Tool.Medium;
      }
  with _ -> None

let connect (cfg : server_config) =
  let open Lwt.Syntax in
  let cmd_arr = Array.of_list (cfg.command :: cfg.args) in
  let env =
    let base = Unix.environment () |> Array.to_list in
    let extra = List.map (fun (k, v) -> k ^ "=" ^ v) cfg.env in
    Array.of_list (base @ extra)
  in
  let process = Lwt_process.open_process_full ~env ("", cmd_arr) in
  let client = { config = cfg; process; next_id = 1; discovered = [] } in
  Logs.info (fun m -> m "MCP client connecting to %s (%s)" cfg.name cfg.command);
  let* init_resp =
    send_request client ~method_:"initialize"
      ~params:
        (`Assoc
           [
             ("protocolVersion", `String "2024-11-05");
             ( "clientInfo",
               `Assoc
                 [ ("name", `String "clawq"); ("version", `String "0.1.0") ] );
             ("capabilities", `Assoc []);
           ])
  in
  ignore init_resp;
  (* Send initialized notification *)
  let notif =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("method", `String "notifications/initialized");
        ("params", `Assoc []);
      ]
  in
  let* () = write_message client.process#stdin notif in
  let* tools_resp =
    send_request client ~method_:"tools/list" ~params:(`Assoc [])
  in
  let open Yojson.Safe.Util in
  let tools_json =
    try tools_resp |> member "result" |> member "tools" |> to_list
    with _ -> []
  in
  let tools = List.filter_map (tool_of_mcp_definition ~client) tools_json in
  client.discovered <- tools;
  Logs.info (fun m ->
      m "MCP client %s: discovered %d tools" cfg.name (List.length tools));
  Lwt.return client

let discovered_tools t = t.discovered

let disconnect t =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      t.process#kill Sys.sigterm;
      let* _status = t.process#status in
      Lwt.return_unit)
    (fun _exn -> Lwt.return_unit)
