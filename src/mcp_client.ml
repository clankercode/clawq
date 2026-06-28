type server_config = {
  name : string;
  command : string;
  args : string list;
  env : (string * string) list;
  credential_handle : string option;
      (** Optional credential handle ID. When set, the MCP connection resolves
          credentials through the snapshot-scoped lease API. Missing or
          unauthorized handles deny connection before any network call. *)
}

type io_transport = {
  process : Lwt_process.process_full;
  stderr_drain : unit Lwt.t;
}

type http_transport = {
  url : string;
  headers : (string * string) list;
  post :
    url:string ->
    headers:(string * string) list ->
    body:string ->
    (int * string * string) Lwt.t;
      (** Returns [(status, body, content_type)]. *)
}

type transport = Stdio of io_transport | Http of http_transport

type t = {
  config : server_config;
  transport : transport;
  mutable next_id : int;
  mutable discovered : Tool.t list;
}

let default_startup_timeout_s = 10.
let cleanup_timeout_s = 0.2

let is_http_url s =
  Mcp_transport.starts_with_ci ~prefix:"http://" s
  || Mcp_transport.starts_with_ci ~prefix:"https://" s

let read_message = Mcp_transport.read_message
let write_message = Mcp_transport.write_message
let frame_message = Mcp_transport.frame_message

let rec drain_channel ic =
  let open Lwt.Syntax in
  let* chunk = Lwt_io.read ~count:4096 ic in
  if chunk = "" then Lwt.return_unit else drain_channel ic

let jsonrpc_request ~id ~method_ ~params =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("method", `String method_);
      ("params", params);
    ]

let jsonrpc_notification ~method_ ~params =
  `Assoc
    [
      ("jsonrpc", `String "2.0"); ("method", `String method_); ("params", params);
    ]

let with_timeout ~seconds ~label promise =
  Lwt.catch
    (fun () -> Lwt_unix.with_timeout seconds (fun () -> promise))
    (function
      | Lwt_unix.Timeout ->
          Lwt.fail_with
            (Printf.sprintf "MCP client: %s timed out after %.0f seconds" label
               seconds)
      | exn -> Lwt.fail exn)

let wait_for_completion ~seconds promise =
  Lwt.catch
    (fun () ->
      let open Lwt.Syntax in
      let* () = Lwt_unix.with_timeout seconds (fun () -> promise) in
      Lwt.return true)
    (function Lwt_unix.Timeout -> Lwt.return false | _ -> Lwt.return false)

let cleanup_stdio_transport { process; stderr_drain } =
  let open Lwt.Syntax in
  let send_signal signal =
    Lwt.catch
      (fun () ->
        process#kill signal;
        Lwt.return_unit)
      (fun _ -> Lwt.return_unit)
  in
  let* () = send_signal Sys.sigterm in
  let* exited =
    wait_for_completion ~seconds:cleanup_timeout_s
      (let* _ = process#status in
       Lwt.return_unit)
  in
  let* () = if exited then Lwt.return_unit else send_signal Sys.sigkill in
  let* () =
    if exited then Lwt.return_unit
    else
      let* _ =
        wait_for_completion ~seconds:cleanup_timeout_s
          (let* _ = process#status in
           Lwt.return_unit)
      in
      Lwt.return_unit
  in
  let* _ = wait_for_completion ~seconds:cleanup_timeout_s stderr_drain in
  Lwt.return_unit

let default_http_post ~url ~headers ~body =
  let open Lwt.Syntax in
  let headers =
    Cohttp.Header.of_list
      (("Content-Type", "application/json")
      :: ("Accept", "application/json, text/event-stream")
      :: headers)
  in
  let* response, response_body =
    Cohttp_lwt_unix.Client.post ~headers
      ~body:(Cohttp_lwt.Body.of_string body)
      (Uri.of_string url)
  in
  let* response_body = Cohttp_lwt.Body.to_string response_body in
  let status = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
  let content_type =
    Cohttp.Header.get (Cohttp.Response.headers response) "content-type"
    |> Option.value ~default:"application/json"
  in
  Lwt.return (status, response_body, content_type)

let parse_sse_body body =
  let flush_event ~data_lines ~events =
    match List.rev !data_lines with
    | [] -> ()
    | lines -> (
        data_lines := [];
        let data = String.concat "\n" lines |> String.trim in
        if data <> "" && data <> "[DONE]" then
          match Yojson.Safe.from_string data with
          | json -> events := json :: !events
          | exception _ -> ())
  in
  let data_lines = ref [] in
  let events = ref [] in
  String.split_on_char '\n' body
  |> List.iter (fun raw_line ->
      let line = String.trim raw_line in
      if line = "" then flush_event ~data_lines ~events
      else if Mcp_transport.starts_with_ci ~prefix:"data:" line then
        let data = String.trim (String.sub line 5 (String.length line - 5)) in
        data_lines := data :: !data_lines);
  flush_event ~data_lines ~events;
  match !events with json :: _ -> Some json | [] -> None

let parse_http_json_response ~response_body ~content_type =
  if String.trim response_body = "" then Ok None
  else if Mcp_transport.starts_with_ci ~prefix:"text/event-stream" content_type
  then Ok (parse_sse_body response_body)
  else
    match Yojson.Safe.from_string response_body with
    | json -> Ok (Some json)
    | exception exn -> Error exn

let send_http_json transport json =
  let open Lwt.Syntax in
  let body = Yojson.Safe.to_string json in
  let* status, response_body, content_type =
    transport.post ~url:transport.url ~headers:transport.headers ~body
  in
  if status < 200 || status >= 300 then
    Lwt.fail_with
      (Printf.sprintf "MCP client: HTTP %d from %s" status transport.url)
  else
    match parse_http_json_response ~response_body ~content_type with
    | Ok json -> Lwt.return json
    | Error exn ->
        Lwt.fail_with
          ("MCP client: failed to parse HTTP response: "
         ^ Printexc.to_string exn)

let send_request t ~method_ ~params =
  let open Lwt.Syntax in
  let id = t.next_id in
  t.next_id <- id + 1;
  let json = jsonrpc_request ~id ~method_ ~params in
  match t.transport with
  | Stdio transport ->
      let* () = write_message transport.process#stdin json in
      let* msg = read_message transport.process#stdout in
      begin match msg with
      | None -> Lwt.fail_with ("MCP client: no response for " ^ method_)
      | Some body -> (
          match Yojson.Safe.from_string body with
          | json -> Lwt.return json
          | exception exn ->
              Lwt.fail_with
                ("MCP client: failed to parse response: "
               ^ Printexc.to_string exn))
      end
  | Http transport ->
      let* resp = send_http_json transport json in
      begin match resp with
      | Some json -> Lwt.return json
      | None -> Lwt.fail_with ("MCP client: no response for " ^ method_)
      end

let send_notification t ~method_ ~params =
  let open Lwt.Syntax in
  let json = jsonrpc_notification ~method_ ~params in
  match t.transport with
  | Stdio transport -> write_message transport.process#stdin json
  | Http transport ->
      let* _ = send_http_json transport json in
      Lwt.return_unit

let initialize_params =
  `Assoc
    [
      ("protocolVersion", `String "2024-11-05");
      ( "clientInfo",
        `Assoc
          [ ("name", `String "clawq"); ("version", `String Build_info.version) ]
      );
      ("capabilities", `Assoc []);
    ]

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
    let invoke ?context:_ args =
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
        invoke_stream = None;
        risk_level = Tool.Medium;
        deferred = false;
      }
  with _ -> None

let server_config_of_json (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let credential_handle =
      try Some (json |> member "credential_handle" |> to_string)
      with _ -> None
    in
    match json |> member "url" with
    | `String url ->
        let headers =
          try
            json |> member "headers" |> to_assoc
            |> List.map (fun (k, v) -> (k, to_string v))
          with _ -> []
        in
        Ok { name; command = url; args = []; env = headers; credential_handle }
    | _ ->
        let command = json |> member "command" |> to_string in
        let args =
          try json |> member "args" |> to_list |> List.map to_string
          with _ -> []
        in
        let env =
          try
            json |> member "env" |> to_assoc
            |> List.map (fun (k, v) -> (k, to_string v))
          with _ -> []
        in
        Ok { name; command; args; env; credential_handle }
  with exn -> Error (Printexc.to_string exn)

let load_server_configs path =
  let json = Yojson.Safe.from_file path in
  let open Yojson.Safe.Util in
  let servers = try json |> to_list with _ -> [] in
  List.filter_map
    (fun server_json ->
      match server_config_of_json server_json with
      | Ok cfg -> Some cfg
      | Error msg ->
          Logs.warn (fun m -> m "MCP server config parse error: %s" msg);
          None)
    servers

let connect ?(startup_timeout_s = default_startup_timeout_s)
    ?(http_post = default_http_post) (cfg : server_config) =
  let open Lwt.Syntax in
  let transport =
    if is_http_url cfg.command then
      Http { url = cfg.command; headers = cfg.env; post = http_post }
    else
      let cmd_arr = Array.of_list (cfg.command :: cfg.args) in
      let env =
        let base = Unix.environment () |> Array.to_list in
        let extra = List.map (fun (k, v) -> k ^ "=" ^ v) cfg.env in
        Array.of_list (base @ extra)
      in
      let process = Lwt_process.open_process_full ~env ("", cmd_arr) in
      let stderr_drain =
        Lwt.catch
          (fun () -> drain_channel process#stderr)
          (fun _ -> Lwt.return_unit)
      in
      Lwt.async (fun () -> stderr_drain);
      Stdio { process; stderr_drain }
  in
  let client = { config = cfg; transport; next_id = 1; discovered = [] } in
  let label =
    if is_http_url cfg.command then "HTTP startup" else "startup handshake"
  in
  let cleanup () =
    match transport with
    | Http _ -> Lwt.return_unit
    | Stdio io -> cleanup_stdio_transport io
  in
  Logs.info (fun m -> m "MCP client connecting to %s (%s)" cfg.name cfg.command);
  Lwt.catch
    (fun () ->
      let startup =
        let* init_resp =
          send_request client ~method_:"initialize" ~params:initialize_params
        in
        ignore init_resp;
        let* () =
          send_notification client ~method_:"notifications/initialized"
            ~params:(`Assoc [])
        in
        let* tools_resp =
          send_request client ~method_:"tools/list" ~params:(`Assoc [])
        in
        let open Yojson.Safe.Util in
        let tools_json =
          try tools_resp |> member "result" |> member "tools" |> to_list
          with _ -> []
        in
        let tools =
          List.filter_map (tool_of_mcp_definition ~client) tools_json
        in
        client.discovered <- tools;
        Logs.info (fun m ->
            m "MCP client %s: discovered %d tools" cfg.name (List.length tools));
        Lwt.return client
      in
      with_timeout ~seconds:startup_timeout_s ~label startup)
    (fun exn ->
      let* () = cleanup () in
      Lwt.fail exn)

let discovered_tools t = t.discovered

let disconnect t =
  match t.transport with
  | Http _ -> Lwt.return_unit
  | Stdio io -> cleanup_stdio_transport io

(** [resolve_mcp_server_credentials ~config ~snapshot cfg] resolves the
    credential handle for an MCP server through the snapshot-scoped lease API.
    Returns [Ok env] with the resolved headers/env vars, or [Error msg] if the
    handle is missing or unauthorized. When [cfg.credential_handle] is [None],
    returns [Ok cfg.env] (legacy path). *)
let resolve_mcp_server_credentials ~(config : Runtime_config.t)
    ~(snapshot : Access_snapshot.t) (cfg : server_config) :
    ((string * string) list, string) result =
  match cfg.credential_handle with
  | None -> Ok cfg.env
  | Some handle_id -> (
      (* Resolve as Authorization header for HTTP, then convert to env/header
         pairs *)
      match
        Credential_lease.resolve_snapshot_lease ~config ~snapshot ~handle_id
          ~header_name:"Authorization"
      with
      | Error err ->
          let msg = Credential_lease.resolution_error_to_string err in
          Logs.err (fun m ->
              m "MCP server '%s': credential lease denied for handle '%s': %s"
                cfg.name handle_id msg);
          Error msg
      | Ok lease ->
          let result = ref [] in
          Credential_lease.apply_headers lease (fun headers ->
              result := headers);
          Ok !result)

(** [connect_with_policy ~config ~snapshot ?startup_timeout_s ?http_post cfg]
    connects to an MCP server after resolving credentials through policy. If
    [cfg.credential_handle] is set, credentials are resolved through the
    snapshot-scoped lease API. Missing or unauthorized handles deny connection
    before any network call. *)
let connect_with_policy ~(config : Runtime_config.t)
    ~(snapshot : Access_snapshot.t) ?startup_timeout_s ?http_post
    (cfg : server_config) =
  let open Lwt.Syntax in
  match resolve_mcp_server_credentials ~config ~snapshot cfg with
  | Error msg ->
      Lwt.fail_with
        (Printf.sprintf "MCP server '%s': credential policy denied: %s" cfg.name
           msg)
  | Ok resolved_env ->
      (* Apply resolved credentials to the config *)
      let cfg_with_creds =
        if cfg.credential_handle <> None then { cfg with env = resolved_env }
        else cfg
      in
      connect ?startup_timeout_s ?http_post cfg_with_creds
