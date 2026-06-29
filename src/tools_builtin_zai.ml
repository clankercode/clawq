(* ───── Z.ai MCP tools ─────
   Implements MCP lifecycle inline (Mcp_client is in clawq_runtime_integrations,
   unavailable from core). Handshake: initialize → tools/list → tools/call.
   Falls back to direct tools/call with hardcoded tool names if discovery fails.
   Auth: Bearer token from zai_mcp.api_key (auto-detected from providers if absent).
   Web Search endpoint: https://api.z.ai/api/mcp/web_search_prime/mcp
   Web Reader endpoint: https://api.z.ai/api/mcp/web_reader/mcp *)

open Tools_builtin_util

let zai_starts_with_ci ~prefix s =
  let p = String.lowercase_ascii prefix in
  let v = String.lowercase_ascii s in
  String.length v >= String.length p && String.sub v 0 (String.length p) = p

let zai_parse_sse_body body =
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
      else if zai_starts_with_ci ~prefix:"data:" line then
        let data = String.trim (String.sub line 5 (String.length line - 5)) in
        data_lines := data :: !data_lines);
  flush_event ~data_lines ~events;
  match !events with json :: _ -> Some json | [] -> None

let zai_mcp_call ~http_post ~api_key ~endpoint ~tool_name ~arguments =
  let open Lwt.Syntax in
  let id = Random.bits () land 0xFFFF in
  let body =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int id);
        ("method", `String "tools/call");
        ( "params",
          `Assoc [ ("name", `String tool_name); ("arguments", arguments) ] );
      ]
    |> Yojson.Safe.to_string
  in
  let* status, resp_body, content_type =
    http_post ~uri:endpoint
      ~headers:[ ("Authorization", "Bearer " ^ api_key) ]
      ~body
  in
  if status < 200 || status >= 300 then
    Lwt.return
      (Printf.sprintf "Error: Z.ai MCP returned HTTP %d: %s" status resp_body)
  else
    let open Yojson.Safe.Util in
    let json_result =
      if String.trim resp_body = "" then Ok `Empty_body
      else if zai_starts_with_ci ~prefix:"text/event-stream" content_type then
        match zai_parse_sse_body resp_body with
        | Some json -> Ok (`Json json)
        | None -> Ok `Empty_sse
      else
        match Yojson.Safe.from_string resp_body with
        | json -> Ok (`Json json)
        | exception exn -> Error exn
    in
    match json_result with
    | Ok `Empty_body ->
        Lwt.return "Error: Z.ai MCP returned an empty response body."
    | Ok `Empty_sse ->
        Lwt.return
          "Error: Z.ai MCP returned an SSE response without a JSON payload."
    | Ok (`Json json) ->
        let rpc_error = try json |> member "error" with _ -> `Null in
        if rpc_error <> `Null then
          let msg =
            try rpc_error |> member "message" |> to_string
            with _ -> Yojson.Safe.to_string rpc_error
          in
          Lwt.return ("Error: Z.ai MCP error: " ^ msg)
        else
          let result = try json |> member "result" with _ -> `Null in
          let is_error =
            try result |> member "isError" |> to_bool with _ -> false
          in
          let content =
            try
              result |> member "content" |> to_list
              |> List.filter_map (fun item ->
                  try Some (item |> member "text" |> to_string) with _ -> None)
              |> String.concat "\n"
            with _ -> ( try Yojson.Safe.to_string result with _ -> resp_body)
          in
          if is_error then Lwt.return ("Error: " ^ content)
          else Lwt.return content
    | Error _exn -> Lwt.return "Error: Z.ai MCP returned malformed JSON."

let zai_default_http_post ~uri ~headers ~body =
  let open Lwt.Syntax in
  let* status, resp_headers, resp_body =
    Http_client.post_json_with_headers ~uri ~headers ~body
  in
  let content_type =
    Cohttp.Header.get resp_headers "content-type"
    |> Option.value ~default:"application/json"
  in
  Lwt.return (status, resp_body, content_type)

let zai_mcp_api_key (config : Runtime_config.t) =
  match config.zai_mcp with
  | Some cfg when Runtime_config.is_key_set cfg.key -> cfg.key
  | _ -> ""

(* MCP lifecycle state: discovered tool name + cache key + timestamp *)
type zai_discovery_cache = (string * string * float) option ref

let zai_jsonrpc_request ~id ~method_ ~params =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("method", `String method_);
      ("params", params);
    ]

let zai_jsonrpc_notification ~method_ ~params =
  `Assoc
    [
      ("jsonrpc", `String "2.0"); ("method", `String method_); ("params", params);
    ]

let zai_initialize_params =
  `Assoc
    [
      ("protocolVersion", `String "2024-11-05");
      ( "clientInfo",
        `Assoc
          [ ("name", `String "clawq"); ("version", `String Build_info.version) ]
      );
      ("capabilities", `Assoc []);
    ]

(* Perform MCP initialize → notifications/initialized → tools/list handshake.
   Returns the name of the first discovered tool, or Error. *)
let zai_discover_tool ~http_post ~api_key ~endpoint =
  let open Lwt.Syntax in
  let headers = [ ("Authorization", "Bearer " ^ api_key) ] in
  let send_json json =
    http_post ~uri:endpoint ~headers ~body:(Yojson.Safe.to_string json)
  in
  let* status, _body, _ct =
    send_json
      (zai_jsonrpc_request ~id:1 ~method_:"initialize"
         ~params:zai_initialize_params)
  in
  if status < 200 || status >= 300 then
    Lwt.return_error (Printf.sprintf "initialize returned HTTP %d" status)
  else
    let* _status, _body, _ct =
      send_json
        (zai_jsonrpc_notification ~method_:"notifications/initialized"
           ~params:(`Assoc []))
    in
    let* status, body, content_type =
      send_json
        (zai_jsonrpc_request ~id:2 ~method_:"tools/list" ~params:(`Assoc []))
    in
    if status < 200 || status >= 300 then
      Lwt.return_error (Printf.sprintf "tools/list returned HTTP %d" status)
    else
      let open Yojson.Safe.Util in
      let parse_json resp_body ct =
        if zai_starts_with_ci ~prefix:"text/event-stream" ct then
          zai_parse_sse_body resp_body
        else
          match Yojson.Safe.from_string resp_body with
          | json -> Some json
          | exception _ -> None
      in
      match parse_json body content_type with
      | Some json -> (
          let tools =
            try json |> member "result" |> member "tools" |> to_list
            with _ -> []
          in
          match tools with
          | tool_json :: _ -> (
              try
                let name = tool_json |> member "name" |> to_string in
                Lwt.return_ok name
              with _ -> Lwt.return_error "could not parse tool name")
          | [] -> Lwt.return_error "tools/list returned empty list")
      | None -> Lwt.return_error "could not parse tools/list response"

(* Get the discovered tool name, using cache when valid *)
let zai_get_discovered_tool_name ~http_post ~api_key ~endpoint
    ~(cache : zai_discovery_cache) =
  let open Lwt.Syntax in
  match !cache with
  | Some (tool_name, cached_key, ts)
    when cached_key = api_key && Unix.gettimeofday () -. ts < 3600.0 ->
      Lwt.return_ok tool_name
  | _ ->
      Lwt.catch
        (fun () ->
          let* result = zai_discover_tool ~http_post ~api_key ~endpoint in
          match result with
          | Ok tool_name ->
              cache := Some (tool_name, api_key, Unix.gettimeofday ());
              Lwt.return_ok tool_name
          | Error msg ->
              Logs.warn (fun m ->
                  m "Z.ai MCP discovery failed for %s: %s" endpoint msg);
              Lwt.return_error msg)
        (fun exn ->
          Logs.warn (fun m ->
              m "Z.ai MCP discovery failed for %s: %s" endpoint
                (Printexc.to_string exn));
          Lwt.return_error (Printexc.to_string exn))

let zai_websearch_with_post ~http_post ~(config : Runtime_config.t) =
  let discovery_cache : zai_discovery_cache = ref None in
  {
    Tool.name = "zai_websearch";
    description =
      "Search the web via Z.ai and return results with titles, URLs, and \
       summaries. Uses Z.ai's web search MCP tool.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "query",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Search query (required)");
                    ] );
              ] );
          ("required", `List [ `String "query" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let query = try args |> member "query" |> to_string with _ -> "" in
        if query = "" then
          Lwt.return
            "Error: parameter \"query\" is required. Provide a search query \
             string, e.g. {\"query\": \"OCaml MCP server\"}."
        else
          (* Resolve API key through credential lease API when handle is set *)
          let credential_handle =
            match config.zai_mcp with
            | Some zm -> zm.credential_handle
            | None -> None
          in
          let api_key_result =
            resolve_credential_handle ~config
              ~handle_id:credential_handle ~header_name:"Authorization"
          in
          let lease_key =
            match api_key_result with
            | Ok key -> key
            | Error _ -> ""
          in
          let api_key =
            if lease_key <> "" then lease_key else zai_mcp_api_key config
          in
          if not (Runtime_config.is_key_set api_key) then
            Lwt.return
              "Error: Z.ai API key not configured. Add a \"zai_mcp\" section \
               to ~/.clawq/config.json with \"enabled\": true, or set \
               providers.zai.api_key / providers.zai_coding.api_key."
          else
            let open Lwt.Syntax in
            let endpoint = "https://api.z.ai/api/mcp/web_search_prime/mcp" in
            let arguments = `Assoc [ ("query", `String query) ] in
            let* discovered =
              zai_get_discovered_tool_name ~http_post ~api_key ~endpoint
                ~cache:discovery_cache
            in
            let tool_name =
              match discovered with
              | Ok name -> name
              | Error _ -> "webSearchPrime"
            in
            Lwt.catch
              (fun () ->
                zai_mcp_call ~http_post ~api_key ~endpoint ~tool_name ~arguments)
              (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let zai_websearch ~(config : Runtime_config.t) =
  zai_websearch_with_post ~http_post:zai_default_http_post ~config

let zai_webfetch_with_post ~http_post ~(config : Runtime_config.t) =
  let discovery_cache : zai_discovery_cache = ref None in
  {
    Tool.name = "zai_webfetch";
    description =
      "Fetch a webpage and return its content (title, main text, links) via \
       Z.ai's web reader MCP tool. Better than web_fetch for structured page \
       extraction.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "url",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "URL of the webpage to fetch (required)" );
                    ] );
              ] );
          ("required", `List [ `String "url" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let url = try args |> member "url" |> to_string with _ -> "" in
        if url = "" then
          Lwt.return
            "Error: parameter \"url\" is required. Provide a fully-formed URL \
             string, e.g. {\"url\": \"https://example.com\"}."
        else
          (* Resolve API key through credential lease API when handle is set *)
          let credential_handle =
            match config.zai_mcp with
            | Some zm -> zm.credential_handle
            | None -> None
          in
          let api_key_result =
            resolve_credential_handle ~config
              ~handle_id:credential_handle ~header_name:"Authorization"
          in
          let lease_key =
            match api_key_result with
            | Ok key -> key
            | Error _ -> ""
          in
          let api_key =
            if lease_key <> "" then lease_key else zai_mcp_api_key config
          in
          if not (Runtime_config.is_key_set api_key) then
            Lwt.return
              "Error: Z.ai API key not configured. Add a \"zai_mcp\" section \
               to ~/.clawq/config.json with \"enabled\": true, or set \
               providers.zai.api_key / providers.zai_coding.api_key."
          else
            let open Lwt.Syntax in
            let endpoint = "https://api.z.ai/api/mcp/web_reader/mcp" in
            let arguments = `Assoc [ ("url", `String url) ] in
            let* discovered =
              zai_get_discovered_tool_name ~http_post ~api_key ~endpoint
                ~cache:discovery_cache
            in
            let tool_name =
              match discovered with Ok name -> name | Error _ -> "webReader"
            in
            Lwt.catch
              (fun () ->
                zai_mcp_call ~http_post ~api_key ~endpoint ~tool_name ~arguments)
              (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let zai_webfetch ~(config : Runtime_config.t) =
  zai_webfetch_with_post ~http_post:zai_default_http_post ~config
