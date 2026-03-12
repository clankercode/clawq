open Tools_builtin_util
open Tools_builtin_io

let http_request ~workspace_only =
  {
    Tool.name = "http_request";
    description =
      (if workspace_only then
         "Make an HTTP request with configurable method, headers, and body \
          (workspace policy: localhost only, truncated at 20KB). For reading \
          web pages use web_fetch."
       else
         "Make an HTTP request with configurable method \
          (GET/POST/PUT/PATCH/DELETE), headers, and body. Returns raw response \
          (truncated at 20KB). For reading web pages use web_fetch.");
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
                      ("description", `String "Request URL");
                    ] );
                ( "method",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "enum",
                        `List
                          [
                            `String "GET";
                            `String "POST";
                            `String "PUT";
                            `String "PATCH";
                            `String "DELETE";
                          ] );
                      ("description", `String "HTTP method (default: GET)");
                    ] );
                ( "headers",
                  `Assoc
                    [
                      ("type", `String "object");
                      ( "description",
                        `String "Request headers as key-value pairs" );
                      ( "additionalProperties",
                        `Assoc [ ("type", `String "string") ] );
                    ] );
                ( "body",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Request body (for POST/PUT/PATCH)" );
                    ] );
              ] );
          ("required", `List [ `String "url" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let url = try args |> member "url" |> to_string with _ -> "" in
        let meth =
          try String.uppercase_ascii (args |> member "method" |> to_string)
          with _ -> "GET"
        in
        let headers =
          try
            args |> member "headers" |> to_assoc
            |> List.filter_map (fun (k, v) ->
                try Some (k, to_string v) with _ -> None)
          with _ -> []
        in
        let body = try args |> member "body" |> to_string with _ -> "" in
        if url = "" then Lwt.return "Error: url is required"
        else if workspace_only && not (is_localhost_url url) then
          Lwt.return "Error: workspace policy restricts HTTP to localhost only"
        else
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let* status, resp_body =
                match meth with
                | "POST" -> Http_client.post_json ~uri:url ~headers ~body
                | "PUT" -> Http_client.put_json ~uri:url ~headers ~body
                | "PATCH" -> Http_client.patch_json ~uri:url ~headers ~body
                | "DELETE" -> Http_client.delete ~uri:url ~headers ~body
                | "GET" | _ -> Http_client.get ~uri:url ~headers
              in
              let truncated =
                if String.length resp_body > 20000 then
                  String.sub resp_body 0 20000 ^ "\n... (truncated)"
                else resp_body
              in
              Lwt.return (Printf.sprintf "HTTP %d\n%s" status truncated))
            (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let strip_html_to_text html =
  let buf = Buffer.create (String.length html) in
  let len = String.length html in
  let i = ref 0 in
  (* Skip until pattern (case-insensitive prefix match) *)
  let skip_to_close_tag tag =
    let close = "</" ^ tag ^ ">" in
    let cl = String.length close in
    let found = ref false in
    while (not !found) && !i + cl <= len do
      let sub = String.sub html !i cl |> String.lowercase_ascii in
      if sub = close then begin
        found := true;
        i := !i + cl
      end
      else incr i
    done
  in
  while !i < len do
    if html.[!i] = '<' then begin
      let remaining =
        let n = min 8 (len - !i) in
        String.sub html !i n |> String.lowercase_ascii
      in
      let is_prefix p =
        String.length remaining >= String.length p
        && String.sub remaining 0 (String.length p) = p
      in
      if is_prefix "<script" then begin
        while !i < len && html.[!i] <> '>' do
          incr i
        done;
        if !i < len then incr i;
        skip_to_close_tag "script"
      end
      else if is_prefix "<style" then begin
        while !i < len && html.[!i] <> '>' do
          incr i
        done;
        if !i < len then incr i;
        skip_to_close_tag "style"
      end
      else begin
        while !i < len && html.[!i] <> '>' do
          incr i
        done;
        if !i < len then begin
          Buffer.add_char buf '\n';
          incr i
        end
      end
    end
    else begin
      Buffer.add_char buf html.[!i];
      incr i
    end
  done;
  let s = Buffer.contents buf in
  (* Decode common HTML entities without regex *)
  let replace_substr src find rep =
    let fl = String.length find in
    let sl = String.length src in
    if fl = 0 then src
    else begin
      let b = Buffer.create sl in
      let j = ref 0 in
      while !j <= sl - fl do
        if String.sub src !j fl = find then begin
          Buffer.add_string b rep;
          j := !j + fl
        end
        else begin
          Buffer.add_char b src.[!j];
          incr j
        end
      done;
      while !j < sl do
        Buffer.add_char b src.[!j];
        incr j
      done;
      Buffer.contents b
    end
  in
  let s = replace_substr s "&amp;" "&" in
  let s = replace_substr s "&lt;" "<" in
  let s = replace_substr s "&gt;" ">" in
  let s = replace_substr s "&quot;" "\"" in
  let s = replace_substr s "&apos;" "'" in
  let s = replace_substr s "&nbsp;" " " in
  (* Collapse runs of whitespace to single newlines *)
  let out = Buffer.create (String.length s) in
  let prev_nl = ref true in
  String.iter
    (fun c ->
      if c = '\n' || c = '\r' || c = '\t' then begin
        if not !prev_nl then Buffer.add_char out '\n';
        prev_nl := true
      end
      else if c = ' ' then begin
        if not !prev_nl then Buffer.add_char out ' '
      end
      else begin
        Buffer.add_char out c;
        prev_nl := false
      end)
    s;
  String.trim (Buffer.contents out)

let web_fetch ~workspace_only =
  {
    Tool.name = "web_fetch";
    description =
      (if workspace_only then
         "Fetch a URL and return the page as readable text with HTML stripped \
          (workspace policy: localhost only, truncated at 20KB). For raw \
          responses use http_get or http_request."
       else
         "Fetch a URL and return the page as readable text with \
          HTML/scripts/styles stripped (truncated at 20KB). Best for reading \
          web pages. For raw API responses use http_get or http_request.");
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
                      ("description", `String "URL to fetch");
                    ] );
              ] );
          ("required", `List [ `String "url" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let url = try args |> member "url" |> to_string with _ -> "" in
        if url = "" then Lwt.return "Error: url is required"
        else if workspace_only && not (is_localhost_url url) then
          Lwt.return "Error: workspace policy restricts HTTP to localhost only"
        else
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let* status, body = Http_client.get ~uri:url ~headers:[] in
              if status >= 400 then
                Lwt.return (Printf.sprintf "Error: HTTP %d from %s" status url)
              else
                let text = strip_html_to_text body in
                let truncated =
                  if String.length text > 20000 then
                    String.sub text 0 20000 ^ "\n... (truncated)"
                  else text
                in
                Lwt.return truncated)
            (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let web_search ~(config : Runtime_config.t) =
  let ws_cfg = config.web_search in
  {
    Tool.name = "web_search";
    description =
      "Search the web and return a list of results with titles, URLs, and \
       snippets";
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
                      ("description", `String "Search query");
                    ] );
                ( "limit",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ("description", `String "Number of results (default 5)");
                    ] );
              ] );
          ("required", `List [ `String "query" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let query = try args |> member "query" |> to_string with _ -> "" in
        let limit =
          try args |> member "limit" |> to_int
          with _ -> (
            match ws_cfg with Some ws -> ws.num_results | None -> 5)
        in
        if query = "" then Lwt.return "Error: query is required"
        else
          match ws_cfg with
          | None ->
              Lwt.return
                "Error: web_search not configured. Add a \"web_search\" \
                 section to ~/.clawq/config.json with provider and api_key."
          | Some ws ->
              let provider = ws.search_provider in
              let api_key = ws.search_api_key in
              Lwt.catch
                (fun () ->
                  let open Lwt.Syntax in
                  let encoded_query =
                    (* Basic URL encoding for the query *)
                    let buf = Buffer.create (String.length query) in
                    String.iter
                      (fun c ->
                        match c with
                        | 'A' .. 'Z'
                        | 'a' .. 'z'
                        | '0' .. '9'
                        | '-' | '_' | '.' | '~' ->
                            Buffer.add_char buf c
                        | ' ' -> Buffer.add_char buf '+'
                        | c ->
                            Buffer.add_string buf
                              (Printf.sprintf "%%%02X" (Char.code c)))
                      query;
                    Buffer.contents buf
                  in
                  match provider with
                  | "brave" ->
                      let base =
                        match ws.search_base_url with
                        | Some u -> u
                        | None ->
                            "https://api.search.brave.com/res/v1/web/search"
                      in
                      let uri =
                        Printf.sprintf "%s?q=%s&count=%d" base encoded_query
                          limit
                      in
                      let* status, body =
                        Http_client.get ~uri
                          ~headers:
                            [
                              ("X-Subscription-Token", api_key);
                              ("Accept", "application/json");
                            ]
                      in
                      if status >= 400 then
                        Lwt.return
                          (Printf.sprintf "Error: Brave API returned HTTP %d"
                             status)
                      else
                        let json =
                          try Yojson.Safe.from_string body
                          with _ ->
                            `Assoc [ ("web", `Assoc [ ("results", `List []) ]) ]
                        in
                        let results =
                          try
                            json |> member "web" |> member "results" |> to_list
                          with _ -> []
                        in
                        let lines =
                          List.mapi
                            (fun i r ->
                              let title =
                                try r |> member "title" |> to_string
                                with _ -> "(no title)"
                              in
                              let url =
                                try r |> member "url" |> to_string
                                with _ -> ""
                              in
                              let snippet =
                                try r |> member "description" |> to_string
                                with _ -> ""
                              in
                              Printf.sprintf "%d. %s\n   %s\n   %s" (i + 1)
                                title url snippet)
                            results
                        in
                        Lwt.return
                          (if lines = [] then "No results found"
                           else String.concat "\n\n" lines)
                  | "ddg" | _ ->
                      (* DuckDuckGo instant answer API — free, no key needed *)
                      let base =
                        match ws.search_base_url with
                        | Some u -> u
                        | None -> "https://api.duckduckgo.com"
                      in
                      let uri =
                        Printf.sprintf
                          "%s/?q=%s&format=json&no_redirect=1&no_html=1" base
                          encoded_query
                      in
                      let* status, body =
                        Http_client.get ~uri
                          ~headers:[ ("Accept", "application/json") ]
                      in
                      if status >= 400 then
                        Lwt.return
                          (Printf.sprintf "Error: DDG API returned HTTP %d"
                             status)
                      else
                        let json =
                          try Yojson.Safe.from_string body with _ -> `Assoc []
                        in
                        let abstract =
                          try json |> member "AbstractText" |> to_string
                          with _ -> ""
                        in
                        let abstract_url =
                          try json |> member "AbstractURL" |> to_string
                          with _ -> ""
                        in
                        let related =
                          try json |> member "RelatedTopics" |> to_list
                          with _ -> []
                        in
                        let lines = ref [] in
                        List.iteri
                          (fun i topic ->
                            if i < limit then
                              try
                                let text =
                                  topic |> member "Text" |> to_string
                                in
                                let url =
                                  try topic |> member "FirstURL" |> to_string
                                  with _ -> ""
                                in
                                lines :=
                                  Printf.sprintf "%d. %s\n   %s" (i + 1) text
                                    url
                                  :: !lines
                              with _ -> ())
                          related;
                        let lines = List.rev !lines in
                        let lines =
                          if abstract <> "" then
                            Printf.sprintf "Answer: %s\n%s" abstract
                              abstract_url
                            :: lines
                          else lines
                        in
                        Lwt.return
                          (if lines = [] then
                             "No results found (DDG instant API has limited \
                              coverage; consider using provider: brave)"
                           else String.concat "\n\n" lines))
                (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

(* ───── Z.ai MCP tools ─────
   Implements MCP lifecycle inline (Mcp_client is in clawq_runtime_integrations,
   unavailable from core). Handshake: initialize → tools/list → tools/call.
   Falls back to direct tools/call with hardcoded tool names if discovery fails.
   Auth: Bearer token from zai_mcp.api_key (auto-detected from providers if absent).
   Web Search endpoint: https://api.z.ai/api/mcp/web_search_prime/mcp
   Web Reader endpoint: https://api.z.ai/api/mcp/web_reader/mcp *)

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
  let headers =
    Cohttp.Header.of_list
      (("Content-Type", "application/json")
      :: ("Accept", "application/json, text/event-stream")
      :: headers)
  in
  let* response, response_body =
    Cohttp_lwt_unix.Client.post ~headers
      ~body:(Cohttp_lwt.Body.of_string body)
      (Uri.of_string uri)
  in
  let* response_body = Cohttp_lwt.Body.to_string response_body in
  let status = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
  let content_type =
    Cohttp.Header.get (Cohttp.Response.headers response) "content-type"
    |> Option.value ~default:"application/json"
  in
  Lwt.return (status, response_body, content_type)

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
        `Assoc [ ("name", `String "clawq"); ("version", `String "0.1.0") ] );
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
                      ("description", `String "Search query");
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
          let api_key = zai_mcp_api_key config in
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
                      ("description", `String "URL of the webpage to fetch");
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
          let api_key = zai_mcp_api_key config in
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

(* ───── Git operations tool ───── *)

let sanitize_git_arg arg =
  let arg_low = String.lowercase_ascii arg in
  let dangerous_prefixes =
    [ "--exec="; "--upload-pack="; "--receive-pack="; "--pager="; "--editor=" ]
  in
  let ok_prefixes =
    not
      (List.exists
         (fun p ->
           String.length arg >= String.length p
           && String.sub arg_low 0 (String.length p) = p)
         dangerous_prefixes)
  in
  ok_prefixes
  && String.lowercase_ascii arg <> "--no-verify"
  && (not (contains_substr ~haystack:arg ~needle:"$(" ~case_sensitive:true))
  && (not (contains_substr ~haystack:arg ~needle:"`" ~case_sensitive:true))
  && (not (String.contains arg '|'))
  && (not (String.contains arg ';'))
  && (not (String.contains arg '>'))
  && not
       (arg = "-c" || arg = "-C"
       || String.length arg > 2
          && arg.[0] = '-'
          && (arg.[1] = 'c' || arg.[1] = 'C')
          && arg.[2] = '=')

let git_operations ~workspace =
  {
    Tool.name = "git_operations";
    description =
      "Perform structured Git operations: status, diff, log, branch, add, \
       commit, checkout, stash, show";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "operation",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "enum",
                        `List
                          [
                            `String "status";
                            `String "diff";
                            `String "log";
                            `String "branch";
                            `String "add";
                            `String "commit";
                            `String "checkout";
                            `String "stash";
                            `String "show";
                          ] );
                      ("description", `String "Git operation to perform");
                    ] );
                ( "paths",
                  `Assoc
                    [
                      ( "oneOf",
                        `List
                          [
                            `Assoc [ ("type", `String "string") ];
                            `Assoc
                              [
                                ("type", `String "array");
                                ("items", `Assoc [ ("type", `String "string") ]);
                              ];
                          ] );
                      ( "description",
                        `String
                          "File paths (for add/diff/show; string or array)" );
                    ] );
                ( "message",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Commit message (for commit)");
                    ] );
                ( "branch",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Branch name (for checkout/branch)" );
                    ] );
                ( "cached",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ("description", `String "Show staged changes (for diff)");
                    ] );
                ( "limit",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ("description", `String "Log entry count (default 10)");
                    ] );
                ( "repo_path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Absolute path to the git repo or worktree root. \
                           When omitted, defaults to the workspace directory. \
                           Use this when operating on a repo outside the \
                           workspace (e.g. ~/src/myproject or a git worktree)."
                      );
                    ] );
              ] );
          ("required", `List [ `String "operation" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let interrupt_check =
          match context with Some c -> c.Tool.interrupt_check | None -> None
        in
        let operation =
          try args |> member "operation" |> to_string with _ -> ""
        in
        let message =
          try args |> member "message" |> to_string with _ -> ""
        in
        let branch = try args |> member "branch" |> to_string with _ -> "" in
        let cached = try args |> member "cached" |> to_bool with _ -> false in
        let limit = try args |> member "limit" |> to_int with _ -> 10 in
        let repo_path =
          try
            match args |> member "repo_path" with
            | `String s when s <> "" -> Some s
            | _ -> None
          with _ -> None
        in
        let paths =
          try
            match args |> member "paths" with
            | `String s -> [ s ]
            | `List items ->
                List.filter_map
                  (fun v -> try Some (to_string v) with _ -> None)
                  items
            | _ -> []
          with _ -> []
        in
        if operation = "" then Lwt.return "Error: operation is required"
        else
          let cwd_result =
            match repo_path with
            | None -> Ok workspace
            | Some p when not (Filename.is_relative p) -> Ok p
            | Some p ->
                Error
                  (Printf.sprintf
                     "Error: repo_path must be an absolute path starting with \
                      \"/\". Received: %S. Provide an absolute path like \
                      \"/home/user/src/myproject\" or omit repo_path to use \
                      the default workspace."
                     p)
          in
          match cwd_result with
          | Error err -> Lwt.return err
          | Ok cwd -> (
              let build_argv () =
                match operation with
                | "status" -> Ok [ "git"; "status"; "--short" ]
                | "diff" ->
                    let argv = [ "git"; "diff" ] in
                    let argv = if cached then argv @ [ "--cached" ] else argv in
                    let argv = if paths <> [] then argv @ paths else argv in
                    Ok argv
                | "log" ->
                    Ok
                      [ "git"; "log"; "--oneline"; Printf.sprintf "-n%d" limit ]
                | "branch" ->
                    if branch <> "" then Ok [ "git"; "branch"; branch ]
                    else Ok [ "git"; "branch"; "-a" ]
                | "add" ->
                    if paths = [] then Error "Error: paths required for add"
                    else Ok ("git" :: "add" :: paths)
                | "commit" ->
                    if message = "" then
                      Error "Error: message required for commit"
                    else Ok [ "git"; "commit"; "-m"; message ]
                | "checkout" ->
                    if branch = "" && paths = [] then
                      Error "Error: branch or paths required for checkout"
                    else if branch <> "" then Ok [ "git"; "checkout"; branch ]
                    else Ok ("git" :: "checkout" :: "--" :: paths)
                | "stash" -> Ok [ "git"; "stash" ]
                | "show" ->
                    let argv = [ "git"; "show"; "--stat" ] in
                    let argv =
                      if paths <> [] then argv @ [ "--" ] @ paths else argv
                    in
                    Ok argv
                | op ->
                    Error (Printf.sprintf "Error: unknown operation '%s'" op)
              in
              match build_argv () with
              | Error msg -> Lwt.return msg
              | Ok argv -> (
                  (* Sanitize user-supplied inputs: paths and branch name.
                 The commit message is intentionally excluded — it is passed
                 as an execve argument, not interpreted by a shell, so shell
                 metacharacters in a commit message are safe. *)
                  let user_inputs =
                    paths @ if branch <> "" then [ branch ] else []
                  in
                  let safe = List.for_all sanitize_git_arg user_inputs in
                  if not safe then
                    Lwt.return
                      "Error: git arguments contain disallowed patterns"
                  else
                    let open Lwt.Syntax in
                    let env =
                      [|
                        ("HOME="
                        ^ try Sys.getenv "HOME" with Not_found -> "/tmp");
                        ("PATH="
                        ^
                          try Sys.getenv "PATH"
                          with Not_found -> "/usr/bin:/bin");
                        "GIT_TERMINAL_PROMPT=0";
                      |]
                    in
                    let proc =
                      Process_group.start ~cwd ~env
                        (Process_group.Exec (Array.of_list argv))
                    in
                    let runner_result, runner_wakener = Lwt.wait () in
                    let forced_result = ref None in
                    let finish_runner result =
                      if Lwt.is_sleeping runner_result then
                        Lwt.wakeup_later runner_wakener result
                    in
                    Lwt.async (fun () ->
                        Lwt.catch
                          (fun () ->
                            Lwt.finalize
                              (fun () ->
                                let* stdout, stderr =
                                  Lwt.both
                                    (Lwt_io.read proc.Process_group.stdout)
                                    (Lwt_io.read proc.Process_group.stderr)
                                in
                                let* status = Process_group.wait proc.pid in
                                let exit_code =
                                  match status with
                                  | Unix.WEXITED n -> n
                                  | Unix.WSIGNALED n -> 128 + n
                                  | Unix.WSTOPPED n -> 128 + n
                                in
                                let output =
                                  (if stdout <> "" then stdout else "")
                                  ^ if stderr <> "" then stderr else ""
                                in
                                finish_runner
                                  (Ok
                                     (if exit_code = 0 then
                                        if output = "" then "(no output)"
                                        else output
                                      else
                                        Printf.sprintf "exit_code: %d\n%s"
                                          exit_code output));
                                Lwt.return_unit)
                              (fun () -> Process_group.close proc))
                          (fun exn ->
                            finish_runner (Error exn);
                            Lwt.return_unit));
                    let timeout =
                      let* () = Lwt_unix.sleep 30.0 in
                      forced_result :=
                        Some "Error: git timed out after 30 seconds";
                      let* () = Process_group.terminate proc.pid in
                      let* _ = runner_result in
                      Lwt.return (`Done "Error: git timed out after 30 seconds")
                    in
                    let interrupt =
                      match interrupt_check with
                      | None -> fst (Lwt.wait ())
                      | Some check ->
                          let rec wait () =
                            match check () with
                            | Some reason
                              when reason
                                   <> Agent.queued_message_interrupt_token ->
                                Lwt.return_unit
                            | _ ->
                                let* () = Lwt_unix.sleep 0.05 in
                                wait ()
                          in
                          let* () = wait () in
                          forced_result :=
                            Some "Git command interrupted by user.";
                          let* () =
                            Process_group.terminate_immediately proc.pid
                          in
                          let* _ = runner_result in
                          Lwt.return (`Done "Git command interrupted by user.")
                    in
                    let* outcome =
                      Lwt.pick
                        [
                          (let* result = runner_result in
                           match !forced_result with
                           | Some output -> Lwt.return (`Done output)
                           | None -> Lwt.return (`Runner result));
                          timeout;
                          interrupt;
                        ]
                    in
                    match outcome with
                    | `Runner (Ok result) -> Lwt.return result
                    | `Runner (Error exn) -> Lwt.fail exn
                    | `Done result -> Lwt.return result)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

(* ───── Messaging tools ───── *)
