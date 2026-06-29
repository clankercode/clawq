open Tools_builtin_util
open Tools_builtin_io

let http_request ~workspace_only =
  let schema =
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
                    ("description", `String "Request URL (required)");
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
                    ("description", `String "Request headers as key-value pairs");
                    ( "additionalProperties",
                      `Assoc [ ("type", `String "string") ] );
                  ] );
              ( "body",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "Request body (for POST/PUT/PATCH)");
                  ] );
            ] );
        ("required", `List [ `String "url" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"http_request" ~parameters_schema:schema
      ~detail
  in
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
    parameters_schema = schema;
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
        if url = "" then
          Lwt.return (param_err "parameter 'url' must be a non-empty string")
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
      if is_prefix "<!--" then begin
        (* Skip HTML comments *)
        i := !i + 4;
        let found_end = ref false in
        while (not !found_end) && !i + 2 < len do
          if html.[!i] = '-' && html.[!i + 1] = '-' && html.[!i + 2] = '>' then begin
            found_end := true;
            i := !i + 3
          end
          else incr i
        done
      end
      else if is_prefix "<script" then begin
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
  let schema =
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
                    ("description", `String "URL to fetch (required)");
                  ] );
            ] );
        ("required", `List [ `String "url" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"web_fetch" ~parameters_schema:schema
      ~detail
  in
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
    parameters_schema = schema;
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let url = try args |> member "url" |> to_string with _ -> "" in
        if url = "" then
          Lwt.return (param_err "parameter 'url' must be a non-empty string")
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

(* B597: retry HTTP GETs that return 429 (Too Many Requests) with
   exponential backoff. Returns (status, body) of the last attempt. The
   delays parameter is exposed for tests so they can run without sleeps. *)
let http_get_with_429_retry ?(delays_s = [ 1.0; 2.0; 4.0 ]) ~uri ~headers () =
  let open Lwt.Syntax in
  let rec loop delays =
    let* status, body = Http_client.get ~uri ~headers in
    if status <> 429 then Lwt.return (status, body)
    else
      match delays with
      | [] -> Lwt.return (status, body)
      | d :: rest ->
          let* () = Lwt_unix.sleep d in
          loop rest
  in
  loop delays_s

let web_search ~(config : Runtime_config.t) =
  let ws_cfg = config.web_search in
  let schema =
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
              ( "limit",
                `Assoc
                  [
                    ("type", `String "integer");
                    ("description", `String "Number of results (default 5)");
                  ] );
            ] );
        ("required", `List [ `String "query" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"web_search" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "web_search";
    description =
      "Search the web and return a list of results with titles, URLs, and \
       snippets";
    parameters_schema = schema;
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let query = try args |> member "query" |> to_string with _ -> "" in
        let limit =
          try args |> member "limit" |> to_int
          with _ -> (
            match ws_cfg with Some ws -> ws.num_results | None -> 5)
        in
        if query = "" then
          Lwt.return (param_err "parameter 'query' must be a non-empty string")
        else
          match ws_cfg with
          | None ->
              Lwt.return
                "Error: web_search not configured. Add a \"web_search\" \
                 section to ~/.clawq/config.json with provider and api_key."
          | Some ws ->
              let provider = ws.search_provider in
              (* Resolve API key through credential lease API when handle is set *)
              let api_key_result =
                resolve_credential_handle ~config ~handle_id:ws.credential_handle
                  ~header_name:"X-Subscription-Token"
              in
              match api_key_result with
              | Error msg -> Lwt.return ("Error: " ^ msg)
              | Ok lease_key ->
              let api_key =
                if lease_key <> "" then lease_key else ws.search_api_key
              in
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
                  (* B670: provider-specific helpers so we can fall back from
                     Brave to DDG on 429 without re-implementing the parser.
                     B672: structured per-attempt log with backend, status, and
                     latency so failover paths are diagnosable from daemon.log. *)
                  let log_attempt ~backend ~status ~t0 =
                    let latency_ms =
                      int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0)
                    in
                    Logs.info (fun m ->
                        m
                          "web_search attempt: backend=%s status=%d \
                           latency_ms=%d query=%S"
                          backend status latency_ms query)
                  in
                  let do_brave () =
                    let base =
                      match ws.search_base_url with
                      | Some u -> u
                      | None -> "https://api.search.brave.com/res/v1/web/search"
                    in
                    let uri =
                      Printf.sprintf "%s?q=%s&count=%d" base encoded_query limit
                    in
                    let t0 = Unix.gettimeofday () in
                    let* status, body =
                      http_get_with_429_retry ~uri
                        ~headers:
                          [
                            ("X-Subscription-Token", api_key);
                            ("Accept", "application/json");
                          ]
                        ()
                    in
                    log_attempt ~backend:"brave" ~status ~t0;
                    if status = 429 then Lwt.return (`Rate_limited "brave")
                    else if status >= 400 then
                      Lwt.return
                        (`Err
                           (Printf.sprintf "Error: Brave API returned HTTP %d"
                              status))
                    else
                      let json =
                        try Yojson.Safe.from_string body
                        with _ ->
                          `Assoc [ ("web", `Assoc [ ("results", `List []) ]) ]
                      in
                      let results =
                        try json |> member "web" |> member "results" |> to_list
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
                              try r |> member "url" |> to_string with _ -> ""
                            in
                            let snippet =
                              try r |> member "description" |> to_string
                              with _ -> ""
                            in
                            Printf.sprintf "%d. %s\n   %s\n   %s" (i + 1) title
                              url snippet)
                          results
                      in
                      Lwt.return
                        (`Ok
                           (if lines = [] then "No results found"
                            else String.concat "\n\n" lines))
                  in
                  let do_ddg ?(base_url = "https://api.duckduckgo.com") () =
                    let uri =
                      Printf.sprintf
                        "%s/?q=%s&format=json&no_redirect=1&no_html=1" base_url
                        encoded_query
                    in
                    let t0 = Unix.gettimeofday () in
                    let* status, body =
                      http_get_with_429_retry ~uri
                        ~headers:[ ("Accept", "application/json") ]
                        ()
                    in
                    log_attempt ~backend:"ddg" ~status ~t0;
                    if status = 429 then Lwt.return (`Rate_limited "ddg")
                    else if status >= 400 then
                      Lwt.return
                        (`Err
                           (Printf.sprintf "Error: DDG API returned HTTP %d"
                              status))
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
                              let text = topic |> member "Text" |> to_string in
                              let url =
                                try topic |> member "FirstURL" |> to_string
                                with _ -> ""
                              in
                              lines :=
                                Printf.sprintf "%d. %s\n   %s" (i + 1) text url
                                :: !lines
                            with _ -> ())
                        related;
                      let lines = List.rev !lines in
                      let lines =
                        if abstract <> "" then
                          Printf.sprintf "Answer: %s\n%s" abstract abstract_url
                          :: lines
                        else lines
                      in
                      Lwt.return
                        (`Ok
                           (if lines = [] then
                              "No results found (DDG instant API has limited \
                               coverage)"
                            else String.concat "\n\n" lines))
                  in
                  match provider with
                  | "brave" -> (
                      let* outcome = do_brave () in
                      match outcome with
                      | `Ok s -> Lwt.return s
                      | `Err e -> Lwt.return e
                      | `Rate_limited _ -> (
                          (* B670: Brave rate-limited; fall back to DDG so the
                             agent gets *something* rather than burning more
                             retry budget on the same 429. *)
                          Logs.info (fun m ->
                              m
                                "web_search: brave HTTP 429 — falling back to \
                                 ddg");
                          let* fallback = do_ddg () in
                          match fallback with
                          | `Ok s ->
                              Lwt.return
                                ("(brave rate-limited, fell back to ddg)\n" ^ s)
                          | `Err e ->
                              Lwt.return
                                ("Error: Brave HTTP 429 and DDG fallback \
                                  failed: " ^ e)
                          | `Rate_limited _ ->
                              Lwt.return
                                "Error: both brave and ddg returned HTTP 429. \
                                 Alternatives the agent can try without \
                                 web_search: (a) wait ~60s and retry; (b) call \
                                 `web_fetch` against a known URL if you have \
                                 one; (c) call `http_get` against a specific \
                                 endpoint; (d) if `web_search_prime` is \
                                 registered, use that instead (zai_mcp \
                                 backend)."))
                  | "ddg" | _ -> (
                      let base_url =
                        match ws.search_base_url with
                        | Some u -> u
                        | None -> "https://api.duckduckgo.com"
                      in
                      let* outcome = do_ddg ~base_url () in
                      match outcome with
                      | `Ok s -> Lwt.return s
                      | `Err e -> Lwt.return e
                      | `Rate_limited _ ->
                          Lwt.return
                            "Error: DuckDuckGo search API rate-limited (HTTP \
                             429) after 3 retries. Alternatives the agent can \
                             try: (a) wait ~60s and retry; (b) call \
                             `web_fetch` against a known URL; (c) call \
                             `http_get` against a specific endpoint; (d) if \
                             `web_search_prime` is registered (zai_mcp \
                             backend), use that instead."))
                (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

(* Z.ai MCP tools (zai_websearch / zai_webfetch and helpers) live in
   tools_builtin_zai.ml; re-exported here so callers keep using
   Tools_builtin_net.* (and Tools_builtin.* via include). *)
include Tools_builtin_zai

(* ───── Git operations tool ───── *)

let sanitize_git_arg arg =
  let arg_low = String.lowercase_ascii arg in
  let dangerous_prefixes =
    [
      "--exec=";
      "--upload-pack=";
      "--receive-pack=";
      "--pager=";
      "--editor=";
      "--config=";
    ]
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
  let schema =
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
                    ( "description",
                      `String "Git operation to perform (required)" );
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
                      `String "File paths (for add/diff/show; string or array)"
                    );
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
                    ("description", `String "Branch name (for checkout/branch)");
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
                        "Absolute path to the git repo or worktree root. When \
                         omitted, defaults to the workspace directory. Use \
                         this when operating on a repo outside the workspace \
                         (e.g. ~/src/myproject or a git worktree)." );
                  ] );
            ] );
        ("required", `List [ `String "operation" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"git_operations" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "git_operations";
    description =
      "Perform structured Git operations: status, diff, log, branch, add, \
       commit, checkout, stash, show";
    parameters_schema = schema;
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
        if operation = "" then
          Lwt.return
            (param_err
               "parameter 'operation' must be a non-empty string \
                (status|diff|log|branch|add|commit|checkout|stash|show)")
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
                      Array.append
                        (Runtime_config.workspace_only_env ())
                        [| "GIT_TERMINAL_PROMPT=0" |]
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

(* B668: Validate the configured web_search provider at daemon startup so the
   agent doesn't waste cron turns burning tokens against a broken backend.
   Performs a single tiny probe query and returns Ok if results are non-empty,
   Error with a short reason otherwise. Caller logs the result. *)
let web_search_health_check ~(config : Runtime_config.t) :
    (string, string) result Lwt.t =
  let open Lwt.Syntax in
  match config.web_search with
  | None -> Lwt.return (Error "web_search not configured")
  | Some ws ->
      let provider = ws.search_provider in
      let api_key = ws.search_api_key in
      Lwt.catch
        (fun () ->
          match provider with
          | "brave" ->
              if api_key = "" then
                Lwt.return
                  (Error
                     "provider=brave but api_key is empty; set \
                      web_search.api_key in ~/.clawq/config.json (free key at \
                      https://api.search.brave.com/app/keys)")
              else
                let base =
                  match ws.search_base_url with
                  | Some u -> u
                  | None -> "https://api.search.brave.com/res/v1/web/search"
                in
                let uri = Printf.sprintf "%s?q=clawq+ping&count=1" base in
                let* status, body =
                  Http_client.get ~uri
                    ~headers:
                      [
                        ("X-Subscription-Token", api_key);
                        ("Accept", "application/json");
                      ]
                in
                if status = 200 then Lwt.return (Ok "brave ok")
                else if status = 401 || status = 403 then
                  Lwt.return
                    (Error
                       (Printf.sprintf
                          "brave HTTP %d (unauthorized) — api_key invalid or \
                           expired; regenerate at \
                           https://api.search.brave.com/app/keys"
                          status))
                else if status = 429 then
                  Lwt.return
                    (Error
                       "brave HTTP 429 (rate limited) — temporarily over \
                        quota, not fatal")
                else
                  Lwt.return
                    (Error
                       (Printf.sprintf "brave HTTP %d: %s" status
                          (String.sub body 0 (min 200 (String.length body)))))
          | "ddg" ->
              let base =
                match ws.search_base_url with
                | Some u -> u
                | None -> "https://api.duckduckgo.com"
              in
              let uri =
                Printf.sprintf "%s/?q=clawq+ping&format=json&no_redirect=1" base
              in
              let* status, _ =
                Http_client.get ~uri ~headers:[ ("Accept", "application/json") ]
              in
              if status = 200 then
                Lwt.return
                  (Ok
                     "ddg reachable (note: instant-answer API has limited \
                      coverage; consider provider=brave if api_key starts with \
                      'BSA')")
              else Lwt.return (Error (Printf.sprintf "ddg HTTP %d" status))
          | other ->
              Lwt.return
                (Error
                   (Printf.sprintf
                      "unknown provider %S; supported: brave, ddg, searxng"
                      other)))
        (fun exn ->
          Lwt.return (Error ("probe exception: " ^ Printexc.to_string exn)))
