let json_headers =
  Cohttp.Header.of_list [ ("Content-Type", "application/json") ]

let json_string_response ?(status = `OK) body =
  Cohttp_lwt_unix.Server.respond_string ~status ~headers:json_headers ~body ()

let bad_request msg =
  Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
    ~headers:json_headers
    ~body:(Yojson.Safe.to_string (`Assoc [ ("error", `String msg) ]))
    ()

let slash_commands_json () =
  `List
    (List.map
       (fun (cmd : Slash_commands.command) ->
         `Assoc
           [
             ("name", `String cmd.name);
             ("description", `String cmd.description);
             ("priority", `Int cmd.priority);
           ])
       (Slash_commands.sorted_by_priority ()))

let json_of_stream_event = function
  | Provider.Delta content ->
      `Assoc [ ("type", `String "delta"); ("content", `String content) ]
  | Provider.ThinkingDelta content ->
      `Assoc
        [ ("type", `String "thinking_delta"); ("content", `String content) ]
  | Provider.ToolCallDelta { index; id; function_name; arguments } ->
      let fields =
        [ ("type", `String "tool_call_delta"); ("index", `Int index) ]
      in
      let fields =
        match id with
        | Some value -> fields @ [ ("id", `String value) ]
        | None -> fields
      in
      let fields =
        match function_name with
        | Some value -> fields @ [ ("function_name", `String value) ]
        | None -> fields
      in
      let fields =
        match arguments with
        | Some value -> fields @ [ ("arguments", `String value) ]
        | None -> fields
      in
      `Assoc fields
  | Provider.ToolStart { id; name; arguments } ->
      `Assoc
        [
          ("type", `String "tool_start");
          ("id", `String id);
          ("name", `String name);
          ("arguments", `String arguments);
        ]
  | Provider.ToolOutputDelta { id; chunk } ->
      `Assoc
        [
          ("type", `String "tool_output_delta");
          ("id", `String id);
          ("chunk", `String chunk);
        ]
  | Provider.ToolResult { id; name; result; is_error } ->
      `Assoc
        [
          ("type", `String "tool_result");
          ("id", `String id);
          ("name", `String name);
          ("result", `String result);
          ("is_error", `Bool is_error);
        ]
  | Provider.Done -> `Assoc [ ("type", `String "done") ]

let extract_bearer req =
  let headers = Cohttp.Request.headers req in
  match Cohttp.Header.get headers "authorization" with
  | Some v ->
      let v = String.trim v in
      let prefix = "Bearer " in
      let plen = String.length prefix in
      if String.length v > plen && String.sub v 0 plen = prefix then
        Some (String.sub v plen (String.length v - plen))
      else None
  | None -> None

let auth_ok ~auth_token ?pairing req =
  let headers = Cohttp.Request.headers req in
  let paired_ok =
    match pairing with
    | None -> false
    | Some p -> (
        match extract_bearer req with
        | Some tok -> Pairing.is_valid_token p ~token:tok
        | None -> (
            match Cohttp.Header.get headers "x-api-key" with
            | Some v -> Pairing.is_valid_token p ~token:(String.trim v)
            | None -> false))
  in
  if paired_ok then true
  else
    match auth_token with
    | None -> true
    | Some token ->
        let bearer =
          match Cohttp.Header.get headers "authorization" with
          | Some v -> Eqaf.equal (String.trim v) ("Bearer " ^ token)
          | None -> false
        in
        let api_key =
          match Cohttp.Header.get headers "x-api-key" with
          | Some v -> Eqaf.equal (String.trim v) token
          | None -> false
        in
        bearer || api_key

let pairing_auth_ok ~auth_token ?pairing req =
  let headers = Cohttp.Request.headers req in
  let paired_ok =
    match pairing with
    | None -> false
    | Some p -> (
        match extract_bearer req with
        | Some tok -> Pairing.is_valid_token p ~token:tok
        | None -> (
            match Cohttp.Header.get headers "x-api-key" with
            | Some v -> Pairing.is_valid_token p ~token:(String.trim v)
            | None -> false))
  in
  if paired_ok then true
  else
    match auth_token with
    | None -> false
    | Some token ->
        let bearer =
          match Cohttp.Header.get headers "authorization" with
          | Some v -> Eqaf.equal (String.trim v) ("Bearer " ^ token)
          | None -> false
        in
        let api_key =
          match Cohttp.Header.get headers "x-api-key" with
          | Some v -> Eqaf.equal (String.trim v) token
          | None -> false
        in
        bearer || api_key

let client_ip _req =
  (* Do not trust X-Forwarded-For from untrusted clients.
     Return "unknown" since peer address is not available from Cohttp request.
     If a reverse proxy is deployed, add trusted-proxy configuration here. *)
  "unknown"

let rate_limit_response () =
  Cohttp_lwt_unix.Server.respond_string ~status:`Too_many_requests
    ~headers:json_headers ~body:{|{"error":"rate limit exceeded"}|} ()

let is_github_webhook_path path = function
  | None -> false
  | Some (gc : Runtime_config.github_config) ->
      List.exists
        (fun (r : Runtime_config.github_repo_config) -> r.webhook_path = path)
        gc.repos

type github_repo_lookup =
  | Missing_github_repo
  | Ambiguous_github_repo
  | Found_github_repo of Runtime_config.github_repo_config

let lookup_github_repo path (gc : Runtime_config.github_config) =
  match
    List.filter
      (fun (r : Runtime_config.github_repo_config) -> r.webhook_path = path)
      gc.repos
  with
  | [] -> Missing_github_repo
  | [ repo_config ] -> Found_github_repo repo_config
  | _ -> Ambiguous_github_repo

let sse_headers =
  Cohttp.Header.of_list
    [
      ("Content-Type", "text/event-stream");
      ("Cache-Control", "no-cache");
      ("Connection", "keep-alive");
    ]

let sse_reply text =
  let data =
    Yojson.Safe.to_string (json_of_stream_event (Provider.Delta text))
  in
  let done_data = Yojson.Safe.to_string (json_of_stream_event Provider.Done) in
  let body =
    Printf.sprintf "data: %s\n\ndata: %s\n\ndata: [DONE]\n\n" data done_data
  in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~headers:sse_headers ~body
    ()
