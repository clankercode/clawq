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
             ("name", `String cmd.name); ("description", `String cmd.description);
           ])
       Slash_commands.commands)

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

(* Stricter auth check for endpoints that require pairing or a static token.
   Unlike auth_ok, does not allow anonymous access when auth_token is None. *)
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

let client_ip req =
  let headers = Cohttp.Request.headers req in
  match Cohttp.Header.get headers "x-forwarded-for" with
  | Some xff -> (
      match String.split_on_char ',' xff with
      | ip :: _ -> String.trim ip
      | [] -> "unknown")
  | None -> "unknown"

let rate_limit_response () =
  Cohttp_lwt_unix.Server.respond_string ~status:`Too_many_requests
    ~headers:json_headers ~body:{|{"error":"rate limit exceeded"}|} ()

let is_github_webhook_path path = function
  | None -> false
  | Some (gc : Runtime_config.github_config) ->
      List.exists
        (fun (r : Runtime_config.github_repo_config) -> r.webhook_path = path)
        gc.repos

let lookup_github_repo path (gc : Runtime_config.github_config) =
  List.find_opt
    (fun (r : Runtime_config.github_repo_config) -> r.webhook_path = path)
    gc.repos

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

let handler ~session_manager ~require_pairing ~auth_token
    ?daemon_run_update_command ?slack_config ?github_config ?github_api_limiter
    ?ip_limiter ?session_limiter ?slack_event_limiter ?slack_run_update_command
    ?web_channel ?whatsapp_config ?line_config ?lark_config ?pairing ?ui_server
    _conn req body =
  let open Lwt.Syntax in
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  let meth = Cohttp.Request.meth req in
  match (meth, path) with
  | ( `GET,
      ( "/" | "/ui" | "/index.html" | "/ui/index.html" | "/chat.js"
      | "/ui/chat.js" | "/chat.css" | "/ui/chat.css" ) ) ->
      let* _ = Cohttp_lwt.Body.drain_body body in
      begin match ui_server with
      | Some server -> (
          let* response = Ui_server.respond server path in
          match response with
          | Some response -> Lwt.return response
          | None ->
              json_string_response ~status:`Not_found {|{"error":"not found"}|})
      | None ->
          json_string_response ~status:`Not_found
            {|{"error":"ui not configured"}|}
      end
  | `GET, "/ui-version" ->
      let* _ = Cohttp_lwt.Body.drain_body body in
      let version =
        match ui_server with
        | Some server -> Ui_server.version server
        | None -> Chat_ui_assets.ui_version
      in
      json_string_response
        (`Assoc [ ("version", `String version) ] |> Yojson.Safe.to_string)
  | `GET, "/commands" ->
      let* _ = Cohttp_lwt.Body.drain_body body in
      json_string_response (Yojson.Safe.to_string (slash_commands_json ()))
  | `GET, "/config-keys" ->
      let* _ = Cohttp_lwt.Body.drain_body body in
      let paths = Config_set.config_leaf_paths () in
      let prefix =
        match Uri.get_query_param uri "prefix" with
        | Some p -> String.lowercase_ascii p
        | None -> ""
      in
      let filtered =
        if prefix = "" then paths
        else
          List.filter
            (fun p ->
              let p_lower = String.lowercase_ascii p in
              String.length p_lower >= String.length prefix
              && String.sub p_lower 0 (String.length prefix) = prefix)
            paths
      in
      json_string_response
        (Yojson.Safe.to_string (`List (List.map (fun p -> `String p) filtered)))
  | `GET, "/health" ->
      let* _ = Cohttp_lwt.Body.drain_body body in
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~headers:json_headers
        ~body:{|{"status":"ok"}|} ()
  | `POST, "/chat" -> (
      let* ip_ok =
        match ip_limiter with
        | Some lim -> Rate_limiter.check_and_consume lim ~key:(client_ip req)
        | None -> Lwt.return true
      in
      if not ip_ok then
        let* _ = Cohttp_lwt.Body.drain_body body in
        rate_limit_response ()
      else if require_pairing && not (pairing_auth_ok ~auth_token ?pairing req)
      then
        let* _ = Cohttp_lwt.Body.drain_body body in
        Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
          ~headers:json_headers
          ~body:
            {|{"error":"pairing required; use a valid paired token to access this endpoint"}|}
          ()
      else if not (auth_ok ~auth_token ?pairing req) then
        let* _ = Cohttp_lwt.Body.drain_body body in
        Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
          ~headers:json_headers ~body:{|{"error":"unauthorized"}|} ()
      else
        let* body_str = Cohttp_lwt.Body.to_string body in
        let json =
          try Ok (Yojson.Safe.from_string body_str)
          with exn -> Error (Printexc.to_string exn)
        in
        match json with
        | Error msg ->
            Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
              ~headers:json_headers
              ~body:
                (Yojson.Safe.to_string
                   (`Assoc [ ("error", `String ("invalid JSON: " ^ msg)) ]))
              ()
        | Ok json -> (
            let open Yojson.Safe.Util in
            let session_id =
              try json |> member "session_id" |> to_string with _ -> ""
            in
            let message =
              try json |> member "message" |> to_string with _ -> ""
            in
            if session_id = "" then bad_request "session_id is required"
            else if message = "" then bad_request "message is required"
            else
              let* sess_ok =
                match session_limiter with
                | Some lim -> Rate_limiter.check_and_consume lim ~key:session_id
                | None -> Lwt.return true
              in
              if not sess_ok then rate_limit_response ()
              else
                let key = "web:" ^ session_id in
                let* result =
                  Lwt.catch
                    (fun () ->
                      let* response =
                        Session.turn session_manager ~key ~message ()
                      in
                      Lwt.return (Ok response))
                    (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
                in
                match result with
                | Ok response ->
                    if not (Session.take_response_deferred session_manager ~key)
                    then Session.mark_response_sent session_manager ~key;
                    let resp_json =
                      `Assoc [ ("response", `String response) ]
                      |> Yojson.Safe.to_string
                    in
                    Cohttp_lwt_unix.Server.respond_string ~status:`OK
                      ~headers:json_headers ~body:resp_json ()
                | Error err ->
                    if not (Session.take_response_deferred session_manager ~key)
                    then Session.mark_response_sent session_manager ~key;
                    Cohttp_lwt_unix.Server.respond_string
                      ~status:`Internal_server_error ~headers:json_headers
                      ~body:
                        (Yojson.Safe.to_string
                           (`Assoc [ ("error", `String err) ]))
                      ()))
  | `POST, "/session/inject" -> (
      if require_pairing && not (pairing_auth_ok ~auth_token ?pairing req) then
        let* _ = Cohttp_lwt.Body.drain_body body in
        Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
          ~headers:json_headers
          ~body:
            {|{"error":"pairing required; use a valid paired token to access this endpoint"}|}
          ()
      else if not (auth_ok ~auth_token ?pairing req) then
        let* _ = Cohttp_lwt.Body.drain_body body in
        Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
          ~headers:json_headers ~body:{|{"error":"unauthorized"}|} ()
      else
        let* body_str = Cohttp_lwt.Body.to_string body in
        let json =
          try Ok (Yojson.Safe.from_string body_str)
          with exn -> Error (Printexc.to_string exn)
        in
        match json with
        | Error msg -> bad_request ("invalid JSON: " ^ msg)
        | Ok json -> (
            let open Yojson.Safe.Util in
            let session_key =
              try json |> member "session_key" |> to_string with _ -> ""
            in
            let message =
              try json |> member "message" |> to_string with _ -> ""
            in
            if session_key = "" then bad_request "session_key is required"
            else if message = "" then bad_request "message is required"
            else
              let* result =
                Lwt.catch
                  (fun () ->
                    Session.with_registered_notifier session_manager
                      ~key:session_key
                      ~notify:(fun _text -> Lwt.return_unit)
                      (fun () ->
                        let* response =
                          Session.turn session_manager ~key:session_key ~message
                            ()
                        in
                        let queued =
                          Session.is_queued_message_response response
                        in
                        if
                          (not queued)
                          && not
                               (Session.take_response_deferred session_manager
                                  ~key:session_key)
                        then
                          Session.mark_response_sent session_manager
                            ~key:session_key;
                        Lwt.return (Ok (queued, response))))
                  (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
              in
              match result with
              | Ok (queued, response) ->
                  json_string_response
                    (Yojson.Safe.to_string
                       (`Assoc
                          [
                            ("queued", `Bool queued);
                            ("response", `String response);
                          ]))
              | Error err ->
                  Cohttp_lwt_unix.Server.respond_string
                    ~status:`Internal_server_error ~headers:json_headers
                    ~body:
                      (Yojson.Safe.to_string
                         (`Assoc [ ("error", `String err) ]))
                    ()))
  | `POST, "/daemon/update" -> (
      if require_pairing && not (pairing_auth_ok ~auth_token ?pairing req) then
        let* _ = Cohttp_lwt.Body.drain_body body in
        Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
          ~headers:json_headers
          ~body:
            {|{"error":"pairing required; use a valid paired token to access this endpoint"}|}
          ()
      else if not (auth_ok ~auth_token ?pairing req) then
        let* _ = Cohttp_lwt.Body.drain_body body in
        Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
          ~headers:json_headers ~body:{|{"error":"unauthorized"}|} ()
      else
        let* body_str = Cohttp_lwt.Body.to_string body in
        let json =
          try Ok (Yojson.Safe.from_string body_str)
          with exn -> Error (Printexc.to_string exn)
        in
        match json with
        | Error msg -> bad_request ("invalid JSON: " ^ msg)
        | Ok json -> (
            let open Yojson.Safe.Util in
            let mode_raw =
              match json |> member "mode" with
              | `Null -> "auto"
              | value -> to_string value
            in
            match
              Update_tool.update_mode_of_string
                (String.lowercase_ascii (String.trim mode_raw))
            with
            | None ->
                bad_request
                  (Printf.sprintf
                     "invalid update mode '%s'; expected auto, git, or binary"
                     mode_raw)
            | Some mode -> (
                match daemon_run_update_command with
                | None ->
                    Cohttp_lwt_unix.Server.respond_string
                      ~status:`Internal_server_error ~headers:json_headers
                      ~body:{|{"error":"daemon update not available"}|} ()
                | Some run_update_command -> (
                    let progress = ref [] in
                    let* result =
                      Lwt.catch
                        (fun () ->
                          let open Lwt.Syntax in
                          let* result =
                            run_update_command ~mode
                              ~send_progress:(fun text ->
                                progress := !progress @ [ text ];
                                Lwt.return_unit)
                              ()
                          in
                          Lwt.return (Ok result))
                        (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
                    in
                    match result with
                    | Ok result ->
                        json_string_response
                          (Yojson.Safe.to_string
                             (`Assoc
                                [
                                  ( "progress",
                                    `List
                                      (List.map
                                         (fun text -> `String text)
                                         !progress) );
                                  ("result", `String result);
                                ]))
                    | Error err ->
                        Cohttp_lwt_unix.Server.respond_string
                          ~status:`Internal_server_error ~headers:json_headers
                          ~body:
                            (Yojson.Safe.to_string
                               (`Assoc [ ("error", `String err) ]))
                          ()))))
  | `POST, "/chat/stream" -> (
      let* ip_ok =
        match ip_limiter with
        | Some lim -> Rate_limiter.check_and_consume lim ~key:(client_ip req)
        | None -> Lwt.return true
      in
      if not ip_ok then
        let* _ = Cohttp_lwt.Body.drain_body body in
        rate_limit_response ()
      else if require_pairing && not (pairing_auth_ok ~auth_token ?pairing req)
      then
        let* _ = Cohttp_lwt.Body.drain_body body in
        Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
          ~headers:json_headers
          ~body:
            {|{"error":"pairing required; use a valid paired token to access this endpoint"}|}
          ()
      else if not (auth_ok ~auth_token ?pairing req) then
        let* _ = Cohttp_lwt.Body.drain_body body in
        Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
          ~headers:json_headers ~body:{|{"error":"unauthorized"}|} ()
      else
        let* body_str = Cohttp_lwt.Body.to_string body in
        let json =
          try Ok (Yojson.Safe.from_string body_str)
          with exn -> Error (Printexc.to_string exn)
        in
        match json with
        | Error msg ->
            Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
              ~headers:json_headers
              ~body:
                (Yojson.Safe.to_string
                   (`Assoc [ ("error", `String ("invalid JSON: " ^ msg)) ]))
              ()
        | Ok json -> (
            let open Yojson.Safe.Util in
            let session_id =
              try json |> member "session_id" |> to_string with _ -> ""
            in
            let message =
              try json |> member "message" |> to_string with _ -> ""
            in
            if session_id = "" then
              Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
                ~headers:json_headers
                ~body:{|{"error":"session_id is required"}|} ()
            else if message = "" then
              Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
                ~headers:json_headers ~body:{|{"error":"message is required"}|}
                ()
            else
              let* sess_ok =
                match session_limiter with
                | Some lim -> Rate_limiter.check_and_consume lim ~key:session_id
                | None -> Lwt.return true
              in
              if not sess_ok then rate_limit_response ()
              else
                match Slash_commands.handle message with
                | Slash_commands.Reply text -> sse_reply text
                | Slash_commands.Reset ->
                    let key = "web:" ^ session_id in
                    let* () = Session.reset session_manager ~key in
                    sse_reply Slash_commands.reset_message
                | Slash_commands.Compact ->
                    let key = "web:" ^ session_id in
                    let* compacted = Session.compact session_manager ~key in
                    let text =
                      if compacted then
                        "Session history compacted. Older messages have been \
                         summarized."
                      else
                        "Nothing to compact — session history is already short \
                         enough."
                    in
                    sse_reply text
                | Slash_commands.Thinking Slash_commands.ShowThinking ->
                    let current =
                      (Session.get_config session_manager).agent_defaults
                        .reasoning_effort
                    in
                    let text =
                      Printf.sprintf "Current thinking level: %s"
                        (Slash_commands.thinking_level_to_string current)
                    in
                    sse_reply text
                | Slash_commands.Thinking (Slash_commands.SetThinking level) ->
                    let cfg = Session.get_config session_manager in
                    let previous = cfg.agent_defaults.reasoning_effort in
                    let text =
                      match Config_set.set_reasoning_effort level with
                      | Ok () ->
                          let agent_defaults =
                            { cfg.agent_defaults with reasoning_effort = level }
                          in
                          Session.update_config session_manager
                            { cfg with agent_defaults };
                          Printf.sprintf "Thinking level changed from %s to %s."
                            (Slash_commands.thinking_level_to_string previous)
                            (Slash_commands.thinking_level_to_string level)
                      | Error err -> err
                    in
                    sse_reply text
                | Slash_commands.ShowThinking action ->
                    let cfg = Session.get_config session_manager in
                    let current = cfg.agent_defaults.show_thinking in
                    let text =
                      match action with
                      | Slash_commands.ShowThinkingStatus ->
                          Printf.sprintf "Show thinking: %s"
                            (if current then "on" else "off")
                      | Slash_commands.ToggleShowThinking -> (
                          let new_val = not current in
                          match Config_set.set_show_thinking new_val with
                          | Ok () ->
                              let agent_defaults =
                                {
                                  cfg.agent_defaults with
                                  show_thinking = new_val;
                                }
                              in
                              Session.update_config session_manager
                                { cfg with agent_defaults };
                              Printf.sprintf "Show thinking: %s"
                                (if new_val then "on" else "off")
                          | Error err ->
                              "Failed to update show_thinking: " ^ err)
                    in
                    sse_reply text
                | Slash_commands.Delegate prompt ->
                    let stream, push = Lwt_stream.create () in
                    let push_sse text =
                      let data =
                        Yojson.Safe.to_string
                          (json_of_stream_event (Provider.Delta text))
                      in
                      push (Some (Printf.sprintf "data: %s\n\n" data))
                    in
                    push_sse "Delegating...";
                    Session.delegate_turn session_manager ~prompt
                      ~send_reply:(fun text ->
                        push_sse text;
                        push (Some "data: [DONE]\n\n");
                        push None;
                        Lwt.return_unit);
                    let headers =
                      Cohttp.Header.of_list
                        [
                          ("Content-Type", "text/event-stream");
                          ("Cache-Control", "no-cache");
                          ("Connection", "keep-alive");
                        ]
                    in
                    Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
                      ~body:(Cohttp_lwt.Body.of_stream stream)
                      ()
                | Slash_commands.ForkAnd prompt ->
                    let key = "web:" ^ session_id in
                    let stream, push = Lwt_stream.create () in
                    let push_sse text =
                      let data =
                        Yojson.Safe.to_string
                          (json_of_stream_event (Provider.Delta text))
                      in
                      push (Some (Printf.sprintf "data: %s\n\n" data))
                    in
                    push_sse "Forking session...";
                    Session.fork_and_run session_manager ~parent_key:key ~prompt
                      ~send_reply:(fun text ->
                        push_sse text;
                        push (Some "data: [DONE]\n\n");
                        push None;
                        Lwt.return_unit);
                    let headers =
                      Cohttp.Header.of_list
                        [
                          ("Content-Type", "text/event-stream");
                          ("Cache-Control", "no-cache");
                          ("Connection", "keep-alive");
                        ]
                    in
                    Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
                      ~body:(Cohttp_lwt.Body.of_stream stream)
                      ()
                | Slash_commands.NotACommand ->
                    let key = "web:" ^ session_id in
                    let stream, push = Lwt_stream.create () in
                    Lwt.async (fun () ->
                        Session.with_registered_notifier session_manager ~key
                          ~notify:(fun text ->
                            let data =
                              Yojson.Safe.to_string
                                (json_of_stream_event (Provider.Delta text))
                            in
                            push (Some (Printf.sprintf "data: %s\n\n" data));
                            Lwt.return_unit)
                          (fun () ->
                            Lwt.catch
                              (fun () ->
                                let* _response =
                                  Session.turn_stream session_manager ~key
                                    ~message
                                    ~on_chunk:(fun chunk ->
                                      let data =
                                        Yojson.Safe.to_string
                                          (json_of_stream_event chunk)
                                      in
                                      push
                                        (Some
                                           (Printf.sprintf "data: %s\n\n" data));
                                      Lwt.return_unit)
                                    ()
                                in
                                if
                                  not
                                    (Session.take_response_deferred
                                       session_manager ~key)
                                then
                                  Session.mark_response_sent session_manager
                                    ~key;
                                push (Some "data: [DONE]\n\n");
                                push None;
                                Lwt.return_unit)
                              (fun exn ->
                                let err = Printexc.to_string exn in
                                if
                                  not
                                    (Session.take_response_deferred
                                       session_manager ~key)
                                then
                                  Session.mark_response_sent session_manager
                                    ~key;
                                push
                                  (Some
                                     (Printf.sprintf
                                        "data: \
                                         {\"type\":\"error\",\"message\":%s}\n\n"
                                        (Yojson.Safe.to_string (`String err))));
                                push (Some "data: [DONE]\n\n");
                                push None;
                                Lwt.return_unit)));
                    let headers =
                      Cohttp.Header.of_list
                        [
                          ("Content-Type", "text/event-stream");
                          ("Cache-Control", "no-cache");
                          ("Connection", "keep-alive");
                        ]
                    in
                    Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
                      ~body:(Cohttp_lwt.Body.of_stream stream)
                      ()))
  | `POST, path
    when match slack_config with
         | Some sc -> path = sc.Runtime_config.events_path
         | None -> false ->
      let sc = Option.get slack_config in
      let* body_str = Cohttp_lwt.Body.to_string body in
      let headers = Cohttp.Request.headers req in
      let signature =
        match Cohttp.Header.get headers "x-slack-signature" with
        | Some v -> v
        | None -> ""
      in
      let timestamp =
        match Cohttp.Header.get headers "x-slack-request-timestamp" with
        | Some v -> v
        | None -> ""
      in
      if
        not
          (Slack.verify_signature ~signing_secret:sc.signing_secret ~timestamp
             ~body:body_str ~signature)
      then
        Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
          ~headers:json_headers ~body:{|{"error":"invalid signature"}|} ()
      else
        let* result =
          Slack.handle_event ~config:sc ~session_manager
            ?run_update_command:slack_run_update_command
            ?event_limiter:slack_event_limiter body_str
        in
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~headers:json_headers
          ~body:result ()
  | `POST, path when is_github_webhook_path path github_config -> (
      let gc = Option.get github_config in
      let* body_str = Cohttp_lwt.Body.to_string body in
      match lookup_github_repo path gc with
      | None ->
          Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
            ~headers:json_headers ~body:{|{"error":"not found"}|} ()
      | Some repo_config -> (
          let event_type =
            match
              Cohttp.Header.get (Cohttp.Request.headers req) "x-github-event"
            with
            | Some v -> v
            | None -> ""
          in
          let req_headers = Cohttp.Request.headers req in
          let api_limiter = Option.get github_api_limiter in
          let* result =
            Github.handle_webhook ~repo_config ~github_config:gc
              ~session_manager ~api_limiter ~event_type ~body:body_str
              ~headers:req_headers
          in
          match result with
          | Github.BadSignature ->
              Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
                ~headers:json_headers ~body:{|{"error":"invalid signature"}|} ()
          | Github.Ok msg ->
              Cohttp_lwt_unix.Server.respond_string ~status:`OK
                ~headers:json_headers
                ~body:
                  (Yojson.Safe.to_string (`Assoc [ ("status", `String msg) ]))
                ()))
  | meth, path
    when match web_channel with
         | Some (wc : Web_channel.t) ->
             let prefix = wc.config.path_prefix in
             String.length path >= String.length prefix
             && String.sub path 0 (String.length prefix) = prefix
         | None -> false ->
      let wc = Option.get web_channel in
      let* body_str = Cohttp_lwt.Body.to_string body in
      Web_channel.handle_request wc path meth req body_str
  | `GET, "/whatsapp/webhook" -> (
      let* _ = Cohttp_lwt.Body.drain_body body in
      match whatsapp_config with
      | None ->
          Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
            ~headers:json_headers ~body:{|{"error":"not configured"}|} ()
      | Some wc -> (
          let uri = Cohttp.Request.uri req in
          match Whatsapp.handle_verify ~config:wc uri with
          | Some challenge ->
              Cohttp_lwt_unix.Server.respond_string ~status:`OK
                ~headers:json_headers ~body:challenge ()
          | None ->
              Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
                ~headers:json_headers ~body:{|{"error":"verification failed"}|}
                ()))
  | `POST, "/whatsapp/webhook" -> (
      match whatsapp_config with
      | None ->
          let* _ = Cohttp_lwt.Body.drain_body body in
          Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
            ~headers:json_headers ~body:{|{"error":"not configured"}|} ()
      | Some wc ->
          let* body_str = Cohttp_lwt.Body.to_string body in
          let* () =
            Whatsapp.handle_inbound ~config:wc ~session_mgr:session_manager
              body_str
          in
          Cohttp_lwt_unix.Server.respond_string ~status:`OK
            ~headers:json_headers ~body:{|{"status":"ok"}|} ())
  | `POST, "/line/webhook" -> (
      match line_config with
      | None ->
          let* _ = Cohttp_lwt.Body.drain_body body in
          Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
            ~headers:json_headers ~body:{|{"error":"not configured"}|} ()
      | Some lc ->
          let* body_str = Cohttp_lwt.Body.to_string body in
          let headers = Cohttp.Request.headers req in
          let signature =
            Cohttp.Header.get headers "x-line-signature"
            |> Option.value ~default:""
          in
          let* ok =
            Line_channel.handle_webhook ~config:lc ~session_mgr:session_manager
              ~signature body_str
          in
          if ok then
            Cohttp_lwt_unix.Server.respond_string ~status:`OK
              ~headers:json_headers ~body:{|{"status":"ok"}|} ()
          else
            Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
              ~headers:json_headers ~body:{|{"error":"invalid signature"}|} ())
  | `GET, "/pair" -> (
      let* _ = Cohttp_lwt.Body.drain_body body in
      match pairing with
      | None ->
          Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
            ~headers:json_headers ~body:{|{"error":"not configured"}|} ()
      | Some p ->
          let s = Pairing.status p in
          let body =
            `Assoc
              [
                ("code", `String s.code);
                ("attempts", `Int s.attempts);
                ("locked", `Bool s.locked);
                ("paired_count", `Int s.paired_count);
              ]
            |> Yojson.Safe.to_string
          in
          Cohttp_lwt_unix.Server.respond_string ~status:`OK
            ~headers:json_headers ~body ())
  | `POST, "/pair" -> (
      match pairing with
      | None ->
          let* _ = Cohttp_lwt.Body.drain_body body in
          Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
            ~headers:json_headers ~body:{|{"error":"not configured"}|} ()
      | Some p -> (
          let* body_str = Cohttp_lwt.Body.to_string body in
          let code =
            try
              let json = Yojson.Safe.from_string body_str in
              Yojson.Safe.Util.(json |> member "code" |> to_string)
            with _ -> ""
          in
          if code = "" then
            Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
              ~headers:json_headers ~body:{|{"error":"code is required"}|} ()
          else
            match Pairing.try_pair p ~code with
            | Pairing.Paired token ->
                let body =
                  `Assoc
                    [ ("status", `String "paired"); ("token", `String token) ]
                  |> Yojson.Safe.to_string
                in
                Cohttp_lwt_unix.Server.respond_string ~status:`OK
                  ~headers:json_headers ~body ()
            | Pairing.WrongCode ->
                Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
                  ~headers:json_headers ~body:{|{"error":"wrong code"}|} ()
            | Pairing.Locked until ->
                let body =
                  `Assoc
                    [
                      ("error", `String "locked"); ("locked_until", `Float until);
                    ]
                  |> Yojson.Safe.to_string
                in
                Cohttp_lwt_unix.Server.respond_string ~status:`Too_many_requests
                  ~headers:json_headers ~body ()
            | Pairing.AlreadyPaired ->
                Cohttp_lwt_unix.Server.respond_string ~status:`Conflict
                  ~headers:json_headers ~body:{|{"error":"already paired"}|} ())
      )
  | `POST, "/pair/regenerate" -> (
      let* _ = Cohttp_lwt.Body.drain_body body in
      match pairing with
      | None ->
          Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
            ~headers:json_headers ~body:{|{"error":"not configured"}|} ()
      | Some p ->
          if not (auth_ok ~auth_token ?pairing req) then
            Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
              ~headers:json_headers ~body:{|{"error":"unauthorized"}|} ()
          else begin
            Pairing.regenerate_code p;
            let s = Pairing.status p in
            let body =
              `Assoc
                [ ("code", `String s.code); ("status", `String "regenerated") ]
              |> Yojson.Safe.to_string
            in
            Cohttp_lwt_unix.Server.respond_string ~status:`OK
              ~headers:json_headers ~body ()
          end)
  | `POST, "/lark/webhook" -> (
      match lark_config with
      | None ->
          let* _ = Cohttp_lwt.Body.drain_body body in
          Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
            ~headers:json_headers ~body:{|{"error":"not configured"}|} ()
      | Some lc -> (
          let* body_str = Cohttp_lwt.Body.to_string body in
          let headers = Cohttp.Request.headers req in
          let sig_ok =
            if lc.Runtime_config.verification_token = "" then true
            else
              let timestamp =
                Cohttp.Header.get headers "x-lark-request-timestamp"
                |> Option.value ~default:""
              in
              let nonce =
                Cohttp.Header.get headers "x-lark-request-nonce"
                |> Option.value ~default:""
              in
              let signature =
                Cohttp.Header.get headers "x-lark-signature"
                |> Option.value ~default:""
              in
              Lark.verify_lark_signature
                ~verification_token:lc.verification_token ~timestamp ~nonce
                ~body:body_str ~signature
          in
          if not sig_ok then
            Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
              ~headers:json_headers ~body:{|{"error":"invalid signature"}|} ()
          else
            let* result =
              Lark.handle_webhook_body ~config:lc ~session_mgr:session_manager
                body_str
            in
            match result with
            | `Challenge resp ->
                Cohttp_lwt_unix.Server.respond_string ~status:`OK
                  ~headers:json_headers ~body:resp ()
            | `Ok _ ->
                Cohttp_lwt_unix.Server.respond_string ~status:`OK
                  ~headers:json_headers ~body:{|{"code":0}|} ()))
  | _ ->
      let* _ = Cohttp_lwt.Body.drain_body body in
      Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
        ~headers:json_headers ~body:{|{"error":"not found"}|} ()

let start ~port ~host ~require_pairing ~auth_token ~session_manager
    ?daemon_run_update_command ?slack_config ?github_config ?github_api_limiter
    ?ip_limiter ?session_limiter ?slack_event_limiter ?slack_run_update_command
    ?web_channel ?whatsapp_config ?line_config ?lark_config ?pairing ?ui_server
    ?stop () =
  let open Lwt.Syntax in
  let callback =
    handler ~session_manager ~require_pairing ~auth_token
      ?daemon_run_update_command ?slack_config ?github_config
      ?github_api_limiter ?ip_limiter ?session_limiter ?slack_event_limiter
      ?slack_run_update_command ?web_channel ?whatsapp_config ?line_config
      ?lark_config ?pairing ?ui_server
  in
  let* ctx = Conduit_lwt_unix.init ~src:host () in
  let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
  let server = Cohttp_lwt_unix.Server.make ~callback () in
  match stop with
  | Some stop ->
      Cohttp_lwt_unix.Server.create ~ctx ~stop ~mode:(`TCP (`Port port)) server
  | None -> Cohttp_lwt_unix.Server.create ~ctx ~mode:(`TCP (`Port port)) server
