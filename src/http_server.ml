include Http_server_0_util

let handler ~session_manager ~require_pairing ~auth_token
    ?daemon_run_update_command ?slack_config ?github_config ?config
    ?github_api_limiter ?ip_limiter ?session_limiter ?slack_event_limiter
    ?teams_event_limiter ?slack_run_update_command ?web_channel ?whatsapp_config
    ?line_config ?lark_config ?teams_config ?pairing ?runner_tokens ?ask_fn
    ?ui_server _conn req body =
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
  | `GET, "/models" ->
      let* _ = Cohttp_lwt.Body.drain_body body in
      Http_server_models.respond_models_json ~session_manager uri
  | `GET, "/usage" ->
      let* _ = Cohttp_lwt.Body.drain_body body in
      let results = Provider_quota.get_all_cached () in
      let json =
        `List
          (List.map
             (fun (name, pq) ->
               `Assoc
                 [
                   ("provider", `String name);
                   ("summary", `String (Provider_quota.to_summary_string pq));
                 ])
             results)
      in
      json_string_response (Yojson.Safe.to_string json)
  | `POST, "/chat" ->
      Http_server_chat.handle_chat ~session_manager ~require_pairing ~auth_token
        ?ip_limiter ?session_limiter ?pairing req body
  | meth, path when String.length path >= 8 && String.sub path 0 8 = "/worker/"
    -> (
      if
        (* B774: remote worker lease surface. Requires the gateway auth token;
         subscriber workers connect outbound only. *)
        require_pairing && not (pairing_auth_ok ~auth_token ?pairing req)
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
        match Session.get_db session_manager with
        | None ->
            Cohttp_lwt_unix.Server.respond_string ~status:`Service_unavailable
              ~headers:json_headers
              ~body:{|{"error":"control-plane database unavailable"}|} ()
        | Some db -> (
            match Http_server_workers.handle ~db ~meth ~path ~body_str () with
            | Some response -> response
            | None ->
                Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
                  ~headers:json_headers
                  ~body:{|{"error":"unknown /worker endpoint"}|} ()))
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
            let session_key = Session.sanitize_session_key session_key in
            let message =
              try json |> member "message" |> to_string with _ -> ""
            in
            let cwd =
              try
                match json |> member "cwd" with
                | `String s when String.trim s <> "" -> Some s
                | _ -> None
              with _ -> None
            in
            if session_key = "" then bad_request "session_key is required"
            else if message = "" then bad_request "message is required"
            else
              let cwd_valid =
                match cwd with
                | Some path ->
                    if not (Sys.file_exists path) then
                      Some (Printf.sprintf "cwd path does not exist: %s" path)
                    else if not (Sys.is_directory path) then
                      Some
                        (Printf.sprintf "cwd path is not a directory: %s" path)
                    else None
                | None -> None
              in
              match cwd_valid with
              | Some err -> bad_request err
              | None -> (
                  let* result =
                    Lwt.catch
                      (fun () ->
                        Session.with_registered_notifier session_manager
                          ~key:session_key
                          ~notify:(fun _text -> Lwt.return_unit)
                          (fun () ->
                            let* response =
                              Session.turn session_manager ~key:session_key
                                ~message ?cwd
                                ~snapshot_work_type:Access_snapshot.Room_turn ()
                            in
                            let queued =
                              Session.should_suppress_response response
                            in
                            if
                              (not queued)
                              && not
                                   (Session.take_response_deferred
                                      session_manager ~key:session_key)
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
                        ())))
  | `POST, "/session/compact" -> (
      (* Apply rate limiting like other endpoints *)
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
        | Error msg -> bad_request ("invalid JSON: " ^ msg)
        | Ok json -> (
            let open Yojson.Safe.Util in
            let session_key =
              try json |> member "session_key" |> to_string with _ -> ""
            in
            if session_key = "" then bad_request "session_key is required"
            else
              (* Check context usage before allowing compaction.
                 get_context_usage_percent may return None if the session
                 is not yet loaded into memory (e.g. after daemon restart).
                 In that case, proceed with compact which will lazily load
                 the session from DB. *)
              let min_compaction_percent = 20 in
              let usage =
                Session.get_context_usage_percent session_manager
                  ~key:session_key
              in
              let skip_low_usage =
                match usage with
                | Some (percent, estimated_tokens, context_window)
                  when percent < min_compaction_percent ->
                    Some (percent, estimated_tokens, context_window)
                | _ -> None
              in
              match skip_low_usage with
              | Some (percent, estimated_tokens, context_window) ->
                  let message =
                    Printf.sprintf
                      "Context usage is only %d%% (%d/%d tokens). Compaction \
                       is only recommended when usage exceeds %d%%."
                      percent estimated_tokens context_window
                      min_compaction_percent
                  in
                  json_string_response
                    (Yojson.Safe.to_string
                       (`Assoc
                          [
                            ("compacted", `Bool false);
                            ("message", `String message);
                            ( "stats",
                              `Assoc
                                [
                                  ("context_usage_percent", `Int percent);
                                  ("estimated_tokens", `Int estimated_tokens);
                                  ("context_window", `Int context_window);
                                ] );
                          ]))
              | None -> (
                  let pre_stats = usage in
                  let debug_notify, debug_fields = make_json_debug_capture () in
                  let* result =
                    Lwt.catch
                      (fun () ->
                        let* compaction_result =
                          Session.with_registered_notifier session_manager
                            ~key:session_key ~notify:debug_notify (fun () ->
                              Session.compact session_manager ~key:session_key
                                ())
                        in
                        Lwt.return (Ok compaction_result))
                      (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
                  in
                  let stats_json =
                    match pre_stats with
                    | Some (percent, estimated_tokens, context_window) ->
                        `Assoc
                          [
                            ("context_usage_percent", `Int percent);
                            ("estimated_tokens", `Int estimated_tokens);
                            ("context_window", `Int context_window);
                          ]
                    | None -> `Assoc []
                  in
                  match result with
                  | Ok (Ok true) ->
                      json_string_response
                        (Yojson.Safe.to_string
                           (`Assoc
                              ([
                                 ("compacted", `Bool true);
                                 ( "message",
                                   `String
                                     "Session history compacted. Older \
                                      messages have been summarized." );
                                 ("stats", stats_json);
                               ]
                              @ debug_fields ())))
                  | Ok (Ok false) ->
                      json_string_response
                        (Yojson.Safe.to_string
                           (`Assoc
                              ([
                                 ("compacted", `Bool false);
                                 ( "message",
                                   `String
                                     "Nothing to compact — session history is \
                                      already short enough." );
                                 ("stats", stats_json);
                               ]
                              @ debug_fields ())))
                  | Ok (Error err) ->
                      Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
                        ~headers:json_headers
                        ~body:
                          (Yojson.Safe.to_string
                             (`Assoc [ ("error", `String err) ]))
                        ()
                  | Error err ->
                      Cohttp_lwt_unix.Server.respond_string
                        ~status:`Internal_server_error ~headers:json_headers
                        ~body:
                          (Yojson.Safe.to_string
                             (`Assoc [ ("error", `String err) ]))
                        ())))
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
                     "invalid update mode '%s'; expected auto, git, binary, or \
                      pkg"
                     mode_raw)
            | Some mode -> (
                match daemon_run_update_command with
                | None ->
                    Cohttp_lwt_unix.Server.respond_string
                      ~status:`Internal_server_error ~headers:json_headers
                      ~body:{|{"error":"daemon update not available"}|} ()
                | Some run_update_command -> (
                    Logs.info (fun m ->
                        m "POST /daemon/update: initiating update (mode=%s)"
                          mode_raw);
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
  | `POST, "/chat/stream" ->
      Http_server_chat.handle_chat_stream ~session_manager ~require_pairing
        ~auth_token ?ip_limiter ?session_limiter ?pairing req body
  | _ -> (
      let* webhook_result =
        Http_server_webhooks.handle ~session_manager ~auth_token ?slack_config
          ?github_config ?config ?github_api_limiter ?slack_event_limiter
          ?teams_event_limiter ?slack_run_update_command ?web_channel
          ?whatsapp_config ?line_config ?lark_config ?teams_config ?pairing
          ?runner_tokens ?ask_fn meth path req body
      in
      match webhook_result with
      | Some resp -> Lwt.return resp
      | None ->
          let* _ = Cohttp_lwt.Body.drain_body body in
          Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
            ~headers:json_headers ~body:{|{"error":"not found"}|} ())

let start ~port ~host ~require_pairing ~auth_token ~session_manager
    ?daemon_run_update_command ?slack_config ?github_config ?config
    ?github_api_limiter ?ip_limiter ?session_limiter ?slack_event_limiter
    ?teams_event_limiter ?slack_run_update_command ?web_channel ?whatsapp_config
    ?line_config ?lark_config ?teams_config ?pairing ?runner_tokens ?ask_fn
    ?ui_server ?stop () =
  let open Lwt.Syntax in
  let callback =
    handler ~session_manager ~require_pairing ~auth_token
      ?daemon_run_update_command ?slack_config ?github_config ?config
      ?github_api_limiter ?ip_limiter ?session_limiter ?slack_event_limiter
      ?teams_event_limiter ?slack_run_update_command ?web_channel
      ?whatsapp_config ?line_config ?lark_config ?teams_config ?pairing
      ?runner_tokens ?ask_fn ?ui_server
  in
  let* ctx = Conduit_lwt_unix.init ~src:host () in
  let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
  let server = Cohttp_lwt_unix.Server.make ~callback () in
  match stop with
  | Some stop ->
      Cohttp_lwt_unix.Server.create ~ctx ~stop ~mode:(`TCP (`Port port)) server
  | None -> Cohttp_lwt_unix.Server.create ~ctx ~mode:(`TCP (`Port port)) server
