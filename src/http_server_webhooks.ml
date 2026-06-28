let handle ~session_manager ~auth_token ?slack_config ?github_config ?config
    ?github_api_limiter ?slack_event_limiter ?teams_event_limiter
    ?slack_run_update_command ?web_channel ?whatsapp_config ?line_config
    ?lark_config ?teams_config ?pairing ?runner_tokens ?ask_fn meth path req
    body =
  let open Lwt.Syntax in
  match (meth, path) with
  | `POST, path
    when match slack_config with
         | Some sc -> path = sc.Runtime_config.events_path
         | None -> false ->
      let sc = Option.get slack_config in
      Logs.info (fun m ->
          m "Incoming webhook: slack path=%s ip=%s" path
            (Http_server_0_util.client_ip req));
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
        let* resp =
          Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
            ~headers:Http_server_0_util.json_headers
            ~body:{|{"error":"invalid signature"}|} ()
        in
        Lwt.return (Some resp)
      else
        let* result =
          Slack.handle_event ~config:sc ~session_manager
            ?run_update_command:slack_run_update_command
            ?event_limiter:slack_event_limiter body_str
        in
        let* resp =
          Cohttp_lwt_unix.Server.respond_string ~status:`OK
            ~headers:Http_server_0_util.json_headers ~body:result ()
        in
        Lwt.return (Some resp)
  | `POST, path
    when Http_server_0_util.is_github_webhook_path path github_config -> (
      let gc = Option.get github_config in
      let event_type_hdr =
        Cohttp.Header.get (Cohttp.Request.headers req) "x-github-event"
        |> Option.value ~default:"unknown"
      in
      Logs.info (fun m ->
          m "Incoming webhook: github path=%s event=%s ip=%s" path
            event_type_hdr
            (Http_server_0_util.client_ip req));
      let* body_str = Cohttp_lwt.Body.to_string body in
      match Http_server_0_util.lookup_github_repo path gc with
      | Http_server_0_util.Missing_github_repo ->
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
              ~headers:Http_server_0_util.json_headers
              ~body:{|{"error":"not found"}|} ()
          in
          Lwt.return (Some resp)
      | Http_server_0_util.Ambiguous_github_repo ->
          Logs.err (fun m ->
              m
                "Incoming webhook: github path=%s matched multiple repos; \
                 webhook_path values must be unique"
                path);
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`Conflict
              ~headers:Http_server_0_util.json_headers
              ~body:
                {|{"error":"github webhook path is ambiguous; make webhook_path values unique"}|}
              ()
          in
          Lwt.return (Some resp)
      | Http_server_0_util.Found_github_repo repo_config ->
          let event_type =
            match
              Cohttp.Header.get (Cohttp.Request.headers req) "x-github-event"
            with
            | Some v -> v
            | None -> ""
          in
          let req_headers = Cohttp.Request.headers req in
          let api_limiter = Option.get github_api_limiter in
          let sig_header =
            Cohttp.Header.get req_headers "x-hub-signature-256"
            |> Option.value ~default:""
          in
          if
            not
              (Github_webhook.verify_signature
                 ~secret:repo_config.webhook_secret ~body:body_str
                 ~signature_header:sig_header)
          then
            let* resp =
              Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
                ~headers:Http_server_0_util.json_headers
                ~body:{|{"error":"invalid signature"}|} ()
            in
            Lwt.return (Some resp)
          else begin
            Lwt.async (fun () ->
                Lwt.catch
                  (fun () ->
                    let* _result =
                      Github.handle_webhook ~repo_config ~github_config:gc
                        ?config ~session_manager ~api_limiter ~event_type
                        ~body:body_str ~headers:req_headers
                    in
                    Lwt.return_unit)
                  (fun exn ->
                    Logs.err (fun m ->
                        m "GitHub webhook handler error: %s"
                          (Printexc.to_string exn));
                    Lwt.return_unit));
            let* resp =
              Cohttp_lwt_unix.Server.respond_string ~status:`OK
                ~headers:Http_server_0_util.json_headers
                ~body:{|{"status":"accepted"}|} ()
            in
            Lwt.return (Some resp)
          end)
  | meth, path
    when match web_channel with
         | Some (wc : Web_channel.t) ->
             let prefix = wc.config.path_prefix in
             String.length path >= String.length prefix
             && String.sub path 0 (String.length prefix) = prefix
         | None -> false ->
      let wc = Option.get web_channel in
      let* body_str = Cohttp_lwt.Body.to_string body in
      let* resp = Web_channel.handle_request wc path meth req body_str in
      Lwt.return (Some resp)
  | `GET, "/whatsapp/webhook" -> (
      Logs.info (fun m ->
          m "Incoming webhook: whatsapp method=GET path=/whatsapp/webhook ip=%s"
            (Http_server_0_util.client_ip req));
      let* _ = Cohttp_lwt.Body.drain_body body in
      match whatsapp_config with
      | None ->
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
              ~headers:Http_server_0_util.json_headers
              ~body:{|{"error":"not configured"}|} ()
          in
          Lwt.return (Some resp)
      | Some wc -> (
          let uri = Cohttp.Request.uri req in
          match Whatsapp.handle_verify ~config:wc uri with
          | Some challenge ->
              let* resp =
                Cohttp_lwt_unix.Server.respond_string ~status:`OK
                  ~headers:Http_server_0_util.json_headers ~body:challenge ()
              in
              Lwt.return (Some resp)
          | None ->
              let* resp =
                Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
                  ~headers:Http_server_0_util.json_headers
                  ~body:{|{"error":"verification failed"}|} ()
              in
              Lwt.return (Some resp)))
  | `POST, "/whatsapp/webhook" -> (
      Logs.info (fun m ->
          m
            "Incoming webhook: whatsapp method=POST path=/whatsapp/webhook \
             ip=%s"
            (Http_server_0_util.client_ip req));
      match whatsapp_config with
      | None ->
          let* _ = Cohttp_lwt.Body.drain_body body in
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
              ~headers:Http_server_0_util.json_headers
              ~body:{|{"error":"not configured"}|} ()
          in
          Lwt.return (Some resp)
      | Some wc ->
          let* body_str = Cohttp_lwt.Body.to_string body in
          let* () =
            Whatsapp.handle_inbound ~config:wc ~session_mgr:session_manager
              body_str
          in
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`OK
              ~headers:Http_server_0_util.json_headers ~body:{|{"status":"ok"}|}
              ()
          in
          Lwt.return (Some resp))
  | `POST, "/line/webhook" -> (
      Logs.info (fun m ->
          m "Incoming webhook: line method=POST path=/line/webhook ip=%s"
            (Http_server_0_util.client_ip req));
      match line_config with
      | None ->
          let* _ = Cohttp_lwt.Body.drain_body body in
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
              ~headers:Http_server_0_util.json_headers
              ~body:{|{"error":"not configured"}|} ()
          in
          Lwt.return (Some resp)
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
          let* resp =
            if ok then
              Cohttp_lwt_unix.Server.respond_string ~status:`OK
                ~headers:Http_server_0_util.json_headers
                ~body:{|{"status":"ok"}|} ()
            else
              Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
                ~headers:Http_server_0_util.json_headers
                ~body:{|{"error":"invalid signature"}|} ()
          in
          Lwt.return (Some resp))
  | `GET, "/pair" -> (
      let* _ = Cohttp_lwt.Body.drain_body body in
      match pairing with
      | None ->
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
              ~headers:Http_server_0_util.json_headers
              ~body:{|{"error":"not configured"}|} ()
          in
          Lwt.return (Some resp)
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
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`OK
              ~headers:Http_server_0_util.json_headers ~body ()
          in
          Lwt.return (Some resp))
  | `POST, "/pair" -> (
      match pairing with
      | None ->
          let* _ = Cohttp_lwt.Body.drain_body body in
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
              ~headers:Http_server_0_util.json_headers
              ~body:{|{"error":"not configured"}|} ()
          in
          Lwt.return (Some resp)
      | Some p -> (
          let* body_str = Cohttp_lwt.Body.to_string body in
          let code =
            try
              let json = Yojson.Safe.from_string body_str in
              Yojson.Safe.Util.(json |> member "code" |> to_string)
            with _ -> ""
          in
          if code = "" then
            let* resp =
              Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
                ~headers:Http_server_0_util.json_headers
                ~body:{|{"error":"code is required"}|} ()
            in
            Lwt.return (Some resp)
          else
            match Pairing.try_pair p ~code with
            | Pairing.Paired token ->
                let body =
                  `Assoc
                    [ ("status", `String "paired"); ("token", `String token) ]
                  |> Yojson.Safe.to_string
                in
                let* resp =
                  Cohttp_lwt_unix.Server.respond_string ~status:`OK
                    ~headers:Http_server_0_util.json_headers ~body ()
                in
                Lwt.return (Some resp)
            | Pairing.WrongCode ->
                let* resp =
                  Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
                    ~headers:Http_server_0_util.json_headers
                    ~body:{|{"error":"wrong code"}|} ()
                in
                Lwt.return (Some resp)
            | Pairing.Locked until ->
                let body =
                  `Assoc
                    [
                      ("error", `String "locked"); ("locked_until", `Float until);
                    ]
                  |> Yojson.Safe.to_string
                in
                let* resp =
                  Cohttp_lwt_unix.Server.respond_string
                    ~status:`Too_many_requests
                    ~headers:Http_server_0_util.json_headers ~body ()
                in
                Lwt.return (Some resp)
            | Pairing.AlreadyPaired ->
                let* resp =
                  Cohttp_lwt_unix.Server.respond_string ~status:`Conflict
                    ~headers:Http_server_0_util.json_headers
                    ~body:{|{"error":"already paired"}|} ()
                in
                Lwt.return (Some resp)))
  | `POST, "/pair/regenerate" -> (
      let* _ = Cohttp_lwt.Body.drain_body body in
      match pairing with
      | None ->
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
              ~headers:Http_server_0_util.json_headers
              ~body:{|{"error":"not configured"}|} ()
          in
          Lwt.return (Some resp)
      | Some p ->
          if not (Http_server_0_util.auth_ok ~auth_token ?pairing req) then
            let* resp =
              Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
                ~headers:Http_server_0_util.json_headers
                ~body:{|{"error":"unauthorized"}|} ()
            in
            Lwt.return (Some resp)
          else begin
            Pairing.regenerate_code p;
            let s = Pairing.status p in
            let body =
              `Assoc
                [ ("code", `String s.code); ("status", `String "regenerated") ]
              |> Yojson.Safe.to_string
            in
            let* resp =
              Cohttp_lwt_unix.Server.respond_string ~status:`OK
                ~headers:Http_server_0_util.json_headers ~body ()
            in
            Lwt.return (Some resp)
          end)
  | `POST, path
    when match teams_config with
         | Some tc -> path = tc.Runtime_config.webhook_path
         | None -> false -> (
      match teams_config with
      | None ->
          let* _ = Cohttp_lwt.Body.drain_body body in
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
              ~headers:Http_server_0_util.json_headers
              ~body:{|{"error":"not configured"}|} ()
          in
          Lwt.return (Some resp)
      | Some tc ->
          Logs.info (fun m ->
              m "Incoming webhook: teams method=POST path=%s ip=%s" path
                (Http_server_0_util.client_ip req));
          let* body_str = Cohttp_lwt.Body.to_string body in
          let headers = Cohttp.Request.headers req in
          let auth_header =
            Cohttp.Header.get headers "authorization"
            |> Option.value ~default:""
          in
          let is_invoke =
            try
              let json = Yojson.Safe.from_string body_str in
              Yojson.Safe.Util.(json |> member "type" |> to_string) = "invoke"
            with _ -> false
          in
          if is_invoke then
            let* resp =
              Lwt.catch
                (fun () ->
                  let* status_code, resp_body =
                    Teams.handle_invoke ~config:tc ~auth_header body_str
                  in
                  let status = Cohttp.Code.status_of_code status_code in
                  Cohttp_lwt_unix.Server.respond_string ~status
                    ~headers:Http_server_0_util.json_headers ~body:resp_body ())
                (fun exn ->
                  Logs.err (fun m ->
                      m "Teams invoke handler error: %s"
                        (Printexc.to_string exn));
                  Cohttp_lwt_unix.Server.respond_string ~status:`OK
                    ~headers:Http_server_0_util.json_headers
                    ~body:{|{"status":200}|} ())
            in
            Lwt.return (Some resp)
          else begin
            Lwt.async (fun () ->
                Lwt.catch
                  (fun () ->
                    Teams.handle_webhook ~config:tc ~session_manager
                      ?event_limiter:teams_event_limiter ~auth_header body_str)
                  (fun exn ->
                    Logs.err (fun m ->
                        m "Teams webhook handler error: %s"
                          (Printexc.to_string exn));
                    Lwt.return_unit));
            let* resp =
              Cohttp_lwt_unix.Server.respond_string ~status:`Accepted
                ~headers:Http_server_0_util.json_headers
                ~body:{|{"status":"accepted"}|} ()
            in
            Lwt.return (Some resp)
          end)
  | `GET, path
    when match teams_config with
         | Some tc -> path = tc.Runtime_config.webhook_path
         | None -> false ->
      Logs.info (fun m ->
          m "Incoming webhook: teams method=GET path=%s ip=%s" path
            (Http_server_0_util.client_ip req));
      let* _ = Cohttp_lwt.Body.drain_body body in
      let body_str =
        match teams_config with
        | None -> {|{"error":"not configured"}|}
        | Some tc ->
            Printf.sprintf
              {|{"status":"ready","channel":"teams","webhook_path":"%s","app_id_prefix":"%s"}|}
              tc.Runtime_config.webhook_path
              (String.sub tc.Runtime_config.app_id 0
                 (min 8 (String.length tc.Runtime_config.app_id)))
      in
      let* resp =
        Cohttp_lwt_unix.Server.respond_string ~status:`OK
          ~headers:Http_server_0_util.json_headers ~body:body_str ()
      in
      Lwt.return (Some resp)
  | `POST, "/lark/webhook" -> (
      Logs.info (fun m ->
          m "Incoming webhook: lark method=POST path=/lark/webhook ip=%s"
            (Http_server_0_util.client_ip req));
      match lark_config with
      | None ->
          let* _ = Cohttp_lwt.Body.drain_body body in
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
              ~headers:Http_server_0_util.json_headers
              ~body:{|{"error":"not configured"}|} ()
          in
          Lwt.return (Some resp)
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
            let* resp =
              Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
                ~headers:Http_server_0_util.json_headers
                ~body:{|{"error":"invalid signature"}|} ()
            in
            Lwt.return (Some resp)
          else
            let* result =
              Lark.handle_webhook_body ~config:lc ~session_mgr:session_manager
                body_str
            in
            match result with
            | `Challenge resp ->
                let* r =
                  Cohttp_lwt_unix.Server.respond_string ~status:`OK
                    ~headers:Http_server_0_util.json_headers ~body:resp ()
                in
                Lwt.return (Some r)
            | `Ok _ ->
                let* r =
                  Cohttp_lwt_unix.Server.respond_string ~status:`OK
                    ~headers:Http_server_0_util.json_headers
                    ~body:{|{"code":0}|} ()
                in
                Lwt.return (Some r)
            | `Error err ->
                Logs.warn (fun m -> m "Lark webhook error: %s" err);
                let* r =
                  Cohttp_lwt_unix.Server.respond_string
                    ~status:`Internal_server_error
                    ~headers:Http_server_0_util.json_headers
                    ~body:{|{"code":1}|} ()
                in
                Lwt.return (Some r)))
  | `GET, path
    when String.length path > 11 && String.sub path 0 11 = "/downloads/" -> (
      let* _ = Cohttp_lwt.Body.drain_body body in
      let token = String.sub path 11 (String.length path - 11) in
      match Temp_downloads.get token with
      | None ->
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
              ~headers:Http_server_0_util.json_headers
              ~body:{|{"error":"not found or expired"}|} ()
          in
          Lwt.return (Some resp)
      | Some entry ->
          let headers =
            Cohttp.Header.of_list
              [
                ("Content-Type", entry.content_type);
                ( "Content-Disposition",
                  Printf.sprintf "attachment; filename=\"%s\"" entry.filename );
              ]
          in
          let* resp =
            Cohttp_lwt_unix.Server.respond_string ~status:`OK ~headers
              ~body:entry.content ()
          in
          Lwt.return (Some resp))
  | `POST, "/mcp" -> (
      let* body_str = Cohttp_lwt.Body.to_string body in
      let ip = Http_server_0_util.client_ip req in
      if not (Runner_relay.is_loopback ip) then
        let* resp =
          Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
            ~headers:Http_server_0_util.json_headers
            ~body:{|{"error":"localhost only"}|} ()
        in
        Lwt.return (Some resp)
      else
        match (runner_tokens, ask_fn) with
        | None, _ | _, None ->
            let* resp =
              Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
                ~headers:Http_server_0_util.json_headers
                ~body:{|{"error":"runner relay not enabled"}|} ()
            in
            Lwt.return (Some resp)
        | Some tokens, Some ask_fn_val -> (
            let bearer = Http_server_0_util.extract_bearer req in
            match bearer with
            | None ->
                let* resp =
                  Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
                    ~headers:Http_server_0_util.json_headers
                    ~body:{|{"error":"bearer token required"}|} ()
                in
                Lwt.return (Some resp)
            | Some tok -> (
                match Runner_relay.validate_token tokens ~token:tok with
                | None ->
                    let* resp =
                      Cohttp_lwt_unix.Server.respond_string
                        ~status:`Unauthorized
                        ~headers:Http_server_0_util.json_headers
                        ~body:{|{"error":"invalid or expired token"}|} ()
                    in
                    Lwt.return (Some resp)
                | Some entry ->
                    let registry =
                      Mcp_server_http.make_relay_registry ~ask_fn:ask_fn_val
                        ~session_key:entry.session_key
                    in
                    let* status_code, resp_body =
                      Mcp_server_http.handle ~registry ~body:body_str
                    in
                    let status = Cohttp.Code.status_of_code status_code in
                    let* resp =
                      Cohttp_lwt_unix.Server.respond_string ~status
                        ~headers:Http_server_0_util.json_headers ~body:resp_body
                        ()
                    in
                    Lwt.return (Some resp))))
  | `POST, "/runner/ask" -> (
      let* body_str = Cohttp_lwt.Body.to_string body in
      let ip = Http_server_0_util.client_ip req in
      if not (Runner_relay.is_loopback ip) then
        let* resp =
          Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
            ~headers:Http_server_0_util.json_headers
            ~body:{|{"error":"localhost only"}|} ()
        in
        Lwt.return (Some resp)
      else
        match (runner_tokens, ask_fn) with
        | None, _ | _, None ->
            let* resp =
              Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
                ~headers:Http_server_0_util.json_headers
                ~body:{|{"error":"runner relay not enabled"}|} ()
            in
            Lwt.return (Some resp)
        | Some tokens, Some ask_fn_val -> (
            let bearer = Http_server_0_util.extract_bearer req in
            match bearer with
            | None ->
                let* resp =
                  Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
                    ~headers:Http_server_0_util.json_headers
                    ~body:{|{"error":"bearer token required"}|} ()
                in
                Lwt.return (Some resp)
            | Some tok -> (
                match Runner_relay.validate_token tokens ~token:tok with
                | None ->
                    let* resp =
                      Cohttp_lwt_unix.Server.respond_string
                        ~status:`Unauthorized
                        ~headers:Http_server_0_util.json_headers
                        ~body:{|{"error":"invalid or expired token"}|} ()
                    in
                    Lwt.return (Some resp)
                | Some entry -> (
                    let json =
                      try Ok (Yojson.Safe.from_string body_str)
                      with _ -> Error "invalid JSON"
                    in
                    match json with
                    | Error msg ->
                        let* resp =
                          Cohttp_lwt_unix.Server.respond_string
                            ~status:`Bad_request
                            ~headers:Http_server_0_util.json_headers
                            ~body:(Printf.sprintf {|{"error":"%s"}|} msg)
                            ()
                        in
                        Lwt.return (Some resp)
                    | Ok json_val ->
                        let questions =
                          Tools_builtin.parse_questions json_val
                        in
                        if questions = [] then
                          let* resp =
                            Cohttp_lwt_unix.Server.respond_string
                              ~status:`Bad_request
                              ~headers:Http_server_0_util.json_headers
                              ~body:
                                {|{"error":"questions array is empty or missing"}|}
                              ()
                          in
                          Lwt.return (Some resp)
                        else
                          let timeout_s =
                            try
                              Yojson.Safe.Util.(
                                json_val |> member "timeout_s" |> to_int)
                            with _ -> 300
                          in
                          let* result =
                            Runner_relay.relay_question ~ask_fn:ask_fn_val
                              ~session_key:entry.session_key ~questions
                              ~timeout_s
                          in
                          let* resp =
                            match result with
                            | Ok results ->
                                let answers_json =
                                  `List
                                    (List.map
                                       (fun (r : Tools_builtin.question_result)
                                          ->
                                         `Assoc
                                           ([
                                              ("question", `String r.question);
                                              ("answer", `String r.answer);
                                            ]
                                           @
                                           match r.notes with
                                           | Some n -> [ ("notes", `String n) ]
                                           | None -> []))
                                       results)
                                in
                                let body =
                                  `Assoc [ ("answers", answers_json) ]
                                  |> Yojson.Safe.to_string
                                in
                                Cohttp_lwt_unix.Server.respond_string
                                  ~status:`OK
                                  ~headers:Http_server_0_util.json_headers ~body
                                  ()
                            | Error msg ->
                                Cohttp_lwt_unix.Server.respond_string
                                  ~status:`Internal_server_error
                                  ~headers:Http_server_0_util.json_headers
                                  ~body:
                                    (`Assoc [ ("error", `String msg) ]
                                    |> Yojson.Safe.to_string)
                                  ()
                          in
                          Lwt.return (Some resp)))))
  | `POST, "/runner/token" -> (
      let* body_str = Cohttp_lwt.Body.to_string body in
      let ip = Http_server_0_util.client_ip req in
      if not (Runner_relay.is_loopback ip) then
        let* resp =
          Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
            ~headers:Http_server_0_util.json_headers
            ~body:{|{"error":"localhost only"}|} ()
        in
        Lwt.return (Some resp)
      else if not (Http_server_0_util.auth_ok ~auth_token ?pairing req) then
        let* resp =
          Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
            ~headers:Http_server_0_util.json_headers
            ~body:{|{"error":"unauthorized"}|} ()
        in
        Lwt.return (Some resp)
      else
        match runner_tokens with
        | None ->
            let* resp =
              Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
                ~headers:Http_server_0_util.json_headers
                ~body:{|{"error":"runner relay not enabled"}|} ()
            in
            Lwt.return (Some resp)
        | Some tokens -> (
            let json =
              try Ok (Yojson.Safe.from_string body_str)
              with _ -> Error "invalid JSON"
            in
            match json with
            | Error msg ->
                let* resp =
                  Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
                    ~headers:Http_server_0_util.json_headers
                    ~body:(Printf.sprintf {|{"error":"%s"}|} msg)
                    ()
                in
                Lwt.return (Some resp)
            | Ok json_val ->
                let session_key =
                  try
                    Yojson.Safe.Util.(
                      json_val |> member "session_key" |> to_string)
                  with _ -> ""
                in
                if session_key = "" then
                  let* resp =
                    Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
                      ~headers:Http_server_0_util.json_headers
                      ~body:{|{"error":"session_key is required"}|} ()
                  in
                  Lwt.return (Some resp)
                else
                  let task_id =
                    try
                      Some
                        Yojson.Safe.Util.(
                          json_val |> member "task_id" |> to_int)
                    with _ -> None
                  in
                  let ttl_hours =
                    try
                      Yojson.Safe.Util.(
                        json_val |> member "ttl_hours" |> to_int)
                    with _ -> 24
                  in
                  let token =
                    Runner_relay.generate_token tokens ~session_key ?task_id
                      ~ttl_hours ()
                  in
                  let entry =
                    Runner_relay.validate_token tokens ~token |> Option.get
                  in
                  let body =
                    `Assoc
                      [
                        ("token", `String token);
                        ("expires_at", `Float entry.expires_at);
                      ]
                    |> Yojson.Safe.to_string
                  in
                  let* resp =
                    Cohttp_lwt_unix.Server.respond_string ~status:`OK
                      ~headers:Http_server_0_util.json_headers ~body ()
                  in
                  Lwt.return (Some resp)))
  | _ -> Lwt.return None
