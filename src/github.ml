type webhook_result = Ok of string | BadSignature

let format_reply ~command ~response =
  if command = "" then response
  else Printf.sprintf "> /clawq %s\n\n%s" command response

let handle_webhook ~(repo_config : Runtime_config.github_repo_config)
    ~(github_config : Runtime_config.github_config)
    ~(session_manager : Session.t) ~(api_limiter : Rate_limiter.t) ~event_type
    ~body ~headers =
  let open Lwt.Syntax in
  let signature_header =
    match Cohttp.Header.get headers "x-hub-signature-256" with
    | Some v -> v
    | None -> ""
  in
  if
    not
      (Github_webhook.verify_signature ~secret:repo_config.webhook_secret ~body
         ~signature_header)
  then Lwt.return BadSignature
  else
    let event = Github_webhook.parse_event ~event_type ~body in
    match event with
    | Github_webhook.Ignored -> Lwt.return (Ok "ignored")
    | _ -> (
        let evt_type = Github_webhook.event_type_string event in
        if
          repo_config.react_to <> []
          && not (List.mem evt_type repo_config.react_to)
        then Lwt.return (Ok "filtered")
        else
          let author = Github_webhook.author_of_event event in
          let user_allowed =
            match repo_config.allow_users with
            | [ "*" ] -> true
            | users -> List.mem author users
          in
          if not user_allowed then begin
            Logs.info (fun m ->
                m "GitHub: ignoring event from unauthorized user @%s" author);
            Lwt.return (Ok "user not allowed")
          end
          else
            let owner, repo = Github_webhook.repo_of_event event in
            let* pr_files =
              if repo_config.include_pr_files then
                let pr_n =
                  match event with
                  | Github_webhook.PullRequest e -> e.pr_number
                  | Github_webhook.PrReviewComment e -> e.pr_number
                  | Github_webhook.IssueComment e ->
                      if e.is_pr then e.issue_number else 0
                  | Github_webhook.Ignored -> 0
                in
                if pr_n > 0 then
                  Lwt.catch
                    (fun () ->
                      let* _ok =
                        Rate_limiter.check_and_consume api_limiter
                          ~key:(Printf.sprintf "github:%s/%s" owner repo)
                      in
                      Github_api.get_pr_files ~auth:github_config.auth ~owner
                        ~repo ~pull_number:pr_n)
                    (fun exn ->
                      Logs.warn (fun m ->
                          m "GitHub: failed to fetch PR files: %s"
                            (Printexc.to_string exn));
                      Lwt.return [])
                else Lwt.return []
              else Lwt.return []
            in
            match Github_webhook.extract_clawq ~event ~pr_files with
            | None -> Lwt.return (Ok "no /clawq command")
            | Some (user_message, preamble) ->
                let key = Github_webhook.session_key event in
                let full_message = preamble ^ "\n\n" ^ user_message in
                let gh_channel_name = "github:" ^ owner ^ "/" ^ repo in
                let* result =
                  Lwt.catch
                    (fun () ->
                      let* response =
                        Session.turn session_manager ~key ~message:full_message
                          ~channel_name:gh_channel_name ~channel_type:"dm"
                          ~sender_id:author ()
                      in
                      Lwt.return (Result.Ok response))
                    (fun exn ->
                      Lwt.return (Result.Error (Printexc.to_string exn)))
                in
                let reply_text, log_result =
                  match result with
                  | Result.Ok response ->
                      (format_reply ~command:user_message ~response, "replied")
                  | Result.Error err ->
                      Logs.err (fun m ->
                          m "GitHub: agent error for %s/%s: %s" owner repo err);
                      ( Printf.sprintf "Sorry, an error occurred: %s" err,
                        "error commented" )
                in
                let* () =
                  Lwt.catch
                    (fun () ->
                      let* _ok =
                        Rate_limiter.check_and_consume api_limiter
                          ~key:(Printf.sprintf "github:%s/%s" owner repo)
                      in
                      match event with
                      | Github_webhook.PrReviewComment e ->
                          Github_api.reply_to_review_comment
                            ~auth:github_config.auth ~owner ~repo
                            ~pull_number:e.pr_number ~comment_id:e.comment_id
                            ~body:reply_text
                      | Github_webhook.PullRequest e ->
                          Github_api.post_comment ~auth:github_config.auth
                            ~owner ~repo ~issue_number:e.pr_number
                            ~body:reply_text
                      | Github_webhook.IssueComment e ->
                          Github_api.post_comment ~auth:github_config.auth
                            ~owner ~repo ~issue_number:e.issue_number
                            ~body:reply_text
                      | Github_webhook.Ignored -> Lwt.return_unit)
                    (fun exn ->
                      Logs.err (fun m ->
                          m "GitHub: failed to post reply: %s"
                            (Printexc.to_string exn));
                      Lwt.return_unit)
                in
                Logs.info (fun m ->
                    m "GitHub: %s/%s %s by @%s -> %s" owner repo evt_type author
                      log_result);
                Lwt.return (Ok log_result))
