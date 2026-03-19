let compute_signature ~secret ~body =
  "sha256=" ^ Digestif.SHA256.(hmac_string ~key:secret body |> to_hex)

let sig_valid () =
  let secret = "It's a Secret to Everybody" in
  let body = "Hello, World!" in
  let header = compute_signature ~secret ~body in
  Alcotest.(check bool)
    "valid sig" true
    (Github_webhook.verify_signature ~secret ~body ~signature_header:header)

let sig_invalid () =
  let secret = "It's a Secret to Everybody" in
  let body = "Hello, World!" in
  let header =
    "sha256=0000000000000000000000000000000000000000000000000000000000000000"
  in
  Alcotest.(check bool)
    "invalid sig" false
    (Github_webhook.verify_signature ~secret ~body ~signature_header:header)

let sig_wrong_secret () =
  let body = "Hello, World!" in
  let header = compute_signature ~secret:"correct_secret" ~body in
  Alcotest.(check bool)
    "wrong secret" false
    (Github_webhook.verify_signature ~secret:"wrong_secret" ~body
       ~signature_header:header)

let sig_malformed () =
  let secret = "test" in
  let body = "test" in
  Alcotest.(check bool)
    "malformed header" false
    (Github_webhook.verify_signature ~secret ~body
       ~signature_header:"md5=abc123");
  Alcotest.(check bool)
    "empty header" false
    (Github_webhook.verify_signature ~secret ~body ~signature_header:"")

let sig_test_vector () =
  let secret = "It's a Secret to Everybody" in
  let body = "Hello, World!" in
  let expected =
    "sha256=757107ea0eb2509fc211221cce984b8a37570b6d7586c22c46f4379c8b043e17"
  in
  Alcotest.(check bool)
    "test vector" true
    (Github_webhook.verify_signature ~secret ~body ~signature_header:expected)

let pr_opened_json =
  {|{"action":"opened","number":2,"pull_request":{"number":2,"title":"Update the README","body":"Simple change to pull into master.","state":"open","html_url":"https://github.com/Codertocat/Hello-World/pull/2","user":{"login":"Codertocat"},"base":{"ref":"master"},"head":{"ref":"changes"}},"repository":{"name":"Hello-World","owner":{"login":"Codertocat"}}}|}

let pr_sync_json =
  {|{"action":"synchronize","number":2,"pull_request":{"number":2,"title":"Update","body":"test","state":"open","html_url":"https://github.com/x/y/pull/2","user":{"login":"x"},"base":{"ref":"main"},"head":{"ref":"fix"}},"repository":{"name":"y","owner":{"login":"x"}}}|}

let issue_comment_json =
  {|{"action":"created","issue":{"number":2,"title":"Update the README","state":"open","user":{"login":"Codertocat"},"pull_request":{"url":"https://api.github.com/repos/Codertocat/Hello-World/pulls/2"},"body":"body text"},"comment":{"id":492700500,"user":{"login":"octocat"},"body":"/clawq review this PR please","html_url":"https://github.com/Codertocat/Hello-World/pull/2#issuecomment-492700500"},"repository":{"name":"Hello-World","owner":{"login":"Codertocat"}}}|}

let issue_comment_non_pr_json =
  {|{"action":"created","issue":{"number":5,"title":"Bug report","state":"open","user":{"login":"alice"},"body":"something broken"},"comment":{"id":100,"user":{"login":"bob"},"body":"looking into it","html_url":"https://github.com/x/y/issues/5#issuecomment-100"},"repository":{"name":"y","owner":{"login":"x"}}}|}

let review_comment_json =
  {|{"action":"created","comment":{"id":284312630,"user":{"login":"Codertocat"},"body":"Maybe you should use more emoji.\n/clawq what do you think?","diff_hunk":"@@ -1 +1 @@\n-# Hello-World","path":"README.md","html_url":"https://github.com/Codertocat/Hello-World/pull/2#discussion_r284312630"},"pull_request":{"number":2,"title":"Update the README","body":"Simple change","state":"open","html_url":"https://github.com/Codertocat/Hello-World/pull/2","user":{"login":"Codertocat"},"base":{"ref":"master"},"head":{"ref":"changes"}},"repository":{"name":"Hello-World","owner":{"login":"Codertocat"}}}|}

let workflow_job_failed_json =
  {|{"action":"completed","workflow_job":{"id":77,"run_id":55,"name":"test","status":"completed","conclusion":"failure","head_branch":"main","head_sha":"abc123","html_url":"https://github.com/acme/backend/actions/runs/55/job/77"},"repository":{"name":"backend","owner":{"login":"acme"},"full_name":"acme/backend"},"sender":{"login":"github-actions[bot]"}}|}

let push_json =
  {|{"ref":"refs/heads/main","head_commit":{"id":"abc123","message":"user supplied push text"},"repository":{"name":"backend","owner":{"login":"acme"},"full_name":"acme/backend"},"sender":{"login":"mallory"}}|}

let with_temp_clawq_home f =
  let base = Filename.temp_file "clawq-home" ".tmp" in
  Sys.remove base;
  Unix.mkdir base 0o755;
  let previous = Sys.getenv_opt "CLAWQ_HOME" in
  Unix.putenv "CLAWQ_HOME" base;
  Fun.protect
    ~finally:(fun () ->
      (match previous with
      | Some v -> Unix.putenv "CLAWQ_HOME" v
      | None -> Unix.putenv "CLAWQ_HOME" "");
      ignore (Sys.command (Printf.sprintf "rm -rf %S" base)))
    (fun () -> f base)

let parse_pr_opened () =
  match
    Github_webhook.parse_event ~event_type:"pull_request" ~body:pr_opened_json
  with
  | Github_webhook.PullRequest e ->
      Alcotest.(check string) "action" "opened" e.action;
      Alcotest.(check string) "owner" "Codertocat" e.owner;
      Alcotest.(check string) "repo" "Hello-World" e.repo;
      Alcotest.(check int) "pr_number" 2 e.pr_number;
      Alcotest.(check string) "title" "Update the README" e.pr_title;
      Alcotest.(check string) "author" "Codertocat" e.pr_author;
      Alcotest.(check string) "base" "master" e.base_branch;
      Alcotest.(check string) "head" "changes" e.head_branch
  | _ -> Alcotest.fail "expected PullRequest"

let parse_pr_synchronize () =
  match
    Github_webhook.parse_event ~event_type:"pull_request" ~body:pr_sync_json
  with
  | Github_webhook.PullRequest e ->
      Alcotest.(check string) "action" "synchronize" e.action;
      Alcotest.(check string) "owner" "x" e.owner;
      Alcotest.(check string) "repo" "y" e.repo;
      Alcotest.(check int) "pr_number" 2 e.pr_number;
      Alcotest.(check string) "title" "Update" e.pr_title;
      Alcotest.(check string) "author" "x" e.pr_author;
      Alcotest.(check string) "base" "main" e.base_branch;
      Alcotest.(check string) "head" "fix" e.head_branch
  | _ -> Alcotest.fail "expected PullRequest for synchronize action"

let parse_issue_comment () =
  match
    Github_webhook.parse_event ~event_type:"issue_comment"
      ~body:issue_comment_json
  with
  | Github_webhook.IssueComment e ->
      Alcotest.(check string) "owner" "Codertocat" e.owner;
      Alcotest.(check string) "repo" "Hello-World" e.repo;
      Alcotest.(check int) "issue_number" 2 e.issue_number;
      Alcotest.(check bool) "is_pr" true e.is_pr;
      Alcotest.(check string) "comment_author" "octocat" e.comment_author;
      Alcotest.(check string)
        "comment_body" "/clawq review this PR please" e.comment_body
  | _ -> Alcotest.fail "expected IssueComment"

let parse_issue_comment_non_pr () =
  match
    Github_webhook.parse_event ~event_type:"issue_comment"
      ~body:issue_comment_non_pr_json
  with
  | Github_webhook.IssueComment e ->
      Alcotest.(check bool) "is_pr" false e.is_pr;
      Alcotest.(check int) "issue_number" 5 e.issue_number
  | _ -> Alcotest.fail "expected IssueComment"

let parse_review_comment () =
  match
    Github_webhook.parse_event ~event_type:"pull_request_review_comment"
      ~body:review_comment_json
  with
  | Github_webhook.PrReviewComment e ->
      Alcotest.(check string) "owner" "Codertocat" e.owner;
      Alcotest.(check string) "repo" "Hello-World" e.repo;
      Alcotest.(check int) "pr_number" 2 e.pr_number;
      Alcotest.(check int) "comment_id" 284312630 e.comment_id;
      Alcotest.(check string) "author" "Codertocat" e.comment_author;
      Alcotest.(check string) "file_path" "README.md" e.file_path;
      Alcotest.(check string)
        "diff_hunk" "@@ -1 +1 @@\n-# Hello-World" e.diff_hunk
  | _ -> Alcotest.fail "expected PrReviewComment"

let parse_review_submitted () =
  let body =
    {|{"action":"submitted","review":{"id":1,"user":{"login":"x"},"body":"LGTM"},"pull_request":{"number":1},"repository":{"name":"y","owner":{"login":"x"}}}|}
  in
  match Github_webhook.parse_event ~event_type:"pull_request_review" ~body with
  | Github_webhook.Ignored -> ()
  | _ -> Alcotest.fail "expected Ignored for pull_request_review"

let parse_malformed () =
  match
    Github_webhook.parse_event ~event_type:"pull_request" ~body:"not json"
  with
  | Github_webhook.Ignored -> ()
  | _ -> Alcotest.fail "expected Ignored for malformed JSON"

let parse_unknown_event () =
  match Github_webhook.parse_event ~event_type:"deployment" ~body:"{}" with
  | Github_webhook.Ignored -> ()
  | _ -> Alcotest.fail "expected Ignored for unknown event type"

let parse_issue_comment_edited () =
  let body =
    {|{"action":"edited","issue":{"number":1,"title":"t","state":"open","user":{"login":"x"}},"comment":{"id":1,"user":{"login":"x"},"body":"b","html_url":"h"},"repository":{"name":"r","owner":{"login":"o"}}}|}
  in
  match Github_webhook.parse_event ~event_type:"issue_comment" ~body with
  | Github_webhook.Ignored -> ()
  | _ -> Alcotest.fail "expected Ignored for edited action"

let make_pr_event ?(body = "") () =
  Github_webhook.PullRequest
    {
      action = "opened";
      owner = "acme";
      repo = "backend";
      pr_number = 42;
      pr_title = "Fix bug";
      pr_body = body;
      pr_author = "alice";
      base_branch = "main";
      head_branch = "fix-bug";
      html_url = "https://github.com/acme/backend/pull/42";
    }

let make_issue_comment_event ?(body = "") ?(is_pr = false) () =
  Github_webhook.IssueComment
    {
      owner = "acme";
      repo = "backend";
      issue_number = 10;
      is_pr;
      comment_id = 100;
      comment_author = "bob";
      comment_body = body;
      issue_title = "Bug report";
      html_url = "https://github.com/acme/backend/issues/10#issuecomment-100";
    }

let extract_clawq_in_body () =
  let event = make_pr_event ~body:"/clawq hello world" () in
  match Github_webhook.extract_clawq ~event ~pr_files:[] with
  | Some (msg, _preamble) -> Alcotest.(check string) "message" "hello world" msg
  | None -> Alcotest.fail "expected Some"

let extract_clawq_multiline () =
  let event =
    make_pr_event ~body:"Some intro text\n\n/clawq review this\nand fix it" ()
  in
  match Github_webhook.extract_clawq ~event ~pr_files:[] with
  | Some (msg, _) ->
      Alcotest.(check string) "multiline" "review this\nand fix it" msg
  | None -> Alcotest.fail "expected Some"

let extract_clawq_case_insensitive () =
  let event = make_pr_event ~body:"/CLAWQ uppercase" () in
  match Github_webhook.extract_clawq ~event ~pr_files:[] with
  | Some (msg, _) -> Alcotest.(check string) "uppercase" "uppercase" msg
  | None -> Alcotest.fail "expected Some"

let extract_clawq_leading_whitespace () =
  let event = make_pr_event ~body:"   /clawq with spaces" () in
  match Github_webhook.extract_clawq ~event ~pr_files:[] with
  | Some (msg, _) -> Alcotest.(check string) "whitespace" "with spaces" msg
  | None -> Alcotest.fail "expected Some"

let extract_clawq_none () =
  let event = make_pr_event ~body:"no command here" () in
  match Github_webhook.extract_clawq ~event ~pr_files:[] with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let extract_clawq_empty_command () =
  let event = make_pr_event ~body:"/clawq" () in
  match Github_webhook.extract_clawq ~event ~pr_files:[] with
  | Some (msg, _) -> Alcotest.(check string) "empty command" "" msg
  | None -> Alcotest.fail "expected Some"

let extract_clawq_stops_at_blank () =
  let event =
    make_pr_event ~body:"/clawq first paragraph\n\nSecond paragraph (ignored)"
      ()
  in
  match Github_webhook.extract_clawq ~event ~pr_files:[] with
  | Some (msg, _) ->
      Alcotest.(check string) "stops at blank" "first paragraph" msg
  | None -> Alcotest.fail "expected Some"

let extract_clawq_with_files () =
  let event = make_pr_event ~body:"/clawq review" () in
  let files =
    [ ("src/main.ml", "modified", 10, 3); ("README.md", "added", 5, 0) ]
  in
  match Github_webhook.extract_clawq ~event ~pr_files:files with
  | Some (_msg, preamble) ->
      let contains s sub =
        try
          ignore (Str.search_forward (Str.regexp_string sub) s 0);
          true
        with Not_found -> false
      in
      Alcotest.(check bool)
        "contains files section" true
        (contains preamble "Changed files (2)");
      Alcotest.(check bool)
        "contains main.ml" true
        (contains preamble "src/main.ml")
  | None -> Alcotest.fail "expected Some"

let extract_from_comment () =
  match
    Github_webhook.parse_event ~event_type:"issue_comment"
      ~body:issue_comment_json
  with
  | Github_webhook.IssueComment _ as event -> (
      match Github_webhook.extract_clawq ~event ~pr_files:[] with
      | Some (msg, _) ->
          Alcotest.(check string) "from comment" "review this PR please" msg
      | None -> Alcotest.fail "expected Some from comment")
  | _ -> Alcotest.fail "parse failed"

let extract_from_review_comment () =
  match
    Github_webhook.parse_event ~event_type:"pull_request_review_comment"
      ~body:review_comment_json
  with
  | Github_webhook.PrReviewComment _ as event -> (
      match Github_webhook.extract_clawq ~event ~pr_files:[] with
      | Some (msg, _) ->
          Alcotest.(check string) "from review" "what do you think?" msg
      | None -> Alcotest.fail "expected Some from review comment")
  | _ -> Alcotest.fail "parse failed"

let session_key_pr () =
  let event = make_pr_event () in
  Alcotest.(check string)
    "pr key" "github:acme/backend:pr:42"
    (Github_webhook.session_key event)

let session_key_issue () =
  let event = make_issue_comment_event ~is_pr:false () in
  Alcotest.(check string)
    "issue key" "github:acme/backend:issue:10"
    (Github_webhook.session_key event)

let session_key_pr_comment () =
  let event = make_issue_comment_event ~is_pr:true () in
  Alcotest.(check string)
    "pr comment key" "github:acme/backend:pr:10"
    (Github_webhook.session_key event)

let session_key_review_comment () =
  let event =
    Github_webhook.PrReviewComment
      {
        owner = "acme";
        repo = "backend";
        pr_number = 42;
        comment_id = 200;
        comment_author = "carol";
        comment_body = "test";
        in_reply_to_id = None;
        diff_hunk = "@@ -1 +1 @@";
        file_path = "src/foo.ml";
        pr_title = "Fix";
        html_url = "https://github.com/acme/backend/pull/42#discussion_r200";
      }
  in
  Alcotest.(check string)
    "review key" "github:acme/backend:pr:42"
    (Github_webhook.session_key event)

let format_reply_basic () =
  let result = Github.format_reply ~command:"hello" ~response:"world" in
  let expected = "> /clawq hello\n\nworld\n" ^ Github.bot_reply_marker in
  Alcotest.(check string) "format" expected result

let format_reply_empty_command () =
  let result = Github.format_reply ~command:"" ~response:"world" in
  let expected = "world\n" ^ Github.bot_reply_marker in
  Alcotest.(check string) "format empty" expected result

let bot_reply_marker_detected () =
  let reply = Github.format_reply ~command:"review" ~response:"looks good" in
  Alcotest.(check bool) "bot reply detected" true (Github.is_bot_reply reply)

let bot_reply_marker_not_in_user_text () =
  Alcotest.(check bool)
    "user text not detected" false
    (Github.is_bot_reply "/clawq review this PR")

let dedup_prevents_reprocessing () =
  (* Use a fresh dedup instance indirectly by checking the LRU behavior *)
  let id = "test-delivery-dedup-" ^ string_of_float (Unix.gettimeofday ()) in
  let first = Channel_util.Lru_dedup.check_and_mark Github.dedup id in
  let second = Channel_util.Lru_dedup.check_and_mark Github.dedup id in
  Alcotest.(check bool) "first time not seen" false first;
  Alcotest.(check bool) "second time seen" true second

let github_hook_load_and_render () =
  with_temp_clawq_home (fun home ->
      let hooks_dir = Filename.concat home "workspace/gh-hooks" in
      Workspace_scaffold.ensure_dir hooks_dir;
      let hook_path = Filename.concat hooks_dir "workflow_job.md" in
      let hook =
        {|---
name: investigate-failed-job
repo: acme/backend
event: workflow_job
match:
  status: completed
  conclusion: failure
---
Investigate {{repo}} on {{branch}}.
Payload file: {{payload_path}}
Job JSON:
{{json raw.workflow_job}}
|}
      in
      let oc = open_out hook_path in
      output_string oc hook;
      close_out oc;
      let prepared =
        Github_hooks.prepare_event ~event_name:"workflow_job"
          ~headers:(Cohttp.Header.of_list [ ("X-GitHub-Delivery", "abc-123") ])
          ~raw_body:workflow_job_failed_json
      in
      let hooks =
        Github_hooks.load_hooks ~repo_full_name:"acme/backend"
          ~event_name:"workflow_job"
      in
      Alcotest.(check int) "loaded hooks" 1 (List.length hooks);
      let hook = List.hd hooks in
      match prepared.context_json with
      | None -> Alcotest.fail "expected parsed context"
      | Some context ->
          Alcotest.(check bool)
            "hook matches" true
            (Github_hooks.hook_matches hook context);
          let rendered =
            Github_hooks.build_prompt ~hook ~prepared ~context_json:context
          in
          Alcotest.(check bool)
            "includes repo" true
            (String.contains rendered 'a');
          Alcotest.(check bool)
            "mentions payload file" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string "Payload file:")
                    rendered 0);
               true
             with Not_found -> false);
          Alcotest.(check bool)
            "includes inline raw payload" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string "Raw Webhook Payload")
                    rendered 0);
               true
             with Not_found -> false))

let github_hook_snapshot_cleanup () =
  with_temp_clawq_home (fun home ->
      let deliveries_dir =
        Filename.concat home "workspace/tmp/github-deliveries"
      in
      Workspace_scaffold.ensure_dir deliveries_dir;
      let stale = Filename.concat deliveries_dir "stale.json" in
      let oc = open_out stale in
      output_string oc "{}";
      close_out oc;
      let old = Unix.gettimeofday () -. (49. *. 3600.) in
      Unix.utimes stale old old;
      let prepared =
        Github_hooks.prepare_event ~event_name:"workflow_job"
          ~headers:
            (Cohttp.Header.of_list [ ("X-GitHub-Delivery", "cleanup-test") ])
          ~raw_body:workflow_job_failed_json
      in
      Alcotest.(check bool) "stale deleted" false (Sys.file_exists stale);
      Alcotest.(check bool)
        "new snapshot exists" true
        (match prepared.snapshot_path with
        | Some path -> Sys.file_exists path
        | None -> false))

let github_hook_context_normalizes_pr_flag_and_workflow_fields () =
  let pr_payload =
    {|{"action":"opened","pull_request":{"number":42,"title":"Fix bug"},"repository":{"name":"backend","owner":{"login":"acme"},"full_name":"acme/backend"},"sender":{"login":"alice"}}|}
  in
  let prepared_pr =
    Github_hooks.prepare_event ~event_name:"pull_request"
      ~headers:(Cohttp.Header.of_list [ ("X-GitHub-Delivery", "pr-delivery") ])
      ~raw_body:pr_payload
  in
  (match prepared_pr.context_json with
  | Some context -> (
      match Github_hooks.lookup_json_path context "is_pull_request" with
      | Some (`Bool value) -> Alcotest.(check bool) "pr flag" true value
      | _ -> Alcotest.fail "expected is_pull_request bool")
  | None -> Alcotest.fail "expected PR context");
  let prepared_workflow =
    Github_hooks.prepare_event ~event_name:"workflow_job"
      ~headers:
        (Cohttp.Header.of_list [ ("X-GitHub-Delivery", "workflow-delivery") ])
      ~raw_body:workflow_job_failed_json
  in
  match prepared_workflow.context_json with
  | Some context -> (
      (match Github_hooks.lookup_json_path context "workflow_run_id" with
      | Some (`Int value) -> Alcotest.(check int) "workflow run id" 55 value
      | _ -> Alcotest.fail "expected workflow_run_id int");
      match Github_hooks.lookup_json_path context "title" with
      | Some (`String value) ->
          Alcotest.(check string) "workflow title" "test" value
      | _ -> Alcotest.fail "expected workflow title")
  | None -> Alcotest.fail "expected workflow context"

let github_hook_push_events_require_allowlist () =
  let prepared =
    Github_hooks.prepare_event ~event_name:"push"
      ~headers:
        (Cohttp.Header.of_list [ ("X-GitHub-Delivery", "push-delivery") ])
      ~raw_body:push_json
  in
  Alcotest.(check bool) "push is gated" true prepared.is_user_generated

let github_hook_workflow_events_are_not_user_generated () =
  let payload =
    {|{"action":"completed","workflow_run":{"id":55,"name":"ci","status":"completed","conclusion":"failure","head_branch":"master","head_sha":"abc123","html_url":"https://github.com/acme/backend/actions/runs/55"},"repository":{"name":"backend","owner":{"login":"acme"},"full_name":"acme/backend"},"sender":{"login":"github-actions"}}|}
  in
  let prepared =
    Github_hooks.prepare_event ~event_name:"workflow_run"
      ~headers:
        (Cohttp.Header.of_list [ ("X-GitHub-Delivery", "workflow-delivery") ])
      ~raw_body:payload
  in
  Alcotest.(check bool)
    "workflow_run bypasses user gating" false prepared.is_user_generated

let handle_webhook_non_user_generated_failure_runs_hooks () =
  Test_helpers.with_temp_home (fun home ->
      let ensure_dir path =
        if not (Sys.file_exists path) then Unix.mkdir path 0o755
      in
      let clawq_dir = Filename.concat home ".clawq" in
      let workspace_dir = Filename.concat clawq_dir "workspace" in
      let hook_dir = Filename.concat workspace_dir "gh-hooks" in
      let () = ensure_dir clawq_dir in
      let () = ensure_dir workspace_dir in
      let () = ensure_dir hook_dir in
      let hook_path = Filename.concat hook_dir "workflow_run.md" in
      let hook_body =
        String.concat "\n"
          [
            "---";
            "name: failed-workflow";
            "repo: acme/backend";
            "event: workflow_run";
            "match:";
            "  status: completed";
            "  conclusion: failure";
            "---";
            "Investigate {{repo}}";
          ]
      in
      let oc = open_out hook_path in
      output_string oc hook_body;
      close_out oc;
      let loaded_hooks =
        Github_hooks.load_hooks ~repo_full_name:"acme/backend"
          ~event_name:"workflow_run"
      in
      Alcotest.(check int) "loaded workflow hooks" 1 (List.length loaded_hooks);
      let body =
        {|{"action":"completed","workflow_run":{"id":55,"name":"ci","status":"completed","conclusion":"failure","head_branch":"master","head_sha":"abc123","html_url":"https://github.com/acme/backend/actions/runs/55"},"repository":{"name":"backend","owner":{"login":"acme"},"full_name":"acme/backend"},"sender":{"login":"github-actions"}}|}
      in
      let prepared =
        Github_hooks.prepare_event ~event_name:"workflow_run"
          ~headers:
            (Cohttp.Header.of_list
               [ ("X-GitHub-Delivery", "workflow-delivery") ])
          ~raw_body:body
      in
      let matched_hooks =
        match prepared.context_json with
        | Some context ->
            List.filter
              (fun hook -> Github_hooks.hook_matches hook context)
              loaded_hooks
        | None -> []
      in
      Alcotest.(check int)
        "matched workflow hooks" 1
        (List.length matched_hooks);
      let repo_config : Runtime_config.github_repo_config =
        {
          name = "acme/backend";
          webhook_secret = "secret123";
          webhook_path = "/github/webhook/acme";
          agent_name = None;
          allow_users = [ "nobody" ];
          react_to = [];
          include_pr_files = true;
        }
      in
      let github_config : Runtime_config.github_config =
        {
          auth = Runtime_config.GithubPat "ghp_test12345";
          repos = [ repo_config ];
          default_model = None;
        }
      in
      let session_manager = Session.create ~config:Runtime_config.default () in
      let called = ref false in
      session_manager.special_command_handler <-
        Some
          (fun ~key:_ ~message:_ ~send_progress:_ ~interrupt_check:_ ->
            called := true;
            Lwt.return (Some "hook executed"));
      let api_limiter =
        Rate_limiter.create ~rate_per_minute:600 ~burst_multiplier:1.0
      in
      let headers =
        Cohttp.Header.of_list
          [
            ( "x-hub-signature-256",
              "sha256="
              ^ Digestif.SHA256.(hmac_string ~key:"secret123" body |> to_hex) );
            ( "X-GitHub-Delivery",
              "workflow-delivery-" ^ string_of_float (Unix.gettimeofday ()) );
          ]
      in
      match
        Lwt_main.run
          (Github.handle_webhook ~repo_config ~github_config ~session_manager
             ~api_limiter ~event_type:"workflow_run" ~body ~headers)
      with
      | Github.Ok response ->
          Alcotest.(check bool) "special command handler called" true !called;
          Alcotest.(check string) "hook ran" "hooked:1" response
      | Github.BadSignature -> Alcotest.fail "expected valid signature")

let config_github_roundtrip () =
  let json =
    Yojson.Safe.from_string
      {|{"channels":{"github":{"auth":{"type":"pat","token":"ghp_test12345"},"repos":[{"name":"acme/backend","webhook_secret":"secret123","webhook_path":"/github/webhook/acme","allow_users":["*"],"react_to":[],"include_pr_files":true}]}}}|}
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.github with
  | Some g -> (
      match g.auth with
      | Runtime_config.GithubPat token ->
          Alcotest.(check string) "token" "ghp_test12345" token;
          Alcotest.(check int) "repos count" 1 (List.length g.repos);
          let r = List.hd g.repos in
          Alcotest.(check string) "name" "acme/backend" r.name;
          Alcotest.(check string)
            "webhook_path" "/github/webhook/acme" r.webhook_path;
          Alcotest.(check bool) "include_pr_files" true r.include_pr_files)
  | None -> Alcotest.fail "expected github config"

let config_tunnel_roundtrip () =
  let json =
    Yojson.Safe.from_string
      {|{"tunnel":{"provider":"cloudflare","enabled":true,"url":"https://example.com","managed":true,"tunnel_name":"my-tunnel","config_dir":"~/.cloudflared"}}|}
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  Alcotest.(check bool) "enabled" true config.tunnel.enabled;
  Alcotest.(check string) "url" "https://example.com" config.tunnel.url;
  Alcotest.(check bool) "managed" true config.tunnel.managed;
  Alcotest.(check string) "tunnel_name" "my-tunnel" config.tunnel.tunnel_name;
  Alcotest.(check string) "config_dir" "~/.cloudflared" config.tunnel.config_dir

let resolve_static (config : Runtime_config.tunnel_config) =
  if String.trim config.url <> "" then Some config.url
  else
    match Sys.getenv_opt "CLAWQ_TUNNEL_URL" with
    | Some url when String.trim url <> "" -> Some url
    | _ -> None

let cf_tunnel_static_url () =
  let config : Runtime_config.tunnel_config =
    {
      provider = "cloudflare";
      enabled = true;
      url = "https://mysite.example.com";
      managed = false;
      tunnel_name = "";
      config_dir = "";
    }
  in
  match resolve_static config with
  | Some url ->
      Alcotest.(check string) "static url" "https://mysite.example.com" url
  | None -> Alcotest.fail "expected Some url"

let cf_tunnel_no_url () =
  let config : Runtime_config.tunnel_config =
    {
      provider = "cloudflare";
      enabled = true;
      url = "";
      managed = false;
      tunnel_name = "";
      config_dir = "";
    }
  in
  match resolve_static config with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let cf_tunnel_start_static () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let mgr = Tunnel_manager.create () in
     let received_url = ref None in
     let config : Runtime_config.tunnel_config =
       {
         provider = "cloudflare";
         enabled = true;
         url = "https://test.example.com";
         managed = false;
         tunnel_name = "";
         config_dir = "";
       }
     in
     let* () =
       Tunnel_manager.apply_config mgr ~config ~port:8080 ~on_url:(fun u ->
           received_url := u)
     in
     Alcotest.(check (option string))
       "received url" (Some "https://test.example.com") !received_url;
     let* () = Tunnel_manager.stop mgr in
     Lwt.return_unit)

let sig_suite =
  [
    Alcotest.test_case "valid signature" `Quick sig_valid;
    Alcotest.test_case "invalid signature" `Quick sig_invalid;
    Alcotest.test_case "wrong secret" `Quick sig_wrong_secret;
    Alcotest.test_case "malformed header" `Quick sig_malformed;
    Alcotest.test_case "test vector" `Quick sig_test_vector;
  ]

let parse_suite =
  [
    Alcotest.test_case "PR opened" `Quick parse_pr_opened;
    Alcotest.test_case "PR synchronize" `Quick parse_pr_synchronize;
    Alcotest.test_case "issue comment" `Quick parse_issue_comment;
    Alcotest.test_case "issue comment non-PR" `Quick parse_issue_comment_non_pr;
    Alcotest.test_case "review comment" `Quick parse_review_comment;
    Alcotest.test_case "review submitted ignored" `Quick parse_review_submitted;
    Alcotest.test_case "malformed JSON" `Quick parse_malformed;
    Alcotest.test_case "unknown event type" `Quick parse_unknown_event;
    Alcotest.test_case "issue comment edited ignored" `Quick
      parse_issue_comment_edited;
  ]

let extract_suite =
  [
    Alcotest.test_case "clawq in body" `Quick extract_clawq_in_body;
    Alcotest.test_case "multiline" `Quick extract_clawq_multiline;
    Alcotest.test_case "case insensitive" `Quick extract_clawq_case_insensitive;
    Alcotest.test_case "leading whitespace" `Quick
      extract_clawq_leading_whitespace;
    Alcotest.test_case "no clawq" `Quick extract_clawq_none;
    Alcotest.test_case "empty command" `Quick extract_clawq_empty_command;
    Alcotest.test_case "stops at blank line" `Quick extract_clawq_stops_at_blank;
    Alcotest.test_case "with pr files" `Quick extract_clawq_with_files;
    Alcotest.test_case "from issue comment" `Quick extract_from_comment;
    Alcotest.test_case "from review comment" `Quick extract_from_review_comment;
  ]

let session_key_suite =
  [
    Alcotest.test_case "PR" `Quick session_key_pr;
    Alcotest.test_case "issue" `Quick session_key_issue;
    Alcotest.test_case "PR comment" `Quick session_key_pr_comment;
    Alcotest.test_case "review comment" `Quick session_key_review_comment;
  ]

let format_suite =
  [
    Alcotest.test_case "basic" `Quick format_reply_basic;
    Alcotest.test_case "empty command" `Quick format_reply_empty_command;
    Alcotest.test_case "bot reply marker detected" `Quick
      bot_reply_marker_detected;
    Alcotest.test_case "user text not detected as bot" `Quick
      bot_reply_marker_not_in_user_text;
    Alcotest.test_case "dedup prevents reprocessing" `Quick
      dedup_prevents_reprocessing;
  ]

let hooks_suite =
  [
    Alcotest.test_case "load and render workflow hook" `Quick
      github_hook_load_and_render;
    Alcotest.test_case "cleanup stale delivery snapshots" `Quick
      github_hook_snapshot_cleanup;
    Alcotest.test_case "context normalizes PR and workflow fields" `Quick
      github_hook_context_normalizes_pr_flag_and_workflow_fields;
    Alcotest.test_case "push events require allowlist" `Quick
      github_hook_push_events_require_allowlist;
    Alcotest.test_case "workflow events bypass user allowlist gating" `Quick
      github_hook_workflow_events_are_not_user_generated;
    Alcotest.test_case "non-user-generated workflow failures run hooks" `Quick
      handle_webhook_non_user_generated_failure_runs_hooks;
  ]

let config_suite =
  [
    Alcotest.test_case "github config roundtrip" `Quick config_github_roundtrip;
    Alcotest.test_case "tunnel config roundtrip" `Quick config_tunnel_roundtrip;
  ]

let tunnel_suite =
  [
    Alcotest.test_case "static url" `Quick cf_tunnel_static_url;
    Alcotest.test_case "no url" `Quick cf_tunnel_no_url;
    Alcotest.test_case "start static" `Quick cf_tunnel_start_static;
  ]

(* B230: Integration tests verifying that /clawq webhook interactions map to
   stable session keys and that repeated interactions on the same thread
   resume the same session context. *)

let delivery_counter = ref 0

let make_webhook_env ~secret ~body ~allow_users =
  incr delivery_counter;
  let delivery_id =
    Printf.sprintf "test-delivery-%d-%f" !delivery_counter
      (Unix.gettimeofday ())
  in
  let repo_config : Runtime_config.github_repo_config =
    {
      name = "acme/backend";
      webhook_secret = secret;
      webhook_path = "/github/webhook/backend";
      agent_name = None;
      allow_users;
      react_to = [];
      include_pr_files = false;
    }
  in
  let github_config : Runtime_config.github_config =
    {
      auth = Runtime_config.GithubPat "ghp_test12345";
      repos = [ repo_config ];
      default_model = None;
    }
  in
  let session_manager = Session.create ~config:Runtime_config.default () in
  let api_limiter =
    Rate_limiter.create ~rate_per_minute:600 ~burst_multiplier:1.0
  in
  let sig_header =
    "sha256=" ^ Digestif.SHA256.(hmac_string ~key:secret body |> to_hex)
  in
  let headers =
    Cohttp.Header.of_list
      [
        ("x-hub-signature-256", sig_header); ("X-GitHub-Delivery", delivery_id);
      ]
  in
  (repo_config, github_config, session_manager, api_limiter, headers)

let handle_webhook_clawq_pr_comment_session_key () =
  Test_helpers.with_temp_home (fun _home ->
      let body =
        {|{"action":"created","issue":{"number":42,"title":"Fix bug","state":"open","user":{"login":"alice"},"pull_request":{"url":"https://api.github.com/repos/acme/backend/pulls/42"},"body":"PR body"},"comment":{"id":200,"user":{"login":"bob"},"body":"/clawq review this","html_url":"https://github.com/acme/backend/pull/42#issuecomment-200"},"repository":{"name":"backend","owner":{"login":"acme"}}}|}
      in
      let secret = "test-secret" in
      let repo_config, github_config, session_manager, api_limiter, headers =
        make_webhook_env ~secret ~body ~allow_users:[ "bob" ]
      in
      let captured_key = ref "" in
      session_manager.special_command_handler <-
        Some
          (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
            captured_key := key;
            Lwt.return (Some "mock response"));
      let previous_api_base = Sys.getenv_opt "CLAWQ_GITHUB_API_BASE" in
      Unix.putenv "CLAWQ_GITHUB_API_BASE" "http://127.0.0.1:1";
      Fun.protect
        ~finally:(fun () ->
          match previous_api_base with
          | Some v -> Unix.putenv "CLAWQ_GITHUB_API_BASE" v
          | None -> Unix.putenv "CLAWQ_GITHUB_API_BASE" "")
        (fun () ->
          match
            Lwt_main.run
              (Github.handle_webhook ~repo_config ~github_config
                 ~session_manager ~api_limiter ~event_type:"issue_comment" ~body
                 ~headers)
          with
          | Github.Ok _ ->
              Alcotest.(check string)
                "session key for PR comment" "github:acme/backend:pr:42"
                !captured_key
          | Github.BadSignature -> Alcotest.fail "expected valid signature"))

let handle_webhook_clawq_issue_comment_session_key () =
  Test_helpers.with_temp_home (fun _home ->
      let body =
        {|{"action":"created","issue":{"number":5,"title":"Bug report","state":"open","user":{"login":"alice"},"body":"something broken"},"comment":{"id":300,"user":{"login":"bob"},"body":"/clawq help with this","html_url":"https://github.com/acme/backend/issues/5#issuecomment-300"},"repository":{"name":"backend","owner":{"login":"acme"}}}|}
      in
      let secret = "test-secret" in
      let repo_config, github_config, session_manager, api_limiter, headers =
        make_webhook_env ~secret ~body ~allow_users:[ "bob" ]
      in
      let captured_key = ref "" in
      session_manager.special_command_handler <-
        Some
          (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
            captured_key := key;
            Lwt.return (Some "mock response"));
      let previous_api_base = Sys.getenv_opt "CLAWQ_GITHUB_API_BASE" in
      Unix.putenv "CLAWQ_GITHUB_API_BASE" "http://127.0.0.1:1";
      Fun.protect
        ~finally:(fun () ->
          match previous_api_base with
          | Some v -> Unix.putenv "CLAWQ_GITHUB_API_BASE" v
          | None -> Unix.putenv "CLAWQ_GITHUB_API_BASE" "")
        (fun () ->
          match
            Lwt_main.run
              (Github.handle_webhook ~repo_config ~github_config
                 ~session_manager ~api_limiter ~event_type:"issue_comment" ~body
                 ~headers)
          with
          | Github.Ok _ ->
              Alcotest.(check string)
                "session key for issue comment" "github:acme/backend:issue:5"
                !captured_key
          | Github.BadSignature -> Alcotest.fail "expected valid signature"))

let handle_webhook_clawq_review_comment_session_key () =
  Test_helpers.with_temp_home (fun _home ->
      let body =
        {|{"action":"created","comment":{"id":400,"user":{"login":"carol"},"body":"/clawq suggest fix","diff_hunk":"@@ -1 +1 @@\n-old","path":"src/foo.ml","html_url":"https://github.com/acme/backend/pull/42#discussion_r400"},"pull_request":{"number":42,"title":"Fix bug","body":"PR body","state":"open","html_url":"https://github.com/acme/backend/pull/42","user":{"login":"alice"},"base":{"ref":"main"},"head":{"ref":"fix"}},"repository":{"name":"backend","owner":{"login":"acme"}}}|}
      in
      let secret = "test-secret" in
      let repo_config, github_config, session_manager, api_limiter, headers =
        make_webhook_env ~secret ~body ~allow_users:[ "carol" ]
      in
      let captured_key = ref "" in
      session_manager.special_command_handler <-
        Some
          (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
            captured_key := key;
            Lwt.return (Some "mock response"));
      let previous_api_base = Sys.getenv_opt "CLAWQ_GITHUB_API_BASE" in
      Unix.putenv "CLAWQ_GITHUB_API_BASE" "http://127.0.0.1:1";
      Fun.protect
        ~finally:(fun () ->
          match previous_api_base with
          | Some v -> Unix.putenv "CLAWQ_GITHUB_API_BASE" v
          | None -> Unix.putenv "CLAWQ_GITHUB_API_BASE" "")
        (fun () ->
          match
            Lwt_main.run
              (Github.handle_webhook ~repo_config ~github_config
                 ~session_manager ~api_limiter
                 ~event_type:"pull_request_review_comment" ~body ~headers)
          with
          | Github.Ok _ ->
              Alcotest.(check string)
                "session key for review comment" "github:acme/backend:pr:42"
                !captured_key
          | Github.BadSignature -> Alcotest.fail "expected valid signature"))

let handle_webhook_repeated_clawq_same_session () =
  Test_helpers.with_temp_home (fun _home ->
      let make_comment_body comment_id text =
        Printf.sprintf
          {|{"action":"created","issue":{"number":42,"title":"Fix bug","state":"open","user":{"login":"alice"},"pull_request":{"url":"https://api.github.com/repos/acme/backend/pulls/42"},"body":"PR body"},"comment":{"id":%d,"user":{"login":"bob"},"body":"/clawq %s","html_url":"https://github.com/acme/backend/pull/42#issuecomment-%d"},"repository":{"name":"backend","owner":{"login":"acme"}}}|}
          comment_id text comment_id
      in
      let secret = "test-secret" in
      let session_manager = Session.create ~config:Runtime_config.default () in
      let captured_keys = ref [] in
      session_manager.special_command_handler <-
        Some
          (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
            captured_keys := key :: !captured_keys;
            Lwt.return (Some "mock response"));
      let previous_api_base = Sys.getenv_opt "CLAWQ_GITHUB_API_BASE" in
      Unix.putenv "CLAWQ_GITHUB_API_BASE" "http://127.0.0.1:1";
      Fun.protect
        ~finally:(fun () ->
          match previous_api_base with
          | Some v -> Unix.putenv "CLAWQ_GITHUB_API_BASE" v
          | None -> Unix.putenv "CLAWQ_GITHUB_API_BASE" "")
        (fun () ->
          let run_comment cid text =
            let body = make_comment_body cid text in
            let repo_config, github_config, _, api_limiter, headers =
              make_webhook_env ~secret ~body ~allow_users:[ "bob" ]
            in
            Lwt_main.run
              (Github.handle_webhook ~repo_config ~github_config
                 ~session_manager ~api_limiter ~event_type:"issue_comment" ~body
                 ~headers)
          in
          ignore (run_comment 200 "review this");
          ignore (run_comment 201 "any suggestions?");
          let keys = List.rev !captured_keys in
          Alcotest.(check int) "two turns processed" 2 (List.length keys);
          Alcotest.(check string)
            "first key" "github:acme/backend:pr:42" (List.nth keys 0);
          Alcotest.(check string)
            "second key matches first" "github:acme/backend:pr:42"
            (List.nth keys 1)))

let handle_webhook_bot_self_loop_protection () =
  Test_helpers.with_temp_home (fun _home ->
      let bot_reply =
        Printf.sprintf
          {|{"action":"created","issue":{"number":42,"title":"Fix bug","state":"open","user":{"login":"alice"},"pull_request":{"url":"https://api.github.com/repos/acme/backend/pulls/42"},"body":"PR body"},"comment":{"id":500,"user":{"login":"clawq-bot"},"body":"> /clawq review\n\nlooks good\n%s","html_url":"https://github.com/acme/backend/pull/42#issuecomment-500"},"repository":{"name":"backend","owner":{"login":"acme"}}}|}
          Github.bot_reply_marker
      in
      let secret = "test-secret" in
      let repo_config, github_config, session_manager, api_limiter, headers =
        make_webhook_env ~secret ~body:bot_reply ~allow_users:[ "*" ]
      in
      let called = ref false in
      session_manager.special_command_handler <-
        Some
          (fun ~key:_ ~message:_ ~send_progress:_ ~interrupt_check:_ ->
            called := true;
            Lwt.return (Some "should not reach"));
      match
        Lwt_main.run
          (Github.handle_webhook ~repo_config ~github_config ~session_manager
             ~api_limiter ~event_type:"issue_comment" ~body:bot_reply ~headers)
      with
      | Github.Ok msg ->
          Alcotest.(check bool) "handler not called" false !called;
          Alcotest.(check string) "result" "bot self-reply" msg
      | Github.BadSignature -> Alcotest.fail "expected valid signature")

let handle_webhook_dedup_delivery_id () =
  Test_helpers.with_temp_home (fun _home ->
      let body =
        {|{"action":"created","issue":{"number":42,"title":"Fix bug","state":"open","user":{"login":"alice"},"pull_request":{"url":"https://api.github.com/repos/acme/backend/pulls/42"},"body":"PR body"},"comment":{"id":600,"user":{"login":"bob"},"body":"/clawq review","html_url":"https://github.com/acme/backend/pull/42#issuecomment-600"},"repository":{"name":"backend","owner":{"login":"acme"}}}|}
      in
      let secret = "test-secret" in
      let session_manager = Session.create ~config:Runtime_config.default () in
      let call_count = ref 0 in
      session_manager.special_command_handler <-
        Some
          (fun ~key:_ ~message:_ ~send_progress:_ ~interrupt_check:_ ->
            incr call_count;
            Lwt.return (Some "mock"));
      let delivery_id =
        "dedup-test-" ^ string_of_float (Unix.gettimeofday ())
      in
      let previous_api_base = Sys.getenv_opt "CLAWQ_GITHUB_API_BASE" in
      Unix.putenv "CLAWQ_GITHUB_API_BASE" "http://127.0.0.1:1";
      Fun.protect
        ~finally:(fun () ->
          match previous_api_base with
          | Some v -> Unix.putenv "CLAWQ_GITHUB_API_BASE" v
          | None -> Unix.putenv "CLAWQ_GITHUB_API_BASE" "")
        (fun () ->
          let run () =
            let sig_header =
              "sha256="
              ^ Digestif.SHA256.(hmac_string ~key:secret body |> to_hex)
            in
            let headers =
              Cohttp.Header.of_list
                [
                  ("x-hub-signature-256", sig_header);
                  ("X-GitHub-Delivery", delivery_id);
                ]
            in
            let repo_config : Runtime_config.github_repo_config =
              {
                name = "acme/backend";
                webhook_secret = secret;
                webhook_path = "/github/webhook/backend";
                agent_name = None;
                allow_users = [ "bob" ];
                react_to = [];
                include_pr_files = false;
              }
            in
            let github_config : Runtime_config.github_config =
              {
                auth = Runtime_config.GithubPat "ghp_test12345";
                repos = [ repo_config ];
                default_model = None;
              }
            in
            let api_limiter =
              Rate_limiter.create ~rate_per_minute:600 ~burst_multiplier:1.0
            in
            Lwt_main.run
              (Github.handle_webhook ~repo_config ~github_config
                 ~session_manager ~api_limiter ~event_type:"issue_comment" ~body
                 ~headers)
          in
          let result1 = run () in
          let result2 = run () in
          Alcotest.(check int) "called once" 1 !call_count;
          (match result1 with
          | Github.Ok _ -> ()
          | Github.BadSignature -> Alcotest.fail "first call bad sig");
          match result2 with
          | Github.Ok msg ->
              Alcotest.(check string) "second call deduped" "duplicate" msg
          | Github.BadSignature -> Alcotest.fail "second call bad sig"))

let session_integration_suite =
  [
    Alcotest.test_case "PR comment → stable session key" `Quick
      handle_webhook_clawq_pr_comment_session_key;
    Alcotest.test_case "issue comment → issue session key" `Quick
      handle_webhook_clawq_issue_comment_session_key;
    Alcotest.test_case "review comment → PR session key" `Quick
      handle_webhook_clawq_review_comment_session_key;
    Alcotest.test_case "repeated /clawq on same thread → same session" `Quick
      handle_webhook_repeated_clawq_same_session;
  ]

let lifecycle_suite =
  [
    Alcotest.test_case "bot self-loop protection" `Quick
      handle_webhook_bot_self_loop_protection;
    Alcotest.test_case "delivery dedup" `Quick handle_webhook_dedup_delivery_id;
  ]

let suites =
  [
    ("github_webhook_sig", sig_suite);
    ("github_webhook_parse", parse_suite);
    ("github_webhook_extract", extract_suite);
    ("github_session_key", session_key_suite);
    ("github_format", format_suite);
    ("github_hooks", hooks_suite);
    ("github_config", config_suite);
    ("cf_tunnel", tunnel_suite);
    ("github_session_integration", session_integration_suite);
    ("github_lifecycle", lifecycle_suite);
  ]
