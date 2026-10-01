(* B830: webhook ingest bounding — body cap, rate-limiter skip, snapshot
   gating, and single-parse threading. *)

let with_env key value f =
  let previous = Sys.getenv_opt key in
  (match value with Some v -> Unix.putenv key v | None -> Unix.putenv key "");
  Fun.protect f ~finally:(fun () ->
      match previous with
      | Some v -> Unix.putenv key v
      | None -> Unix.putenv key "")

let make_repo_config ~include_pr_files : Runtime_config.github_repo_config =
  {
    Runtime_config.name = "acme/backend";
    webhook_secret = "gate-secret";
    webhook_path = "/github/webhook/backend";
    agent_name = None;
    allow_users = [ "*" ];
    react_to = [ "issue_comment" ];
    include_pr_files;
    respect_quiet_hours = false;
    local_repo_path = None;
  }

let make_github_config ~repo_config : Runtime_config.github_config =
  {
    auth = Runtime_config.GithubPat "ghp_test12345";
    repos = [ repo_config ];
    default_model = None;
    trigger_login = None;
    trigger_label = None;
    auth_credential_handle = None;
  }

(* ---- GitHub webhook body cap (http_server_webhooks) ---- *)

let test_github_webhook_body_cap () =
  let repo_config = make_repo_config ~include_pr_files:false in
  let github_config = make_github_config ~repo_config in
  let run ~content_length ~config =
    let db = Memory.init ~db_path:":memory:" () in
    let session_manager = Session.create ~config ~db () in
    let headers =
      Cohttp.Header.of_list
        ([
           ("X-GitHub-Event", "issue_comment");
           ("X-Hub-Signature-256", "sha256=deadbeef");
         ]
        @
        match content_length with
        | Some len -> [ ("Content-Length", string_of_int len) ]
        | None -> [])
    in
    let req =
      Cohttp.Request.make ~headers ~meth:`POST
        (Uri.of_string "http://127.0.0.1/github/webhook/backend")
    in
    let resp, _body =
      Lwt_main.run
        (Http_server.handler ~session_manager ~require_pairing:false
           ~auth_token:None ~github_config
           ~github_api_limiter:
             (Rate_limiter.create ~rate_per_minute:60 ~burst_multiplier:1.0)
           (Obj.magic ()) req
           (Cohttp_lwt.Body.of_string "{}"))
    in
    Cohttp.Code.code_of_status (Cohttp.Response.status resp)
  in
  let with_cap bytes =
    {
      Runtime_config.default with
      webhooks = { Runtime_config.default.webhooks with max_body_bytes = bytes };
    }
  in
  Alcotest.(check int)
    "oversize rejected with 413" 413
    (run
       ~content_length:(Some ((1024 * 1024) + 1))
       ~config:Runtime_config.default);
  Alcotest.(check int)
    "within cap proceeds to signature check" 403
    (run ~content_length:(Some 2) ~config:Runtime_config.default);
  Alcotest.(check int)
    "missing Content-Length proceeds" 403
    (run ~content_length:None ~config:Runtime_config.default);
  Alcotest.(check int)
    "custom cap rejects" 413
    (run ~content_length:(Some 11) ~config:(with_cap 10));
  Alcotest.(check int)
    "zero cap means unlimited" 403
    (run ~content_length:(Some 99_999_999) ~config:(with_cap 0))

(* ---- fetch_pr_files honors the rate limiter ---- *)

let with_fake_github_api callback f =
  let port = Test_helpers.free_port () in
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  Lwt.async (fun () -> Lwt.pick [ server; stop ]);
  Fun.protect
    ~finally:(fun () -> Lwt.wakeup_later stopper ())
    (fun () -> f (Printf.sprintf "http://127.0.0.1:%d" port))

(* An exhausted limiter skips the API call entirely and returns an empty
   file list; a fresh limiter still reaches the API. *)
let test_fetch_pr_files_skips_api_when_rate_limited () =
  let called = ref false in
  let callback _conn _req _body =
    called := true;
    Cohttp_lwt_unix.Server.respond_string ~status:`Created ~body:"[]" ()
  in
  with_fake_github_api callback (fun github_api_base ->
      with_env "CLAWQ_GITHUB_API_BASE" (Some github_api_base) (fun () ->
          let repo_config = make_repo_config ~include_pr_files:true in
          let github_config = make_github_config ~repo_config in
          let payload =
            {|{"action":"synchronize","number":42,"pull_request":{"number":42,"title":"T","body":"b","state":"open","html_url":"https://github.com/acme/backend/pull/42","user":{"login":"alice"},"base":{"ref":"main"},"head":{"ref":"feat"}},"repository":{"name":"backend","owner":{"login":"acme"},"full_name":"acme/backend"}}|}
          in
          let event =
            Github_webhook.parse_event ~event_type:"pull_request" ~body:payload
          in
          let exhausted =
            Rate_limiter.create ~rate_per_minute:0 ~burst_multiplier:1.0
          in
          let files =
            Lwt_main.run
              (Github.fetch_pr_files ~repo_config ~github_config
                 ~api_limiter:exhausted ~owner:"acme" ~repo:"backend" event)
          in
          Alcotest.(check int)
            "no files while rate limited" 0 (List.length files);
          Alcotest.(check bool) "no API call while rate limited" false !called;
          let fresh =
            Rate_limiter.create ~rate_per_minute:60 ~burst_multiplier:1.0
          in
          let _ =
            Lwt_main.run
              (Github.fetch_pr_files ~repo_config ~github_config
                 ~api_limiter:fresh ~owner:"acme" ~repo:"backend" event)
          in
          Alcotest.(check bool) "fresh limiter reaches API" true !called))

(* ---- delivery snapshots gated by repo/event filters ---- *)

let test_snapshot_gated_by_filters () =
  let base = Filename.temp_file "clawq-home" ".tmp" in
  Sys.remove base;
  Unix.mkdir base 0o755;
  with_env "CLAWQ_HOME" (Some base) (fun () ->
      Fun.protect
        ~finally:(fun () ->
          ignore (Sys.command (Printf.sprintf "rm -rf %S" base)))
        (fun () ->
          let deliveries =
            Filename.concat base "workspace/tmp/github-deliveries"
          in
          let count_snapshots () =
            if Sys.file_exists deliveries then
              Array.length (Sys.readdir deliveries)
            else 0
          in
          let repo_config = make_repo_config ~include_pr_files:false in
          let github_config = make_github_config ~repo_config in
          let api_limiter =
            Rate_limiter.create ~rate_per_minute:600 ~burst_multiplier:1.0
          in
          let config =
            {
              Runtime_config.default with
              channels =
                {
                  Runtime_config.default.channels with
                  github = Some github_config;
                };
            }
          in
          let session_manager = Session.create ~config () in
          let deliver ~event_type ~body ~delivery_id =
            let headers =
              Cohttp.Header.of_list
                [
                  ( "x-hub-signature-256",
                    Test_helpers.github_signature ~secret:"gate-secret" ~body );
                  ("X-GitHub-Delivery", delivery_id);
                ]
            in
            Lwt_main.run
              (Github.handle_webhook ~repo_config ~github_config ~config
                 ~session_manager ~api_limiter ~event_type ~body ~headers)
          in
          let mismatch_body =
            {|{"action":"created","repository":{"name":"backend","owner":{"login":"other"},"full_name":"other/backend"},"sender":{"login":"octocat"}}|}
          in
          (match
             deliver ~event_type:"issue_comment" ~body:mismatch_body
               ~delivery_id:"gate-mismatch"
           with
          | Github.Ok msg ->
              Alcotest.(check string) "repo mismatch" "repo mismatch" msg
          | Github.BadSignature -> Alcotest.fail "expected valid signature");
          Alcotest.(check int)
            "no snapshot for repo mismatch" 0 (count_snapshots ());
          let push_body =
            {|{"ref":"refs/heads/main","repository":{"name":"backend","owner":{"login":"acme"},"full_name":"acme/backend"},"sender":{"login":"octocat"}}|}
          in
          (match
             deliver ~event_type:"push" ~body:push_body
               ~delivery_id:"gate-filtered"
           with
          | Github.Ok msg ->
              Alcotest.(check string) "event filtered" "filtered" msg
          | Github.BadSignature -> Alcotest.fail "expected valid signature");
          Alcotest.(check int)
            "no snapshot for filtered event" 0 (count_snapshots ());
          let passing_body =
            {|{"action":"created","issue":{"number":7,"title":"T","state":"open","user":{"login":"alice"},"body":"b"},"comment":{"id":9,"user":{"login":"octocat"},"body":"plain comment","html_url":"https://github.com/acme/backend/issues/7#issuecomment-9"},"repository":{"name":"backend","owner":{"login":"acme"},"full_name":"acme/backend"},"sender":{"login":"octocat"}}|}
          in
          (match
             deliver ~event_type:"issue_comment" ~body:passing_body
               ~delivery_id:"gate-pass"
           with
          | Github.Ok _ -> ()
          | Github.BadSignature -> Alcotest.fail "expected valid signature");
          Alcotest.(check int)
            "snapshot written after filters pass" 1 (count_snapshots ())))

(* ---- single-parse threading ---- *)

let test_parse_event_uses_threaded_payload () =
  (* The body is invalid JSON while the threaded payload carries the real
     event — proves the single-parse threading consumes the pre-parsed value
     instead of re-parsing the body. *)
  let payload =
    Yojson.Safe.from_string
      {|{"action":"opened","number":2,"pull_request":{"number":2,"title":"Update the README","body":"Simple change to pull into master.","state":"open","html_url":"https://github.com/Codertocat/Hello-World/pull/2","user":{"login":"Codertocat"},"base":{"ref":"master"},"head":{"ref":"changes"}},"repository":{"name":"Hello-World","owner":{"login":"Codertocat"}}}|}
  in
  (match
     Github_webhook.parse_event_opt ~event_type:"pull_request"
       ~body:"not json at all" (Some payload)
   with
  | Github_webhook.PullRequest e ->
      Alcotest.(check int) "pr number from threaded payload" 2 e.pr_number
  | _ -> Alcotest.fail "expected PullRequest");
  match
    Github_webhook.parse_event_opt ~event_type:"pull_request"
      ~body:"not json at all" None
  with
  | Github_webhook.Ignored -> ()
  | _ -> Alcotest.fail "expected Ignored for unparseable body"

(* B830: webhooks ingest bounding config (pool size, queue bound, body cap). *)
let test_parse_webhooks_config () =
  let parse s = Config_loader.parse_config (Yojson.Safe.from_string s) in
  let default = Runtime_config.default.webhooks in
  Alcotest.(check int) "default workers" 4 default.max_concurrent;
  Alcotest.(check int) "default queue" 1000 default.max_queue;
  Alcotest.(check int) "default body bytes" 1048576 default.max_body_bytes;
  let w =
    (parse
       {|{"webhooks":{"max_concurrent":8,"max_queue":50,"max_body_bytes":2048}}|})
      .webhooks
  in
  Alcotest.(check int) "parsed workers" 8 w.max_concurrent;
  Alcotest.(check int) "parsed queue" 50 w.max_queue;
  Alcotest.(check int) "parsed body bytes" 2048 w.max_body_bytes;
  let missing = (parse {|{"workspace":"/tmp"}|}).webhooks in
  Alcotest.(check int)
    "missing section defaults workers" 4 missing.max_concurrent;
  Alcotest.(check int) "missing section defaults queue" 1000 missing.max_queue;
  Alcotest.(check int)
    "missing section defaults body bytes" 1048576 missing.max_body_bytes;
  let bad = (parse {|{"webhooks":{"max_concurrent":"many"}}|}).webhooks in
  Alcotest.(check int) "bad type defaults" 4 bad.max_concurrent

let test_to_json_serializes_webhooks_config () =
  let cfg =
    {
      Runtime_config.default with
      webhooks = { max_concurrent = 8; max_queue = 50; max_body_bytes = 2048 };
    }
  in
  let json = Runtime_config.to_json cfg in
  let open Yojson.Safe.Util in
  Alcotest.(check int)
    "serialized workers" 8
    (json |> member "webhooks" |> member "max_concurrent" |> to_int);
  Alcotest.(check int)
    "serialized queue" 50
    (json |> member "webhooks" |> member "max_queue" |> to_int);
  Alcotest.(check int)
    "serialized body bytes" 2048
    (json |> member "webhooks" |> member "max_body_bytes" |> to_int);
  let default_json = Runtime_config.to_json Runtime_config.default in
  Alcotest.(check bool)
    "default omitted from json" true
    (default_json |> member "webhooks" = `Null)

let suite =
  [
    Alcotest.test_case "github webhook body cap returns 413" `Quick
      test_github_webhook_body_cap;
    Alcotest.test_case "fetch_pr_files skips API when rate limited" `Quick
      test_fetch_pr_files_skips_api_when_rate_limited;
    Alcotest.test_case "delivery snapshots gated by repo/event filters" `Quick
      test_snapshot_gated_by_filters;
    Alcotest.test_case "parse_event uses threaded payload" `Quick
      test_parse_event_uses_threaded_payload;
    Alcotest.test_case "parse webhooks ingest bounds" `Quick
      test_parse_webhooks_config;
    Alcotest.test_case "to_json serializes webhooks config" `Quick
      test_to_json_serializes_webhooks_config;
  ]
