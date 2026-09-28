(* B827: 401/403 responses from Github_api.get_pr_files are permanent auth
   failures. Repeated PR dispatches against a broken credential must not WARN
   per dispatch: at most one WARN per (repo, status) per 10-minute window, with
   a suppressed count when warnings resume. Paging stops after the auth-status
   response and the dispatch still completes without file data. Non-auth
   statuses keep the existing per-occurrence warn behavior.

   Tests use a local Cohttp server as the fake GitHub API (established
   pattern, see test_http_server.ml) and an injected clock so the 10-minute
   window is exercised without sleeping. *)

let with_env key value f =
  let previous = Sys.getenv_opt key in
  Unix.putenv key value;
  Fun.protect f ~finally:(fun () ->
      match previous with
      | Some v -> Unix.putenv key v
      | None -> Unix.putenv key "")

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

let with_captured_logs f =
  let messages = ref [] in
  let reporter =
    {
      Logs.report =
        (fun _src level ~over k msgf ->
          let k _ =
            over ();
            k ()
          in
          msgf @@ fun ?header ?tags:_ fmt ->
          Format.kasprintf
            (fun msg ->
              let level = Logs.level_to_string (Some level) in
              messages := (level, msg) :: !messages;
              k ())
            fmt);
    }
  in
  let prev = Logs.reporter () in
  Logs.set_reporter reporter;
  Fun.protect
    ~finally:(fun () -> Logs.set_reporter prev)
    (fun () ->
      let result = f () in
      (result, List.rev !messages))

let get_pr_files_warns logs =
  List.filter_map
    (fun (level, msg) ->
      if
        level = "warning"
        && Test_helpers.string_contains msg "GitHub API get_pr_files"
      then Some msg
      else None)
    logs

let files_page count =
  Printf.sprintf "[%s]"
    (String.concat ","
       (List.init count (fun i ->
            Printf.sprintf
              {|{"filename":"f%d.ml","status":"modified","additions":1,"deletions":0}|}
              i)))

(* Fake files endpoint state: page-1 requests fail with [fail_status] while
   [failures_left] is positive; every other request returns an [ok_files]-sized
   page (a full page keeps paging going, so a regression that pages past an
   auth failure shows up as extra requests). *)
type files_state = {
  mutable requests : int;
  mutable failures_left : int;
  ok_files : int;
}

(* /repos/{owner}/{repo}/pulls/{n}/files *)
let is_pulls_files_path path =
  match String.split_on_char '/' path with
  | [ ""; "repos"; _owner; _repo; "pulls"; _number; "files" ] -> true
  | _ -> false

let make_files_callback ~fail_status (st : files_state) _conn req body =
  let open Lwt.Syntax in
  let* (_ : string) = Cohttp_lwt.Body.to_string body in
  let uri = Cohttp.Request.uri req in
  let page =
    match Uri.get_query_param uri "page" with
    | Some p -> ( try int_of_string p with _ -> 1)
    | None -> 1
  in
  let status, resp_body =
    if is_pulls_files_path (Uri.path uri) then begin
      st.requests <- st.requests + 1;
      if page = 1 && st.failures_left > 0 then begin
        st.failures_left <- st.failures_left - 1;
        (fail_status, {|{"message":"Bad credentials"}|})
      end
      else (200, files_page st.ok_files)
    end
    else (404, {|{"message":"not found"}|})
  in
  Cohttp_lwt_unix.Server.respond_string
    ~status:(Cohttp.Code.status_of_code status)
    ~body:resp_body ()

(* One get_pr_files dispatch against the fake API; resolves even on failure. *)
let dispatch_pr_files ~owner ~repo ~pull_number =
  Lwt_main.run
    (Github_api.get_pr_files ~app_token:None
       ~auth:(Runtime_config.GithubPat "ghp_test") ~owner ~repo ~pull_number ())

(* Installs a fake clock and resets the auth-failure dedup state; restores the
   real clock when the test body finishes. *)
let with_fake_clock f =
  let clock = ref 0.0 in
  Github_api.set_auth_failure_clock_for_tests (fun () -> !clock);
  Github_api.reset_auth_failures_for_tests ();
  Fun.protect
    ~finally:(fun () ->
      Github_api.set_auth_failure_clock_for_tests Unix.gettimeofday)
    (fun () -> f clock)

let test_auth_failure_warns_once_per_window () =
  let st = { requests = 0; failures_left = max_int; ok_files = 100 } in
  let callback = make_files_callback ~fail_status:401 st in
  with_fake_github_api callback (fun base ->
      with_env "CLAWQ_GITHUB_API_BASE" base (fun () ->
          with_fake_clock (fun clock ->
              let results, logs =
                with_captured_logs (fun () ->
                    List.init 4 (fun i ->
                        clock := float_of_int (i * 30);
                        dispatch_pr_files ~owner:"octo" ~repo:"widgets"
                          ~pull_number:42))
              in
              let warns = get_pr_files_warns logs in
              Alcotest.(check int)
                "one request per dispatch (paging stopped)" 4 st.requests;
              Alcotest.(check int)
                "every dispatch completes degraded" 4
                (List.length (List.filter (fun files -> files = []) results));
              Alcotest.(check int)
                "exactly one warn in the 10m window" 1 (List.length warns);
              Alcotest.(check bool)
                "first warn is the plain status line" true
                (match warns with
                | [ msg ] -> Test_helpers.string_contains msg "returned 401"
                | _ -> false))))

let test_auth_failure_records_last_failure () =
  let st = { requests = 0; failures_left = max_int; ok_files = 100 } in
  let callback = make_files_callback ~fail_status:403 st in
  with_fake_github_api callback (fun base ->
      with_env "CLAWQ_GITHUB_API_BASE" base (fun () ->
          with_fake_clock (fun clock ->
              let _ =
                with_captured_logs (fun () ->
                    clock := 5.0;
                    ignore
                      (dispatch_pr_files ~owner:"octo" ~repo:"widgets"
                         ~pull_number:42);
                    clock := 25.0;
                    ignore
                      (dispatch_pr_files ~owner:"octo" ~repo:"widgets"
                         ~pull_number:42))
              in
              let failures = Github_api.last_auth_failures () in
              Alcotest.(check int)
                "auth failure recorded" 1 (List.length failures);
              match failures with
              | [ f ] ->
                  Alcotest.(check string)
                    "repo recorded" "octo/widgets" f.Github_api.repo_full_name;
                  Alcotest.(check int) "status recorded" 403 f.Github_api.status;
                  Alcotest.(check (float 1e-9))
                    "last seen ts recorded" 25.0 f.Github_api.last_seen_at;
                  Alcotest.(check int)
                    "total count recorded" 2 f.Github_api.total
              | _ -> ())))

let test_dedup_keyed_per_repo_and_status () =
  let st = { requests = 0; failures_left = max_int; ok_files = 100 } in
  let callback = make_files_callback ~fail_status:401 st in
  with_fake_github_api callback (fun base ->
      with_env "CLAWQ_GITHUB_API_BASE" base (fun () ->
          with_fake_clock (fun _ ->
              let _, logs =
                with_captured_logs (fun () ->
                    ignore
                      (dispatch_pr_files ~owner:"octo" ~repo:"widgets"
                         ~pull_number:42);
                    ignore
                      (dispatch_pr_files ~owner:"octo" ~repo:"widgets"
                         ~pull_number:43);
                    ignore
                      (dispatch_pr_files ~owner:"octo" ~repo:"gadgets"
                         ~pull_number:7))
              in
              let warns = get_pr_files_warns logs in
              Alcotest.(check int)
                "separate repos warn independently" 2 (List.length warns);
              Alcotest.(check int)
                "two dedup records" 2
                (List.length (Github_api.last_auth_failures ())))))

let test_window_expiry_resumes_warns_with_suppressed_count () =
  let st = { requests = 0; failures_left = max_int; ok_files = 100 } in
  let callback = make_files_callback ~fail_status:401 st in
  with_fake_github_api callback (fun base ->
      with_env "CLAWQ_GITHUB_API_BASE" base (fun () ->
          with_fake_clock (fun clock ->
              let _, logs =
                with_captured_logs (fun () ->
                    clock := 0.0;
                    ignore
                      (dispatch_pr_files ~owner:"octo" ~repo:"widgets"
                         ~pull_number:42);
                    clock := 100.0;
                    ignore
                      (dispatch_pr_files ~owner:"octo" ~repo:"widgets"
                         ~pull_number:42);
                    clock := 200.0;
                    ignore
                      (dispatch_pr_files ~owner:"octo" ~repo:"widgets"
                         ~pull_number:42);
                    clock := 601.0;
                    ignore
                      (dispatch_pr_files ~owner:"octo" ~repo:"widgets"
                         ~pull_number:42))
              in
              let warns = get_pr_files_warns logs in
              Alcotest.(check int)
                "warns resume after window expiry" 2 (List.length warns);
              Alcotest.(check bool)
                "resumed warn reports the suppressed count" true
                (match warns with
                | [ first; resumed ] ->
                    Test_helpers.string_contains first "returned 401"
                    && (not (Test_helpers.string_contains first "suppressed"))
                    && Test_helpers.string_contains resumed "returned 401"
                    && Test_helpers.string_contains resumed
                         "2 similar warnings suppressed"
                | _ -> false))))

let test_recovers_when_200_follows_401 () =
  let st = { requests = 0; failures_left = 1; ok_files = 2 } in
  let callback = make_files_callback ~fail_status:401 st in
  with_fake_github_api callback (fun base ->
      with_env "CLAWQ_GITHUB_API_BASE" base (fun () ->
          with_fake_clock (fun clock ->
              let (failed, recovered), logs =
                with_captured_logs (fun () ->
                    clock := 0.0;
                    let failed =
                      dispatch_pr_files ~owner:"octo" ~repo:"widgets"
                        ~pull_number:42
                    in
                    clock := 700.0;
                    let recovered =
                      dispatch_pr_files ~owner:"octo" ~repo:"widgets"
                        ~pull_number:42
                    in
                    (failed, recovered))
              in
              let warns = get_pr_files_warns logs in
              Alcotest.(check int)
                "auth-failed dispatch has no files" 0 (List.length failed);
              Alcotest.(check int)
                "recovered dispatch returns files" 2 (List.length recovered);
              Alcotest.(check int)
                "no warn for the healthy dispatch" 1 (List.length warns))))

let test_non_auth_statuses_keep_existing_behavior () =
  List.iter
    (fun status ->
      let st = { requests = 0; failures_left = max_int; ok_files = 100 } in
      let callback = make_files_callback ~fail_status:status st in
      with_fake_github_api callback (fun base ->
          with_env "CLAWQ_GITHUB_API_BASE" base (fun () ->
              with_fake_clock (fun clock ->
                  let (first, second), logs =
                    with_captured_logs (fun () ->
                        clock := 0.0;
                        let first =
                          dispatch_pr_files ~owner:"octo" ~repo:"widgets"
                            ~pull_number:42
                        in
                        clock := 10.0;
                        let second =
                          dispatch_pr_files ~owner:"octo" ~repo:"widgets"
                            ~pull_number:42
                        in
                        (first, second))
                  in
                  let warns = get_pr_files_warns logs in
                  Alcotest.(check int)
                    (Printf.sprintf "status %d warns per occurrence" status)
                    2 (List.length warns);
                  Alcotest.(check bool)
                    (Printf.sprintf "status %d keeps the plain warn format"
                       status)
                    true
                    (List.for_all
                       (fun msg ->
                         Test_helpers.string_contains msg
                           (Printf.sprintf "returned %d" status)
                         && not (Test_helpers.string_contains msg "suppressed"))
                       warns);
                  Alcotest.(check int)
                    (Printf.sprintf "status %d returns no files" status)
                    0
                    (List.length first + List.length second);
                  Alcotest.(check int)
                    (Printf.sprintf "status %d records no auth failure" status)
                    0
                    (List.length (Github_api.last_auth_failures ()))))))
    [ 500; 404; 409 ]

let suite =
  [
    Alcotest.test_case "auth failure warns once per window" `Quick
      test_auth_failure_warns_once_per_window;
    Alcotest.test_case "auth failure records last failure" `Quick
      test_auth_failure_records_last_failure;
    Alcotest.test_case "dedup keyed per repo and status" `Quick
      test_dedup_keyed_per_repo_and_status;
    Alcotest.test_case "window expiry resumes warns with suppressed count"
      `Quick test_window_expiry_resumes_warns_with_suppressed_count;
    Alcotest.test_case "recovers when 200 follows 401" `Quick
      test_recovers_when_200_follows_401;
    Alcotest.test_case "non-auth statuses keep existing behavior" `Quick
      test_non_auth_statuses_keep_existing_behavior;
  ]
