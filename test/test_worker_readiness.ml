(* B776: worker-fleet readiness classification + redaction tests. *)

let off_policy =
  {
    Runner_isolation.mode = Runner_isolation.Off;
    backend = Sandbox.None;
    extra_paths = [];
  }

let base ?(role = Worker_readiness.Worker) () : Worker_readiness.inputs =
  {
    Worker_readiness.role;
    db_present = true;
    worker_id = Some "pc-1";
    token = Some "ghp_supersecrettoken_1234567890";
    runners = [];
    hosts = [ "direct" ];
    repos = [ "o/r" ];
    isolation = off_policy;
    github_configured = true;
  }

let find name checks =
  List.find_opt (fun (c : Worker_readiness.check) -> c.name = name) checks

let level_of name checks =
  match find name checks with
  | Some c -> Worker_readiness.string_of_level c.level
  | None -> "missing"

let test_queue_and_identity_classification () =
  let fail_db = Worker_readiness.run { (base ()) with db_present = false } in
  Alcotest.(check string) "queue fail" "fail" (level_of "queue" fail_db);
  let no_id = Worker_readiness.run { (base ()) with worker_id = None } in
  Alcotest.(check string) "identity fail" "fail" (level_of "worker" no_id);
  let ok = Worker_readiness.run (base ()) in
  Alcotest.(check string) "queue pass" "pass" (level_of "queue" ok);
  Alcotest.(check string) "identity pass" "pass" (level_of "worker" ok)

let test_queue_auth_warns_without_token () =
  let checks = Worker_readiness.run { (base ()) with token = None } in
  Alcotest.(check string) "auth warn" "warn" (level_of "queue-auth" checks)

let test_host_and_repo_classification () =
  let unknown_host =
    Worker_readiness.run { (base ()) with hosts = [ "no-such-host" ] }
  in
  Alcotest.(check string)
    "unknown host fail" "fail"
    (level_of "host:no-such-host" unknown_host);
  let no_repos = Worker_readiness.run { (base ()) with repos = [] } in
  Alcotest.(check string) "no repos fail" "fail" (level_of "repos" no_repos);
  let direct_ok = Worker_readiness.run (base ()) in
  Alcotest.(check string)
    "direct host pass" "pass"
    (level_of "host:direct" direct_ok)

let test_sandbox_off_warns_require_fails () =
  let off = Worker_readiness.run (base ()) in
  Alcotest.(check string) "sandbox off warns" "warn" (level_of "sandbox" off);
  let require_no_backend =
    Worker_readiness.run
      {
        (base ()) with
        isolation =
          {
            Runner_isolation.mode = Runner_isolation.Require;
            backend = Sandbox.None;
            extra_paths = [];
          };
      }
  in
  Alcotest.(check string)
    "require without backend fails" "fail"
    (level_of "sandbox" require_no_backend)

let test_control_plane_role_checks_publisher () =
  let cp =
    Worker_readiness.run (base ~role:Worker_readiness.Control_plane ())
  in
  Alcotest.(check string) "publisher pass" "pass" (level_of "publisher" cp);
  (* control-plane readiness does not demand a worker identity or runners *)
  Alcotest.(check bool)
    "no worker check on control plane" true
    (find "worker" cp = None);
  let no_github =
    Worker_readiness.run
      {
        (base ~role:Worker_readiness.Control_plane ()) with
        github_configured = false;
      }
  in
  Alcotest.(check string)
    "publisher warns without github" "warn"
    (level_of "publisher" no_github)

let test_overall_and_no_secret_leak () =
  let checks = Worker_readiness.run (base ()) in
  (* worker id is present but redacted in its detail *)
  let output = Worker_readiness.format checks in
  Alcotest.(check bool)
    "token value never printed" false
    (String_util.contains output "ghp_supersecrettoken_1234567890");
  (* overall reflects the worst check (warn here: no token + sandbox off) *)
  Alcotest.(check string)
    "overall warn" "warn"
    (Worker_readiness.string_of_level (Worker_readiness.overall checks))

let suite =
  [
    Alcotest.test_case "queue and worker-identity classification" `Quick
      test_queue_and_identity_classification;
    Alcotest.test_case "queue auth warns without token" `Quick
      test_queue_auth_warns_without_token;
    Alcotest.test_case "host and repo classification" `Quick
      test_host_and_repo_classification;
    Alcotest.test_case "sandbox off warns, require without backend fails" `Quick
      test_sandbox_off_warns_require_fails;
    Alcotest.test_case "control-plane role checks the publisher boundary" `Quick
      test_control_plane_role_checks_publisher;
    Alcotest.test_case "overall level and no secret leakage" `Quick
      test_overall_and_no_secret_leak;
  ]
