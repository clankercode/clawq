(* B772: repository-owned execution/publication policy tests. *)

let test_compat_default_when_missing () =
  let policy =
    match Repo_execution_policy.load ~repo_path:"/nonexistent-dir-xyz" with
    | Ok p -> p
    | Error msg -> Alcotest.fail msg
  in
  Alcotest.(check string)
    "draft pr allowed" "draft_pr"
    (Repo_execution_policy.string_of_publication_mode policy.publication);
  Alcotest.(check bool) "legacy automerge" true policy.allow_automerge;
  Alcotest.(check (option string)) "no hardcoded base" None policy.base_branch;
  Alcotest.(check string) "restricted prefix" "clawq/" policy.branch_prefix

let test_policy_parse () =
  let json =
    Yojson.Safe.from_string
      {|{"publication":"reply_only","base_branch":"develop",
         "allow_rebase":false,"allow_automerge":false,
         "forbidden_commands":["git push --force","rm -rf"],
         "forbidden_labels":["af:ready"],"validation":["make test"]}|}
  in
  match Repo_execution_policy.of_json json with
  | Error msg -> Alcotest.fail msg
  | Ok policy ->
      Alcotest.(check string)
        "reply only" "reply_only"
        (Repo_execution_policy.string_of_publication_mode policy.publication);
      Alcotest.(check (option string))
        "base branch" (Some "develop") policy.base_branch;
      Alcotest.(check bool) "rebase forbidden" false policy.allow_rebase;
      Alcotest.(check bool) "automerge forbidden" false policy.allow_automerge;
      Alcotest.(check (list string))
        "forbidden commands"
        [ "git push --force"; "rm -rf" ]
        policy.forbidden_commands;
      Alcotest.(check (list string))
        "forbidden labels" [ "af:ready" ] policy.forbidden_labels

let test_invalid_policy_rejected () =
  (match
     Repo_execution_policy.of_json
       (Yojson.Safe.from_string {|{"publication":"yolo"}|})
   with
  | Ok _ -> Alcotest.fail "invalid publication must be rejected"
  | Error msg ->
      Alcotest.(check bool)
        "actionable" true
        (String_util.contains msg "publication"));
  match
    Repo_execution_policy.of_json
      (Yojson.Safe.from_string {|{"branch_prefix":"../escape/"}|})
  with
  | Ok _ -> Alcotest.fail "path-escaping branch prefix must be rejected"
  | Error _ -> ()

let test_malformed_policy_file_is_error () =
  let dir = Filename.temp_file "clawq-policy" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Unix.mkdir (Filename.concat dir ".clawq") 0o755;
  let oc =
    open_out (Filename.concat dir Repo_execution_policy.policy_file_name)
  in
  output_string oc "{not json";
  close_out oc;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () ->
      match Repo_execution_policy.load ~repo_path:dir with
      | Ok _ -> Alcotest.fail "malformed declared policy must not fall back"
      | Error msg ->
          Alcotest.(check bool)
            "mentions file" true
            (String_util.contains msg "publication-policy"))

let test_branch_and_prompt () =
  let policy =
    { Repo_execution_policy.compat_default with allow_rebase = false }
  in
  Alcotest.(check string)
    "deterministic branch" "clawq/wi-42"
    (Repo_execution_policy.work_item_branch policy ~work_item_id:42);
  let prompt =
    Repo_execution_policy.prompt_fragment policy ~base_branch:"develop"
  in
  Alcotest.(check bool)
    "base branch stated" true
    (String_util.contains prompt "develop");
  Alcotest.(check bool)
    "push forbidden" true
    (String_util.contains prompt "Do NOT push");
  Alcotest.(check bool)
    "rebase forbidden line present" true
    (String_util.contains prompt "NOT rebase")

let git dir args =
  ignore
    (Sys.command
       (Printf.sprintf "git -C %s %s >/dev/null 2>&1" (Filename.quote dir) args))

let test_worktree_commit_state () =
  let dir = Filename.temp_file "clawq-wtstate" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () ->
      git dir "init -b main -q";
      git dir "config user.email t@e.c";
      git dir "config user.name t";
      git dir "commit --allow-empty -m init -q";
      let state base =
        Lwt_main.run
          (Github.worktree_commit_state ~run_git:Github.run_git_argv
             ~worktree:dir ~base_branch:base)
      in
      (match state "main" with
      | Ok `No_commits -> ()
      | _ -> Alcotest.fail "clean checkout should be No_commits");
      git dir "checkout -q -b clawq/wi-1";
      git dir "commit --allow-empty -m change -q";
      (match state "main" with
      | Ok (`Commits 1) -> ()
      | _ -> Alcotest.fail "one commit ahead expected");
      let oc = open_out (Filename.concat dir "dirty.txt") in
      output_string oc "x";
      close_out oc;
      git dir "add dirty.txt";
      match state "main" with
      | Ok `Dirty -> ()
      | _ -> Alcotest.fail "staged change should be Dirty")

let test_pr_publication_idempotent () =
  let db = Memory.init ~db_path:":memory:" () in
  Github_work_item.init_schema db;
  let item =
    match
      Github_work_item.create_if_new ~db ~dedup_key:"o/r#1:comment:1"
        ~repo_full_name:"o/r" ~issue_number:1 ~requester:"alice" ~prompt:"fix"
        ()
    with
    | Ok (Github_work_item.Created item) -> item
    | _ -> Alcotest.fail "create failed"
  in
  ignore
    (Github_work_item.record_pr_publication ~db ~id:item.id ~branch:"clawq/wi-1"
       ~pr_number:None ~publication_status:"pending");
  Alcotest.(check bool)
    "first pr recorded" true
    (Github_work_item.record_pr_publication ~db ~id:item.id ~branch:"clawq/wi-1"
       ~pr_number:(Some 42) ~publication_status:"published");
  Alcotest.(check bool)
    "second pr refused" false
    (Github_work_item.record_pr_publication ~db ~id:item.id ~branch:"clawq/wi-1"
       ~pr_number:(Some 99) ~publication_status:"published");
  match Github_work_item.get ~db ~id:item.id with
  | Some fresh ->
      Alcotest.(check (option int))
        "original pr kept" (Some 42) fresh.published_pr_number;
      Alcotest.(check (option string))
        "branch recorded" (Some "clawq/wi-1") fresh.publication_branch
  | None -> Alcotest.fail "item vanished"

let suite =
  [
    Alcotest.test_case "missing policy uses compat default" `Quick
      test_compat_default_when_missing;
    Alcotest.test_case "policy json parses" `Quick test_policy_parse;
    Alcotest.test_case "invalid policy values rejected" `Quick
      test_invalid_policy_rejected;
    Alcotest.test_case "malformed declared policy is an error" `Quick
      test_malformed_policy_file_is_error;
    Alcotest.test_case "deterministic branch and policy prompt" `Quick
      test_branch_and_prompt;
    Alcotest.test_case "worktree commit state detection" `Quick
      test_worktree_commit_state;
    Alcotest.test_case "duplicate completion cannot create second PR" `Quick
      test_pr_publication_idempotent;
  ]
