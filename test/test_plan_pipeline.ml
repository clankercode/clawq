let init_db () = Memory.init ~db_path:":memory:" ()

let with_temp_dir f =
  let dir = Filename.temp_file "clawq-plan-test" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Fun.protect
    (fun () -> f dir)
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))

let with_temp_git_repo f =
  with_temp_dir (fun repo ->
      let cmd =
        Printf.sprintf "git -C %s init -q >/dev/null 2>&1" (Filename.quote repo)
      in
      (match Sys.command cmd with
      | 0 -> ()
      | code -> Alcotest.failf "git init failed for %s (exit %d)" repo code);
      f repo)

(* 1. init_schema is idempotent *)
let test_init_schema_idempotent () =
  let db = init_db () in
  Plan_pipeline.init_schema db;
  Plan_pipeline.init_schema db;
  (* Should not throw *)
  Alcotest.(check pass) "idempotent" () ()

(* 2. create inserts a pipeline with correct defaults *)
let test_create_inserts_defaults () =
  with_temp_git_repo (fun repo ->
      let db = init_db () in
      Plan_pipeline.init_schema db;
      let pipeline =
        Plan_pipeline.create ~db ~prompt:"add a greet command" ~repo_path:repo
          ~model_config:Plan_pipeline.default_model_config
      in
      Alcotest.(check bool) "id >= 1" true (pipeline.Plan_pipeline.id >= 1);
      Alcotest.(check string)
        "prompt" "add a greet command" pipeline.Plan_pipeline.prompt;
      Alcotest.(check string) "repo_path" repo pipeline.Plan_pipeline.repo_path;
      Alcotest.(check string) "status" "running" pipeline.Plan_pipeline.status;
      Alcotest.(check bool)
        "max_plan_review_iters default" true
        (pipeline.Plan_pipeline.max_plan_review_iters = 3);
      Alcotest.(check bool)
        "max_code_review_iters default" true
        (pipeline.Plan_pipeline.max_code_review_iters = 3);
      Alcotest.(check bool)
        "pipeline_dir nonempty" true
        (String.length pipeline.Plan_pipeline.pipeline_dir > 0))

(* 3. build_stage_prompt Planning contains expected sections *)
let test_build_stage_prompt_planning () =
  with_temp_git_repo (fun repo ->
      let db = init_db () in
      Plan_pipeline.init_schema db;
      let pipeline =
        Plan_pipeline.create ~db ~prompt:"add greet" ~repo_path:repo
          ~model_config:Plan_pipeline.default_model_config
      in
      let prompt =
        Plan_pipeline.build_stage_prompt ~stage:Plan_pipeline.Planning ~pipeline
      in
      let contains sub =
        let hlen = String.length prompt in
        let nlen = String.length sub in
        let rec loop i =
          if i + nlen > hlen then false
          else if String.sub prompt i nlen = sub then true
          else loop (i + 1)
        in
        nlen > 0 && loop 0
      in
      Alcotest.(check bool)
        "contains '## Overview'" true (contains "## Overview");
      Alcotest.(check bool)
        "contains '## Relevant Files'" true
        (contains "## Relevant Files");
      Alcotest.(check bool)
        "contains '## Implementation Steps'" true
        (contains "## Implementation Steps");
      Alcotest.(check bool)
        "contains PLAN_WRITTEN" true (contains "PLAN_WRITTEN");
      Alcotest.(check bool) "contains repo path" true (contains repo))

(* 4. build_stage_prompt PlanReview contains PLAN_STABLE instruction *)
let test_build_stage_prompt_plan_review () =
  with_temp_git_repo (fun repo ->
      let db = init_db () in
      Plan_pipeline.init_schema db;
      let pipeline =
        Plan_pipeline.create ~db ~prompt:"test" ~repo_path:repo
          ~model_config:Plan_pipeline.default_model_config
      in
      let prompt =
        Plan_pipeline.build_stage_prompt ~stage:(Plan_pipeline.PlanReview 0)
          ~pipeline
      in
      let contains sub =
        let hlen = String.length prompt in
        let nlen = String.length sub in
        let rec loop i =
          if i + nlen > hlen then false
          else if String.sub prompt i nlen = sub then true
          else loop (i + 1)
        in
        nlen > 0 && loop 0
      in
      Alcotest.(check bool) "contains PLAN_STABLE" true (contains "PLAN_STABLE"))

(* 5. check_plan_stable returns true when hash unchanged *)
let test_check_plan_stable_hash_unchanged () =
  with_temp_dir (fun dir ->
      with_temp_git_repo (fun repo ->
          let db = init_db () in
          Plan_pipeline.init_schema db;
          let pipeline =
            Plan_pipeline.create ~db ~prompt:"test" ~repo_path:repo
              ~model_config:Plan_pipeline.default_model_config
          in
          (* Override pipeline_dir to our temp dir *)
          let pipeline = { pipeline with Plan_pipeline.pipeline_dir = dir } in
          (* Write plan.md *)
          let plan_path = Plan_pipeline.plan_file_path pipeline in
          let oc = open_out plan_path in
          output_string oc "# My Plan\n\n## Overview\nTest plan.\n";
          close_out oc;
          (* Save hash before *)
          Plan_pipeline.save_plan_hash pipeline;
          (* Do NOT change plan.md *)
          (* Make a minimal bg_task with no log and no result_preview *)
          let fake_task : Background_task.task =
            {
              id = 99;
              runner = Background_task.Claude;
              model = None;
              repo_path = repo;
              prompt = "review";
              branch = "test";
              worktree_path = None;
              log_path = None;
              status = Background_task.Succeeded;
              session_key = None;
              channel = None;
              channel_id = None;
              pid = None;
              result_preview = None;
              created_at = "";
              started_at = None;
              finished_at = None;
              automerge = false;
              use_worktree = true;
              merge_status = None;
              retry_count = 0;
              parent_task_id = None;
              replaced_by = None;
              runner_session_id = None;
              acp = false;
              agent_name = None;
              notification_status = None;
              notification_error = None;
              notification_attempts = 0;
            }
          in
          let stable =
            Lwt_main.run
              (Plan_pipeline.check_plan_stable ~pipeline ~bg_task:fake_task)
          in
          Alcotest.(check bool) "stable when hash unchanged" true stable))

(* 6. check_plan_stable returns true when PLAN_STABLE in mock output *)
let test_check_plan_stable_marker () =
  with_temp_dir (fun dir ->
      with_temp_git_repo (fun repo ->
          let db = init_db () in
          Plan_pipeline.init_schema db;
          let pipeline =
            Plan_pipeline.create ~db ~prompt:"test" ~repo_path:repo
              ~model_config:Plan_pipeline.default_model_config
          in
          let pipeline = { pipeline with Plan_pipeline.pipeline_dir = dir } in
          let fake_task : Background_task.task =
            {
              id = 100;
              runner = Background_task.Claude;
              model = None;
              repo_path = repo;
              prompt = "review";
              branch = "test";
              worktree_path = None;
              log_path = None;
              status = Background_task.Succeeded;
              session_key = None;
              channel = None;
              channel_id = None;
              pid = None;
              result_preview = Some "The plan looks good. PLAN_STABLE";
              created_at = "";
              started_at = None;
              finished_at = None;
              automerge = false;
              use_worktree = true;
              merge_status = None;
              retry_count = 0;
              parent_task_id = None;
              replaced_by = None;
              runner_session_id = None;
              acp = false;
              agent_name = None;
              notification_status = None;
              notification_error = None;
              notification_attempts = 0;
            }
          in
          let stable =
            Lwt_main.run
              (Plan_pipeline.check_plan_stable ~pipeline ~bg_task:fake_task)
          in
          Alcotest.(check bool) "stable when marker in preview" true stable))

(* 7. pipeline_dir_root returns correct path *)
let test_pipeline_dir_root () =
  let root = Plan_pipeline.pipeline_dir_root () in
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let expected = Filename.concat (Filename.concat home ".clawq") "plans" in
  Alcotest.(check string) "pipeline_dir_root" expected root

(* 8. Stage transitions: Planning -> PlanReview 0 -> PlanReview 1 (after change) -> Coding *)
let test_stage_transitions () =
  let fake_task : Background_task.task =
    {
      id = 1;
      runner = Background_task.Claude;
      model = None;
      repo_path = "/tmp";
      prompt = "review";
      branch = "test";
      worktree_path = None;
      log_path = None;
      status = Background_task.Succeeded;
      session_key = None;
      channel = None;
      channel_id = None;
      pid = None;
      result_preview = None;
      created_at = "";
      started_at = None;
      finished_at = None;
      automerge = false;
      use_worktree = true;
      merge_status = None;
      retry_count = 0;
      parent_task_id = None;
      replaced_by = None;
      runner_session_id = None;
      acp = false;
      agent_name = None;
      notification_status = None;
      notification_error = None;
      notification_attempts = 0;
    }
  in
  with_temp_git_repo (fun repo ->
      let db = init_db () in
      Plan_pipeline.init_schema db;
      let mc =
        {
          Plan_pipeline.planner_model = None;
          reviewer_model = None;
          coder_model = None;
          max_plan_review_iters = 3;
          max_code_review_iters = 3;
        }
      in
      let pipeline =
        Plan_pipeline.create ~db ~prompt:"test" ~repo_path:repo ~model_config:mc
      in
      (* Planning -> PlanReview 0 (not stable) *)
      let s0 =
        Plan_pipeline.advance_stage ~pipeline ~bg_task:fake_task
          ~is_stable:false
      in
      Alcotest.(check string)
        "Planning -> PlanReview 0"
        (Plan_pipeline.string_of_stage (Plan_pipeline.PlanReview 0))
        (Plan_pipeline.string_of_stage s0);
      (* PlanReview 0 -> PlanReview 1 (not stable) *)
      let pipeline =
        { pipeline with Plan_pipeline.stage = Plan_pipeline.PlanReview 0 }
      in
      let s1 =
        Plan_pipeline.advance_stage ~pipeline ~bg_task:fake_task
          ~is_stable:false
      in
      Alcotest.(check string)
        "PlanReview 0 -> PlanReview 1"
        (Plan_pipeline.string_of_stage (Plan_pipeline.PlanReview 1))
        (Plan_pipeline.string_of_stage s1);
      (* PlanReview 1 -> Coding (stable) *)
      let pipeline =
        { pipeline with Plan_pipeline.stage = Plan_pipeline.PlanReview 1 }
      in
      let s2 =
        Plan_pipeline.advance_stage ~pipeline ~bg_task:fake_task ~is_stable:true
      in
      Alcotest.(check string)
        "PlanReview 1 -> Coding (stable)"
        (Plan_pipeline.string_of_stage Plan_pipeline.Coding)
        (Plan_pipeline.string_of_stage s2))

(* 9. Stage transitions: force-stop at max_plan_review_iters *)
let test_stage_force_stop_at_max () =
  let fake_task : Background_task.task =
    {
      id = 1;
      runner = Background_task.Claude;
      model = None;
      repo_path = "/tmp";
      prompt = "review";
      branch = "test";
      worktree_path = None;
      log_path = None;
      status = Background_task.Succeeded;
      session_key = None;
      channel = None;
      channel_id = None;
      pid = None;
      result_preview = None;
      created_at = "";
      started_at = None;
      finished_at = None;
      automerge = false;
      use_worktree = true;
      merge_status = None;
      retry_count = 0;
      parent_task_id = None;
      replaced_by = None;
      runner_session_id = None;
      acp = false;
      agent_name = None;
      notification_status = None;
      notification_error = None;
      notification_attempts = 0;
    }
  in
  with_temp_git_repo (fun repo ->
      let db = init_db () in
      Plan_pipeline.init_schema db;
      let mc =
        {
          Plan_pipeline.planner_model = None;
          reviewer_model = None;
          coder_model = None;
          max_plan_review_iters = 2;
          max_code_review_iters = 2;
        }
      in
      let pipeline =
        Plan_pipeline.create ~db ~prompt:"test" ~repo_path:repo ~model_config:mc
      in
      (* PlanReview 1 with max=2, not stable -> should go to Coding *)
      let pipeline =
        { pipeline with Plan_pipeline.stage = Plan_pipeline.PlanReview 1 }
      in
      let s =
        Plan_pipeline.advance_stage ~pipeline ~bg_task:fake_task
          ~is_stable:false
      in
      Alcotest.(check string)
        "PlanReview max reached -> Coding"
        (Plan_pipeline.string_of_stage Plan_pipeline.Coding)
        (Plan_pipeline.string_of_stage s))

(* 10. plan cancel sets status=cancelled *)
let test_cancel_pipeline () =
  with_temp_git_repo (fun repo ->
      let db = init_db () in
      Plan_pipeline.init_schema db;
      let pipeline =
        Plan_pipeline.create ~db ~prompt:"cancel test" ~repo_path:repo
          ~model_config:Plan_pipeline.default_model_config
      in
      let id = pipeline.Plan_pipeline.id in
      (match Plan_pipeline.cancel_pipeline ~db ~id with
      | Ok _ -> ()
      | Error msg -> Alcotest.fail ("cancel failed: " ^ msg));
      match Plan_pipeline.get_pipeline ~db ~id with
      | None -> Alcotest.fail "pipeline not found after cancel"
      | Some p ->
          Alcotest.(check string)
            "status=cancelled" "cancelled" p.Plan_pipeline.status)

let suite =
  [
    ("init_schema_idempotent", `Quick, test_init_schema_idempotent);
    ("create_inserts_defaults", `Quick, test_create_inserts_defaults);
    ("build_stage_prompt_planning", `Quick, test_build_stage_prompt_planning);
    ( "build_stage_prompt_plan_review",
      `Quick,
      test_build_stage_prompt_plan_review );
    ( "check_plan_stable_hash_unchanged",
      `Quick,
      test_check_plan_stable_hash_unchanged );
    ("check_plan_stable_marker", `Quick, test_check_plan_stable_marker);
    ("pipeline_dir_root", `Quick, test_pipeline_dir_root);
    ("stage_transitions", `Quick, test_stage_transitions);
    ("stage_force_stop_at_max", `Quick, test_stage_force_stop_at_max);
    ("cancel_pipeline", `Quick, test_cancel_pipeline);
  ]
