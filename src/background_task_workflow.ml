(** Workflow-triggered background task launches. *)

include Background_task_spawn

(** [trigger_workflow_from_room_command ~db ~config ~pipeline_name ~inputs
     ~room_id ~requester_id ()] triggers a workflow run from a room command.
    Validates the pipeline, creates a workflow run record, and launches it as a
    background task with room progress reporting.

    Returns [Ok (workflow_run, task_id)] on success or [Error msg] on failure.
*)
let trigger_workflow_from_room_command ~db ~(config : Runtime_config.t)
    ~pipeline_name ~inputs ~room_id ~requester_id () =
  match Structured_pipeline.find_pipeline pipeline_name with
  | None ->
      Error
        (Printf.sprintf
           "Pipeline \"%s\" not found. Use 'clawq pipeline list' to see \
            available pipelines."
           pipeline_name)
  | Some pipeline -> (
      match
        Workflow_run_trigger.validate_and_resolve_inputs ~pipeline ~inputs ()
      with
      | Error msg -> Error msg
      | Ok effective_inputs -> (
          let run =
            Workflow_run_trigger.create ~db ~pipeline_name:pipeline.name
              ~pipeline_version:pipeline.version ~inputs:effective_inputs
              ~trigger_source:
                (Workflow_run_trigger.Room_command { room_id; requester_id })
              ~room_id ~requester_id ()
          in
          let prompt =
            Workflow_run_trigger.build_workflow_prompt ~pipeline
              ~inputs:effective_inputs ()
          in
          match
            launch_room_bg_task ~db ~session_key:room_id ~connector:"" ~room_id
              ~requester_id ~goal:prompt ~use_worktree:false ~config ()
          with
          | Ok task_id ->
              let attached =
                Workflow_run_trigger.set_running ~db ~id:run.id ~task_id
              in
              if not attached then
                Logs.warn (fun m ->
                    m "Workflow run %d: set_running failed" run.id);
              Logs.info (fun m ->
                  m "Workflow run %d launched as bg task %d for pipeline %s"
                    run.id task_id pipeline_name);
              Ok (run, task_id)
          | Error msg ->
              ignore
                (Workflow_run_trigger.set_failed ~db ~id:run.id
                   ~error_message:("Launch failed: " ^ msg));
              Error msg))

(** [trigger_security_review_workflow ~db ~config ~review_run ~room_id
     ~requester_id ~pr_title ~pr_author ~pr_body ~base_branch ~head_branch
     ~pr_files ()] triggers a security review run. If a "security-review"
    pipeline is configured, uses it for multi-step analysis. Otherwise falls
    back to the single-prompt security scan.

    Returns [Ok task_id] on success or [Error msg] on failure. *)
let trigger_security_review_workflow ~db ~(config : Runtime_config.t)
    ~(review_run : Github_review_run.review_run) ~room_id ~requester_id
    ~pr_title ~pr_author ~pr_body ~base_branch ~head_branch ~pr_files () =
  match Structured_pipeline.find_pipeline "security-review" with
  | Some pipeline -> (
      let inputs =
        [
          ("repo", review_run.repo);
          ("pr_number", string_of_int review_run.pr_number);
          ("head_sha", review_run.head_sha);
          ("pr_title", pr_title);
          ("pr_author", pr_author);
          ("base_branch", base_branch);
          ("head_branch", head_branch);
        ]
      in
      match
        Workflow_run_trigger.validate_and_resolve_inputs ~pipeline ~inputs ()
      with
      | Error msg -> Error msg
      | Ok effective_inputs -> (
          let run =
            Workflow_run_trigger.create ~db ~pipeline_name:pipeline.name
              ~pipeline_version:pipeline.version ~inputs:effective_inputs
              ~trigger_source:
                (Workflow_run_trigger.Room_command { room_id; requester_id })
              ~room_id ~requester_id ()
          in
          let prompt =
            Workflow_run_trigger.build_workflow_prompt ~pipeline
              ~inputs:effective_inputs ()
            ^ "\n\n"
            ^ Github_review_run.build_review_prompt ~repo:review_run.repo
                ~pr_number:review_run.pr_number ~pr_title ~pr_author ~pr_body
                ~base_branch ~head_branch ~head_sha:review_run.head_sha
                ~pr_files ~run_kind:review_run.run_kind
                ~trigger_source:review_run.trigger_source ()
          in
          let snap_id =
            Access_snapshot.record_for_work ~db ~config
              ~work_type:Access_snapshot.Background_task ~session_key:room_id ()
          in
          match
            launch_room_bg_task ~db ~session_key:room_id ~connector:"" ~room_id
              ~requester_id ~goal:prompt ~use_worktree:false
              ~access_snapshot_id:snap_id ~config ()
          with
          | Ok task_id ->
              let attached =
                Workflow_run_trigger.set_running ~db ~id:run.id ~task_id
              in
              if not attached then
                Logs.warn (fun m ->
                    m "Workflow run %d: set_running failed" run.id);
              ignore
                (Github_review_run.set_running ~db ~id:review_run.id ~task_id);
              Logs.info (fun m ->
                  m
                    "Security review pipeline run %d launched as bg task %d \
                     for %s PR #%d"
                    run.id task_id review_run.repo review_run.pr_number);
              Ok task_id
          | Error msg ->
              ignore
                (Workflow_run_trigger.set_failed ~db ~id:run.id
                   ~error_message:("Launch failed: " ^ msg));
              let is_pending =
                match Github_review_run.find_by_id ~db ~id:review_run.id with
                | Some r -> r.status = Github_review_run.Pending
                | None -> false
              in
              if is_pending then
                ignore
                  (Github_review_run.set_failed ~db ~id:review_run.id
                     ~error_message:("Pipeline launch failed: " ^ msg));
              Error msg))
  | None ->
      launch_triggered_run ~db ~config ~review_run ~room_id ~requester_id
        ~pr_title ~pr_author ~pr_body ~base_branch ~head_branch ~pr_files ()
