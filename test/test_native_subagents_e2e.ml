let enabled () = Sys.getenv_opt "CLAWQ_E2E_MIMO" = Some "1"

let require_enabled () =
  if not (enabled ()) then Alcotest.skip ();
  match Xiaomi.resolve_api_key "xiaomi-token-plan-sgp" with
  | Some _ -> ()
  | None -> Alcotest.skip ()

let excerpt s =
  let n = min 800 (String.length s) in
  String.sub s 0 n

let check_contains name haystack needle =
  if not (Test_helpers.string_contains haystack needle) then
    Alcotest.failf "%s: expected %S in:\n%s" name needle (excerpt haystack)

let run_ok cmd =
  let code = Sys.command cmd in
  if code <> 0 then Alcotest.failf "command failed (%d): %s" code cmd

let with_temp_home f =
  let old = Sys.getenv_opt "CLAWQ_HOME" in
  let dir = Filename.temp_dir "clawq-native-subagents-e2e-home" "" in
  Fun.protect
    ~finally:(fun () ->
      (match old with
      | Some value -> Unix.putenv "CLAWQ_HOME" value
      | None -> Unix.putenv "CLAWQ_HOME" "");
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () ->
      Unix.putenv "CLAWQ_HOME" dir;
      ignore (Dot_dir.ensure ());
      f dir)

let with_temp_git_repo f =
  let dir = Filename.temp_dir "clawq-native-subagents-e2e-repo" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () ->
      run_ok (Printf.sprintf "git -C %s init -q" (Filename.quote dir));
      let readme = Filename.concat dir "README.md" in
      let oc = open_out readme in
      output_string oc "# native subagent e2e\n";
      close_out oc;
      run_ok
        (Printf.sprintf
           "git -C %s -c user.email=e2e@example.invalid -c user.name='E2E' add \
            README.md"
           (Filename.quote dir));
      run_ok
        (Printf.sprintf
           "git -C %s -c user.email=e2e@example.invalid -c user.name='E2E' \
            commit -q -m init"
           (Filename.quote dir));
      f dir)

let e2e_config () =
  let cfg = Config_loader.load () in
  {
    cfg with
    prompt = { cfg.prompt with dynamic_enabled = false };
    security = { cfg.security with tools_enabled = false };
    agent_defaults =
      {
        cfg.agent_defaults with
        show_thinking = false;
        show_tool_calls = false;
        max_tool_iterations = 1;
      };
  }

let open_db () =
  let cfg = Config_loader.load () in
  let db_path =
    if cfg.memory.db_path <> "" then cfg.memory.db_path else Dot_dir.db_path ()
  in
  let db = Memory.init ~db_path ~search_enabled:cfg.memory.search_enabled () in
  Background_task.init_schema db;
  db

let newest_task_id ~db =
  match List.rev (Background_task.list_tasks ~db) with
  | task :: _ -> task.Background_task.id
  | [] -> Alcotest.fail "expected at least one background task"

let wait_terminal ~db ~id =
  let rec loop attempts =
    if attempts <= 0 then Alcotest.failf "task %d did not finish" id
    else
      match Background_task.get_task ~db ~id with
      | Some task when Background_task.is_terminal_status task.status -> task
      | _ ->
          Lwt_main.run (Lwt_unix.sleep 1.0);
          loop (attempts - 1)
  in
  loop 240

let drain_queued ~db ~session_manager =
  Background_task.start_queued_with_local_runner ~timeout_seconds:220.0 ~db
    ~run_turn:(Daemon.run_local_background_turn ~session_manager)
    ~on_task_started:(fun _ -> Lwt.return_unit)
    ~on_task_finished:(fun _ -> Lwt.return_unit)
    ();
  Lwt_main.run (Lwt_unix.sleep 0.1)

let run_task_to_success ~db ~session_manager ~id =
  drain_queued ~db ~session_manager;
  let task = wait_terminal ~db ~id in
  Alcotest.(check string)
    "task succeeded" "succeeded"
    (Background_task.string_of_status task.status);
  task

let models =
  [ "xiaomi-token-plan-sgp:mimo-v2.5-pro"; "xiaomi-token-plan-sgp:mimo-v2.5" ]

let stable_session_key id = Printf.sprintf "__bg_task:%d" id

let store_transcript_probe ~db ~id marker =
  Memory.store_message ~db ~session_key:(stable_session_key id)
    (Provider.make_message ~role:"assistant"
       ~content:(Printf.sprintf "deterministic transcript probe %s" marker))

let start_subagent ~repo ~model ~marker =
  Command_bridge.handle
    [
      "subagents";
      "start";
      "--model";
      model;
      "--agent";
      "coder";
      repo;
      Printf.sprintf
        "Native subagent E2E. Reply with exactly %s and no extra text." marker;
    ]

let test_subagents_start_send_transcript_each_model () =
  require_enabled ();
  with_temp_home (fun _home ->
      with_temp_git_repo (fun repo ->
          let db = open_db () in
          let session_manager = Session.create ~config:(e2e_config ()) ~db () in
          List.iteri
            (fun idx model ->
              let marker = Printf.sprintf "NATIVE_E2E_%d" idx in
              let output = start_subagent ~repo ~model ~marker in
              check_contains "start output mentions subagent" output
                "Queued subagent task";
              let id = newest_task_id ~db in
              ignore (run_task_to_success ~db ~session_manager ~id);
              let full_transcript =
                Command_bridge.handle
                  [
                    "subagents";
                    "transcript";
                    string_of_int id;
                    "--max-lines";
                    "300";
                  ]
              in
              check_contains "transcript uses stable task session"
                full_transcript (stable_session_key id);
              store_transcript_probe ~db ~id marker;
              let transcript =
                Command_bridge.handle
                  [
                    "subagents";
                    "transcript";
                    string_of_int id;
                    "--regex";
                    marker;
                  ]
              in
              check_contains "regex transcript includes marker" transcript
                marker;
              let follow = Printf.sprintf "FOLLOWUP_E2E_%d" idx in
              let send =
                Command_bridge.handle
                  [
                    "subagents";
                    "send";
                    string_of_int id;
                    Printf.sprintf "Reply with exactly %s and no extra text."
                      follow;
                  ]
              in
              check_contains "send queued" send "Queued message";
              ignore (run_task_to_success ~db ~session_manager ~id);
              store_transcript_probe ~db ~id follow;
              let transcript2 =
                Command_bridge.handle
                  [
                    "subagents";
                    "transcript";
                    string_of_int id;
                    "--regex";
                    follow;
                  ]
              in
              check_contains "follow-up transcript includes marker" transcript2
                follow)
            models))

let test_background_aliases_transcript_export () =
  require_enabled ();
  with_temp_home (fun _home ->
      with_temp_git_repo (fun repo ->
          let db = open_db () in
          let session_manager = Session.create ~config:(e2e_config ()) ~db () in
          let marker = "ALIAS_E2E_PRIMARY" in
          let output =
            Command_bridge.handle
              [
                "background";
                "start";
                "local";
                "--model";
                List.hd models;
                "--agent";
                "coder";
                repo;
                Printf.sprintf "Reply with exactly %s and no extra text." marker;
              ]
          in
          check_contains "background start alias queues task" output
            "Queued background task";
          let id = newest_task_id ~db in
          ignore (run_task_to_success ~db ~session_manager ~id);
          let follow = "ALIAS_E2E_FOLLOWUP" in
          ignore
            (Command_bridge.handle
               [
                 "background";
                 "send";
                 string_of_int id;
                 Printf.sprintf "Reply with exactly %s and no extra text."
                   follow;
               ]);
          ignore (run_task_to_success ~db ~session_manager ~id);
          store_transcript_probe ~db ~id marker;
          store_transcript_probe ~db ~id follow;
          let transcript =
            Command_bridge.handle
              [
                "background";
                "transcript";
                string_of_int id;
                "--regex";
                "ALIAS_E2E";
                "--max-lines";
                "1";
                "--export";
              ]
          in
          check_contains "export path returned" transcript "JSONL export:"))

let test_multiple_native_subagents_drain_together () =
  require_enabled ();
  with_temp_home (fun _home ->
      with_temp_git_repo (fun repo ->
          let db = open_db () in
          let session_manager = Session.create ~config:(e2e_config ()) ~db () in
          let ids =
            models
            |> List.mapi (fun idx model ->
                let marker = Printf.sprintf "BATCH_E2E_%d" idx in
                ignore (start_subagent ~repo ~model ~marker);
                (newest_task_id ~db, marker))
          in
          Background_task.start_queued_with_local_runner ~timeout_seconds:220.0
            ~max_running_tasks:2 ~db
            ~run_turn:(Daemon.run_local_background_turn ~session_manager)
            ~on_task_started:(fun _ -> Lwt.return_unit)
            ~on_task_finished:(fun _ -> Lwt.return_unit)
            ();
          List.iter
            (fun (id, marker) ->
              ignore (wait_terminal ~db ~id);
              store_transcript_probe ~db ~id marker;
              let transcript =
                Command_bridge.handle
                  [
                    "subagents";
                    "transcript";
                    string_of_int id;
                    "--regex";
                    marker;
                  ]
              in
              check_contains "batch transcript includes marker" transcript
                marker)
            ids))

let suite =
  [
    Alcotest.test_case "MiMo subagents start/send/transcript per model" `Slow
      test_subagents_start_send_transcript_each_model;
    Alcotest.test_case "MiMo background aliases transcript export" `Slow
      test_background_aliases_transcript_export;
    Alcotest.test_case "MiMo multiple native subagents drain together" `Slow
      test_multiple_native_subagents_drain_together;
  ]
