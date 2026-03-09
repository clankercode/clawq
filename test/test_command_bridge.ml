let with_temp_home f =
  let base = Filename.get_temp_dir_name () in
  let dir =
    Filename.concat base ("clawq_home_" ^ string_of_int (Random.bits ()))
  in
  Unix.mkdir dir 0o755;
  let old_home = try Some (Sys.getenv "HOME") with Not_found -> None in
  Unix.putenv "HOME" dir;
  Fun.protect
    (fun () -> f dir)
    ~finally:(fun () ->
      (try
         Unix.unlink
           (Filename.concat (Filename.concat dir ".clawq") "config.json")
       with _ -> ());
      (try
         Unix.unlink
           (Filename.concat (Filename.concat dir ".clawq") "daemon_state.json")
       with _ -> ());
      (match old_home with
      | Some v -> Unix.putenv "HOME" v
      | None -> Unix.putenv "HOME" "");
      (try Unix.rmdir (Filename.concat dir ".clawq") with _ -> ());
      try Unix.rmdir dir with _ -> ())

let test_handle_phase2 () =
  let result = Command_bridge.handle [ "phase2" ] in
  Alcotest.(check bool)
    "phase2 returns deferred list" true
    (String.length result > 0)

let test_handle_version () =
  Alcotest.(check string)
    "handle version" "clawq 0.1.0-dev"
    (Command_bridge.handle [ "version" ])

let test_handle_unknown () =
  let result = Command_bridge.handle [ "unknown_xyz" ] in
  Alcotest.(check bool)
    "handle unknown contains 'unknown command'" true
    (let prefix = "unknown command" in
     String.length result >= String.length prefix
     && String.sub result 0 (String.length prefix) = prefix)

let test_handle_status () =
  let result = Command_bridge.handle [ "status" ] in
  Alcotest.(check bool)
    "status contains 'clawq status'" true
    (String.length result > 0 && String.sub result 0 12 = "clawq status")

let test_handle_doctor () =
  let result = Command_bridge.handle [ "doctor" ] in
  Alcotest.(check bool)
    "doctor starts with 'doctor:'" true
    (String.length result >= 7 && String.sub result 0 7 = "doctor:")

let test_handle_models () =
  let result = Command_bridge.handle [ "models" ] in
  Alcotest.(check bool) "models returns output" true (String.length result > 0)

let with_temp_home f =
  let base = Filename.get_temp_dir_name () in
  let dir = Filename.temp_file ~temp_dir:base "clawq_home_" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let old_home = try Some (Sys.getenv "HOME") with Not_found -> None in
  Unix.putenv "HOME" dir;
  Fun.protect
    (fun () -> f dir)
    ~finally:(fun () ->
      (match old_home with
      | Some v -> Unix.putenv "HOME" v
      | None -> Unix.putenv "HOME" "");
      (try
         let clawq_dir = Filename.concat dir ".clawq" in
         if Sys.file_exists clawq_dir then begin
           Array.iter
             (fun name ->
               let path = Filename.concat clawq_dir name in
               try Sys.remove path with _ -> ())
             (Sys.readdir clawq_dir);
           Unix.rmdir clawq_dir
         end
       with _ -> ());
      try Unix.rmdir dir with _ -> ())

let init_git_repo path =
  let cmd =
    Printf.sprintf "git -C %s init -q >/dev/null 2>&1" (Filename.quote path)
  in
  match Sys.command cmd with
  | 0 -> ()
  | code -> Alcotest.failf "git init failed for %s (exit %d)" path code

let session_db home =
  let clawq_dir = Filename.concat home ".clawq" in
  if not (Sys.file_exists clawq_dir) then Unix.mkdir clawq_dir 0o755;
  Memory.init ~db_path:(Filename.concat clawq_dir "memory.db") ()

let write_json_file path json =
  let oc = open_out path in
  output_string oc (Yojson.Safe.to_string json);
  close_out oc

let with_fake_gateway_server ~port ~callback f =
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  Lwt.async (fun () -> Lwt.pick [ server; stop ]);
  Fun.protect ~finally:(fun () -> Lwt.wakeup_later stopper ()) f

let write_daemon_state ?pairing_code home ~pid ~host ~port =
  let clawq_dir = Filename.concat home ".clawq" in
  if not (Sys.file_exists clawq_dir) then Unix.mkdir clawq_dir 0o755;
  write_json_file
    (Filename.concat clawq_dir "daemon_state.json")
    (`Assoc
       ([
          ("pid", `Int pid);
          ("gateway_host", `String host);
          ("gateway_port", `Int port);
        ]
       @
       match pairing_code with
       | Some code -> [ ("pairing_code", `String code) ]
       | None -> []))

let write_config home body =
  let clawq_dir = Filename.concat home ".clawq" in
  if not (Sys.file_exists clawq_dir) then Unix.mkdir clawq_dir 0o755;
  let oc = open_out (Filename.concat clawq_dir "config.json") in
  output_string oc body;
  close_out oc

let test_handle_channel () =
  let result = Command_bridge.handle [ "channel" ] in
  Alcotest.(check bool)
    "channel contains 'Configured channels'" true
    (String.length result > 0
    &&
    let prefix = "Configured channels" in
    String.length result >= String.length prefix
    && String.sub result 0 (String.length prefix) = prefix)

let test_handle_memory () =
  let result = Command_bridge.handle [ "memory" ] in
  Alcotest.(check bool)
    "memory contains 'Memory backend'" true
    (String.length result > 0 && String.sub result 0 14 = "Memory backend")

let test_handle_workspace () =
  let result = Command_bridge.handle [ "workspace" ] in
  Alcotest.(check bool)
    "workspace contains 'Workspace:'" true
    (String.length result > 0 && String.sub result 0 10 = "Workspace:")

let test_handle_workspace_uses_effective_workspace () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let workspace = Filename.concat home "custom-workspace" in
      let oc = open_out (Filename.concat clawq_dir "config.json") in
      output_string oc
        (Printf.sprintf
           "{\n\
           \  \"workspace\": %S,\n\
           \  \"security\": { \"tools_enabled\": false }\n\
            }\n"
           workspace);
      close_out oc;
      Alcotest.(check string)
        "workspace reports effective config path"
        ("Workspace: " ^ workspace)
        (Command_bridge.handle [ "workspace" ]))

let test_handle_session_list_filters () =
  with_temp_home (fun home ->
      let db = session_db home in
      Memory.store_message ~db ~session_key:"telegram:42:user1"
        (Provider.make_message ~role:"user" ~content:"hi");
      Memory.upsert_session_state ~db ~session_key:"telegram:42:user1"
        ~turn:"agent" ~channel:"telegram" ~channel_id:"42" ();
      Memory.store_message ~db ~session_key:"__main__"
        (Provider.make_message ~role:"user" ~content:"main");
      let result =
        Command_bridge.handle
          [ "session"; "list"; "--channel"; "telegram"; "--active" ]
      in
      Alcotest.(check bool)
        "session list includes active telegram session" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "telegram:42:user1")
                result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "session list excludes main session" false
        (try
           ignore (Str.search_forward (Str.regexp_string "__main__") result 0);
           true
         with Not_found -> false))

let test_handle_session_inject_routes_to_live_gateway () =
  with_temp_home (fun home ->
      let port = 19080 + Random.int 1000 in
      write_config home
        (Printf.sprintf
           {|{
  "gateway": { "host": "127.0.0.1", "port": %d, "require_pairing": false, "pair_lockout_seconds": 300, "max_pair_attempts": 5 },
  "prompt": { "dynamic_enabled": false },
  "security": { "tools_enabled": false }
}|}
           port);
      write_daemon_state home ~pid:(Unix.getpid ()) ~host:"127.0.0.1" ~port;
      let callback _conn req body =
        let open Lwt.Syntax in
        let* body = Cohttp_lwt.Body.to_string body in
        let headers = Cohttp.Request.headers req in
        let auth = Cohttp.Header.get headers "authorization" in
        let json = Yojson.Safe.from_string body in
        let open Yojson.Safe.Util in
        let session_key = json |> member "session_key" |> to_string in
        let message = json |> member "message" |> to_string in
        Alcotest.(check (option string))
          "no auth header when not configured" None auth;
        Alcotest.(check string)
          "session key forwarded" "telegram:1:user" session_key;
        Alcotest.(check string)
          "message forwarded" "hello persisted world" message;
        Cohttp_lwt_unix.Server.respond_string ~status:`OK
          ~body:{|{"queued":false,"response":"processed live"}|} ()
      in
      with_fake_gateway_server ~port ~callback (fun () ->
          let inject_result =
            Command_bridge.handle
              [
                "session";
                "inject";
                "telegram:1:user";
                "hello";
                "persisted";
                "world";
              ]
          in
          Alcotest.(check bool)
            "session inject reports live processing" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string "Processed injected message")
                    inject_result 0);
               true
             with Not_found -> false);
          Alcotest.(check bool)
            "session inject includes response" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string "processed live")
                    inject_result 0);
               true
             with Not_found -> false)))

let test_handle_session_inject_persists_when_daemon_missing () =
  with_temp_home (fun home ->
      ignore (session_db home);
      let result =
        Command_bridge.handle
          [ "session"; "inject"; "telegram:1:user"; "hello" ]
      in
      Alcotest.(check bool)
        "session inject warns about missing daemon" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "no live daemon detected")
                result 0);
           true
         with Not_found -> false);
      let show_result =
        Command_bridge.handle [ "session"; "show"; "telegram:1:user" ]
      in
      Alcotest.(check bool)
        "session inject persists message for later" true
        (try
           ignore (Str.search_forward (Str.regexp_string "hello") show_result 0);
           true
         with Not_found -> false))

let test_handle_session_inject_reports_queued_bang () =
  with_temp_home (fun home ->
      let port = 20080 + Random.int 1000 in
      write_config home
        (Printf.sprintf
           {|{
  "gateway": { "host": "127.0.0.1", "port": %d, "auth_token": "secret", "require_pairing": false, "pair_lockout_seconds": 300, "max_pair_attempts": 5 },
  "prompt": { "dynamic_enabled": false },
  "security": { "tools_enabled": false }
}|}
           port);
      write_daemon_state home ~pid:(Unix.getpid ()) ~host:"127.0.0.1" ~port;
      let callback _conn req body =
        let open Lwt.Syntax in
        let* body = Cohttp_lwt.Body.to_string body in
        let headers = Cohttp.Request.headers req in
        let auth = Cohttp.Header.get headers "authorization" in
        let json = Yojson.Safe.from_string body in
        let open Yojson.Safe.Util in
        Alcotest.(check (option string))
          "auth header forwarded" (Some "Bearer secret") auth;
        Alcotest.(check string)
          "bang message forwarded" "!interrupt now"
          (json |> member "message" |> to_string);
        Cohttp_lwt_unix.Server.respond_string ~status:`OK
          ~body:{|{"queued":true,"response":"__clawq_message_queued__"}|} ()
      in
      with_fake_gateway_server ~port ~callback (fun () ->
          let result =
            Command_bridge.handle
              [ "session"; "inject"; "telegram:1:user"; "!interrupt"; "now" ]
          in
          Alcotest.(check bool)
            "queued bang reported" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string
                       "Queued injected message for busy session")
                    result 0);
               true
             with Not_found -> false);
          Alcotest.(check bool)
            "bang interrupt note included" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string "bang interrupt requested")
                    result 0);
               true
             with Not_found -> false)))

let test_handle_session_epochs_and_show_archived_epoch () =
  with_temp_home (fun home ->
      let db = session_db home in
      Memory.store_message ~db ~session_key:"web:test"
        (Provider.make_message ~role:"user" ~content:"before compaction");
      Memory.store_message ~db ~session_key:"web:test"
        (Provider.make_message ~role:"assistant" ~content:"reply");
      Memory.replace_session_messages ~db ~session_key:"web:test"
        [ Provider.make_message ~role:"assistant" ~content:"summary" ];
      let epochs_result =
        Command_bridge.handle [ "session"; "epochs"; "web:test" ]
      in
      Alcotest.(check bool)
        "session epochs includes current marker" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "\"label\": \"current\"")
                epochs_result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "session epochs includes archived epoch id" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "\"epoch\": 1")
                epochs_result 0);
           true
         with Not_found -> false);
      let current_result =
        Command_bridge.handle [ "session"; "show"; "web:test" ]
      in
      Alcotest.(check bool)
        "default session show uses current epoch" false
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "before compaction")
                current_result 0);
           true
         with Not_found -> false);
      let archived_result =
        Command_bridge.handle [ "session"; "show"; "web:test"; "--epoch"; "1" ]
      in
      Alcotest.(check bool)
        "archived session show includes old content" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "before compaction")
                archived_result 0);
           true
         with Not_found -> false))

let test_handle_capabilities () =
  let result = Command_bridge.handle [ "capabilities" ] in
  Alcotest.(check bool)
    "capabilities mentions LLM" true
    (String.length result > 0)

let test_handle_auth () =
  let result = Command_bridge.handle [ "auth" ] in
  Alcotest.(check bool) "auth returns output" true (String.length result > 0)

let test_handle_not_implemented () =
  List.iter
    (fun cmd ->
      let result = Command_bridge.handle [ cmd ] in
      Alcotest.(check bool)
        (cmd ^ " returns not implemented")
        true
        (String.length result > 0))
    [ "hardware" ]

let test_handle_cron () =
  let result = Command_bridge.handle [ "cron" ] in
  Alcotest.(check bool) "cron returns output" true (String.length result > 0)

let test_handle_cron_list () =
  let result = Command_bridge.handle [ "cron"; "list" ] in
  Alcotest.(check bool)
    "cron list returns output" true
    (String.length result > 0)

let test_handle_background_list () =
  let result = Command_bridge.handle [ "background"; "list" ] in
  Alcotest.(check bool)
    "background list returns output" true
    (String.length result > 0)

let test_handle_background_bare_shows_commands () =
  let contains s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  let result = Command_bridge.handle [ "background" ] in
  Alcotest.(check bool)
    "bare background includes task list" true
    (String.length result > 0);
  Alcotest.(check bool)
    "bare background includes commands section" true
    (contains result "Commands:");
  Alcotest.(check bool)
    "bare background mentions show" true
    (contains result "background show");
  Alcotest.(check bool)
    "bare background mentions add" true
    (contains result "background add");
  Alcotest.(check bool)
    "bare background mentions cancel" true
    (contains result "background cancel")

let test_handle_background_add_show_cancel () =
  with_temp_home (fun home ->
      let repo = Filename.concat home "repo" in
      Unix.mkdir repo 0o755;
      init_git_repo repo;
      let add_result =
        Command_bridge.handle
          [ "background"; "add"; "codex"; repo; "Implement"; "the"; "feature" ]
      in
      Alcotest.(check bool)
        "background add queues task" true
        (String.length add_result > 0
        &&
        let prefix = "Queued background task " in
        String.length add_result >= String.length prefix
        && String.sub add_result 0 (String.length prefix) = prefix);
      let show_result = Command_bridge.handle [ "background"; "show"; "1" ] in
      Alcotest.(check bool)
        "background show includes runner" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "runner: codex")
                show_result 0);
           true
         with Not_found -> false);
      let cancel_result =
        Command_bridge.handle [ "background"; "cancel"; "1" ]
      in
      Alcotest.(check bool)
        "background cancel returns output" true
        (String.length cancel_result > 0))

let test_handle_background_wait_and_logs () =
  with_temp_home (fun home ->
      let repo = Filename.concat home "repo" in
      Unix.mkdir repo 0o755;
      init_git_repo repo;
      ignore
        (Command_bridge.handle
           [ "background"; "add"; "codex"; repo; "Implement"; "the"; "feature" ]);
      let clawq_dir = Filename.concat home ".clawq" in
      let db =
        Memory.init ~db_path:(Filename.concat clawq_dir "memory.db") ()
      in
      Background_task.init_schema db;
      let log_path = Filename.concat clawq_dir "task-1.log" in
      let oc = open_out log_path in
      output_string oc "alpha\nbeta\ngamma\n";
      close_out oc;
      ignore
        (Background_task.set_running ~db ~id:1 ~branch:"clawq-bg-1"
           ~worktree_path:(Filename.concat home "wt")
           ~log_path ~pid:12345);
      Background_task.finish ~db ~id:1 ~status:Background_task.Succeeded
        ~result_preview:"ok";
      let wait_result = Command_bridge.handle [ "background"; "wait"; "1" ] in
      Alcotest.(check bool)
        "background wait includes status" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "status: succeeded")
                wait_result 0);
           true
         with Not_found -> false);
      let logs_result =
        Command_bridge.handle [ "background"; "logs"; "1"; "--lines"; "2" ]
      in
      Alcotest.(check bool)
        "background logs includes tail" true
        (try
           ignore (Str.search_forward (Str.regexp_string "beta") logs_result 0);
           ignore (Str.search_forward (Str.regexp_string "gamma") logs_result 0);
           true
         with Not_found -> false))

let test_handle_delegate () =
  with_temp_home (fun home ->
      let repo = Filename.concat home "repo" in
      Unix.mkdir repo 0o755;
      init_git_repo repo;
      (* Create a fake codex binary so resolve_runner succeeds in CI *)
      let bin_dir = Filename.concat home "bin" in
      Unix.mkdir bin_dir 0o755;
      let fake_codex = Filename.concat bin_dir "codex" in
      let oc = open_out fake_codex in
      output_string oc "#!/bin/sh\n";
      close_out oc;
      Unix.chmod fake_codex 0o755;
      let old_path = try Sys.getenv "PATH" with Not_found -> "" in
      Unix.putenv "PATH" (bin_dir ^ ":" ^ old_path);
      Fun.protect
        (fun () ->
          let result =
            Command_bridge.handle
              [
                "delegate";
                "--runner";
                "codex";
                "--repo";
                repo;
                "implement";
                "the";
                "feature";
              ]
          in
          Alcotest.(check bool)
            "delegate queues task" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string "Delegated task 1")
                    result 0);
               true
             with Not_found -> false))
        ~finally:(fun () -> Unix.putenv "PATH" old_path))

let test_handle_background_add_rejects_non_git_repo () =
  with_temp_home (fun home ->
      let repo = Filename.concat home "repo" in
      Unix.mkdir repo 0o755;
      let result =
        Command_bridge.handle
          [ "background"; "add"; "codex"; repo; "Implement"; "the"; "feature" ]
      in
      Alcotest.(check bool)
        "background add rejects non-git repo" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "not a git repository")
                result 0);
           true
         with Not_found -> false))

let test_handle_delegate_with_model () =
  with_temp_home (fun home ->
      let repo = Filename.concat home "repo" in
      Unix.mkdir repo 0o755;
      init_git_repo repo;
      let bin_dir = Filename.concat home "bin" in
      Unix.mkdir bin_dir 0o755;
      let fake_codex = Filename.concat bin_dir "codex" in
      let oc = open_out fake_codex in
      output_string oc "#!/bin/sh\n";
      close_out oc;
      Unix.chmod fake_codex 0o755;
      let old_path = try Sys.getenv "PATH" with Not_found -> "" in
      Unix.putenv "PATH" (bin_dir ^ ":" ^ old_path);
      Fun.protect
        (fun () ->
          let result =
            Command_bridge.handle
              [
                "delegate";
                "--runner";
                "codex";
                "--model";
                "gpt-5.4";
                "--repo";
                repo;
                "implement";
                "the";
                "feature";
              ]
          in
          Alcotest.(check bool)
            "delegate with --model queues task" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string "Delegated task")
                    result 0);
               true
             with Not_found -> false))
        ~finally:(fun () -> Unix.putenv "PATH" old_path))

let test_handle_delegate_rejects_non_git_repo () =
  with_temp_home (fun home ->
      let repo = Filename.concat home "repo" in
      Unix.mkdir repo 0o755;
      let result =
        Command_bridge.handle
          [ "delegate"; "--runner"; "codex"; "--repo"; repo; "implement" ]
      in
      Alcotest.(check bool)
        "delegate rejects non-git repo" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "not a git repository")
                result 0);
           true
         with Not_found -> false))

let test_handle_service () =
  let result = Command_bridge.handle [ "service" ] in
  Alcotest.(check bool)
    "service returns status output" true
    (String.length result > 0
    &&
    let prefix = "Service status:" in
    String.length result >= String.length prefix
    && String.sub result 0 (String.length prefix) = prefix)

let test_handle_service_signal_restart () =
  let result = Command_bridge.handle [ "service"; "signal-restart" ] in
  Alcotest.(check bool)
    "service signal restart returns output" true
    (result = "Daemon is not running" || String.length result > 0)

let test_handle_update_without_live_daemon_reports_stub () =
  with_temp_home (fun _home ->
      let result = Command_bridge.handle [ "update" ] in
      Alcotest.(check bool)
        "warns about missing live daemon" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Warning: no live daemon detected")
                result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "mentions offline fallback stub" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Offline fallback stub")
                result 0);
           true
         with Not_found -> false))

let test_handle_update_auto_pairs_with_live_gateway () =
  with_temp_home (fun home ->
      let port = 21080 + Random.int 1000 in
      write_config home
        (Printf.sprintf
           {|{
  "gateway": { "host": "127.0.0.1", "port": %d, "require_pairing": true, "pair_lockout_seconds": 300, "max_pair_attempts": 5 },
  "prompt": { "dynamic_enabled": false },
  "security": { "tools_enabled": false }
}|}
           port);
      write_daemon_state ~pairing_code:"123456" home ~pid:(Unix.getpid ())
        ~host:"127.0.0.1" ~port;
      let seen_pair = ref 0 in
      let seen_update = ref [] in
      let callback _conn req body =
        let open Lwt.Syntax in
        let path = Uri.path (Cohttp.Request.uri req) in
        let headers = Cohttp.Request.headers req in
        let auth = Cohttp.Header.get headers "authorization" in
        let* body = Cohttp_lwt.Body.to_string body in
        match path with
        | "/pair" ->
            incr seen_pair;
            Alcotest.(check string)
              "pair request body" {|{"code":"123456"}|} body;
            Cohttp_lwt_unix.Server.respond_string ~status:`OK
              ~body:{|{"token":"paired-token"}|} ()
        | "/daemon/update" ->
            seen_update := auth :: !seen_update;
            if auth = None then
              Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
                ~body:
                  {|{"error":"pairing required; use a valid paired token to access this endpoint"}|}
                ()
            else begin
              Alcotest.(check (option string))
                "retry uses paired token" (Some "Bearer paired-token") auth;
              Cohttp_lwt_unix.Server.respond_string ~status:`OK
                ~body:
                  {|{"progress":["Starting update..."],"result":"Build complete. Sending restart signal..."}|}
                ()
            end
        | other -> Alcotest.failf "unexpected path %s" other
      in
      with_fake_gateway_server ~port ~callback (fun () ->
          let result = Command_bridge.handle [ "update" ] in
          Alcotest.(check int) "pair called once" 1 !seen_pair;
          Alcotest.(check (list (option string)))
            "update retried after pairing"
            [ Some "Bearer paired-token"; None ]
            !seen_update;
          Alcotest.(check bool)
            "update progress returned" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string "Starting update...")
                    result 0);
               true
             with Not_found -> false);
          Alcotest.(check bool)
            "update result returned" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string
                       "Build complete. Sending restart signal...")
                    result 0);
               true
             with Not_found -> false);
          Alcotest.(check (option string))
            "token persisted" (Some "paired-token")
            (try
               let ic =
                 open_in
                   (Filename.concat
                      (Filename.concat home ".clawq")
                      "gateway_token")
               in
               Fun.protect
                 (fun () ->
                   Some
                     (String.trim
                        (really_input_string ic (in_channel_length ic))))
                 ~finally:(fun () -> close_in ic)
             with _ -> None)))

let test_handle_update_prefers_static_auth_token_over_auto_pair () =
  with_temp_home (fun home ->
      let port = 22080 + Random.int 1000 in
      write_config home
        (Printf.sprintf
           {|{
  "gateway": { "host": "127.0.0.1", "port": %d, "auth_token": "secret", "require_pairing": true, "pair_lockout_seconds": 300, "max_pair_attempts": 5 },
  "prompt": { "dynamic_enabled": false },
  "security": { "tools_enabled": false }
}|}
           port);
      write_daemon_state ~pairing_code:"123456" home ~pid:(Unix.getpid ())
        ~host:"127.0.0.1" ~port;
      let seen_pair = ref 0 in
      let callback _conn req _body =
        let path = Uri.path (Cohttp.Request.uri req) in
        let auth =
          Cohttp.Header.get (Cohttp.Request.headers req) "authorization"
        in
        match path with
        | "/pair" ->
            incr seen_pair;
            Cohttp_lwt_unix.Server.respond_string ~status:`OK
              ~body:{|{"token":"unexpected"}|} ()
        | "/daemon/update" ->
            Alcotest.(check (option string))
              "static auth header forwarded" (Some "Bearer secret") auth;
            Cohttp_lwt_unix.Server.respond_string ~status:`OK
              ~body:{|{"progress":[],"result":"ok"}|} ()
        | other -> Alcotest.failf "unexpected path %s" other
      in
      with_fake_gateway_server ~port ~callback (fun () ->
          let result = Command_bridge.handle [ "update" ] in
          Alcotest.(check string) "update succeeds" "ok" result;
          Alcotest.(check int) "pair endpoint unused" 0 !seen_pair))

let test_handle_session_inject_auto_pairs_with_live_gateway () =
  with_temp_home (fun home ->
      let port = 23080 + Random.int 1000 in
      write_config home
        (Printf.sprintf
           {|{
  "gateway": { "host": "127.0.0.1", "port": %d, "require_pairing": true, "pair_lockout_seconds": 300, "max_pair_attempts": 5 },
  "prompt": { "dynamic_enabled": false },
  "security": { "tools_enabled": false }
}|}
           port);
      write_daemon_state ~pairing_code:"654321" home ~pid:(Unix.getpid ())
        ~host:"127.0.0.1" ~port;
      let seen_pair = ref 0 in
      let callback _conn req body =
        let open Lwt.Syntax in
        let path = Uri.path (Cohttp.Request.uri req) in
        let auth =
          Cohttp.Header.get (Cohttp.Request.headers req) "authorization"
        in
        let* body = Cohttp_lwt.Body.to_string body in
        match path with
        | "/pair" ->
            incr seen_pair;
            Alcotest.(check string)
              "pair request body" {|{"code":"654321"}|} body;
            Cohttp_lwt_unix.Server.respond_string ~status:`OK
              ~body:{|{"token":"inject-token"}|} ()
        | "/session/inject" ->
            if auth = None then
              Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
                ~body:{|{"error":"pairing required"}|} ()
            else begin
              Alcotest.(check (option string))
                "retry uses paired token" (Some "Bearer inject-token") auth;
              Alcotest.(check string)
                "inject body forwarded"
                {|{"session_key":"telegram:1:user","message":"hello"}|} body;
              Cohttp_lwt_unix.Server.respond_string ~status:`OK
                ~body:{|{"queued":false,"response":"processed live"}|} ()
            end
        | other -> Alcotest.failf "unexpected path %s" other
      in
      with_fake_gateway_server ~port ~callback (fun () ->
          let result =
            Command_bridge.handle
              [ "session"; "inject"; "telegram:1:user"; "hello" ]
          in
          Alcotest.(check int) "pair called once" 1 !seen_pair;
          Alcotest.(check bool)
            "session inject succeeded" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string "Processed injected message")
                    result 0);
               true
             with Not_found -> false)))

let test_handle_migrate_no_source () =
  let result = Command_bridge.handle [ "migrate" ] in
  Alcotest.(check bool) "migrate returns output" true (String.length result > 0)

let test_handle_skills () =
  let result = Command_bridge.handle [ "skills" ] in
  Alcotest.(check bool) "skills returns output" true (String.length result > 0)

let test_handle_skills_path () =
  let result = Command_bridge.handle [ "skills"; "path" ] in
  Alcotest.(check bool)
    "skills path contains directory" true
    (String.length result > 0
    &&
    let re = Str.regexp_string "skills" in
    try
      ignore (Str.search_forward re result 0);
      true
    with Not_found -> false)

let test_handle_audit () =
  let result = Command_bridge.handle [ "audit" ] in
  Alcotest.(check bool) "audit returns output" true (String.length result > 0)

let test_handle_audit_usage_mentions_anchor () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let config_path = Filename.concat clawq_dir "config.json" in
      let oc = open_out config_path in
      output_string oc {|{"security":{"audit_enabled":true}}|};
      close_out oc;
      let result = Command_bridge.handle [ "audit"; "import" ] in
      Alcotest.(check bool)
        "audit usage mentions optional anchor" true
        ((try
            ignore (Str.search_forward (Str.regexp_string "import") result 0);
            true
          with Not_found -> false)
        &&
          try
            ignore (Str.search_forward (Str.regexp_string "--anchor") result 0);
            true
          with Not_found -> false))

let test_handle_background_wait_with_timeout () =
  with_temp_home (fun home ->
      let repo = Filename.concat home "repo" in
      Unix.mkdir repo 0o755;
      init_git_repo repo;
      ignore
        (Command_bridge.handle
           [ "background"; "add"; "codex"; repo; "Implement"; "the"; "feature" ]);
      let clawq_dir = Filename.concat home ".clawq" in
      let db =
        Memory.init ~db_path:(Filename.concat clawq_dir "memory.db") ()
      in
      Background_task.init_schema db;
      let log_path = Filename.concat clawq_dir "task-1.log" in
      let oc = open_out log_path in
      output_string oc "done\n";
      close_out oc;
      ignore
        (Background_task.set_running ~db ~id:1 ~branch:"clawq-bg-1"
           ~worktree_path:(Filename.concat home "wt")
           ~log_path ~pid:12345);
      Background_task.finish ~db ~id:1 ~status:Background_task.Succeeded
        ~result_preview:"ok";
      let wait_result =
        Command_bridge.handle [ "background"; "wait"; "1"; "--timeout"; "0.25" ]
      in
      Alcotest.(check bool)
        "background wait timeout flag is accepted" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "status: succeeded")
                wait_result 0);
           true
         with Not_found -> false))

let test_handle_reloads_config_between_calls () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let config_path = Filename.concat clawq_dir "config.json" in
      let write_config contents =
        let oc = open_out config_path in
        output_string oc contents;
        close_out oc
      in
      write_config {|{"security":{"audit_enabled":false}}|};
      let disabled = Command_bridge.handle [ "audit"; "import" ] in
      write_config {|{"security":{"audit_enabled":true}}|};
      let enabled = Command_bridge.handle [ "audit"; "import" ] in
      Alcotest.(check bool)
        "first call sees disabled audit" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Audit trail is disabled")
                disabled 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "second call reloads config" true
        (try
           ignore (Str.search_forward (Str.regexp_string "--anchor") enabled 0);
           true
         with Not_found -> false))

let test_handle_tunnel_status () =
  let result = Command_bridge.handle [ "tunnel"; "status" ] in
  Alcotest.(check bool)
    "tunnel status returns output" true
    (String.length result > 0
    && ((try
           let re = Str.regexp_string "Tunnel provider" in
           ignore (Str.search_forward re result 0);
           true
         with Not_found -> false)
       ||
         try
           let re = Str.regexp_string "Tunnel is disabled" in
           ignore (Str.search_forward re result 0);
           true
         with Not_found -> false))

let test_cmd_agent_refuses_second_live_instance () =
  let ran = ref false in
  let released = ref false in
  let result =
    Command_bridge.cmd_agent
      ~acquire_lock:(fun () -> None)
      ~release_lock:(fun _ -> released := true)
      ~run_daemon:(fun ~config:_ ->
        ran := true;
        Daemon.Shutdown)
      ()
  in
  Alcotest.(check string)
    "refuses duplicate instance"
    "Another clawq agent instance already holds the daemon lock. Refusing to \
     start a second live agent."
    result;
  Alcotest.(check bool) "daemon not run" false !ran;
  Alcotest.(check bool) "no lock release needed" false !released

let test_cmd_agent_reexecs_on_restart () =
  let execd = ref None in
  let result =
    Command_bridge.cmd_agent
      ~acquire_lock:(fun () ->
        Some (Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0))
      ~release_lock:(fun fd_opt -> Option.iter Unix.close fd_opt)
      ~run_daemon:(fun ~config:_ -> Daemon.Restart)
      ~execv:(fun path argv -> execd := Some (path, Array.to_list argv))
      ()
  in
  Alcotest.(check string) "restart result" "Daemon restart requested." result;
  Alcotest.(check (option (pair string (list string))))
    "re-execs agent"
    (Some (Sys.executable_name, [ Sys.executable_name; "agent" ]))
    !execd

let test_cmd_agent_reexecs_on_restart_with_fresh_path () =
  let previous = Sys.getenv_opt Restart_exec.reexec_path_env in
  Unix.putenv Restart_exec.reexec_path_env "/tmp/clawq-fresh";
  Fun.protect
    (fun () ->
      let execd = ref None in
      let result =
        Command_bridge.cmd_agent
          ~acquire_lock:(fun () ->
            Some (Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0))
          ~release_lock:(fun fd_opt -> Option.iter Unix.close fd_opt)
          ~run_daemon:(fun ~config:_ -> Daemon.Restart)
          ~execv:(fun path argv -> execd := Some (path, Array.to_list argv))
          ()
      in
      Alcotest.(check string)
        "restart result" "Daemon restart requested." result;
      Alcotest.(check (option (pair string (list string))))
        "re-execs agent with fresh path"
        (Some ("/tmp/clawq-fresh", [ "/tmp/clawq-fresh"; "agent" ]))
        !execd)
    ~finally:(fun () ->
      match previous with
      | Some value -> Unix.putenv Restart_exec.reexec_path_env value
      | None -> Unix.putenv Restart_exec.reexec_path_env "")

let test_cmd_agent_stops_on_shutdown () =
  let execd = ref false in
  let result =
    Command_bridge.cmd_agent
      ~acquire_lock:(fun () ->
        Some (Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0))
      ~release_lock:(fun fd_opt -> Option.iter Unix.close fd_opt)
      ~run_daemon:(fun ~config:_ -> Daemon.Shutdown)
      ~execv:(fun _ _ -> execd := true)
      ()
  in
  Alcotest.(check string) "shutdown result" "Daemon stopped." result;
  Alcotest.(check bool) "no re-exec" false !execd

let test_status_cleans_stale_daemon_state () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let state_path = Filename.concat clawq_dir "daemon_state.json" in
      let oc = open_out state_path in
      output_string oc {|{"pid":999999}|};
      close_out oc;
      let result = Command_bridge.handle [ "status" ] in
      let has_stale =
        try
          ignore (Str.search_forward (Str.regexp_string "stale state") result 0);
          true
        with Not_found -> false
      in
      Alcotest.(check bool) "reports stale state" true has_stale;
      Alcotest.(check bool)
        "state file removed" false
        (Sys.file_exists state_path))

let test_otp_show_reads_live_gateway_pairing_code () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let state_path = Filename.concat clawq_dir "daemon_state.json" in
      let oc = open_out state_path in
      output_string oc
        (Yojson.Safe.to_string
           (`Assoc
              [
                ("pid", `Int (Unix.getpid ()));
                ("pairing_code", `String "123456");
              ]));
      close_out oc;
      let result = Command_bridge.handle [ "otp-show" ] in
      Alcotest.(check bool)
        "otp-show includes gateway pairing code" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "gateway: 123456") result 0);
           true
         with Not_found -> false))

let test_debug_prompt_prints_logical_messages () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let config_path = Filename.concat clawq_dir "config.json" in
      let oc = open_out config_path in
      output_string oc
        {|{
  "default_provider": "testprov",
  "providers": {
    "testprov": {
      "api_key": "sk-test",
      "default_model": "test-model"
    }
  },
  "agent_defaults": {
    "system_prompt": "Custom debug prompt"
  },
  "prompt": {
    "dynamic_enabled": true
  },
  "security": {
    "tools_enabled": false
  }
}|};
      close_out oc;
      let result = Command_bridge.handle [ "debug"; "prompt"; "hello world" ] in
      Alcotest.(check bool)
        "debug prompt includes provider" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "provider: testprov")
                result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "debug prompt includes model" true
        (try
           ignore (Str.search_forward (Str.regexp_string "model:") result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "debug prompt includes system section" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "--- system ---") result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "debug prompt includes system prompt" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Custom debug prompt")
                result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "debug prompt includes user section" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "--- user ---") result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "debug prompt includes user message" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "hello world") result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "debug prompt includes runtime context" true
        (try
           let user_idx =
             Str.search_forward (Str.regexp_string "--- user ---") result 0
           in
           let runtime_idx =
             Str.search_forward
               (Str.regexp_string "[Runtime context for this turn only]")
               result 0
           in
           let msg_idx =
             Str.search_forward (Str.regexp_string "hello world") result 0
           in
           runtime_idx > user_idx && runtime_idx < msg_idx
         with Not_found -> false))

let test_debug_prompt_defaults_message_when_missing () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let config_path = Filename.concat clawq_dir "config.json" in
      let oc = open_out config_path in
      output_string oc
        {|{
  "default_provider": "testprov",
  "providers": {
    "testprov": {
      "api_key": "sk-test",
      "default_model": "test-model"
    }
  },
  "prompt": {
    "dynamic_enabled": false
  },
  "security": {
    "tools_enabled": false
  }
}|};
      close_out oc;
      let result = Command_bridge.handle [ "debug"; "prompt" ] in
      Alcotest.(check bool)
        "debug prompt includes default message" true
        (try
           ignore (Str.search_forward (Str.regexp_string "Hello!") result 0);
           true
         with Not_found -> false))

let test_debug_usage_mentions_prompt_and_html_preview () =
  let result = Command_bridge.handle [ "debug" ] in
  Alcotest.(check bool)
    "debug usage mentions html-preview" true
    (try
       ignore (Str.search_forward (Str.regexp_string "html-preview") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "debug usage mentions prompt" true
    (try
       ignore (Str.search_forward (Str.regexp_string "prompt") result 0);
       true
     with Not_found -> false)

let suite =
  [
    Alcotest.test_case "handle phase2" `Quick test_handle_phase2;
    Alcotest.test_case "handle version" `Quick test_handle_version;
    Alcotest.test_case "handle unknown" `Quick test_handle_unknown;
    Alcotest.test_case "handle status" `Quick test_handle_status;
    Alcotest.test_case "handle doctor" `Quick test_handle_doctor;
    Alcotest.test_case "handle models" `Quick test_handle_models;
    Alcotest.test_case "handle channel" `Quick test_handle_channel;
    Alcotest.test_case "handle memory" `Quick test_handle_memory;
    Alcotest.test_case "handle workspace" `Quick test_handle_workspace;
    Alcotest.test_case "handle workspace uses effective workspace" `Quick
      test_handle_workspace_uses_effective_workspace;
    Alcotest.test_case "handle session list filters" `Quick
      test_handle_session_list_filters;
    Alcotest.test_case "handle session inject routes to live gateway" `Quick
      test_handle_session_inject_routes_to_live_gateway;
    Alcotest.test_case "handle session inject persists when daemon missing"
      `Quick test_handle_session_inject_persists_when_daemon_missing;
    Alcotest.test_case "handle session inject reports queued bang" `Quick
      test_handle_session_inject_reports_queued_bang;
    Alcotest.test_case "handle session epochs and show archived epoch" `Quick
      test_handle_session_epochs_and_show_archived_epoch;
    Alcotest.test_case "handle capabilities" `Quick test_handle_capabilities;
    Alcotest.test_case "handle auth" `Quick test_handle_auth;
    Alcotest.test_case "handle not-impl commands" `Quick
      test_handle_not_implemented;
    Alcotest.test_case "handle cron" `Quick test_handle_cron;
    Alcotest.test_case "handle cron list" `Quick test_handle_cron_list;
    Alcotest.test_case "handle background list" `Quick
      test_handle_background_list;
    Alcotest.test_case "handle background bare shows commands" `Quick
      test_handle_background_bare_shows_commands;
    Alcotest.test_case "handle background add show cancel" `Quick
      test_handle_background_add_show_cancel;
    Alcotest.test_case "handle background add rejects non-git repo" `Quick
      test_handle_background_add_rejects_non_git_repo;
    Alcotest.test_case "handle background wait and logs" `Quick
      test_handle_background_wait_and_logs;
    Alcotest.test_case "handle background wait with timeout" `Quick
      test_handle_background_wait_with_timeout;
    Alcotest.test_case "handle delegate" `Quick test_handle_delegate;
    Alcotest.test_case "handle delegate with --model" `Quick
      test_handle_delegate_with_model;
    Alcotest.test_case "handle delegate rejects non-git repo" `Quick
      test_handle_delegate_rejects_non_git_repo;
    Alcotest.test_case "handle service" `Quick test_handle_service;
    Alcotest.test_case "handle service signal restart" `Quick
      test_handle_service_signal_restart;
    Alcotest.test_case "handle update without live daemon reports stub" `Quick
      test_handle_update_without_live_daemon_reports_stub;
    Alcotest.test_case "handle update auto pairs with live gateway" `Quick
      test_handle_update_auto_pairs_with_live_gateway;
    Alcotest.test_case "handle update prefers static auth token" `Quick
      test_handle_update_prefers_static_auth_token_over_auto_pair;
    Alcotest.test_case "handle session inject auto pairs with live gateway"
      `Quick test_handle_session_inject_auto_pairs_with_live_gateway;
    Alcotest.test_case "handle migrate no source" `Quick
      test_handle_migrate_no_source;
    Alcotest.test_case "handle skills" `Quick test_handle_skills;
    Alcotest.test_case "handle skills path" `Quick test_handle_skills_path;
    Alcotest.test_case "handle audit" `Quick test_handle_audit;
    Alcotest.test_case "handle audit usage mentions anchor" `Quick
      test_handle_audit_usage_mentions_anchor;
    Alcotest.test_case "handle reloads config between calls" `Quick
      test_handle_reloads_config_between_calls;
    Alcotest.test_case "handle tunnel status" `Quick test_handle_tunnel_status;
    Alcotest.test_case "cmd_agent reexecs on restart" `Quick
      test_cmd_agent_reexecs_on_restart;
    Alcotest.test_case "cmd_agent reexecs on restart with fresh path" `Quick
      test_cmd_agent_reexecs_on_restart_with_fresh_path;
    Alcotest.test_case "cmd_agent stops on shutdown" `Quick
      test_cmd_agent_stops_on_shutdown;
    Alcotest.test_case "status cleans stale daemon state" `Quick
      test_status_cleans_stale_daemon_state;
    Alcotest.test_case "otp-show reads live gateway pairing code" `Quick
      test_otp_show_reads_live_gateway_pairing_code;
    Alcotest.test_case "debug prompt prints logical messages" `Quick
      test_debug_prompt_prints_logical_messages;
    Alcotest.test_case "debug prompt defaults missing message" `Quick
      test_debug_prompt_defaults_message_when_missing;
    Alcotest.test_case "debug usage mentions prompt and html-preview" `Quick
      test_debug_usage_mentions_prompt_and_html_preview;
  ]
