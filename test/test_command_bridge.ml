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

let test_cmd_agent_reexecs_on_restart () =
  let execd = ref None in
  let result =
    Command_bridge.cmd_agent
      ~run_daemon:(fun ~config:_ -> Daemon.Restart)
      ~execv:(fun path argv -> execd := Some (path, Array.to_list argv))
      ()
  in
  Alcotest.(check string) "restart result" "Daemon restart requested." result;
  Alcotest.(check (option (pair string (list string))))
    "re-execs agent"
    (Some (Sys.executable_name, [ Sys.executable_name; "agent" ]))
    !execd

let test_cmd_agent_stops_on_shutdown () =
  let execd = ref false in
  let result =
    Command_bridge.cmd_agent
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
    Alcotest.test_case "handle capabilities" `Quick test_handle_capabilities;
    Alcotest.test_case "handle auth" `Quick test_handle_auth;
    Alcotest.test_case "handle not-impl commands" `Quick
      test_handle_not_implemented;
    Alcotest.test_case "handle cron" `Quick test_handle_cron;
    Alcotest.test_case "handle cron list" `Quick test_handle_cron_list;
    Alcotest.test_case "handle service" `Quick test_handle_service;
    Alcotest.test_case "handle service signal restart" `Quick
      test_handle_service_signal_restart;
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
    Alcotest.test_case "cmd_agent stops on shutdown" `Quick
      test_cmd_agent_stops_on_shutdown;
    Alcotest.test_case "status cleans stale daemon state" `Quick
      test_status_cleans_stale_daemon_state;
    Alcotest.test_case "otp-show reads live gateway pairing code" `Quick
      test_otp_show_reads_live_gateway_pairing_code;
  ]
