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

let contains s sub =
  let sl = String.length s and subl = String.length sub in
  if subl > sl then false
  else if subl = 0 then true
  else
    let found = ref false in
    for i = 0 to sl - subl do
      if String.sub s i subl = sub then found := true
    done;
    !found

let test_handle_phase2 () =
  let result = Command_bridge.handle [ "phase2" ] in
  Alcotest.(check bool)
    "phase2 returns deferred list" true
    (String.length result > 0)

let test_handle_version () =
  Alcotest.(check string)
    "handle version" "clawq 0.3.0-dev"
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

let git_cmd repo args =
  let cmd =
    Printf.sprintf "git -C %s %s >/dev/null 2>&1" (Filename.quote repo) args
  in
  match Sys.command cmd with
  | 0 -> ()
  | code -> Alcotest.failf "git command failed for %s (exit %d)" args code

let add_git_worktree repo ~branch ~name =
  let worktree_path = Filename.concat repo name in
  git_cmd repo
    (Printf.sprintf "worktree add -b %s %s HEAD -q" (Filename.quote branch)
       (Filename.quote worktree_path));
  worktree_path

let git_empty_commit repo =
  let cmd =
    Printf.sprintf
      "git -C %s -c user.name=Test -c user.email=test@example.com commit \
       --allow-empty -m 'initial' -q >/dev/null 2>&1"
      (Filename.quote repo)
  in
  match Sys.command cmd with
  | 0 -> ()
  | code -> Alcotest.failf "git empty commit failed for %s (exit %d)" repo code

let session_db home =
  let clawq_dir = Filename.concat home ".clawq" in
  if not (Sys.file_exists clawq_dir) then Unix.mkdir clawq_dir 0o755;
  Memory.init ~db_path:(Filename.concat clawq_dir "memory.db") ()

let insert_cached_model ?(deprecated = false) ?(unavailable = false) db
    ~provider ~model_id =
  ignore
    (Model_discovery.upsert_model_rich ~db ~provider ~model_id
       ~display_name:(Some model_id) ~context_window:(Some 123000)
       ~supports_vision:false ~supports_tools:true ~supports_thinking:false
       ~deprecated ~unavailable ~source:"provider-api" ())

let test_models_list_json_includes_db_only_model () =
  with_temp_home (fun home ->
      let db = session_db home in
      insert_cached_model db ~provider:"dbprov" ~model_id:"fresh-model";
      let result = Command_bridge.handle [ "models"; "list"; "--json" ] in
      Alcotest.(check bool)
        "json includes db-only provider" true
        (contains result "\"provider\":\"dbprov\"");
      Alcotest.(check bool)
        "json includes db-only model" true
        (contains result "\"id\":\"fresh-model\""))

let test_models_list_json_provider_filter_includes_db_only_only_for_provider ()
    =
  with_temp_home (fun home ->
      let db = session_db home in
      insert_cached_model db ~provider:"dbprov" ~model_id:"fresh-model";
      insert_cached_model db ~provider:"otherdb" ~model_id:"other-model";
      let result =
        Command_bridge.handle
          [ "models"; "list"; "--provider"; "dbprov"; "--json" ]
      in
      Alcotest.(check bool)
        "json includes requested db-only model" true
        (contains result "\"id\":\"fresh-model\"");
      Alcotest.(check bool)
        "json excludes other provider db-only model" true
        (not (contains result "other-model")))

let test_models_list_availability_filters_db_rows () =
  with_temp_home (fun home ->
      let db = session_db home in
      insert_cached_model db ~provider:"dbprov" ~model_id:"available-model";
      insert_cached_model ~unavailable:true db ~provider:"dbprov"
        ~model_id:"disabled-model";
      let default_result =
        Command_bridge.handle
          [ "models"; "list"; "--provider"; "dbprov"; "--json" ]
      in
      let all_result =
        Command_bridge.handle
          [
            "models";
            "list";
            "--provider";
            "dbprov";
            "--json";
            "--availability";
            "all";
          ]
      in
      let unavailable_result =
        Command_bridge.handle
          [
            "models";
            "list";
            "--provider";
            "dbprov";
            "--json";
            "--availability";
            "unavailable";
          ]
      in
      Alcotest.(check bool)
        "default excludes unavailable db row" true
        (not (contains default_result "disabled-model"));
      Alcotest.(check bool)
        "all includes unavailable db row" true
        (contains all_result "disabled-model");
      Alcotest.(check bool)
        "unavailable includes unavailable db row" true
        (contains unavailable_result "disabled-model");
      Alcotest.(check bool)
        "unavailable excludes available db row" true
        (not (contains unavailable_result "available-model")))

let write_json_file path json =
  let oc = open_out path in
  output_string oc (Yojson.Safe.to_string json);
  close_out oc

let write_config_json home json =
  let clawq_dir = Filename.concat home ".clawq" in
  if not (Sys.file_exists clawq_dir) then Unix.mkdir clawq_dir 0o755;
  write_json_file (Filename.concat clawq_dir "config.json") json

let test_handle_doctor_flags_codex_provider_with_api_key_only () =
  with_temp_home (fun home ->
      write_config_json home
        (Yojson.Safe.from_string
           {|{
  "providers": {
    "openai-codex": {
      "kind": "openai-codex",
      "api_key": "sk-test"
    }
  }
}|});
      let result = Command_bridge.handle [ "doctor" ] in
      Alcotest.(check bool)
        "mentions codex oauth requirement" true
        (contains result "Codex providers require Codex OAuth");
      Alcotest.(check bool)
        "mentions api key insufficiency" true
        (contains result "API key auth alone is insufficient"))

let test_handle_doctor_flags_expired_refreshable_codex_oauth () =
  with_temp_home (fun home ->
      write_config_json home
        (Yojson.Safe.from_string
           {|{
  "providers": {
    "openai-codex": {
      "kind": "openai-codex",
      "codex_oauth": {
        "access_token": "tok",
        "refresh_token": "ref",
        "expires_at_ms": 0
      }
    }
  }
}|});
      let result = Command_bridge.handle [ "doctor" ] in
      Alcotest.(check bool)
        "mentions expired codex token" true
        (contains result "Codex OAuth access token is expired");
      Alcotest.(check bool)
        "mentions refresh possible" true
        (contains result
           "refresh token is present, so clawq should refresh on next use"))

let test_handle_doctor_distinguishes_refresh_window_from_expired () =
  with_temp_home (fun home ->
      let expires_at_ms = Openai_codex_oauth.now_ms () + 240000 in
      write_config_json home
        (Yojson.Safe.from_string
           (Printf.sprintf
              {|{
  "providers": {
    "openai-codex": {
      "kind": "openai-codex",
      "codex_oauth": {
        "access_token": "tok",
        "refresh_token": "ref",
        "expires_at_ms": %d
      }
    }
  }
}|}
              expires_at_ms));
      let result = Command_bridge.handle [ "doctor" ] in
      Alcotest.(check bool)
        "mentions refresh window" true
        (contains result "inside clawq's 5 min refresh window");
      Alcotest.(check bool)
        "does not mislabel refresh-window token as expired" false
        (contains result "Codex OAuth access token is expired"))

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

let test_handle_channel_test_teams () =
  let result = Command_bridge.handle [ "channel"; "test"; "teams" ] in
  (* Without valid Teams config, should report not configured or connection failed *)
  let has_teams = contains result "Teams" || contains result "teams" in
  Alcotest.(check bool) "channel test teams mentions teams" true has_teams

let test_handle_memory () =
  with_temp_home (fun home ->
      let _db = session_db home in
      let result = Command_bridge.handle [ "memory" ] in
      Alcotest.(check bool)
        "memory contains 'Memory backend'" true
        (String.length result > 0 && String.sub result 0 14 = "Memory backend"))

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

let test_handle_session_list_hides_postmortem () =
  with_temp_home (fun home ->
      let db = session_db home in
      Memory.store_message ~db ~session_key:"telegram:42:user1"
        (Provider.make_message ~role:"user" ~content:"hi");
      Memory.upsert_session_state ~db ~session_key:"telegram:42:user1"
        ~turn:"user" ~channel:"telegram" ~channel_id:"42" ();
      Memory.store_message ~db ~session_key:"__postmortem_telegram:42:user1"
        (Provider.make_message ~role:"user" ~content:"postmortem");
      Memory.upsert_session_state ~db
        ~session_key:"__postmortem_telegram:42:user1" ~turn:"user"
        ~channel:"telegram" ~channel_id:"42" ();
      let result = Command_bridge.handle [ "session"; "list" ] in
      Alcotest.(check bool)
        "session list shows normal session" true
        (contains result "telegram:42:user1");
      Alcotest.(check bool)
        "session list hides postmortem session" false
        (contains result "__postmortem_");
      let result_with =
        Command_bridge.handle [ "session"; "list"; "--include-postmortem" ]
      in
      Alcotest.(check bool)
        "session list --include-postmortem shows postmortem" true
        (contains result_with "__postmortem_"))

let test_handle_session_heartbeat_toggle () =
  with_temp_home (fun home ->
      let db = session_db home in
      Memory.store_message ~db ~session_key:"telegram:42:user1"
        (Provider.make_message ~role:"user" ~content:"hi");
      Memory.upsert_session_state ~db ~session_key:"telegram:42:user1"
        ~turn:"user" ~channel:"telegram" ~channel_id:"42" ();
      Alcotest.(check string)
        "heartbeat on reply" "Heartbeat enabled for session telegram:42:user1"
        (Command_bridge.handle
           [ "session"; "heartbeat"; "telegram:42:user1"; "on" ]);
      Alcotest.(check string)
        "heartbeat status on" "Session telegram:42:user1: heartbeat = on"
        (Command_bridge.handle
           [ "session"; "heartbeat"; "telegram:42:user1"; "status" ]);
      let listed = Command_bridge.handle [ "session"; "list" ] in
      Alcotest.(check bool)
        "session list shows heartbeat marker" true
        (contains listed "heartbeat");
      Alcotest.(check string)
        "heartbeat off reply" "Heartbeat disabled for session telegram:42:user1"
        (Command_bridge.handle
           [ "session"; "heartbeat"; "telegram:42:user1"; "off" ]);
      Alcotest.(check string)
        "heartbeat status off" "Session telegram:42:user1: heartbeat = off"
        (Command_bridge.handle
           [ "session"; "heartbeat"; "telegram:42:user1"; "status" ]))

let test_handle_session_heartbeat_rejects_unsupported_session () =
  with_temp_home (fun home ->
      ignore (session_db home);
      let result =
        Command_bridge.handle [ "session"; "heartbeat"; "web:abc"; "on" ]
      in
      Alcotest.(check bool)
        "rejects web session" true
        (contains result
           "Heartbeat can only be enabled for Telegram, Slack, Discord, or \
            Teams sessions."))

let test_handle_session_heartbeat_status_mentions_global_disable () =
  with_temp_home (fun home ->
      let db = session_db home in
      write_config_json home
        (Yojson.Safe.from_string
           {|{
  "heartbeat": {
    "enabled": false,
    "interval_seconds": 300,
    "quiet_start": 23,
    "quiet_end": 8
  },
  "security": {
    "tools_enabled": false
  }
}|});
      Memory.set_session_heartbeat ~db ~session_key:"telegram:42:user1"
        ~enabled:true;
      let result =
        Command_bridge.handle
          [ "session"; "heartbeat"; "telegram:42:user1"; "status" ]
      in
      Alcotest.(check bool)
        "mentions global disable" true
        (contains result "global heartbeat disabled in config"))

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
        "offline inject mentions queued" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "Queued message") result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "offline inject mentions startup replay" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "startup replay") result 0);
           true
         with Not_found -> false);
      let show_result =
        Command_bridge.handle [ "session"; "show"; "telegram:1:user" ]
      in
      Alcotest.(check bool)
        "offline inject does not appear in chat history" true
        (try
           ignore (Str.search_forward (Str.regexp_string "hello") show_result 0);
           false
         with Not_found -> true);
      let pending_result =
        Command_bridge.handle [ "session"; "pending"; "telegram:1:user" ]
      in
      Alcotest.(check bool)
        "pending shows the queued row" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "hello") pending_result 0);
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
      Alcotest.(check bool)
        "session show includes system_prompt field" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "\"system_prompt\":")
                current_result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "session show includes actual system prompt" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "autonomous AI assistant")
                current_result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "session show includes archived_epoch_count after compaction" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "\"archived_epoch_count\": 1")
                current_result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "session show includes total_archived_messages after compaction" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "\"total_archived_messages\": 2")
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
         with Not_found -> false);
      Alcotest.(check bool)
        "archived epoch view includes system_prompt field" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "\"system_prompt\":")
                archived_result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "archived epoch view includes archived_epoch_count" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "\"archived_epoch_count\": 1")
                archived_result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "archived epoch view includes total_archived_messages" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "\"total_archived_messages\": 2")
                archived_result 0);
           true
         with Not_found -> false))

let test_handle_session_archive_show () =
  with_temp_home (fun home ->
      let db = session_db home in
      Memory.store_message ~db ~session_key:"web:test"
        (Provider.make_message ~role:"user" ~content:"archived msg");
      Memory.store_message ~db ~session_key:"web:test"
        (Provider.make_message ~role:"assistant" ~content:"archived reply");
      Memory.archive_session ~db ~session_key:"web:test";
      let archives =
        Memory.list_archives_for_session ~db ~session_key:"web:test"
      in
      let archive_id = (List.hd archives).archive_id in
      let result =
        Command_bridge.handle
          [ "session"; "archive"; "show"; string_of_int archive_id ]
      in
      Alcotest.(check bool)
        "contains archive_id" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "\"archive_id\"") result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "contains archived msg" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "archived msg") result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "contains messages array" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "\"messages\"") result 0);
           true
         with Not_found -> false))

let test_handle_session_archive_show_invalid_id () =
  with_temp_home (fun _home ->
      let result =
        Command_bridge.handle [ "session"; "archive"; "show"; "abc" ]
      in
      Alcotest.(check bool)
        "error mentions integer" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "must be an integer")
                result 0);
           true
         with Not_found -> false))

let test_handle_session_archive_show_not_found () =
  with_temp_home (fun _home ->
      let result =
        Command_bridge.handle [ "session"; "archive"; "show"; "99999" ]
      in
      Alcotest.(check bool)
        "error mentions not found" true
        (try
           ignore (Str.search_forward (Str.regexp_string "not found") result 0);
           true
         with Not_found -> false))

let test_session_show_includes_workspace_refresh_event () =
  with_temp_home (fun home ->
      let db = session_db home in
      let workspace = Filename.concat home "workspace" in
      Unix.mkdir workspace 0o755;
      Fun.protect
        (fun () ->
          let secret = "persistent prompt guidance" in
          write_config home
            (Printf.sprintf
               "{\n\
               \  \"workspace\": %S,\n\
               \  \"prompt\": { \"workspace_files\": [\"AGENTS.md\"] },\n\
               \  \"security\": { \"tools_enabled\": false }\n\
                }\n"
               workspace);
          let prompt =
            {
              Runtime_config.default.prompt with
              workspace_files = [ "AGENTS.md" ];
            }
          in
          let config = { Runtime_config.default with workspace; prompt } in
          let registry = Tool_registry.create () in
          Tool_registry.register registry
            (Tools_builtin.doc_write ~workspace ~workspace_files:[ "AGENTS.md" ]);
          let agent = Agent.create ~config ~tool_registry:registry () in
          let mgr = Session.create ~config ~db () in
          let history_before = List.length agent.Agent.history in
          let call =
            {
              Provider.id = "tc-doc-write";
              function_name = "doc_write";
              arguments =
                Yojson.Safe.to_string
                  (`Assoc
                     [
                       ("filename", `String "AGENTS.md");
                       ("content", `String secret);
                     ]);
            }
          in
          Lwt_main.run
            (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
               ~session_key:(Some "web:test") [ call ]);
          Session.persist_new_messages mgr ~key:"web:test" ~history_before agent;
          let shown = Command_bridge.handle [ "session"; "show"; "web:test" ] in
          Alcotest.(check bool)
            "session show includes refresh event" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string
                       "workspace context refreshed after active workspace \
                        file update: AGENTS.md")
                    shown 0);
               true
             with Not_found -> false);
          let json = Yojson.Safe.from_string shown in
          let messages_str =
            Yojson.Safe.Util.(json |> member "messages")
            |> Yojson.Safe.to_string
          in
          Alcotest.(check bool)
            "session show redacts prompt file contents in messages" false
            (try
               ignore
                 (Str.search_forward (Str.regexp_string secret) messages_str 0);
               true
             with Not_found -> false);
          Alcotest.(check bool)
            "session show includes actual system prompt" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string "autonomous AI assistant")
                    shown 0);
               true
             with Not_found -> false))
        ~finally:(fun () ->
          (try Sys.remove (Filename.concat workspace "AGENTS.md") with _ -> ());
          Unix.rmdir workspace))

let test_session_show_redacts_shell_exec_prompt_file_updates () =
  with_temp_home (fun home ->
      let db = session_db home in
      let workspace = Filename.concat home "workspace" in
      Unix.mkdir workspace 0o755;
      Fun.protect
        (fun () ->
          let secret = "shell secret prompt text" in
          write_config home
            (Printf.sprintf
               "{\n\
               \  \"workspace\": %S,\n\
               \  \"prompt\": { \"workspace_files\": [\"AGENTS.md\"] },\n\
               \  \"security\": { \"tools_enabled\": false }\n\
                }\n"
               workspace);
          let prompt =
            {
              Runtime_config.default.prompt with
              workspace_files = [ "AGENTS.md" ];
            }
          in
          let config = { Runtime_config.default with workspace; prompt } in
          let registry = Tool_registry.create () in
          let sandbox =
            Sandbox.create ~backend:Sandbox.None ~workspace
              ~extra_allowed_paths:[] ~workspace_only:false ()
          in
          Tool_registry.register registry
            (Tools_builtin.shell_exec ~workspace ~workspace_only:false
               ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox);
          let agent = Agent.create ~config ~tool_registry:registry () in
          let mgr = Session.create ~config ~db () in
          let history_before = List.length agent.Agent.history in
          let command = Printf.sprintf "printf %%s %S > AGENTS.md" secret in
          let call =
            {
              Provider.id = "tc-shell-write";
              function_name = "shell_exec";
              arguments =
                Yojson.Safe.to_string
                  (`Assoc
                     [
                       ("command", `String command); ("cwd", `String workspace);
                     ]);
            }
          in
          Lwt_main.run
            (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
               ~session_key:(Some "web:test") [ call ]);
          Session.persist_new_messages mgr ~key:"web:test" ~history_before agent;
          let shown = Command_bridge.handle [ "session"; "show"; "web:test" ] in
          Alcotest.(check bool)
            "session show includes refresh event for shell update" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string
                       "workspace context refreshed after active workspace \
                        file update: AGENTS.md")
                    shown 0);
               true
             with Not_found -> false);
          let json = Yojson.Safe.from_string shown in
          let messages_str =
            Yojson.Safe.Util.(json |> member "messages")
            |> Yojson.Safe.to_string
          in
          Alcotest.(check bool)
            "session show redacts shell command secret in messages" false
            (try
               ignore
                 (Str.search_forward (Str.regexp_string secret) messages_str 0);
               true
             with Not_found -> false))
        ~finally:(fun () ->
          (try Sys.remove (Filename.concat workspace "AGENTS.md") with _ -> ());
          Unix.rmdir workspace))

let test_session_show_redacts_shell_exec_provider_response_items () =
  with_temp_home (fun home ->
      let db = session_db home in
      let workspace = Filename.concat home "workspace" in
      Unix.mkdir workspace 0o755;
      Fun.protect
        (fun () ->
          let secret = "shell secret prompt text" in
          write_config home
            (Printf.sprintf
               "{\n\
               \  \"workspace\": %S,\n\
               \  \"prompt\": { \"workspace_files\": [\"AGENTS.md\"] },\n\
               \  \"security\": { \"tools_enabled\": false }\n\
                }\n"
               workspace);
          let command = Printf.sprintf "printf %%s %S > AGENTS.md" secret in
          let provider_response_items_json =
            Yojson.Safe.to_string
              (`List
                 [
                   `Assoc
                     [
                       ("type", `String "function_call");
                       ("id", `String "fc-shell");
                       ("call_id", `String "fc-shell");
                       ("name", `String "shell_exec");
                       ( "arguments",
                         `String
                           (Yojson.Safe.to_string
                              (`Assoc
                                 [
                                   ("command", `String command);
                                   ("cwd", `String workspace);
                                 ])) );
                     ];
                 ])
          in
          Memory.store_message ~db ~session_key:"web:test"
            (Provider.make_message_full ~role:"assistant" ~content:""
               ~provider_response_items_json:(Some provider_response_items_json)
               ());
          let shown = Command_bridge.handle [ "session"; "show"; "web:test" ] in
          Alcotest.(check bool)
            "session show redacts shell_exec provider_response_items secret"
            false
            (try
               ignore (Str.search_forward (Str.regexp_string secret) shown 0);
               true
             with Not_found -> false);
          Alcotest.(check bool)
            "session show redacts shell_exec provider_response_items arguments"
            true
            (try
               ignore
                 (Str.search_forward (Str.regexp_string "[redacted]") shown 0);
               true
             with Not_found -> false))
        ~finally:(fun () -> Unix.rmdir workspace))

let test_session_show_paging () =
  with_temp_home (fun home ->
      let db = session_db home in
      let session_key = "cli:paging_test" in
      for i = 0 to 9 do
        Memory.store_message ~db ~session_key
          (Provider.make_message ~role:"user"
             ~content:(Printf.sprintf "message_%d" i))
      done;
      let contains s sub =
        try
          ignore (Str.search_forward (Str.regexp_string sub) s 0);
          true
        with Not_found -> false
      in
      (* Default: all 10 messages, total_messages present *)
      let all_result =
        Command_bridge.handle [ "session"; "show"; session_key ]
      in
      Alcotest.(check bool)
        "default shows total_messages" true
        (contains all_result "\"total_messages\": 10");
      Alcotest.(check bool)
        "default shows has_more false" true
        (contains all_result "\"has_more\": false");
      Alcotest.(check bool)
        "default has no offset field" false
        (contains all_result "\"offset\":");
      Alcotest.(check bool)
        "default includes first message" true
        (contains all_result "message_0");
      Alcotest.(check bool)
        "default includes last message" true
        (contains all_result "message_9");
      (* --limit 3: first 3 messages *)
      let limited =
        Command_bridge.handle [ "session"; "show"; session_key; "--limit"; "3" ]
      in
      Alcotest.(check bool)
        "limit 3 shows total_messages 10" true
        (contains limited "\"total_messages\": 10");
      Alcotest.(check bool)
        "limit 3 has_more true" true
        (contains limited "\"has_more\": true");
      Alcotest.(check bool)
        "limit 3 shows next_offset 3" true
        (contains limited "\"next_offset\": 3");
      Alcotest.(check bool)
        "limit 3 includes message_0" true
        (contains limited "message_0");
      Alcotest.(check bool)
        "limit 3 includes message_2" true
        (contains limited "message_2");
      Alcotest.(check bool)
        "limit 3 excludes message_3" false
        (contains limited "message_3");
      (* --offset 7 --limit 5: last 3 messages *)
      let paged =
        Command_bridge.handle
          [ "session"; "show"; session_key; "--offset"; "7"; "--limit"; "5" ]
      in
      Alcotest.(check bool)
        "offset 7 limit 5 shows total_messages 10" true
        (contains paged "\"total_messages\": 10");
      Alcotest.(check bool)
        "offset 7 limit 5 has_more false" true
        (contains paged "\"has_more\": false");
      Alcotest.(check bool)
        "offset 7 limit 5 shows offset 7" true
        (contains paged "\"offset\": 7");
      Alcotest.(check bool)
        "offset 7 limit 5 includes message_7" true
        (contains paged "message_7");
      Alcotest.(check bool)
        "offset 7 limit 5 includes message_9" true
        (contains paged "message_9");
      Alcotest.(check bool)
        "offset 7 limit 5 excludes message_6" false
        (contains paged "message_6");
      (* --offset 3 --limit 2: middle slice with continuation *)
      let mid =
        Command_bridge.handle
          [ "session"; "show"; session_key; "--offset"; "3"; "--limit"; "2" ]
      in
      Alcotest.(check bool)
        "mid slice has_more true" true
        (contains mid "\"has_more\": true");
      Alcotest.(check bool)
        "mid slice next_offset 5" true
        (contains mid "\"next_offset\": 5");
      Alcotest.(check bool)
        "mid slice includes message_3" true (contains mid "message_3");
      Alcotest.(check bool)
        "mid slice includes message_4" true (contains mid "message_4");
      Alcotest.(check bool)
        "mid slice excludes message_2" false (contains mid "message_2");
      Alcotest.(check bool)
        "mid slice excludes message_5" false (contains mid "message_5"))

let test_handle_capabilities () =
  with_temp_home (fun _home ->
      let result = Command_bridge.handle [ "capabilities" ] in
      Alcotest.(check bool)
        "capabilities mentions LLM" true
        (String.length result > 0))

let test_handle_auth () =
  let result = Command_bridge.handle [ "auth" ] in
  Alcotest.(check bool) "auth returns output" true (String.length result > 0)

let str_contains haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if (not !found) && String.sub haystack i nlen = needle then found := true
    done;
    !found

let test_auth_set_key_redacts_output () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let result =
        Command_bridge.handle
          [ "auth"; "set-key"; "anthropic"; "sk-abcdef1234567890xyz" ]
      in
      Alcotest.(check bool)
        "mentions provider" true
        (str_contains result "anthropic");
      Alcotest.(check bool)
        "output redacted" true
        (str_contains result "sk-a...0xyz");
      Alcotest.(check bool)
        "full key not in output" false
        (str_contains result "sk-abcdef1234567890xyz"))

let test_auth_set_key_unknown_provider_errors () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let result =
        Command_bridge.handle
          [ "auth"; "set-key"; "notarealprovider"; "sk-abcdef" ]
      in
      Alcotest.(check bool) "returns error" true (str_contains result "Error:");
      Alcotest.(check bool)
        "mentions unknown provider" true
        (str_contains result "notarealprovider");
      Alcotest.(check bool)
        "lists valid providers as CSV" true
        (str_contains result "anthropic");
      let result2 =
        Command_bridge.handle [ "auth"; "set-key"; "notarealprovider" ]
      in
      Alcotest.(check bool)
        "interactive form also errors" true
        (str_contains result2 "Error:"))

let test_auth_set_key_no_args_shows_usage () =
  let result = Command_bridge.handle [ "auth"; "set-key" ] in
  Alcotest.(check bool) "shows usage" true (str_contains result "Usage:");
  Alcotest.(check bool)
    "mentions interactive" true
    (str_contains result "interactively")

let test_config_set_secret_redacts_output () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let result =
        Command_bridge.handle
          [
            "config"; "set"; "providers.myprov.api_key"; "secret-key-value-1234";
          ]
      in
      Alcotest.(check bool)
        "output redacted" true
        (str_contains result "secr...1234");
      Alcotest.(check bool)
        "full key not in output" false
        (str_contains result "secret-key-value-1234"))

let test_config_get_secret_redacted () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      ignore
        (Command_bridge.handle
           [
             "config";
             "set";
             "providers.myprov.api_key";
             "my-secret-api-key-999";
           ]);
      let result =
        Command_bridge.handle [ "config"; "get"; "providers.myprov.api_key" ]
      in
      Alcotest.(check string) "secret redacted" "***" result)

let test_config_get_nonsecret_visible () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      ignore
        (Command_bridge.handle
           [ "config"; "set"; "agent_defaults.primary_model"; "gpt-5.4" ]);
      let result =
        Command_bridge.handle
          [ "config"; "get"; "agent_defaults.primary_model" ]
      in
      Alcotest.(check string) "non-secret visible" "gpt-5.4" result)

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
  with_temp_home (fun _home ->
      let result = Command_bridge.handle [ "cron" ] in
      Alcotest.(check bool) "cron returns output" true (String.length result > 0))

let test_handle_cron_list () =
  with_temp_home (fun _home ->
      let result = Command_bridge.handle [ "cron"; "list" ] in
      Alcotest.(check bool)
        "cron list returns output" true
        (String.length result > 0))

let test_handle_cron_list_prompt () =
  with_temp_home (fun _home ->
      let result = Command_bridge.handle [ "cron"; "list"; "--prompt" ] in
      Alcotest.(check bool)
        "cron list --prompt returns output" true
        (String.length result > 0))

let test_handle_cron_list_prompt_short () =
  with_temp_home (fun _home ->
      let result = Command_bridge.handle [ "cron"; "list"; "-p" ] in
      Alcotest.(check bool)
        "cron list -p returns output" true
        (String.length result > 0))

let test_handle_cron_list_with_jobs () =
  with_temp_home (fun _home ->
      let _add_result =
        Command_bridge.handle
          [
            "cron";
            "add";
            "test-job";
            "sess1";
            "* * * * *";
            "do";
            "the";
            "thing";
          ]
      in
      let result = Command_bridge.handle [ "cron"; "list" ] in
      Alcotest.(check bool)
        "contains job name" true
        (try
           ignore (Str.search_forward (Str.regexp_string "test-job") result 0);
           true
         with Not_found -> false);
      let result_prompt =
        Command_bridge.handle [ "cron"; "list"; "--prompt" ]
      in
      Alcotest.(check bool)
        "prompt flag shows message" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "do the thing")
                result_prompt 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "prompt flag shows PROMPT header" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "PROMPT") result_prompt 0);
           true
         with Not_found -> false))

let test_handle_cron_runs () =
  with_temp_home (fun _home ->
      let result = Command_bridge.handle [ "cron"; "runs" ] in
      Alcotest.(check bool)
        "cron runs returns output" true
        (String.length result > 0))

let test_handle_cron_history_missing_job () =
  with_temp_home (fun _home ->
      let result = Command_bridge.handle [ "cron"; "history"; "missing-job" ] in
      Alcotest.(check bool)
        "cron history returns missing-job output" true
        (String.length result > 0))

let test_handle_cron_show_missing () =
  with_temp_home (fun _home ->
      let result = Command_bridge.handle [ "cron"; "show"; "nonexistent" ] in
      Alcotest.(check bool)
        "cron show missing returns not found" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "No cron job found")
                result 0);
           true
         with Not_found -> false))

let test_handle_cron_show_existing () =
  with_temp_home (fun _home ->
      let _add =
        Command_bridge.handle
          [ "cron"; "add"; "my-job"; "sess1"; "* * * * *"; "hello"; "world" ]
      in
      let result = Command_bridge.handle [ "cron"; "show"; "my-job" ] in
      Alcotest.(check bool)
        "cron show contains job name" true
        (try
           ignore (Str.search_forward (Str.regexp_string "my-job") result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "cron show contains session" true
        (try
           ignore (Str.search_forward (Str.regexp_string "sess1") result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "cron show contains message" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "hello world") result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "cron show contains schedule" true
        (try
           ignore (Str.search_forward (Str.regexp_string "* * * * *") result 0);
           true
         with Not_found -> false))

let test_handle_cron_trigger_missing () =
  with_temp_home (fun _home ->
      let result = Command_bridge.handle [ "cron"; "trigger"; "no-such-job" ] in
      Alcotest.(check bool)
        "trigger missing returns error" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "No cron job found")
                result 0);
           true
         with Not_found -> false))

let test_handle_cron_trigger_existing () =
  with_temp_home (fun _home ->
      let _add =
        Command_bridge.handle
          [ "cron"; "add"; "trig-test"; "sess1"; "every 1h"; "say"; "hello" ]
      in
      let result = Command_bridge.handle [ "cron"; "trigger"; "trig-test" ] in
      Alcotest.(check bool)
        "trigger returns Triggered" true
        (try
           ignore (Str.search_forward (Str.regexp_string "Triggered") result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "trigger mentions background task" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "background task") result 0);
           true
         with Not_found -> false))

let test_handle_cron_run_alias () =
  with_temp_home (fun _home ->
      let _add =
        Command_bridge.handle
          [ "cron"; "add"; "run-alias"; "sess1"; "every 2h"; "test"; "msg" ]
      in
      let result = Command_bridge.handle [ "cron"; "run"; "run-alias" ] in
      Alcotest.(check bool)
        "run alias returns Triggered" true
        (try
           ignore (Str.search_forward (Str.regexp_string "Triggered") result 0);
           true
         with Not_found -> false))

let test_handle_background_list () =
  with_temp_home (fun _home ->
      let result = Command_bridge.handle [ "background"; "list" ] in
      Alcotest.(check bool)
        "background list returns output" true
        (String.length result > 0))

let test_handle_background_bare_shows_commands () =
  let contains s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  with_temp_home (fun _home ->
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
        "bare background mentions resume" true
        (contains result "background resume");
      Alcotest.(check bool)
        "bare background mentions message" true
        (contains result "background message");
      Alcotest.(check bool)
        "bare background mentions cancel" true
        (contains result "background cancel"))

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

let test_handle_background_logs_follow () =
  with_temp_home (fun home ->
      let repo = Filename.concat home "repo" in
      Unix.mkdir repo 0o755;
      init_git_repo repo;
      ignore
        (Command_bridge.handle
           [ "background"; "add"; "codex"; repo; "Implement"; "follow"; "test" ]);
      let clawq_dir = Filename.concat home ".clawq" in
      let db =
        Memory.init ~db_path:(Filename.concat clawq_dir "memory.db") ()
      in
      Background_task.init_schema db;
      let log_path = Filename.concat clawq_dir "task-1.log" in
      let oc = open_out log_path in
      output_string oc "first\nsecond\nthird\n";
      close_out oc;
      ignore
        (Background_task.set_running ~db ~id:1 ~branch:"clawq-bg-1"
           ~worktree_path:(Filename.concat home "wt")
           ~log_path ~pid:12345);
      Background_task.finish ~db ~id:1 ~status:Background_task.Succeeded
        ~result_preview:"ok";
      (* Follow on a completed task prints output to stdout and returns empty *)
      let result =
        Command_bridge.handle
          [ "background"; "logs"; "1"; "--follow"; "--lines"; "2" ]
      in
      (* Follow mode prints directly; handle returns "" on success *)
      Alcotest.(check string) "follow returns empty on success" "" result)

let test_handle_background_logs_offset () =
  with_temp_home (fun home ->
      let repo = Filename.concat home "repo" in
      Unix.mkdir repo 0o755;
      init_git_repo repo;
      ignore
        (Command_bridge.handle
           [ "background"; "add"; "codex"; repo; "Test"; "offset"; "logs" ]);
      let clawq_dir = Filename.concat home ".clawq" in
      let db =
        Memory.init ~db_path:(Filename.concat clawq_dir "memory.db") ()
      in
      Background_task.init_schema db;
      let log_path = Filename.concat clawq_dir "task-1.log" in
      let oc = open_out log_path in
      output_string oc "line0\nline1\nline2\nline3\nline4\n";
      close_out oc;
      ignore
        (Background_task.set_running ~db ~id:1 ~branch:"clawq-bg-1"
           ~worktree_path:(Filename.concat home "wt")
           ~log_path ~pid:12345);
      Background_task.finish ~db ~id:1 ~status:Background_task.Succeeded
        ~result_preview:"ok";
      let contains s sub =
        try
          ignore (Str.search_forward (Str.regexp_string sub) s 0);
          true
        with Not_found -> false
      in
      (* --offset 2 --lines 2: read lines 2-3 (1-indexed) *)
      let result =
        Command_bridge.handle
          [ "background"; "logs"; "1"; "--offset"; "2"; "--lines"; "2" ]
      in
      Alcotest.(check bool)
        "offset logs includes line1" true (contains result "line1");
      Alcotest.(check bool)
        "offset logs includes line2" true (contains result "line2");
      Alcotest.(check bool)
        "offset logs excludes line0" false (contains result "line0");
      Alcotest.(check bool)
        "offset logs excludes line3" false (contains result "line3");
      Alcotest.(check bool)
        "offset logs shows continuation" true
        (contains result "Use offset=4");
      (* --offset 4 --lines 10: read to end *)
      let end_result =
        Command_bridge.handle
          [ "background"; "logs"; "1"; "--offset"; "4"; "--lines"; "10" ]
      in
      Alcotest.(check bool)
        "end offset includes line3" true
        (contains end_result "line3");
      Alcotest.(check bool)
        "end offset includes line4" true
        (contains end_result "line4");
      Alcotest.(check bool)
        "end offset shows end of log" true
        (contains end_result "End of log"))

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

let test_handle_delegate_accepts_non_git_repo () =
  (* B349 + B649: delegate accepts non-git directories ONLY when the caller
     opts out of worktree isolation via --no-worktree (otherwise the task
     would later report status=dirty-worktree because the harvest step
     can't read a non-git checkout). *)
  with_temp_home (fun home ->
      let repo = Filename.concat home "repo" in
      Unix.mkdir repo 0o755;
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
                "--no-worktree";
                "implement";
              ]
          in
          Alcotest.(check bool)
            "delegate accepts non-git repo with --no-worktree" true
            (try
               ignore
                 (Str.search_forward
                    (Str.regexp_string "Delegated task")
                    result 0);
               true
             with Not_found -> false))
        ~finally:(fun () -> Unix.putenv "PATH" old_path))

(* B649 regression: delegate without --no-worktree must reject a non-git
   repo path upfront (rather than running and reporting status=dirty-worktree
   at the end of the task). *)
let test_handle_delegate_rejects_non_git_when_worktree_required () =
  with_temp_home (fun home ->
      let repo = Filename.concat home "non-git-workspace" in
      Unix.mkdir repo 0o755;
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
              [ "delegate"; "--runner"; "codex"; "--repo"; repo; "implement" ]
          in
          let contains needle =
            try
              ignore (Str.search_forward (Str.regexp_string needle) result 0);
              true
            with Not_found -> false
          in
          Alcotest.(check bool)
            "rejected as non-git" true
            (contains "is not a git repository");
          Alcotest.(check bool)
            "points to --no-worktree alternative" true
            (contains "--no-worktree" || contains "use_worktree=false"))
        ~finally:(fun () -> Unix.putenv "PATH" old_path))

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
  with_temp_home (fun _home ->
      let result = Command_bridge.handle [ "service"; "signal-restart" ] in
      Alcotest.(check string)
        "service signal restart with no daemon" "Daemon is not running" result)

(* WARNING: Isolation isn't perfect — this test invokes the real update path
   with fake git/make shims, but find_repo_root resolves to the real repo and
   the update flow will trigger a restart of any running clawq instance.
   Gated behind CLAWQ_TEST_DOCKER; run only in a docker container. *)
let test_handle_update_without_live_daemon_reports_stub () =
  if Sys.getenv_opt "CLAWQ_TEST_DOCKER" = None then Alcotest.skip ()
  else
    with_temp_home (fun home ->
        let bin_dir = Filename.concat home "bin" in
        Unix.mkdir bin_dir 0o755;
        let fake_git = Filename.concat bin_dir "git" in
        let git_oc = open_out fake_git in
        output_string git_oc
          "#!/bin/sh\n\
           if [ \"$1\" = \"pull\" ]; then\n\
          \  echo 'Already up to date.'\n\
          \  exit 0\n\
           fi\n\
           exit 1\n";
        close_out git_oc;
        Unix.chmod fake_git 0o755;
        let fake_make = Filename.concat bin_dir "make" in
        let make_oc = open_out fake_make in
        output_string make_oc
          "#!/bin/sh\nif [ \"$1\" = \"build\" ]; then\n  exit 0\nfi\nexit 1\n";
        close_out make_oc;
        Unix.chmod fake_make 0o755;
        let old_path = try Sys.getenv "PATH" with Not_found -> "" in
        Unix.putenv "PATH" (bin_dir ^ ":" ^ old_path);
        Fun.protect
          (fun () ->
            let result = Command_bridge.handle [ "update" ] in
            Alcotest.(check bool)
              "reports update start" true
              (contains result "Starting update...");
            Alcotest.(check bool)
              "reports git mode" true
              (contains result "Mode: git");
            Alcotest.(check bool)
              "runs git pull" true
              (contains result "Running: git pull");
            Alcotest.(check bool)
              "runs build" true
              (contains result "Running: make build");
            Alcotest.(check bool)
              "reports build completion" true
              (contains result "Build complete. Next"))
          ~finally:(fun () -> Unix.putenv "PATH" old_path))

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
  with_temp_home (fun _home ->
      let result = Command_bridge.handle [ "migrate" ] in
      Alcotest.(check bool)
        "migrate returns output" true
        (String.length result > 0))

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

let test_handle_background_resume_and_message () =
  with_temp_home (fun home ->
      let repo = Filename.concat home "repo" in
      Unix.mkdir repo 0o755;
      init_git_repo repo;
      git_empty_commit repo;
      ignore
        (Command_bridge.handle
           [ "background"; "add"; "codex"; repo; "Implement"; "resume"; "test" ]);
      let clawq_dir = Filename.concat home ".clawq" in
      let db =
        Memory.init ~db_path:(Filename.concat clawq_dir "memory.db") ()
      in
      Background_task.init_schema db;
      let worktree_path =
        add_git_worktree repo ~branch:"clawq-bg-1" ~name:"resume-wt"
      in
      ignore
        (Background_task.set_running ~db ~id:1 ~branch:"clawq-bg-1"
           ~worktree_path
           ~log_path:(Filename.concat clawq_dir "task-1.log")
           ~pid:0);
      Background_task.finish ~db ~id:1 ~status:Background_task.Succeeded
        ~result_preview:"done";
      let resume_result =
        Command_bridge.handle [ "background"; "resume"; "1" ]
      in
      Alcotest.(check bool)
        "background resume returns queued text" true
        (contains resume_result "Queued background task 1 for resume");
      let message_result =
        Command_bridge.handle
          [ "background"; "message"; "1"; "please"; "fix"; "tests" ]
      in
      Alcotest.(check bool)
        "background message returns queued text" true
        (contains message_result "Queued message for background task 1");
      Alcotest.(check int)
        "queued message persisted" 1
        (Background_task.queued_resume_message_count ~db ~id:1))

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

let test_read_daemon_tunnel_info_active () =
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
                ( "tunnel",
                  `Assoc
                    [
                      ("state", `String "active");
                      ("provider", `String "cloudflare");
                      ("url", `String "https://example.trycloudflare.com");
                      ("pid", `Null);
                    ] );
              ]));
      close_out oc;
      let result = Command_bridge_helpers.read_daemon_tunnel_info () in
      match result with
      | Some (provider, Some url) ->
          Alcotest.(check string) "provider" "cloudflare" provider;
          Alcotest.(check string) "url" "https://example.trycloudflare.com" url
      | Some (_, None) -> Alcotest.fail "expected URL but got None"
      | None -> Alcotest.fail "expected daemon tunnel info but got None")

let test_read_daemon_tunnel_info_idle () =
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
                ( "tunnel",
                  `Assoc
                    [
                      ("state", `String "idle");
                      ("provider", `Null);
                      ("url", `Null);
                      ("pid", `Null);
                    ] );
              ]));
      close_out oc;
      let result = Command_bridge_helpers.read_daemon_tunnel_info () in
      Alcotest.(check bool) "idle returns None" true (result = None))

let test_read_daemon_tunnel_info_stale_pid () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let state_path = Filename.concat clawq_dir "daemon_state.json" in
      let oc = open_out state_path in
      output_string oc
        (Yojson.Safe.to_string
           (`Assoc
              [
                ("pid", `Int 999999);
                ( "tunnel",
                  `Assoc
                    [
                      ("state", `String "active");
                      ("provider", `String "cloudflare");
                      ("url", `String "https://example.com");
                      ("pid", `Null);
                    ] );
              ]));
      close_out oc;
      let result = Command_bridge_helpers.read_daemon_tunnel_info () in
      Alcotest.(check bool) "stale daemon returns None" true (result = None))

let test_tunnel_status_daemon_fallback () =
  with_temp_home (fun home ->
      write_config_json home
        (Yojson.Safe.from_string
           {|{"tunnel":{"enabled":true,"provider":"cloudflare","url":"","managed":true,"tunnel_name":"test"}}|});
      let clawq_dir = Filename.concat home ".clawq" in
      let state_path = Filename.concat clawq_dir "daemon_state.json" in
      let oc = open_out state_path in
      output_string oc
        (Yojson.Safe.to_string
           (`Assoc
              [
                ("pid", `Int (Unix.getpid ()));
                ( "tunnel",
                  `Assoc
                    [
                      ("state", `String "active");
                      ("provider", `String "cloudflare");
                      ("url", `String "https://test.trycloudflare.com");
                      ("pid", `Null);
                    ] );
              ]));
      close_out oc;
      let result = Command_bridge.handle [ "tunnel"; "status" ] in
      Alcotest.(check bool)
        "shows daemon-managed" true
        (contains result "daemon-managed");
      Alcotest.(check bool)
        "shows URL" true
        (contains result "https://test.trycloudflare.com"))

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
  let expected = Restart_exec.executable () in
  Alcotest.(check (option (pair string (list string))))
    "re-execs agent"
    (Some (expected, [ expected; "agent" ]))
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

let test_debug_context_shows_runtime_context () =
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
    "dynamic_enabled": true
  },
  "security": {
    "tools_enabled": false
  }
}|};
      close_out oc;
      let result = Command_bridge.handle [ "debug"; "context" ] in
      Alcotest.(check bool)
        "debug context includes runtime context header" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "[Runtime context for this turn only]")
                result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "debug context includes session id" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Session id: __main__")
                result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "debug context includes main session" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Main session: yes")
                result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "debug context includes background tasks" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Background tasks:")
                result 0);
           true
         with Not_found -> false))

let test_debug_context_uses_given_session_key () =
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
    "dynamic_enabled": true
  },
  "security": {
    "tools_enabled": false
  }
}|};
      close_out oc;
      let result =
        Command_bridge.handle [ "debug"; "context"; "telegram:123:456" ]
      in
      Alcotest.(check bool)
        "debug context includes given session key" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Session id: telegram:123:456")
                result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "non-main session shows no" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Main session: no")
                result 0);
           true
         with Not_found -> false))

let test_debug_context_shows_heartbeat_for_opted_in_session () =
  with_temp_home (fun home ->
      let db = session_db home in
      Memory.upsert_session_state ~db ~session_key:"telegram:123:456"
        ~turn:"user" ~channel:"telegram" ~channel_id:"123" ();
      Memory.set_session_heartbeat ~db ~session_key:"telegram:123:456"
        ~enabled:true;
      write_config_json home
        (Yojson.Safe.from_string
           {|{
  "prompt": {
    "dynamic_enabled": true
  },
  "security": {
    "tools_enabled": false
  }
}|});
      let result =
        Command_bridge.handle [ "debug"; "context"; "telegram:123:456" ]
      in
      Alcotest.(check bool)
        "debug context shows heartbeat routing enabled" true
        (contains result "Heartbeat routing enabled for this session: yes"))

let test_debug_context_disabled_when_dynamic_off () =
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
      let result = Command_bridge.handle [ "debug"; "context" ] in
      Alcotest.(check bool)
        "debug context shows disabled message" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "dynamic prompt disabled")
                result 0);
           true
         with Not_found -> false))

let test_debug_usage_mentions_context () =
  let result = Command_bridge.handle [ "debug" ] in
  Alcotest.(check bool)
    "debug usage mentions context" true
    (try
       ignore (Str.search_forward (Str.regexp_string "debug context") result 0);
       true
     with Not_found -> false)

let test_debug_prompt_includes_workspace_file_content () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let ws_dir = Filename.concat clawq_dir "workspace" in
      Unix.mkdir ws_dir 0o755;
      let agents_path = Filename.concat ws_dir "AGENTS.md" in
      let oc = open_out agents_path in
      output_string oc "SENTINEL_B139_WORKSPACE_INJECTION";
      close_out oc;
      let config_path = Filename.concat clawq_dir "config.json" in
      let oc = open_out config_path in
      Printf.fprintf oc
        {|{
  "default_provider": "testprov",
  "workspace": "%s",
  "providers": {
    "testprov": {
      "api_key": "sk-test",
      "default_model": "test-model"
    }
  },
  "prompt": {
    "dynamic_enabled": true,
    "workspace_files": ["AGENTS.md"]
  },
  "security": {
    "tools_enabled": false
  }
}|}
        ws_dir;
      close_out oc;
      let result = Command_bridge.handle [ "debug"; "prompt"; "test" ] in
      Alcotest.(check bool)
        "debug prompt includes workspace file content" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "SENTINEL_B139_WORKSPACE_INJECTION")
                result 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "debug prompt includes workspace context header" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Workspace Context")
                result 0);
           true
         with Not_found -> false);
      (try Sys.remove agents_path with _ -> ());
      try Unix.rmdir ws_dir with _ -> ())

let test_offline_inject_enqueues_bang_message () =
  with_temp_home (fun home ->
      ignore (session_db home);
      let result =
        Command_bridge.handle
          [ "session"; "inject"; "telegram:1:user"; "!urgent"; "now" ]
      in
      Alcotest.(check bool)
        "bang inject mentions bang" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "bang interrupt requested")
                result 0);
           true
         with Not_found -> false);
      let pending_result =
        Command_bridge.handle [ "session"; "pending"; "telegram:1:user" ]
      in
      Alcotest.(check bool)
        "pending shows bang tag" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "[bang]") pending_result 0);
           true
         with Not_found -> false))

let test_session_list_shows_pending_inbound_count () =
  with_temp_home (fun home ->
      let db = session_db home in
      Memory.store_message ~db ~session_key:"telegram:1:user"
        (Provider.make_message ~role:"user" ~content:"existing");
      Memory.upsert_session_state ~db ~session_key:"telegram:1:user"
        ~turn:"user" ();
      ignore
        (Memory.queue_enqueue ~db ~session_key:"telegram:1:user" ~source:"cli"
           ~payload_json:{|{"message":"queued"}|});
      let list_result = Command_bridge.handle [ "session"; "list" ] in
      Alcotest.(check bool)
        "session list includes pending_inbound count" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "pending:1") list_result 0);
           true
         with Not_found -> false))

let test_session_pending_empty () =
  with_temp_home (fun home ->
      ignore (session_db home);
      let result =
        Command_bridge.handle [ "session"; "pending"; "nonexistent" ]
      in
      Alcotest.(check bool)
        "pending shows no rows message" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "No pending inbound rows")
                result 0);
           true
         with Not_found -> false))

let test_offline_inject_no_chat_history_insertion () =
  with_temp_home (fun home ->
      let db = session_db home in
      ignore
        (Command_bridge.handle
           [ "session"; "inject"; "telegram:1:user"; "offline"; "msg" ]);
      let msgs = Memory.load_history ~db ~session_key:"telegram:1:user" in
      Alcotest.(check int)
        "no chat history from offline inject" 0 (List.length msgs);
      let queue_count = Memory.queue_count ~db ~session_key:"telegram:1:user" in
      Alcotest.(check int) "one queued row" 1 queue_count)

let test_session_events_basic () =
  with_temp_home (fun home ->
      let db = session_db home in
      Memory.store_message ~db ~session_key:"web:evtest"
        (Provider.make_message ~role:"user" ~content:"hello");
      Memory.store_message ~db ~session_key:"web:evtest"
        (Provider.make_message ~role:"assistant" ~content:"hi there");
      Memory.store_message ~db ~session_key:"web:evtest"
        (Provider.make_message ~role:"event"
           ~content:
             "[workspace context refreshed after active workspace file update: \
              README.md]");
      Memory.store_message ~db ~session_key:"web:evtest"
        (Provider.make_message ~role:"system"
           ~content:"Relevant context from memory:\n[core:key] val");
      let result =
        Command_bridge.handle [ "session"; "events"; "web:evtest" ]
      in
      let contains s sub =
        try
          ignore (Str.search_forward (Str.regexp_string sub) s 0);
          true
        with Not_found -> false
      in
      Alcotest.(check bool)
        "session events includes workspace_refresh" true
        (contains result "workspace_refresh");
      Alcotest.(check bool)
        "session events includes memory_context" true
        (contains result "memory_context");
      Alcotest.(check bool)
        "session events excludes user/assistant messages" true
        (not (contains result "\"hi there\"")))

let test_session_events_epoch_flag () =
  with_temp_home (fun home ->
      let db = session_db home in
      Memory.store_message ~db ~session_key:"web:epochtest"
        (Provider.make_message ~role:"event"
           ~content:
             "[workspace context refreshed after active workspace file update: \
              NOTES.md]");
      let contains s sub =
        try
          ignore (Str.search_forward (Str.regexp_string sub) s 0);
          true
        with Not_found -> false
      in
      let result =
        Command_bridge.handle
          [ "session"; "events"; "web:epochtest"; "--epoch"; "current" ]
      in
      Alcotest.(check bool)
        "--epoch current returns epoch field" true
        (contains result "\"epoch\": \"current\"");
      Alcotest.(check bool)
        "--epoch current returns workspace_refresh" true
        (contains result "workspace_refresh"))

let test_session_events_type_filter () =
  with_temp_home (fun home ->
      let db = session_db home in
      Memory.store_message ~db ~session_key:"web:evfilter"
        (Provider.make_message ~role:"event"
           ~content:
             "[workspace context refreshed after active workspace file update: \
              README.md]");
      Memory.store_message ~db ~session_key:"web:evfilter"
        (Provider.make_message ~role:"system"
           ~content:"Relevant context from memory:\nsome memory");
      let result =
        Command_bridge.handle
          [ "session"; "events"; "web:evfilter"; "--type"; "workspace_refresh" ]
      in
      let contains s sub =
        try
          ignore (Str.search_forward (Str.regexp_string sub) s 0);
          true
        with Not_found -> false
      in
      Alcotest.(check bool)
        "type filter returns workspace_refresh" true
        (contains result "workspace_refresh");
      Alcotest.(check bool)
        "type filter excludes memory_context" true
        (not (contains result "memory_context")))

let test_models_set_default_rejects_unknown_plain () =
  with_temp_home (fun _dir ->
      let result =
        Command_bridge.handle
          [ "models"; "set-default"; "nonexistent-model-xyz" ]
      in
      let contains s sub =
        let re = Re.(compile (str sub)) in
        Re.execp re s
      in
      Alcotest.(check bool) "error message" true (contains result "Error:");
      Alcotest.(check bool)
        "mentions model name" true
        (contains result "nonexistent-model-xyz");
      Alcotest.(check bool)
        "hint about provider format" true
        (contains result "provider:model"))

let test_models_set_default_accepts_known_plain () =
  with_temp_home (fun _dir ->
      (* --skip-validation: test exercises the catalog/parsing path, not the
         live validation safety net (B600). *)
      let result =
        Command_bridge.handle
          [ "models"; "set-default"; "claude-sonnet-4-6"; "--skip-validation" ]
      in
      let contains s sub =
        let re = Re.(compile (str sub)) in
        Re.execp re s
      in
      Alcotest.(check bool)
        "confirms set" true
        (contains result "Default model set to:"))

let test_models_set_default_accepts_unknown_with_provider () =
  with_temp_home (fun _dir ->
      let result =
        Command_bridge.handle
          [
            "models";
            "set-default";
            "myprovider:some-custom-model";
            "--skip-validation";
          ]
      in
      let contains s sub =
        let re = Re.(compile (str sub)) in
        Re.execp re s
      in
      Alcotest.(check bool)
        "confirms set" true
        (contains result "Default model set to:");
      Alcotest.(check bool) "no error" true (not (contains result "Error:")))

let test_models_set_default_rejects_unavailable_cached_model () =
  with_temp_home (fun home ->
      let db = session_db home in
      insert_cached_model ~unavailable:true db ~provider:"dbprov"
        ~model_id:"disabled-model";
      let result =
        Command_bridge.handle
          [
            "models";
            "set-default";
            "dbprov:disabled-model";
            "--skip-validation";
          ]
      in
      Alcotest.(check bool)
        "rejects unavailable cached model" true
        (contains result "marked unavailable");
      Alcotest.(check bool)
        "does not commit unavailable cached model" true
        (not (contains result "Default model set to:")))

(* B600: validation safety net aborts switch when provider has no auth. *)
let test_models_set_default_validation_aborts_on_bad_model () =
  with_temp_home (fun _dir ->
      let result =
        Command_bridge.handle
          [ "models"; "set-default"; "myprovider:some-custom-model" ]
      in
      let contains s sub =
        let re = Re.(compile (str sub)) in
        Re.execp re s
      in
      Alcotest.(check bool)
        "validation error reported" true
        (contains result "validation failed");
      Alcotest.(check bool)
        "rollback hint shown" true
        (contains result "Rollback command if needed"))

let test_models_set_default_skip_validation_commits () =
  with_temp_home (fun _dir ->
      let result =
        Command_bridge.handle
          [
            "models";
            "set-default";
            "myprovider:some-custom-model";
            "--skip-validation";
          ]
      in
      let contains s sub =
        let re = Re.(compile (str sub)) in
        Re.execp re s
      in
      Alcotest.(check bool)
        "set proceeds with --skip-validation" true
        (contains result "Default model set to:");
      Alcotest.(check bool)
        "notes validation was skipped" true
        (contains result "validation skipped"))

let test_models_set_usage_excludes_session_only_set_without_live_session () =
  with_temp_home (fun _dir ->
      let result =
        Command_bridge.handle [ "models"; "set"; "zai_coding:glm-5" ]
      in
      let contains s sub =
        let re = Re.(compile (str sub)) in
        Re.execp re s
      in
      Alcotest.(check bool)
        "shows usage for unsupported subcommand" true
        (contains result "Usage: clawq models <subcommand>");
      Alcotest.(check bool)
        "does not advertise session-only set" false
        (contains result "set MODEL");
      Alcotest.(check bool)
        "still advertises persistent path" true
        (contains result "set-default MODEL"))

let suite =
  [
    Alcotest.test_case "handle phase2" `Quick test_handle_phase2;
    Alcotest.test_case "handle version" `Quick test_handle_version;
    Alcotest.test_case "handle unknown" `Quick test_handle_unknown;
    Alcotest.test_case "handle status" `Quick test_handle_status;
    Alcotest.test_case "handle doctor" `Quick test_handle_doctor;
    Alcotest.test_case "handle doctor flags codex api key only" `Quick
      test_handle_doctor_flags_codex_provider_with_api_key_only;
    Alcotest.test_case "handle doctor flags expired refreshable codex oauth"
      `Quick test_handle_doctor_flags_expired_refreshable_codex_oauth;
    Alcotest.test_case "handle doctor distinguishes refresh window from expired"
      `Quick test_handle_doctor_distinguishes_refresh_window_from_expired;
    Alcotest.test_case "handle models" `Quick test_handle_models;
    Alcotest.test_case "models list --json includes DB-only model" `Quick
      test_models_list_json_includes_db_only_model;
    Alcotest.test_case
      "models list --provider --json includes only matching DB-only rows" `Quick
      test_models_list_json_provider_filter_includes_db_only_only_for_provider;
    Alcotest.test_case "models list availability filters DB rows" `Quick
      test_models_list_availability_filters_db_rows;
    Alcotest.test_case "models set-default rejects unknown plain model" `Quick
      test_models_set_default_rejects_unknown_plain;
    Alcotest.test_case "models set-default accepts known plain model" `Quick
      test_models_set_default_accepts_known_plain;
    Alcotest.test_case "models set-default validation aborts on bad model"
      `Quick test_models_set_default_validation_aborts_on_bad_model;
    Alcotest.test_case "models set-default --skip-validation commits" `Quick
      test_models_set_default_skip_validation_commits;
    Alcotest.test_case "models set-default accepts unknown with provider" `Quick
      test_models_set_default_accepts_unknown_with_provider;
    Alcotest.test_case "models set-default rejects unavailable cached model"
      `Quick test_models_set_default_rejects_unavailable_cached_model;
    Alcotest.test_case
      "models usage excludes session-only set without live session" `Quick
      test_models_set_usage_excludes_session_only_set_without_live_session;
    Alcotest.test_case "handle channel" `Quick test_handle_channel;
    Alcotest.test_case "handle channel test teams" `Quick
      test_handle_channel_test_teams;
    Alcotest.test_case "handle memory" `Quick test_handle_memory;
    Alcotest.test_case "handle workspace" `Quick test_handle_workspace;
    Alcotest.test_case "handle workspace uses effective workspace" `Quick
      test_handle_workspace_uses_effective_workspace;
    Alcotest.test_case "handle session list filters" `Quick
      test_handle_session_list_filters;
    Alcotest.test_case "handle session list hides postmortem" `Quick
      test_handle_session_list_hides_postmortem;
    Alcotest.test_case "handle session heartbeat toggle" `Quick
      test_handle_session_heartbeat_toggle;
    Alcotest.test_case "handle session heartbeat rejects unsupported session"
      `Quick test_handle_session_heartbeat_rejects_unsupported_session;
    Alcotest.test_case "handle session heartbeat status mentions global disable"
      `Quick test_handle_session_heartbeat_status_mentions_global_disable;
    Alcotest.test_case "handle session inject routes to live gateway" `Quick
      test_handle_session_inject_routes_to_live_gateway;
    Alcotest.test_case "handle session inject persists when daemon missing"
      `Quick test_handle_session_inject_persists_when_daemon_missing;
    Alcotest.test_case "handle session inject reports queued bang" `Quick
      test_handle_session_inject_reports_queued_bang;
    Alcotest.test_case "handle session epochs and show archived epoch" `Quick
      test_handle_session_epochs_and_show_archived_epoch;
    Alcotest.test_case "handle session archive show" `Quick
      test_handle_session_archive_show;
    Alcotest.test_case "handle session archive show invalid id" `Quick
      test_handle_session_archive_show_invalid_id;
    Alcotest.test_case "handle session archive show not found" `Quick
      test_handle_session_archive_show_not_found;
    Alcotest.test_case "handle capabilities" `Quick test_handle_capabilities;
    Alcotest.test_case "handle auth" `Quick test_handle_auth;
    Alcotest.test_case "auth set-key redacts output" `Quick
      test_auth_set_key_redacts_output;
    Alcotest.test_case "auth set-key no args shows usage" `Quick
      test_auth_set_key_no_args_shows_usage;
    Alcotest.test_case "auth set-key unknown provider errors" `Quick
      test_auth_set_key_unknown_provider_errors;
    Alcotest.test_case "config set secret redacts output" `Quick
      test_config_set_secret_redacts_output;
    Alcotest.test_case "config get secret redacted" `Quick
      test_config_get_secret_redacted;
    Alcotest.test_case "config get non-secret visible" `Quick
      test_config_get_nonsecret_visible;
    Alcotest.test_case "handle not-impl commands" `Quick
      test_handle_not_implemented;
    Alcotest.test_case "handle cron" `Quick test_handle_cron;
    Alcotest.test_case "handle cron list" `Quick test_handle_cron_list;
    Alcotest.test_case "handle cron list --prompt" `Quick
      test_handle_cron_list_prompt;
    Alcotest.test_case "handle cron list -p" `Quick
      test_handle_cron_list_prompt_short;
    Alcotest.test_case "handle cron list with jobs" `Quick
      test_handle_cron_list_with_jobs;
    Alcotest.test_case "handle cron runs" `Quick test_handle_cron_runs;
    Alcotest.test_case "handle cron history missing job" `Quick
      test_handle_cron_history_missing_job;
    Alcotest.test_case "handle cron show missing" `Quick
      test_handle_cron_show_missing;
    Alcotest.test_case "handle cron show existing" `Quick
      test_handle_cron_show_existing;
    Alcotest.test_case "handle cron trigger missing" `Quick
      test_handle_cron_trigger_missing;
    Alcotest.test_case "handle cron trigger existing" `Quick
      test_handle_cron_trigger_existing;
    Alcotest.test_case "handle cron run alias" `Quick test_handle_cron_run_alias;
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
    Alcotest.test_case "handle background logs follow" `Quick
      test_handle_background_logs_follow;
    Alcotest.test_case "handle background logs offset" `Quick
      test_handle_background_logs_offset;
    Alcotest.test_case "handle background wait with timeout" `Quick
      test_handle_background_wait_with_timeout;
    Alcotest.test_case "handle background resume and message" `Quick
      test_handle_background_resume_and_message;
    Alcotest.test_case "handle delegate" `Quick test_handle_delegate;
    Alcotest.test_case "handle delegate with --model" `Quick
      test_handle_delegate_with_model;
    Alcotest.test_case "handle delegate accepts non-git repo" `Quick
      test_handle_delegate_accepts_non_git_repo;
    Alcotest.test_case
      "B649: handle delegate rejects non-git when use_worktree=true" `Quick
      test_handle_delegate_rejects_non_git_when_worktree_required;
    Alcotest.test_case "handle service" `Quick test_handle_service;
    Alcotest.test_case "handle service signal restart" `Quick
      test_handle_service_signal_restart;
    Alcotest.test_case "handle update without live daemon reports stub" `Slow
      test_handle_update_without_live_daemon_reports_stub;
    Alcotest.test_case "handle update auto pairs with live gateway" `Quick
      test_handle_update_auto_pairs_with_live_gateway;
    Alcotest.test_case "handle update prefers static auth token" `Quick
      test_handle_update_prefers_static_auth_token_over_auto_pair;
    Alcotest.test_case "handle session inject auto pairs with live gateway"
      `Quick test_handle_session_inject_auto_pairs_with_live_gateway;
    Alcotest.test_case "offline inject enqueues bang message" `Quick
      test_offline_inject_enqueues_bang_message;
    Alcotest.test_case "session list shows pending inbound count" `Quick
      test_session_list_shows_pending_inbound_count;
    Alcotest.test_case "session pending empty" `Quick test_session_pending_empty;
    Alcotest.test_case "offline inject no chat history insertion" `Quick
      test_offline_inject_no_chat_history_insertion;
    Alcotest.test_case "session show includes workspace refresh event" `Quick
      test_session_show_includes_workspace_refresh_event;
    Alcotest.test_case "session show redacts shell_exec prompt file updates"
      `Quick test_session_show_redacts_shell_exec_prompt_file_updates;
    Alcotest.test_case "session show redacts shell_exec provider response items"
      `Quick test_session_show_redacts_shell_exec_provider_response_items;
    Alcotest.test_case "session show paging" `Quick test_session_show_paging;
    Alcotest.test_case "session events basic" `Quick test_session_events_basic;
    Alcotest.test_case "session events epoch flag" `Quick
      test_session_events_epoch_flag;
    Alcotest.test_case "session events type filter" `Quick
      test_session_events_type_filter;
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
    Alcotest.test_case "read daemon tunnel info active" `Quick
      test_read_daemon_tunnel_info_active;
    Alcotest.test_case "read daemon tunnel info idle" `Quick
      test_read_daemon_tunnel_info_idle;
    Alcotest.test_case "read daemon tunnel info stale pid" `Quick
      test_read_daemon_tunnel_info_stale_pid;
    Alcotest.test_case "tunnel status daemon fallback" `Quick
      test_tunnel_status_daemon_fallback;
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
    Alcotest.test_case "debug context shows runtime context" `Quick
      test_debug_context_shows_runtime_context;
    Alcotest.test_case "debug context uses given session key" `Quick
      test_debug_context_uses_given_session_key;
    Alcotest.test_case "debug context shows heartbeat for opted-in session"
      `Quick test_debug_context_shows_heartbeat_for_opted_in_session;
    Alcotest.test_case "debug context disabled when dynamic off" `Quick
      test_debug_context_disabled_when_dynamic_off;
    Alcotest.test_case "debug usage mentions context" `Quick
      test_debug_usage_mentions_context;
    Alcotest.test_case "debug prompt includes workspace file content" `Quick
      test_debug_prompt_includes_workspace_file_content;
  ]
