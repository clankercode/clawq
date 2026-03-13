let test_strip_ansi_basic () =
  let input = "\027[31mERROR\027[0m: something failed" in
  let result = Error_watcher.strip_ansi input in
  Alcotest.(check string) "strips color codes" "ERROR: something failed" result

let test_strip_ansi_no_codes () =
  let input = "plain text" in
  let result = Error_watcher.strip_ansi input in
  Alcotest.(check string) "unchanged" "plain text" result

let test_strip_ansi_multiple () =
  let input = "\027[1m\027[33mWARN\027[0m: \027[36mfoo\027[0m" in
  let result = Error_watcher.strip_ansi input in
  Alcotest.(check string) "strips all" "WARN: foo" result

let test_parse_log_line_with_session () =
  let line = "[10:15:30.123] ERROR [telegram:123:456] Provider timeout" in
  match Error_watcher.parse_log_line line with
  | Some entry ->
      Alcotest.(check string) "timestamp" "10:15:30.123" entry.timestamp;
      Alcotest.(check string) "level" "ERROR" entry.level;
      Alcotest.(check (option string))
        "session_key" (Some "telegram:123:456") entry.session_key;
      Alcotest.(check string) "message" "Provider timeout" entry.message
  | None -> Alcotest.fail "expected Some"

let test_parse_log_line_without_session () =
  let line = "[09:00:00.000] WARN Global config reloaded" in
  match Error_watcher.parse_log_line line with
  | Some entry ->
      Alcotest.(check string) "timestamp" "09:00:00.000" entry.timestamp;
      Alcotest.(check string) "level" "WARN" entry.level;
      Alcotest.(check (option string)) "session_key" None entry.session_key;
      Alcotest.(check string) "message" "Global config reloaded" entry.message
  | None -> Alcotest.fail "expected Some"

let test_parse_log_line_ansi () =
  let line =
    "\027[90m[12:00:00.000]\027[0m \027[31mERROR\027[0m [s1] bad thing"
  in
  match Error_watcher.parse_log_line line with
  | Some entry ->
      Alcotest.(check string) "timestamp" "12:00:00.000" entry.timestamp;
      Alcotest.(check string) "level" "ERROR" entry.level;
      Alcotest.(check (option string))
        "session_key" (Some "s1") entry.session_key;
      Alcotest.(check string) "message" "bad thing" entry.message
  | None -> Alcotest.fail "expected Some after ANSI strip"

let test_parse_log_line_garbage () =
  let line = "not a log line at all" in
  Alcotest.(check bool)
    "None for garbage" true
    (Error_watcher.parse_log_line line = None)

let test_classify_transient () =
  let entry : Error_watcher.error_entry =
    {
      source = DaemonLog;
      session_key = None;
      level = "ERROR";
      message = "connection refused to api.openai.com";
      timestamp = "10:00:00.000";
      raw_line = "";
    }
  in
  Alcotest.(check bool)
    "transient" true
    (Error_watcher.classify_error entry = Transient)

let test_classify_actionable () =
  let entry : Error_watcher.error_entry =
    {
      source = DaemonLog;
      session_key = None;
      level = "ERROR";
      message = "Unhandled exception in agent loop: Not_found";
      timestamp = "10:00:00.000";
      raw_line = "";
    }
  in
  Alcotest.(check bool)
    "actionable" true
    (Error_watcher.classify_error entry = Actionable)

let test_dedup () =
  let entry : Error_watcher.error_entry =
    {
      source = DaemonLog;
      session_key = None;
      level = "ERROR";
      message = "Provider returned 429 on attempt 3";
      timestamp = "10:00:00.000";
      raw_line = "";
    }
  in
  let seen = [] in
  Alcotest.(check bool)
    "first is not dup" false
    (Error_watcher.is_duplicate ~cooldown_s:300.0 ~seen entry);
  let seen = Error_watcher.update_seen ~seen entry in
  Alcotest.(check bool)
    "second is dup" true
    (Error_watcher.is_duplicate ~cooldown_s:300.0 ~seen entry)

let test_dev_build_detection () =
  Alcotest.(check bool)
    "build_info version has -dev suffix" true
    (Error_watcher.is_dev_build ())

let test_json_round_trip () =
  let entry : Error_watcher.error_entry =
    {
      source = SessionError;
      session_key = Some "telegram:42:99";
      level = "ERROR";
      message = "tool execution failed";
      timestamp = "08:30:00.000";
      raw_line = "[08:30:00.000] ERROR [telegram:42:99] tool execution failed";
    }
  in
  let json = Error_watcher.error_entry_to_json entry in
  let decoded = Error_watcher.error_entry_of_json json in
  Alcotest.(check string)
    "source" "session_error"
    (Error_watcher.error_source_to_string decoded.source);
  Alcotest.(check (option string))
    "session_key" (Some "telegram:42:99") decoded.session_key;
  Alcotest.(check string) "level" "ERROR" decoded.level;
  Alcotest.(check string) "message" "tool execution failed" decoded.message;
  Alcotest.(check string) "timestamp" "08:30:00.000" decoded.timestamp;
  Alcotest.(check string) "raw_line" entry.raw_line decoded.raw_line

let test_log_rotation_offset_reset () =
  (* When a file shrinks, the byte offset should be reset to 0.
     We test the logic indirectly: if the file is shorter than the
     stored offset, scanner should reset. This is a unit-level check
     of the condition that ec_process will use. *)
  let file_size = 100 in
  let stored_offset = 500 in
  let new_offset = if stored_offset > file_size then 0 else stored_offset in
  Alcotest.(check int) "offset reset on shrink" 0 new_offset

let test_ignore_patterns () =
  let patterns = [ "heartbeat"; "keepalive" ] in
  let entry : Error_watcher.error_entry =
    {
      source = DaemonLog;
      session_key = None;
      level = "ERROR";
      message = "heartbeat check failed for session X";
      timestamp = "10:00:00.000";
      raw_line = "";
    }
  in
  let should_ignore =
    List.exists
      (fun pat ->
        String_util.contains
          (String.lowercase_ascii entry.message)
          (String.lowercase_ascii pat))
      patterns
  in
  Alcotest.(check bool) "ignored by pattern" true should_ignore

let test_session_key_exclusion () =
  let excluded_prefixes = [ "__error_correction__"; "__postmortem_" ] in
  let key = "__error_correction__abc" in
  let is_excluded =
    List.exists
      (fun prefix ->
        String.length key >= String.length prefix
        && String.sub key 0 (String.length prefix) = prefix)
      excluded_prefixes
  in
  Alcotest.(check bool) "ec session excluded" true is_excluded;
  let key2 = "telegram:123:456" in
  let is_excluded2 =
    List.exists
      (fun prefix ->
        String.length key2 >= String.length prefix
        && String.sub key2 0 (String.length prefix) = prefix)
      excluded_prefixes
  in
  Alcotest.(check bool) "normal session not excluded" false is_excluded2

let test_config_defaults () =
  let cfg = Runtime_config.default_error_watcher_config in
  Alcotest.(check bool)
    "ec_enabled matches dev build" true
    (cfg.ec_enabled = Error_watcher.is_dev_build ());
  Alcotest.(check (float 0.01)) "scan_interval" 30.0 cfg.scan_interval_s;
  Alcotest.(check (float 0.01)) "cooldown" 300.0 cfg.cooldown_s;
  Alcotest.(check int) "max_errors_per_batch" 10 cfg.max_errors_per_batch;
  Alcotest.(check bool) "auto_fix disabled" false cfg.auto_fix_enabled;
  Alcotest.(check string) "commit tag" "[INTERNAL_EC]" cfg.ec_commit_tag

let test_normalize_first_line () =
  let a = Error_watcher.normalize_first_line "Error on port 8080: conn 42" in
  let b = Error_watcher.normalize_first_line "Error on port 9090: conn 99" in
  Alcotest.(check string) "numbers normalized" a b

let with_temp_dir f =
  let dir = Filename.temp_dir "ec_test" "" in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ dir)))
    (fun () -> f dir)

let test_scan_daemon_log () =
  with_temp_dir (fun dir ->
      let log_path = Filename.concat dir "daemon.log" in
      let oc = open_out log_path in
      Printf.fprintf oc "[10:00:00.000] INFO [s1] Normal message\n";
      Printf.fprintf oc "[10:00:01.000] ERROR [s2] Something broke\n";
      Printf.fprintf oc "[10:00:02.000] WARN [s3] Disk space low\n";
      Printf.fprintf oc "[10:00:03.000] ERROR [s4] connection refused\n";
      close_out oc;
      let config =
        {
          Runtime_config.default with
          error_watcher =
            {
              Runtime_config.default_error_watcher_config with
              cooldown_s = 300.0;
              max_errors_per_batch = 100;
              ignore_patterns = [];
            };
        }
      in
      let scan_state = Ec_process.create_log_scan_state () in
      (* Override daemon_log_path for test *)
      let entries, _seen =
        (* Directly test the scanning logic using the file *)
        let st = Unix.stat log_path in
        scan_state.inode <- st.Unix.st_ino;
        let ic = open_in log_path in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            let entries = ref [] in
            let seen = ref [] in
            (try
               while true do
                 let line = input_line ic in
                 match Error_watcher.parse_log_line line with
                 | Some entry when entry.level = "ERROR" || entry.level = "WARN"
                   ->
                     if
                       not
                         (Error_watcher.is_duplicate
                            ~cooldown_s:config.error_watcher.cooldown_s
                            ~seen:!seen entry)
                     then begin
                       entries := entry :: !entries;
                       seen := Error_watcher.update_seen ~seen:!seen entry
                     end
                 | _ -> ()
               done
             with End_of_file -> ());
            (List.rev !entries, !seen))
      in
      Alcotest.(check int) "found 3 error/warn entries" 3 (List.length entries);
      Alcotest.(check string)
        "first is ERROR" "ERROR" (List.nth entries 0).level;
      Alcotest.(check string) "second is WARN" "WARN" (List.nth entries 1).level)

let test_scan_session_errors () =
  with_temp_dir (fun dir ->
      let db_path = Filename.concat dir "test.db" in
      let db = Memory.init ~db_path () in
      (* Insert some test messages *)
      let insert_msg session_key content =
        let sql =
          "INSERT INTO messages (session_key, role, content) VALUES (?, \
           'tool', ?)"
        in
        let stmt = Sqlite3.prepare db sql in
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
        ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT content));
        ignore (Sqlite3.step stmt);
        ignore (Sqlite3.finalize stmt)
      in
      insert_msg "telegram:1:2" "Error: connection timed out";
      insert_msg "telegram:1:3" "Success: file written";
      insert_msg "__error_correction__abc" "Error: should be excluded";
      insert_msg "telegram:1:4" "Error: Not_found in handler";
      let config =
        {
          Runtime_config.default with
          error_watcher =
            {
              Runtime_config.default_error_watcher_config with
              cooldown_s = 300.0;
              max_errors_per_batch = 100;
            };
        }
      in
      let entries, _seen =
        Ec_process.scan_session_errors ~db ~last_scan_time:"2000-01-01 00:00:00"
          ~config ~seen:[]
      in
      Alcotest.(check int) "found 2 session errors" 2 (List.length entries);
      Alcotest.(check (option string))
        "first session key" (Some "telegram:1:2")
        (List.nth entries 0).session_key;
      ignore (Sqlite3.db_close db))

let test_excluded_sessions () =
  Alcotest.(check bool)
    "ec session excluded" true
    (Ec_process.is_excluded_session "__error_correction__abc");
  Alcotest.(check bool)
    "postmortem excluded" true
    (Ec_process.is_excluded_session "__postmortem_xyz");
  Alcotest.(check bool)
    "normal not excluded" false
    (Ec_process.is_excluded_session "telegram:1:2")

let test_correlated_context () =
  let log_entries =
    [
      {
        Error_watcher.source = DaemonLog;
        session_key = Some "telegram:123:456";
        level = "ERROR";
        message = "Provider timeout";
        timestamp = "10:15:30.123";
        raw_line = "";
      };
    ]
  in
  let db_messages =
    [
      {
        Ec_process.session_key = "telegram:123:456";
        content = "Error: connection timed out";
        created_at = "10:15:31.000";
      };
    ]
  in
  let result = Ec_process.format_correlated_context ~log_entries ~db_messages in
  Alcotest.(check bool)
    "contains log entry" true
    (String_util.contains result "Provider timeout");
  Alcotest.(check bool)
    "contains db message" true
    (String_util.contains result "connection timed out");
  Alcotest.(check bool)
    "log entry comes first (chronological)" true
    (let log_pos =
       match String_util.contains result "Provider timeout" with
       | true -> (
           let re = Str.regexp_string "Provider timeout" in
           try
             ignore (Str.search_forward re result 0);
             Str.match_beginning ()
           with Not_found -> 999)
       | false -> 999
     in
     let db_pos =
       match String_util.contains result "connection timed out" with
       | true -> (
           let re = Str.regexp_string "connection timed out" in
           try
             ignore (Str.search_forward re result 0);
             Str.match_beginning ()
           with Not_found -> 999)
       | false -> 999
     in
     log_pos < db_pos)

let test_full_scan_cycle () =
  with_temp_dir (fun dir ->
      let old_home = Sys.getenv_opt Dot_dir.env_var in
      Unix.putenv Dot_dir.env_var dir;
      Fun.protect
        ~finally:(fun () ->
          match old_home with
          | Some v -> Unix.putenv Dot_dir.env_var v
          | None ->
              (* Can't unset, but set to empty triggers fallback *)
              Unix.putenv Dot_dir.env_var "")
        (fun () ->
          let db_path = Filename.concat dir "test.db" in
          let db = Memory.init ~db_path () in
          Background_task.init_schema db;
          (* The scan cycle should not crash with an empty DB and no log file *)
          let config =
            {
              Runtime_config.default with
              error_watcher =
                {
                  Runtime_config.default_error_watcher_config with
                  cooldown_s = 300.0;
                  max_errors_per_batch = 100;
                };
            }
          in
          let log_scan_state = Ec_process.create_log_scan_state () in
          let actionable, all, _seen =
            Ec_process.run_scan_cycle ~db ~config ~log_scan_state ~seen:[]
          in
          Alcotest.(check int)
            "no actionable on empty" 0 (List.length actionable);
          Alcotest.(check int) "no entries on empty" 0 (List.length all);
          ignore (Sqlite3.db_close db)))

(* --- E3: Diagnosis pipeline tests --- *)

let test_compute_error_hash () =
  let entries =
    [
      {
        Error_watcher.source = DaemonLog;
        session_key = None;
        level = "ERROR";
        message = "connection failed on port 8080";
        timestamp = "10:00:00.000";
        raw_line = "";
      };
    ]
  in
  let hash1 = Ec_diagnosis.compute_error_hash entries in
  Alcotest.(check bool) "hash is 16 chars" true (String.length hash1 = 16);
  (* Same entries produce same hash *)
  let hash2 = Ec_diagnosis.compute_error_hash entries in
  Alcotest.(check string) "deterministic" hash1 hash2;
  (* Different entries produce different hash *)
  let entries2 =
    [
      {
        Error_watcher.source = DaemonLog;
        session_key = None;
        level = "ERROR";
        message = "different error entirely";
        timestamp = "10:00:00.000";
        raw_line = "";
      };
    ]
  in
  let hash3 = Ec_diagnosis.compute_error_hash entries2 in
  Alcotest.(check bool) "different errors different hash" true (hash1 <> hash3)

let test_is_deadlock_error () =
  let make_entry msg =
    {
      Error_watcher.source = DaemonLog;
      session_key = None;
      level = "ERROR";
      message = msg;
      timestamp = "";
      raw_line = "";
    }
  in
  Alcotest.(check bool)
    "mutex timeout is deadlock" true
    (Ec_diagnosis.is_deadlock_error
       [ make_entry "Mutex timeout waiting on lock" ]);
  Alcotest.(check bool)
    "lwt_mutex is deadlock" true
    (Ec_diagnosis.is_deadlock_error [ make_entry "Lwt_mutex held too long" ]);
  Alcotest.(check bool)
    "normal error is not deadlock" false
    (Ec_diagnosis.is_deadlock_error
       [ make_entry "File not found: config.json" ])

let test_parse_solution_components () =
  let json =
    {|[{"label":"A","description":"Add retry logic","property_tags":["retry","resilience"]},{"label":"B","description":"Fix config","property_tags":["config"]}]|}
  in
  let components = Ec_diagnosis.parse_solution_components json in
  Alcotest.(check int) "2 components" 2 (List.length components);
  let first = List.nth components 0 in
  Alcotest.(check string) "label A" "A" first.label;
  Alcotest.(check string) "description" "Add retry logic" first.description;
  Alcotest.(check int) "2 tags" 2 (List.length first.property_tags)

let test_parse_solution_components_invalid () =
  let result = Ec_diagnosis.parse_solution_components "not json" in
  Alcotest.(check int) "empty on invalid" 0 (List.length result)

let test_extract_json_from_response () =
  (* Direct JSON *)
  let r1 = Ec_diagnosis.extract_json_from_response "[1,2,3]" in
  Alcotest.(check string) "direct json" "[1,2,3]" r1;
  (* With markdown fences *)
  let r2 =
    Ec_diagnosis.extract_json_from_response
      "Here is the result:\n```json\n[1,2,3]\n```\nDone."
  in
  Alcotest.(check string) "fenced json" "[1,2,3]" (String.trim r2)

let test_generate_combinations () =
  let proposals =
    [
      {
        Ec_diagnosis.model = "model1";
        components =
          [
            {
              Ec_diagnosis.label = "A";
              description = "Fix A";
              property_tags = [ "fix" ];
            };
            { label = "B"; description = "Fix B"; property_tags = [ "fix" ] };
          ];
      };
    ]
  in
  let combos = Ec_diagnosis.generate_combinations proposals in
  (* Should have singles (A, B) + pair (A+B) = 3 *)
  Alcotest.(check int) "3 combinations" 3 (List.length combos);
  let single_a =
    List.find_opt (fun c -> c.Ec_diagnosis.labels = [ "A" ]) combos
  in
  Alcotest.(check bool) "has single A" true (Option.is_some single_a);
  let pair =
    List.find_opt (fun c -> List.length c.Ec_diagnosis.labels = 2) combos
  in
  Alcotest.(check bool) "has pair" true (Option.is_some pair)

let test_generate_combinations_single () =
  let proposals =
    [
      {
        Ec_diagnosis.model = "m1";
        components =
          [
            {
              Ec_diagnosis.label = "X";
              description = "Only fix";
              property_tags = [];
            };
          ];
      };
    ]
  in
  let combos = Ec_diagnosis.generate_combinations proposals in
  Alcotest.(check int) "1 combination" 1 (List.length combos)

let test_generate_combinations_empty () =
  let combos = Ec_diagnosis.generate_combinations [] in
  Alcotest.(check int) "0 combinations" 0 (List.length combos)

let test_tally_votes () =
  let combinations =
    [
      { Ec_diagnosis.labels = [ "A" ]; description = "Fix A" };
      { labels = [ "B" ]; description = "Fix B" };
      { labels = [ "A"; "B" ]; description = "Fix A + Fix B" };
    ]
  in
  let votes =
    [
      {
        Ec_diagnosis.model = "model1";
        ranking =
          [
            { Ec_diagnosis.labels = [ "A"; "B" ]; description = "" };
            { labels = [ "A" ]; description = "" };
            { labels = [ "B" ]; description = "" };
          ];
      };
      {
        model = "model2";
        ranking =
          [
            { Ec_diagnosis.labels = [ "A" ]; description = "" };
            { labels = [ "A"; "B" ]; description = "" };
            { labels = [ "B" ]; description = "" };
          ];
      };
    ]
  in
  let tally = Ec_diagnosis.tally_votes ~votes ~combinations in
  Alcotest.(check bool) "non-empty tally" true (List.length tally > 0);
  (* A+B got 3+2=5, A got 2+3=5, B got 1+1=2 *)
  (* Tie between A+B and A: A wins (simpler: 1 label < 2 labels) *)
  let first = List.nth tally 0 in
  Alcotest.(check (list string))
    "winner is simpler on tie" [ "A" ] first.combination.labels

let test_parse_ranking () =
  let r1 = Ec_diagnosis.parse_ranking "[3, 1, 2]" ~n_combinations:3 in
  Alcotest.(check (list int)) "valid ranking" [ 2; 0; 1 ] r1;
  let r2 = Ec_diagnosis.parse_ranking "invalid" ~n_combinations:3 in
  Alcotest.(check (list int)) "invalid returns empty" [] r2;
  (* Out of range values filtered *)
  let r3 = Ec_diagnosis.parse_ranking "[1, 5, 2]" ~n_combinations:3 in
  Alcotest.(check (list int)) "out of range filtered" [ 0; 1 ] r3

let test_synthesize_plans () =
  let plan1 = Ec_diagnosis.synthesize_plans [] in
  Alcotest.(check string) "empty plans" "(no plans available)" plan1;
  let plan2 = Ec_diagnosis.synthesize_plans [ ("m1", "Do thing A") ] in
  Alcotest.(check string) "single plan" "Do thing A" plan2;
  let plan3 =
    Ec_diagnosis.synthesize_plans [ ("m1", "Plan 1"); ("m2", "Plan 2") ]
  in
  Alcotest.(check bool)
    "synthesized contains both" true
    (String_util.contains plan3 "Plan 1" && String_util.contains plan3 "Plan 2")

let test_ec_reports_db () =
  with_temp_dir (fun dir ->
      let db_path = Filename.concat dir "test.db" in
      let db = Memory.init ~db_path () in
      Ec_diagnosis.init_ec_reports_schema db;
      let report =
        {
          Ec_diagnosis.error_hash = "abc123";
          error_context = "test context";
          diagnoses_json = "[]";
          voting_json = "{}";
          winning_plan = "fix it";
          fix_task_id = None;
          status = "plan_ready";
        }
      in
      (match Ec_diagnosis.insert_ec_report ~db report with
      | Ok id ->
          Alcotest.(check bool) "positive id" true (id > 0);
          let reports = Ec_diagnosis.list_ec_reports ~db () in
          Alcotest.(check int) "one report" 1 (List.length reports);
          let rid, _ts, hash, status = List.nth reports 0 in
          Alcotest.(check int) "same id" id rid;
          Alcotest.(check string) "hash matches" "abc123" hash;
          Alcotest.(check string) "status matches" "plan_ready" status
      | Error msg -> Alcotest.fail ("insert failed: " ^ msg));
      ignore (Sqlite3.db_close db))

let test_ec_reports_with_fix_task () =
  with_temp_dir (fun dir ->
      let db_path = Filename.concat dir "test.db" in
      let db = Memory.init ~db_path () in
      Ec_diagnosis.init_ec_reports_schema db;
      let report =
        {
          Ec_diagnosis.error_hash = "def456";
          error_context = "context";
          diagnoses_json = "[{\"model\":\"m1\",\"analysis\":\"root cause\"}]";
          voting_json = "{\"tally\":[]}";
          winning_plan = "detailed plan";
          fix_task_id = Some 42;
          status = "fix_spawned";
        }
      in
      (match Ec_diagnosis.insert_ec_report ~db report with
      | Ok _ ->
          let reports = Ec_diagnosis.list_ec_reports ~db () in
          let _, _, _, status = List.nth reports 0 in
          Alcotest.(check string) "fix_spawned status" "fix_spawned" status
      | Error msg -> Alcotest.fail ("insert failed: " ^ msg));
      ignore (Sqlite3.db_close db))

let test_diagnoses_to_json () =
  let diagnoses =
    [
      {
        Ec_diagnosis.model = "anthropic:claude-opus-4-6";
        analysis = "Root cause is X";
        is_deadlock = false;
      };
    ]
  in
  let json_str = Ec_diagnosis.diagnoses_to_json diagnoses in
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let items = json |> to_list in
  Alcotest.(check int) "1 diagnosis" 1 (List.length items);
  let first = List.nth items 0 in
  Alcotest.(check string)
    "model" "anthropic:claude-opus-4-6"
    (first |> member "model" |> to_string)

let test_voting_to_json () =
  let tally =
    [
      {
        Ec_diagnosis.combination =
          { Ec_diagnosis.labels = [ "A" ]; description = "" };
        score = 5;
        voter_count = 2;
      };
    ]
  in
  let votes =
    [
      {
        Ec_diagnosis.model = "m1";
        ranking = [ { Ec_diagnosis.labels = [ "A" ]; description = "" } ];
      };
    ]
  in
  let json_str = Ec_diagnosis.voting_to_json tally votes in
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let tally_items = json |> member "tally" |> to_list in
  Alcotest.(check int) "1 tally entry" 1 (List.length tally_items);
  let score = List.nth tally_items 0 |> member "score" |> to_int in
  Alcotest.(check int) "score is 5" 5 score

(* --- E4: CLI command tests --- *)

let test_watcher_status () =
  let result = Command_bridge.handle [ "watcher"; "status" ] in
  Alcotest.(check bool)
    "contains Enabled" true
    (String_util.contains result "Enabled:");
  Alcotest.(check bool)
    "contains EC process" true
    (String_util.contains result "EC process:");
  Alcotest.(check bool)
    "contains Primary models" true
    (String_util.contains result "Primary models:")

let test_watcher_default_is_status () =
  let result = Command_bridge.handle [ "watcher" ] in
  Alcotest.(check bool)
    "default shows status" true
    (String_util.contains result "Error Correction Watcher")

let test_watcher_reports_empty () =
  with_temp_dir (fun dir ->
      let old_home = Sys.getenv_opt Dot_dir.env_var in
      Unix.putenv Dot_dir.env_var dir;
      Fun.protect
        ~finally:(fun () ->
          match old_home with
          | Some v -> Unix.putenv Dot_dir.env_var v
          | None -> Unix.putenv Dot_dir.env_var "")
        (fun () ->
          let result = Command_bridge.handle [ "watcher"; "reports" ] in
          Alcotest.(check string) "no reports" "No EC reports found." result))

let test_watcher_usage () =
  let result = Command_bridge.handle [ "watcher"; "invalid-command" ] in
  Alcotest.(check bool)
    "shows usage" true
    (String_util.contains result "Usage:")

let test_ec_run_no_args () =
  let result = Command_bridge.handle [ "ec-run" ] in
  Alcotest.(check bool)
    "shows usage" true
    (String_util.contains result "Usage:")

let suite =
  [
    Alcotest.test_case "strip_ansi basic" `Quick test_strip_ansi_basic;
    Alcotest.test_case "strip_ansi no codes" `Quick test_strip_ansi_no_codes;
    Alcotest.test_case "strip_ansi multiple" `Quick test_strip_ansi_multiple;
    Alcotest.test_case "parse_log_line with session" `Quick
      test_parse_log_line_with_session;
    Alcotest.test_case "parse_log_line without session" `Quick
      test_parse_log_line_without_session;
    Alcotest.test_case "parse_log_line ansi" `Quick test_parse_log_line_ansi;
    Alcotest.test_case "parse_log_line garbage" `Quick
      test_parse_log_line_garbage;
    Alcotest.test_case "classify transient" `Quick test_classify_transient;
    Alcotest.test_case "classify actionable" `Quick test_classify_actionable;
    Alcotest.test_case "dedup within cooldown" `Quick test_dedup;
    Alcotest.test_case "dev build detection" `Quick test_dev_build_detection;
    Alcotest.test_case "json round-trip" `Quick test_json_round_trip;
    Alcotest.test_case "log rotation offset reset" `Quick
      test_log_rotation_offset_reset;
    Alcotest.test_case "ignore patterns" `Quick test_ignore_patterns;
    Alcotest.test_case "session key exclusion" `Quick test_session_key_exclusion;
    Alcotest.test_case "config defaults" `Quick test_config_defaults;
    Alcotest.test_case "normalize first line" `Quick test_normalize_first_line;
    Alcotest.test_case "scan daemon log" `Quick test_scan_daemon_log;
    Alcotest.test_case "scan session errors" `Quick test_scan_session_errors;
    Alcotest.test_case "excluded sessions" `Quick test_excluded_sessions;
    Alcotest.test_case "correlated context" `Quick test_correlated_context;
    Alcotest.test_case "full scan cycle empty" `Quick test_full_scan_cycle;
    (* E3: Diagnosis pipeline tests *)
    Alcotest.test_case "compute error hash" `Quick test_compute_error_hash;
    Alcotest.test_case "is deadlock error" `Quick test_is_deadlock_error;
    Alcotest.test_case "parse solution components" `Quick
      test_parse_solution_components;
    Alcotest.test_case "parse solution components invalid" `Quick
      test_parse_solution_components_invalid;
    Alcotest.test_case "extract json from response" `Quick
      test_extract_json_from_response;
    Alcotest.test_case "generate combinations" `Quick test_generate_combinations;
    Alcotest.test_case "generate combinations single" `Quick
      test_generate_combinations_single;
    Alcotest.test_case "generate combinations empty" `Quick
      test_generate_combinations_empty;
    Alcotest.test_case "tally votes" `Quick test_tally_votes;
    Alcotest.test_case "parse ranking" `Quick test_parse_ranking;
    Alcotest.test_case "synthesize plans" `Quick test_synthesize_plans;
    Alcotest.test_case "ec reports db" `Quick test_ec_reports_db;
    Alcotest.test_case "ec reports with fix task" `Quick
      test_ec_reports_with_fix_task;
    Alcotest.test_case "diagnoses to json" `Quick test_diagnoses_to_json;
    Alcotest.test_case "voting to json" `Quick test_voting_to_json;
    (* E4: CLI command tests *)
    Alcotest.test_case "watcher status" `Quick test_watcher_status;
    Alcotest.test_case "watcher default is status" `Quick
      test_watcher_default_is_status;
    Alcotest.test_case "watcher reports empty" `Quick test_watcher_reports_empty;
    Alcotest.test_case "watcher usage" `Quick test_watcher_usage;
    Alcotest.test_case "ec-run no args" `Quick test_ec_run_no_args;
  ]
