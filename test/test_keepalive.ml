let with_db f = Test_helpers.with_memory_store f

(* --- set and get keepalive flag --- *)

let test_set_get_keepalive () =
  with_db (fun db ->
      (* Enable keepalive for a session *)
      Memory.set_session_keepalive ~db ~session_key:"mysession" ~enabled:true;
      let keys = Memory.list_keepalive_session_keys ~db in
      Alcotest.(check (list string))
        "keepalive session listed" [ "mysession" ] keys)

let test_set_keepalive_off () =
  with_db (fun db ->
      Memory.set_session_keepalive ~db ~session_key:"mysession" ~enabled:true;
      Memory.set_session_keepalive ~db ~session_key:"mysession" ~enabled:false;
      let keys = Memory.list_keepalive_session_keys ~db in
      Alcotest.(check (list string)) "no keepalive sessions" [] keys)

let test_multiple_keepalive_sessions () =
  with_db (fun db ->
      Memory.set_session_keepalive ~db ~session_key:"s1" ~enabled:true;
      Memory.set_session_keepalive ~db ~session_key:"s2" ~enabled:true;
      Memory.set_session_keepalive ~db ~session_key:"s3" ~enabled:false;
      let keys = Memory.list_keepalive_session_keys ~db in
      Alcotest.(check int) "two keepalive sessions" 2 (List.length keys);
      Alcotest.(check bool) "s1 in list" true (List.mem "s1" keys);
      Alcotest.(check bool) "s2 in list" true (List.mem "s2" keys);
      Alcotest.(check bool) "s3 not in list" false (List.mem "s3" keys))

(* --- keepalive flag appears in session_info --- *)

let test_keepalive_reflected_in_session_info () =
  with_db (fun db ->
      Memory.upsert_session_state ~db ~session_key:"s1" ~turn:"user" ();
      Memory.set_session_keepalive ~db ~session_key:"s1" ~enabled:true;
      let infos = Memory.list_session_infos ~db () in
      match
        List.find_opt
          (fun (r : Memory.session_info) -> r.session_key = "s1")
          infos
      with
      | None -> Alcotest.fail "session s1 not found in list"
      | Some r ->
          Alcotest.(check bool)
            "keepalive_enabled true" true r.keepalive_enabled)

let test_keepalive_default_false_in_session_info () =
  with_db (fun db ->
      Memory.upsert_session_state ~db ~session_key:"s2" ~turn:"user" ();
      let infos = Memory.list_session_infos ~db () in
      match
        List.find_opt
          (fun (r : Memory.session_info) -> r.session_key = "s2")
          infos
      with
      | None -> Alcotest.fail "session s2 not found in list"
      | Some r ->
          Alcotest.(check bool)
            "keepalive_enabled default false" false r.keepalive_enabled)

(* --- keepalive flag survives upsert_session_state --- *)

let test_keepalive_survives_upsert () =
  with_db (fun db ->
      Memory.upsert_session_state ~db ~session_key:"sk" ~turn:"user" ();
      Memory.set_session_keepalive ~db ~session_key:"sk" ~enabled:true;
      (* Simulate normal session activity update *)
      Memory.upsert_session_state ~db ~session_key:"sk" ~turn:"agent" ();
      Memory.mark_response_sent ~db ~session_key:"sk";
      let keys = Memory.list_keepalive_session_keys ~db in
      Alcotest.(check bool)
        "keepalive still set after upsert" true (List.mem "sk" keys))

let test_set_get_heartbeat () =
  with_db (fun db ->
      Memory.set_session_heartbeat ~db ~session_key:"telegram:42:user"
        ~enabled:true;
      let keys = Memory.list_heartbeat_session_keys ~db in
      Alcotest.(check (list string))
        "heartbeat session listed" [ "telegram:42:user" ] keys)

let test_set_heartbeat_off () =
  with_db (fun db ->
      Memory.set_session_heartbeat ~db ~session_key:"telegram:42:user"
        ~enabled:true;
      Memory.set_session_heartbeat ~db ~session_key:"telegram:42:user"
        ~enabled:false;
      let keys = Memory.list_heartbeat_session_keys ~db in
      Alcotest.(check (list string)) "no heartbeat sessions" [] keys)

let test_heartbeat_reflected_in_session_info () =
  with_db (fun db ->
      Memory.upsert_session_state ~db ~session_key:"telegram:42:user"
        ~turn:"user" ~channel:"telegram" ~channel_id:"42" ();
      Memory.set_session_heartbeat ~db ~session_key:"telegram:42:user"
        ~enabled:true;
      let infos = Memory.list_session_infos ~db () in
      match
        List.find_opt
          (fun (r : Memory.session_info) -> r.session_key = "telegram:42:user")
          infos
      with
      | None -> Alcotest.fail "session not found in list"
      | Some r ->
          Alcotest.(check bool)
            "heartbeat_enabled true" true r.heartbeat_enabled)

let test_heartbeat_survives_upsert () =
  with_db (fun db ->
      Memory.upsert_session_state ~db ~session_key:"telegram:42:user"
        ~turn:"user" ();
      Memory.set_session_heartbeat ~db ~session_key:"telegram:42:user"
        ~enabled:true;
      Memory.upsert_session_state ~db ~session_key:"telegram:42:user"
        ~turn:"agent" ();
      let keys = Memory.list_heartbeat_session_keys ~db in
      Alcotest.(check bool)
        "heartbeat still set after upsert" true
        (List.mem "telegram:42:user" keys))

let test_teams_heartbeat_supported () =
  with_db (fun db ->
      let key = "teams:tenant1:19:abc@thread.tacv2" in
      Memory.set_session_heartbeat ~db ~session_key:key ~enabled:true;
      let keys = Memory.list_heartbeat_session_keys ~db in
      Alcotest.(check bool)
        "teams session listed in heartbeat keys" true (List.mem key keys);
      Alcotest.(check bool)
        "teams heartbeat supported" true
        (Session.heartbeat_supported_session_key key))

(* --- keepalive nudge prompt constant is defined --- *)

let test_nudge_prompt_hides_stay_idle () =
  Alcotest.(check bool)
    "nudge prompt hides STAY_IDLE" false
    (let re = Str.regexp_string Session.autonomous_stay_idle_message in
     try
       ignore (Str.search_forward re Session.keepalive_nudge_prompt 0);
       true
     with Not_found -> false)

let test_nudge_prompt_contains_header () =
  Alcotest.(check bool)
    "nudge prompt contains keepalive header" true
    (let re = Str.regexp_string "Automated Keepalive Check-In" in
     try
       ignore (Str.search_forward re Session.keepalive_nudge_prompt 0);
       true
     with Not_found -> false)

let suite =
  [
    Alcotest.test_case "set and get keepalive flag" `Quick
      test_set_get_keepalive;
    Alcotest.test_case "set keepalive off clears" `Quick test_set_keepalive_off;
    Alcotest.test_case "multiple keepalive sessions" `Quick
      test_multiple_keepalive_sessions;
    Alcotest.test_case "keepalive reflected in session_info" `Quick
      test_keepalive_reflected_in_session_info;
    Alcotest.test_case "keepalive default false in session_info" `Quick
      test_keepalive_default_false_in_session_info;
    Alcotest.test_case "keepalive survives upsert_session_state" `Quick
      test_keepalive_survives_upsert;
    Alcotest.test_case "set and get heartbeat flag" `Quick
      test_set_get_heartbeat;
    Alcotest.test_case "set heartbeat off clears" `Quick test_set_heartbeat_off;
    Alcotest.test_case "heartbeat reflected in session_info" `Quick
      test_heartbeat_reflected_in_session_info;
    Alcotest.test_case "heartbeat survives upsert_session_state" `Quick
      test_heartbeat_survives_upsert;
    Alcotest.test_case "teams heartbeat supported" `Quick
      test_teams_heartbeat_supported;
    Alcotest.test_case "nudge prompt hides STAY_IDLE" `Quick
      test_nudge_prompt_hides_stay_idle;
    Alcotest.test_case "nudge prompt contains header" `Quick
      test_nudge_prompt_contains_header;
  ]
