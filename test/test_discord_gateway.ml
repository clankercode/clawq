let test_handle_hello () =
  let dispatches = ref [] in
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = None;
      resume_gateway_url = None;
      seq = None;
      heartbeat_interval = 0.0;
      heartbeat_ack_received = false;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch =
        (fun name d ->
          dispatches := (name, d) :: !dispatches;
          Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  let hello_msg = {|{"op":10,"d":{"heartbeat_interval":41250}}|} in
  Lwt_main.run (Discord_gateway.handle_gateway_message gw hello_msg);
  Alcotest.(check (float 0.1))
    "heartbeat_interval" 41250.0 gw.heartbeat_interval;
  Alcotest.(check bool) "ack set" true gw.heartbeat_ack_received;
  (* Stop heartbeat to avoid background thread *)
  match gw.heartbeat_stop with
  | Some u -> Lwt.wakeup_later u ()
  | None -> ()

let test_handle_heartbeat_ack () =
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = None;
      resume_gateway_url = None;
      seq = None;
      heartbeat_interval = 41250.0;
      heartbeat_ack_received = false;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch = (fun _ _ -> Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  Lwt_main.run (Discord_gateway.handle_gateway_message gw {|{"op":11}|});
  Alcotest.(check bool) "ack received" true gw.heartbeat_ack_received

let test_handle_dispatch_ready () =
  let dispatches = ref [] in
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = None;
      resume_gateway_url = None;
      seq = None;
      heartbeat_interval = 41250.0;
      heartbeat_ack_received = true;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch =
        (fun name d ->
          dispatches := (name, d) :: !dispatches;
          Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  let ready_msg =
    {|{"op":0,"s":1,"t":"READY","d":{"session_id":"abc123","resume_gateway_url":"wss://resume.example.com"}}|}
  in
  Lwt_main.run (Discord_gateway.handle_gateway_message gw ready_msg);
  Alcotest.(check (option string)) "session_id" (Some "abc123") gw.session_id;
  Alcotest.(check (option string))
    "resume_url" (Some "wss://resume.example.com") gw.resume_gateway_url;
  Alcotest.(check (option int)) "seq" (Some 1) gw.seq;
  Alcotest.(check int) "dispatch count" 1 (List.length !dispatches);
  let name, _ = List.hd !dispatches in
  Alcotest.(check string) "dispatch event" "READY" name

let test_handle_dispatch_message_create () =
  let dispatches = ref [] in
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = Some "sess";
      resume_gateway_url = None;
      seq = Some 5;
      heartbeat_interval = 41250.0;
      heartbeat_ack_received = true;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch =
        (fun name d ->
          dispatches := (name, d) :: !dispatches;
          Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  let msg =
    {|{"op":0,"s":6,"t":"MESSAGE_CREATE","d":{"channel_id":"ch1","author":{"id":"u1","bot":false},"content":"hello"}}|}
  in
  Lwt_main.run (Discord_gateway.handle_gateway_message gw msg);
  Alcotest.(check (option int)) "seq updated" (Some 6) gw.seq;
  Alcotest.(check int) "dispatch count" 1 (List.length !dispatches);
  let name, _ = List.hd !dispatches in
  Alcotest.(check string) "dispatch event" "MESSAGE_CREATE" name

let test_handle_invalid_session_resumable () =
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = Some "sess";
      resume_gateway_url = None;
      seq = Some 10;
      heartbeat_interval = 41250.0;
      heartbeat_ack_received = true;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch = (fun _ _ -> Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  (* resumable=true: session_id should be preserved *)
  Lwt_main.run (Discord_gateway.handle_gateway_message gw {|{"op":9,"d":true}|});
  Alcotest.(check (option string))
    "session_id preserved" (Some "sess") gw.session_id;
  Alcotest.(check (option int)) "seq preserved" (Some 10) gw.seq

let test_handle_invalid_session_not_resumable () =
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = Some "sess";
      resume_gateway_url = None;
      seq = Some 10;
      heartbeat_interval = 41250.0;
      heartbeat_ack_received = true;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch = (fun _ _ -> Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  (* resumable=false: session_id should be cleared *)
  Lwt_main.run
    (Discord_gateway.handle_gateway_message gw {|{"op":9,"d":false}|});
  Alcotest.(check (option string)) "session_id cleared" None gw.session_id;
  Alcotest.(check (option int)) "seq cleared" None gw.seq

let test_handle_heartbeat_request () =
  (* op 1: heartbeat request from server. We can't easily test send without a real ws,
     but we verify it doesn't error. *)
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = None;
      resume_gateway_url = None;
      seq = Some 5;
      heartbeat_interval = 41250.0;
      heartbeat_ack_received = true;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch = (fun _ _ -> Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  (* No ws connected so send is a no-op, just verify no crash *)
  Lwt_main.run (Discord_gateway.handle_gateway_message gw {|{"op":1}|});
  Alcotest.(check unit) "no crash on heartbeat request" () ()

let test_handle_malformed_json () =
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = None;
      resume_gateway_url = None;
      seq = None;
      heartbeat_interval = 41250.0;
      heartbeat_ack_received = true;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch = (fun _ _ -> Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  Lwt_main.run (Discord_gateway.handle_gateway_message gw "not json at all");
  Alcotest.(check unit) "no crash on malformed json" () ()

let test_seq_updates () =
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = None;
      resume_gateway_url = None;
      seq = None;
      heartbeat_interval = 41250.0;
      heartbeat_ack_received = true;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch = (fun _ _ -> Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  Lwt_main.run
    (Discord_gateway.handle_gateway_message gw
       {|{"op":0,"s":1,"t":"GUILD_CREATE","d":{}}|});
  Alcotest.(check (option int)) "seq=1" (Some 1) gw.seq;
  Lwt_main.run
    (Discord_gateway.handle_gateway_message gw
       {|{"op":0,"s":42,"t":"TYPING_START","d":{}}|});
  Alcotest.(check (option int)) "seq=42" (Some 42) gw.seq

let test_handle_reconnect () =
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = Some "sess";
      resume_gateway_url = None;
      seq = Some 5;
      heartbeat_interval = 41250.0;
      heartbeat_ack_received = true;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch = (fun _ _ -> Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  (* No ws connected so close is a no-op, just verify no crash *)
  Lwt_main.run (Discord_gateway.handle_gateway_message gw {|{"op":7}|});
  Alcotest.(check unit) "no crash on reconnect" () ()

let test_handle_unknown_opcode () =
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = None;
      resume_gateway_url = None;
      seq = None;
      heartbeat_interval = 41250.0;
      heartbeat_ack_received = true;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch = (fun _ _ -> Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  Lwt_main.run (Discord_gateway.handle_gateway_message gw {|{"op":99}|});
  Alcotest.(check unit) "no crash on unknown opcode" () ()

let test_hello_with_existing_session_triggers_resume () =
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = Some "existing-session";
      resume_gateway_url = Some "wss://resume.example.com";
      seq = Some 42;
      heartbeat_interval = 0.0;
      heartbeat_ack_received = false;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch = (fun _ _ -> Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  let hello_msg = {|{"op":10,"d":{"heartbeat_interval":41250}}|} in
  Lwt_main.run (Discord_gateway.handle_gateway_message gw hello_msg);
  (* session_id should still be set (resume path, not fresh identify) *)
  Alcotest.(check (option string))
    "session_id preserved" (Some "existing-session") gw.session_id;
  Alcotest.(check (option int)) "seq preserved" (Some 42) gw.seq;
  Alcotest.(check (float 0.1))
    "heartbeat_interval" 41250.0 gw.heartbeat_interval;
  match gw.heartbeat_stop with Some u -> Lwt.wakeup_later u () | None -> ()

let test_dispatch_no_seq_field () =
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = None;
      resume_gateway_url = None;
      seq = Some 10;
      heartbeat_interval = 41250.0;
      heartbeat_ack_received = true;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch = (fun _ _ -> Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  (* Dispatch with no "s" field -- seq should remain unchanged *)
  Lwt_main.run
    (Discord_gateway.handle_gateway_message gw
       {|{"op":0,"t":"TYPING_START","d":{}}|});
  Alcotest.(check (option int)) "seq unchanged" (Some 10) gw.seq

let test_hello_integer_heartbeat_interval () =
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = None;
      resume_gateway_url = None;
      seq = None;
      heartbeat_interval = 0.0;
      heartbeat_ack_received = false;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch = (fun _ _ -> Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  (* heartbeat_interval as integer instead of float *)
  let hello_msg = {|{"op":10,"d":{"heartbeat_interval":45000}}|} in
  Lwt_main.run (Discord_gateway.handle_gateway_message gw hello_msg);
  Alcotest.(check (float 0.1))
    "heartbeat_interval from int" 45000.0 gw.heartbeat_interval;
  match gw.heartbeat_stop with Some u -> Lwt.wakeup_later u () | None -> ()

let test_accessor_functions () =
  let gw : Discord_gateway.t =
    {
      ws = None;
      session_id = Some "sid1";
      resume_gateway_url = Some "wss://resume.example.com";
      seq = Some 99;
      heartbeat_interval = 41250.0;
      heartbeat_ack_received = true;
      heartbeat_stop = None;
      bot_token = "test-token";
      intents = 513;
      on_dispatch = (fun _ _ -> Lwt.return_unit);
      on_close = (fun _ -> Lwt.return_unit);
    }
  in
  Alcotest.(check (option string))
    "session_id" (Some "sid1")
    (Discord_gateway.session_id gw);
  Alcotest.(check (option int))
    "last_seq" (Some 99)
    (Discord_gateway.last_seq gw);
  Alcotest.(check (option string))
    "resume_url" (Some "wss://resume.example.com")
    (Discord_gateway.resume_url gw)

let suite =
  [
    Alcotest.test_case "handle hello" `Quick test_handle_hello;
    Alcotest.test_case "handle heartbeat ack" `Quick test_handle_heartbeat_ack;
    Alcotest.test_case "handle dispatch ready" `Quick test_handle_dispatch_ready;
    Alcotest.test_case "handle dispatch message_create" `Quick
      test_handle_dispatch_message_create;
    Alcotest.test_case "handle invalid session resumable" `Quick
      test_handle_invalid_session_resumable;
    Alcotest.test_case "handle invalid session not resumable" `Quick
      test_handle_invalid_session_not_resumable;
    Alcotest.test_case "handle heartbeat request" `Quick
      test_handle_heartbeat_request;
    Alcotest.test_case "handle malformed json" `Quick test_handle_malformed_json;
    Alcotest.test_case "seq updates" `Quick test_seq_updates;
    Alcotest.test_case "handle reconnect" `Quick test_handle_reconnect;
    Alcotest.test_case "handle unknown opcode" `Quick test_handle_unknown_opcode;
    Alcotest.test_case "hello with session triggers resume" `Quick
      test_hello_with_existing_session_triggers_resume;
    Alcotest.test_case "dispatch no seq field" `Quick test_dispatch_no_seq_field;
    Alcotest.test_case "hello integer heartbeat interval" `Quick
      test_hello_integer_heartbeat_interval;
    Alcotest.test_case "accessor functions" `Quick test_accessor_functions;
  ]
