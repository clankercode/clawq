let test_parse_envelope_valid () =
  let msg =
    {|{"envelope_id":"env123","type":"events_api","payload":{"event":{"type":"message","channel":"C1","user":"U1","text":"hi"}}}|}
  in
  match Slack_socket.parse_envelope msg with
  | None -> Alcotest.fail "expected Some envelope"
  | Some env ->
      Alcotest.(check string) "envelope_id" "env123" env.envelope_id;
      Alcotest.(check string) "payload_type" "events_api" env.payload_type;
      Alcotest.(check bool) "payload not null" true (env.payload <> `Null)

let test_parse_envelope_disconnect () =
  let msg = {|{"envelope_id":"env456","type":"disconnect","payload":{}}|} in
  match Slack_socket.parse_envelope msg with
  | None -> Alcotest.fail "expected Some envelope"
  | Some env ->
      Alcotest.(check string) "envelope_id" "env456" env.envelope_id;
      Alcotest.(check string) "payload_type" "disconnect" env.payload_type

let test_parse_envelope_no_id () =
  let msg = {|{"type":"events_api","payload":{}}|} in
  Alcotest.(check bool)
    "no envelope_id returns None" true
    (Slack_socket.parse_envelope msg = None)

let test_parse_envelope_invalid_json () =
  Alcotest.(check bool)
    "invalid json returns None" true
    (Slack_socket.parse_envelope "not json" = None)

let test_extract_event_body () =
  let payload =
    Yojson.Safe.from_string
      {|{"event":{"type":"message","channel":"C1","user":"U1","text":"hello"}}|}
  in
  let body = Slack_socket.extract_event_body payload in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "type" "event_callback"
    (json |> member "type" |> to_string);
  let event = json |> member "event" in
  Alcotest.(check string)
    "event type" "message"
    (event |> member "type" |> to_string);
  Alcotest.(check string) "channel" "C1" (event |> member "channel" |> to_string);
  Alcotest.(check string) "text" "hello" (event |> member "text" |> to_string)

let test_extract_event_body_reusable_by_slack () =
  (* Verify extracted body can be parsed by Slack.parse_event *)
  let payload =
    Yojson.Safe.from_string
      {|{"event":{"type":"message","channel":"C123","user":"U456","text":"world"}}|}
  in
  let body = Slack_socket.extract_event_body payload in
  match Slack.parse_event body with
  | Some (Slack.Message { channel_id; user_id; text; bot_id }) ->
      Alcotest.(check string) "channel_id" "C123" channel_id;
      Alcotest.(check string) "user_id" "U456" user_id;
      Alcotest.(check string) "text" "world" text;
      Alcotest.(check (option string)) "no bot_id" None bot_id
  | _ -> Alcotest.fail "expected Slack.Message"

let test_parse_envelope_empty_string_id () =
  let msg = {|{"envelope_id":"","type":"events_api","payload":{}}|} in
  Alcotest.(check bool)
    "empty envelope_id returns None" true
    (Slack_socket.parse_envelope msg = None)

let test_extract_event_body_non_object_payload () =
  (* When payload is not an Assoc, member raises Type_error and fallback fires *)
  let payload = `String "raw-text" in
  let body = Slack_socket.extract_event_body payload in
  Alcotest.(check string)
    "fallback returns raw payload string" {|"raw-text"|} body

let test_parse_envelope_empty_payload () =
  let msg = {|{"envelope_id":"env789","type":"hello"}|} in
  match Slack_socket.parse_envelope msg with
  | None -> Alcotest.fail "expected Some envelope"
  | Some env ->
      Alcotest.(check string) "envelope_id" "env789" env.envelope_id;
      Alcotest.(check string) "payload_type" "hello" env.payload_type

let test_disconnect_without_envelope_id () =
  List.iter
    (fun reason ->
      let msg = Printf.sprintf {|{"type":"disconnect","reason":"%s"}|} reason in
      match Slack_socket.parse_envelope msg with
      | None -> Alcotest.fail "documented disconnect control was ignored"
      | Some env ->
          Alcotest.(check string) "control type" "disconnect" env.payload_type)
    [ "warning"; "refresh_requested"; "link_disabled" ]

let test_extract_event_body_preserves_event_id () =
  let payload =
    Yojson.Safe.from_string
      {|{"type":"event_callback","event_id":"Ev1","event":{"type":"message","text":"hello"}}|}
  in
  Alcotest.(check string)
    "preserve Slack delivery identity"
    (Yojson.Safe.to_string payload)
    (Slack_socket.extract_event_body payload)

(* Timers advance only when explicitly fired; every cancelled timer remains
   observable so these tests also detect leaked retry/heartbeat work. *)
type timer = { delay : float; promise : unit Lwt.t; wake : unit Lwt.u }
type clock = { mutable timers : timer list; mutable time : float }

let make_clock () = { timers = []; time = Unix.gettimeofday () }

let sleep clock delay =
  let promise, wake = Lwt.task () in
  clock.timers <- clock.timers @ [ { delay; promise; wake } ];
  promise

let rec settle n =
  let open Lwt.Syntax in
  if n = 0 then Lwt.return_unit
  else
    let* () = Lwt.pause () in
    settle (n - 1)

let fire clock delay =
  match
    List.find_opt
      (fun t -> t.delay = delay && Lwt.is_sleeping t.promise)
      clock.timers
  with
  | None -> Alcotest.failf "no pending %.0fs timer" delay
  | Some t ->
      clock.time <- clock.time +. delay;
      Lwt.wakeup_later t.wake ()

type socket = {
  transport : Slack_socket.transport;
  emit : string -> unit Lwt.t;
  drop : unit -> unit;
  acks : string list ref;
  closes : int ref;
  pings : int ref;
  fail_ack : bool ref;
  ack_pending : unit Lwt.t option ref;
}

let make_socket ?(half_open = false) () =
  let callback = ref (fun _ -> Lwt.return_unit) in
  let closed, closed_u = Lwt.task () in
  let acks = ref [] and closes = ref 0 and pings = ref 0 in
  let fail_ack = ref false in
  let ack_pending = ref None in
  let drop () = if Lwt.is_sleeping closed then Lwt.wakeup_later closed_u () in
  let transport : Slack_socket.transport =
    {
      on_message = (fun cb -> callback := cb);
      send_text =
        (fun text ->
          if !fail_ack then Lwt.fail_with "ack transport lost"
          else (
            acks := text :: !acks;
            Option.value !ack_pending ~default:Lwt.return_unit));
      ping =
        (fun () ->
          incr pings;
          if half_open then fst (Lwt.task ()) else Lwt.return_unit);
      closed;
      close =
        (fun () ->
          incr closes;
          drop ();
          Lwt.return_unit);
    }
  in
  {
    transport;
    emit = (fun msg -> !callback msg);
    drop;
    acks;
    closes;
    pings;
    fail_ack;
    ack_pending;
  }

let hello socket =
  let open Lwt.Syntax in
  let* () = socket.emit {|{"type":"hello"}|} in
  settle 20

let event ?(event_id = "Ev-recovered") envelope_id =
  Printf.sprintf
    {|{"type":"events_api","envelope_id":"%s","payload":{"type":"event_callback","event_id":"%s","event":{"type":"message","channel":"C1","user":"U1","text":"after recovery"}}}|}
    envelope_id event_id

let with_connector ?handle_event sockets f =
  Test_helpers.with_temp_home (fun _ ->
      Test_helpers.with_memory_store (fun db ->
          ignore (Dot_dir.ensure ());
          Yojson.Safe.to_file (Dot_dir.config_path ())
            (`Assoc
               [
                 ( "channels",
                   `Assoc
                     [
                       ( "slack",
                         `Assoc
                           [
                             ("bot_token", `String "xoxb-unused-fixture");
                             ("signing_secret", `String "unused");
                             ("app_token", `String "xapp-unused-fixture");
                             ("socket_mode", `Bool true);
                           ] );
                     ] );
               ]);
          let clock = make_clock () in
          let connections = ref 0 and healths = ref [] and delivered = ref [] in
          let connect () =
            incr connections;
            match List.nth_opt sockets (!connections - 1) with
            | Some socket -> Lwt.return socket.transport
            | None -> fst (Lwt.task ())
          in
          let sc : Runtime_config.slack_config =
            {
              bot_token = "xoxb-unused-fixture";
              signing_secret = "unused";
              events_path = "/unused";
              allow_channels = [ "*" ];
              allow_users = [ "*" ];
              allow_private_channels = [];
              private_channel_policy = Pc_deny;
              app_token = "xapp-unused-fixture";
              socket_mode = true;
              default_model = None;
              event_log = Slack_config_types.default_event_log;
            }
          in
          let config =
            {
              Runtime_config.default with
              channels =
                { Runtime_config.default.channels with slack = Some sc };
            }
          in
          let on_health h =
            Slack_socket.publish_health h;
            healths := h :: !healths
          in
          let handle_event =
            Option.value handle_event ~default:(fun body ->
                (match Slack.parse_event body with
                | Some (Slack.Message { text; _ }) ->
                    delivered := text :: !delivered
                | _ -> Alcotest.fail "event did not reach Slack parser");
                Lwt.return_unit)
          in
          let session_manager = Session.create ~config ~db () in
          let event_limiter =
            Rate_limiter.create ~rate_per_minute:100 ~burst_multiplier:1.0
          in
          let run =
            Slack_socket.start_with ~connect ~handle_event ~on_health
              ~sleep:(sleep clock)
              ~now:(fun () -> clock.time)
              ~connect_timeout:7.0 ~ping_interval:5.0 ~pong_timeout:2.0 ~config
              ~session_manager ~event_limiter ()
          in
          Lwt_main.run
            (Lwt.finalize
               (fun () -> f clock connections healths delivered run)
               (fun () ->
                 Lwt.cancel run;
                 settle 20));
          Alcotest.(check bool)
            "all timers cleaned up" true
            (List.for_all
               (fun t -> not (Lwt.is_sleeping t.promise))
               clock.timers)))

let has_state state healths =
  List.exists (fun (h : Slack_socket.health) -> h.state = state) !healths

let assert_status text =
  Alcotest.(check bool)
    ("status exposes " ^ text) true
    (Test_helpers.string_contains (Command_bridge.handle [ "status" ]) text)

let test_control_disconnect_recovers () =
  List.iter
    (fun reason ->
      let first = make_socket () and second = make_socket () in
      with_connector [ first; second ]
        (fun clock connections healths delivered _ ->
          let open Lwt.Syntax in
          let* () = hello first in
          let* () =
            first.emit
              (Printf.sprintf {|{"type":"disconnect","reason":"%s"}|} reason)
          in
          let* () = settle 20 in
          assert_status "reconnecting";
          Alcotest.(check bool)
            "reason observable" true
            (Test_helpers.string_contains (Slack_socket.status_line ()) reason);
          Alcotest.(check int) "old transport closed" 1 !(first.closes);
          Alcotest.(check int)
            "control not acknowledged" 0
            (List.length !(first.acks));
          fire clock 1.0;
          let* () = settle 20 in
          let* () = hello second in
          let* () = second.emit (event "new-envelope") in
          let* () = settle 20 in
          Alcotest.(check int) "reconnected" 2 !connections;
          Alcotest.(check (list string))
            "message after recovery" [ "after recovery" ] !delivered;
          Alcotest.(check bool)
            "health transitioned" true
            (has_state Slack_socket.Connected healths);
          assert_status "connected";
          Lwt.return_unit))
    [ "warning"; "refresh_requested"; "link_disabled" ]

let test_half_open_recovers () =
  let first = make_socket ~half_open:true () and second = make_socket () in
  with_connector [ first; second ] (fun clock connections _ delivered _ ->
      let open Lwt.Syntax in
      let* () = hello first in
      fire clock 5.0;
      let* () = settle 20 in
      Alcotest.(check int) "transport probed" 1 !(first.pings);
      fire clock 2.0;
      let* () = settle 20 in
      assert_status "pong timeout";
      Alcotest.(check int) "half-open transport closed" 1 !(first.closes);
      fire clock 1.0;
      let* () = settle 20 in
      let* () = hello second in
      let* () = second.emit (event "restored") in
      let* () = settle 20 in
      Alcotest.(check int) "automatic recovery" 2 !connections;
      Alcotest.(check int) "inbound resumed" 1 (List.length !delivered);
      Lwt.return_unit)

let test_transport_drop_deduplicates_across_reconnect () =
  let first = make_socket () and second = make_socket () in
  with_connector [ first; second ] (fun clock _ _ delivered _ ->
      let open Lwt.Syntax in
      let* () = hello first in
      let* () = first.emit (event "old-envelope") in
      first.drop ();
      let* () = settle 20 in
      assert_status "reconnecting";
      fire clock 1.0;
      let* () = settle 20 in
      let* () = hello second in
      let* () = second.emit (event "retry-envelope") in
      let* () = second.emit (event ~event_id:"Ev-next" "next-envelope") in
      let* () = settle 20 in
      Alcotest.(check int)
        "duplicate acknowledged" 2
        (List.length !(second.acks));
      Alcotest.(check int) "duplicate not dispatched" 2 (List.length !delivered);
      Lwt.return_unit)

let test_connect_deadline_and_cancel_retry () =
  with_connector [] (fun clock connections _ _ run ->
      let open Lwt.Syntax in
      fire clock 7.0;
      let* () = settle 20 in
      assert_status "connection timeout";
      Lwt.cancel run;
      let* () = settle 20 in
      Alcotest.(check int) "cancel stops reconnect" 1 !connections;
      assert_status "stopped";
      Lwt.return_unit)

let test_backoff_is_bounded () =
  with_connector [] (fun clock connections _ _ _ ->
      let open Lwt.Syntax in
      let rec retry = function
        | [] -> Lwt.return_unit
        | delay :: rest ->
            fire clock 7.0;
            let* () = settle 20 in
            fire clock delay;
            let* () = settle 20 in
            retry rest
      in
      let* () = retry [ 1.; 2.; 4.; 8.; 16.; 30.; 30. ] in
      Alcotest.(check int) "bounded retries continue" 8 !connections;
      Lwt.return_unit)

let test_cancel_active_connection () =
  let socket = make_socket () in
  with_connector [ socket ] (fun _ connections _ _ run ->
      let open Lwt.Syntax in
      let* () = hello socket in
      Lwt.cancel run;
      let* () = settle 20 in
      Alcotest.(check int) "active transport closed" 1 !(socket.closes);
      Alcotest.(check int) "no retry on cancel" 1 !connections;
      assert_status "stopped";
      Lwt.return_unit)

let test_inflight_delivery_survives_reconnect () =
  let first = make_socket () and second = make_socket () in
  let completed = ref 0 and accepted = ref 0 in
  let pending, wake = Lwt.task () in
  let handle_event _ =
    let open Lwt.Syntax in
    incr accepted;
    let* () = pending in
    incr completed;
    Lwt.return_unit
  in
  with_connector ~handle_event [ first; second ] (fun clock _ _ _ _ ->
      let open Lwt.Syntax in
      let* () = hello first in
      let* () = first.emit (event "accepted") in
      first.drop ();
      let* () = settle 20 in
      fire clock 1.;
      let* () = settle 20 in
      let* () = hello second in
      let* () = second.emit (event "retry-inflight") in
      Lwt.wakeup_later wake ();
      let* () = settle 20 in
      Alcotest.(check int) "accepted once" 1 !accepted;
      Alcotest.(check int) "accepted event completes" 1 !completed;
      Lwt.return_unit)

let test_failed_ack_allows_retry () =
  let first = make_socket () and second = make_socket () in
  first.fail_ack := true;
  with_connector [ first; second ] (fun clock _ _ delivered _ ->
      let open Lwt.Syntax in
      let* () = hello first in
      let* () = first.emit (event "unacked") in
      let* () = settle 20 in
      Alcotest.(check int)
        "unacked event not accepted" 0 (List.length !delivered);
      fire clock 1.;
      let* () = settle 20 in
      let* () = hello second in
      let* () = second.emit (event "retry-unacked") in
      let* () = settle 20 in
      Alcotest.(check int) "retry accepted" 1 (List.length !delivered);
      Lwt.return_unit)

let test_cancel_cleans_pending_ack () =
  let socket = make_socket () in
  let pending, _ = Lwt.task () in
  socket.ack_pending := Some pending;
  with_connector [ socket ] (fun _ _ _ _ run ->
      let open Lwt.Syntax in
      let* () = hello socket in
      let ack = socket.emit (event "pending-ack") in
      Lwt.cancel run;
      let* () = settle 20 in
      Alcotest.(check bool) "pending ack cancelled" false (Lwt.is_sleeping ack);
      Alcotest.(check bool) "send cancelled" false (Lwt.is_sleeping pending);
      Lwt.return_unit)

let test_hello_deadline () =
  let socket = make_socket () in
  with_connector [ socket ] (fun clock _ _ _ _ ->
      let open Lwt.Syntax in
      let* () = settle 20 in
      fire clock 7.;
      let* () = settle 20 in
      assert_status "hello timeout";
      Alcotest.(check int) "unready transport closed" 1 !(socket.closes);
      Lwt.return_unit)

(* These loopback peers exercise the real WebSocket protocol and descriptor
   ownership. They never contact Slack and use only explicitly fired timers. *)
let with_ws_peer f =
  Lwt_main.run
    (let open Lwt.Syntax in
     let listener = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
     let peer_channels = ref None in
     let* () =
       Lwt_unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0))
     in
     Lwt_unix.listen listener 1;
     let port =
       match Lwt_unix.getsockname listener with
       | Unix.ADDR_INET (_, port) -> port
       | _ -> assert false
     in
     let accept =
       let* fd, _ = Lwt_unix.accept listener in
       let input = Lwt_io.of_fd ~mode:Lwt_io.input fd in
       let output = Lwt_io.of_fd ~mode:Lwt_io.output fd in
       peer_channels := Some (input, output);
       Lwt.return (input, output)
     in
     Lwt.finalize
       (fun () -> f (Printf.sprintf "ws://127.0.0.1:%d/socket" port) accept)
       (fun () ->
         Lwt.cancel accept;
         let* () =
           match !peer_channels with
           | Some (input, output) ->
               let* () = Ws_client.quiet (fun () -> Lwt_io.abort input) in
               Ws_client.quiet (fun () -> Lwt_io.abort output)
           | None -> Lwt.return_unit
         in
         let* () = Lwt_unix.close listener in
         settle 20))

let handshake input output =
  let open Lwt.Syntax in
  let rec headers key =
    let* line = Lwt_io.read_line input in
    if String.trim line = "" then Lwt.return key
    else
      let key =
        if
          String.starts_with ~prefix:"sec-websocket-key:"
            (String.lowercase_ascii line)
        then String.sub line 18 (String.length line - 18) |> String.trim
        else key
      in
      headers key
  in
  let* key = headers "" in
  let accept =
    Digestif.SHA1.(
      to_raw_string
        (digest_string (key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))
    |> Base64.encode_exn
  in
  let* () =
    Lwt_io.write output
      ("HTTP/1.1 101 Switching Protocols\r\n"
     ^ "Upgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: "
     ^ accept ^ "\r\n\r\n")
  in
  Lwt_io.flush output

let write_frame output opcode payload =
  let open Lwt.Syntax in
  let header =
    String.init 2 (function
      | 0 -> Char.chr (0x80 lor opcode)
      | _ -> Char.chr (String.length payload))
  in
  let* () = Lwt_io.write output (header ^ payload) in
  Lwt_io.flush output

let read_frame input =
  let open Lwt.Syntax in
  let* a = Lwt_io.read_char input in
  let* b = Lwt_io.read_char input in
  let len = Char.code b land 127 in
  let* mask = Lwt_io.read ~count:4 input in
  let* payload = Lwt_io.read ~count:len input in
  let decoded =
    String.mapi
      (fun i c -> Char.chr (Char.code c lxor Char.code mask.[i mod 4]))
      payload
  in
  Lwt.return (Char.code a land 15, decoded)

let test_ws_ping_requires_matching_pong () =
  with_ws_peer (fun uri accept ->
      let open Lwt.Syntax in
      let connect = Ws_client.connect_ws ~uri () in
      let* input, output = accept in
      let* () = handshake input output in
      let* ws = connect in
      Lwt.finalize
        (fun () ->
          let ping = Ws_client.ping ws in
          let* opcode, payload = read_frame input in
          Alcotest.(check int) "real ping frame" 9 opcode;
          let* () = write_frame output 10 "unsolicited" in
          let* () = settle 20 in
          Alcotest.(check bool)
            "unmatched pong does not prove health" true (Lwt.is_sleeping ping);
          let* () = write_frame output 10 payload in
          let* () = ping in
          let* () = write_frame output 9 "server-probe" in
          let* opcode, echoed = read_frame input in
          Alcotest.(check int) "server ping answered" 10 opcode;
          Alcotest.(check string) "pong echoes payload" "server-probe" echoed;
          Lwt.return_unit)
        (fun () -> Ws_client.shutdown ws))

let test_ws_handshake_cleanup cancel =
  let clock = make_clock () in
  with_ws_peer (fun uri accept ->
      let open Lwt.Syntax in
      let connect =
        Ws_client.connect_ws ~sleep:(sleep clock) ~timeout:7. ~uri ()
      in
      let* input, _ = accept in
      let* _ = Lwt_io.read_line input in
      if cancel then Lwt.cancel connect else fire clock 7.;
      let* failed =
        Lwt.catch
          (fun () ->
            let* _ = connect in
            Lwt.return false)
          (fun _ -> Lwt.return true)
      in
      Alcotest.(check bool) "handshake terminated" true failed;
      let rec drain () =
        let* text = Lwt_io.read ~count:1024 input in
        if text = "" then Lwt.return_unit else drain ()
      in
      let* () = drain () in
      Alcotest.(check bool)
        "connect timer cleaned up" true
        (List.for_all (fun t -> not (Lwt.is_sleeping t.promise)) clock.timers);
      Lwt.return_unit)

let test_ws_buffers_early_hello () =
  with_ws_peer (fun uri accept ->
      let open Lwt.Syntax in
      let connect = Ws_client.connect_ws ~uri () in
      let* input, output = accept in
      let* () = handshake input output in
      let* () = write_frame output 1 {|{"type":"hello"}|} in
      let* () = write_frame output 9 "delivery-barrier" in
      let* ws = connect in
      Lwt.finalize
        (fun () ->
          let* opcode, _ = read_frame input in
          Alcotest.(check int) "early frame processing completed" 10 opcode;
          let messages = ref [] in
          Ws_client.on_message ws (fun msg ->
              messages := msg :: !messages;
              Lwt.return_unit);
          let* () = settle 20 in
          Alcotest.(check (list string))
            "early Slack hello retained" [ {|{"type":"hello"}|} ] !messages;
          Lwt.return_unit)
        (fun () -> Ws_client.shutdown ws))

let test_status_rejects_stale_health () =
  let socket = make_socket () in
  with_connector [ socket ] (fun _ _ _ _ _ ->
      let open Lwt.Syntax in
      let* () = hello socket in
      assert_status "connected";
      let replace name value =
        let path = Slack_socket.health_path () in
        let fields = Yojson.Safe.Util.to_assoc (Yojson.Safe.from_file path) in
        Yojson.Safe.to_file path
          (`Assoc ((name, value) :: List.remove_assoc name fields))
      in
      replace "updated_at" (`Float (Unix.gettimeofday () -. 100.));
      assert_status "health is stale";
      replace "pid" (`Int (-1));
      assert_status "connector process exited";
      let path = Slack_socket.health_path () in
      Unix.rename path (path ^ ".saved");
      assert_status "unknown";
      Lwt.return_unit)

let test_cancel_during_connect () =
  with_connector [] (fun _ connections _ _ run ->
      let open Lwt.Syntax in
      Lwt.cancel run;
      let* () = settle 20 in
      Alcotest.(check int) "connect cancelled without retry" 1 !connections;
      assert_status "stopped";
      Lwt.return_unit)

let test_failed_handler_is_not_dispatched_twice () =
  let first = make_socket () and second = make_socket () in
  let attempts = ref 0 in
  let handle_event _ =
    incr attempts;
    Lwt.fail_with "failed after acceptance"
  in
  with_connector ~handle_event [ first; second ] (fun clock _ _ _ _ ->
      let open Lwt.Syntax in
      let* () = hello first in
      let* () = first.emit (event "accepted-once") in
      first.drop ();
      let* () = settle 20 in
      fire clock 1.;
      let* () = settle 20 in
      let* () = hello second in
      let* () = second.emit (event "retry-failed-handler") in
      let* () = settle 20 in
      Alcotest.(check int) "accepted event dispatched once" 1 !attempts;
      Lwt.return_unit)

let check_terminated clock connecting =
  Alcotest.(check bool)
    "public attempt ended" true
    (match Lwt.state connecting with Lwt.Fail _ -> true | _ -> false);
  Alcotest.(check bool)
    "deadline cancelled" true
    (List.for_all (fun t -> not (Lwt.is_sleeping t.promise)) clock.timers)

let end_attempt clock timeout connecting =
  let open Lwt.Syntax in
  if timeout then fire clock 7. else Lwt.cancel connecting;
  Lwt.catch
    (fun () ->
      let* _ = connecting in
      Lwt.return_unit)
    (fun _ -> Lwt.return_unit)

let test_ws_late_dns timeout =
  with_ws_peer (fun uri _ ->
      let open Lwt.Syntax in
      let clock = make_clock () in
      let lookup, resolve = Lwt.wait () in
      let allocations = ref [] and runtimes = ref 0 in
      let socket family kind protocol =
        let fd = Lwt_unix.socket family kind protocol in
        allocations := fd :: !allocations;
        fd
      in
      let do_connect ~is_active:_ ~nonce:_ ~host:_ ~port:_ ~resource:_
          ~error_handler:_ ~websocket_handler:_ _ =
        incr runtimes;
        fst (Lwt.task ())
      in
      let connecting =
        Ws_client.connect_common ~default_port:80
          ~getaddrinfo:(fun _ _ _ -> lookup)
          ~socket ~do_connect ~sleep:(sleep clock) ~timeout:7. ~uri ()
      in
      Lwt.finalize
        (fun () ->
          let* () = end_attempt clock timeout connecting in
          check_terminated clock connecting;
          Alcotest.(check bool)
            "DNS really is uncancellable" true (Lwt.is_sleeping lookup);
          let port = Option.get (Uri.port (Uri.of_string uri)) in
          let addr : Unix.addr_info =
            {
              ai_family = Unix.PF_INET;
              ai_socktype = Unix.SOCK_STREAM;
              ai_protocol = 0;
              ai_addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port);
              ai_canonname = "localhost";
            }
          in
          Lwt.wakeup_later resolve [ addr ];
          let* () = settle 20 in
          Alcotest.(check int)
            "no socket allocated after attempt ended" 0
            (List.length !allocations);
          Alcotest.(check int) "no late runtime started" 0 !runtimes;
          check_terminated clock connecting;
          Lwt.return_unit)
        (fun () ->
          Lwt.cancel connecting;
          Lwt_list.iter_s
            (fun fd -> Ws_client.quiet (fun () -> Lwt_unix.close fd))
            !allocations))

let test_ws_late_transport ?(tls = false) timeout =
  with_ws_peer (fun uri accept ->
      let open Lwt.Syntax in
      let clock = make_clock () in
      let late, deliver = Lwt.wait () in
      let entered, enter = Lwt.task () in
      let shutdowns = ref 0 and runtime_starts = ref 0 and fd_ref = ref None in
      let do_connect ~is_active ~nonce:_ ~host:_ ~port:_ ~resource:_
          ~error_handler:_ ~websocket_handler:_ fd =
        fd_ref := Some fd;
        Lwt.wakeup_later enter ();
        if tls then
          Ws_client.connect_tls_transport ~is_active
            ~handshake:(fun _ -> late)
            ~close_tls:(fun (tr : Ws_client.transport) -> tr.shutdown ())
            ~start:(fun tr ->
              incr runtime_starts;
              Lwt.return tr)
            fd
        else late
      in
      let connecting =
        Ws_client.connect_common ~default_port:80 ~do_connect
          ~sleep:(sleep clock) ~timeout:7. ~uri ()
      in
      let* input, _ = accept in
      let* () = entered in
      let* () = end_attempt clock timeout connecting in
      check_terminated clock connecting;
      let* eof = Lwt_io.read ~count:1 input in
      Alcotest.(check string)
        "owned descriptor closed before late result" "" eof;
      Lwt.wakeup_later deliver
        Ws_client.
          {
            keepalive = Any ();
            shutdown =
              (fun () ->
                incr shutdowns;
                Lwt.return_unit);
          };
      let* () = settle 20 in
      Alcotest.(check int) "late transport disposed exactly once" 1 !shutdowns;
      Alcotest.(check int) "late TLS does not start a runtime" 0 !runtime_starts;
      Alcotest.(check bool)
        "descriptor stays closed" true
        (Lwt_unix.state (Option.get !fd_ref) = Lwt_unix.Closed);
      Lwt.cancel connecting;
      check_terminated clock connecting;
      Lwt.return_unit)

let test_connector_cancel_during_uncancellable_dns () =
  let open Lwt.Syntax in
  let clock = make_clock () in
  let lookup, resolve = Lwt.wait () in
  let attempts = ref 0 and allocations = ref 0 in
  let socket _ _ _ =
    incr allocations;
    failwith "late socket allocation"
  in
  let do_connect ~is_active:_ ~nonce:_ ~host:_ ~port:_ ~resource:_
      ~error_handler:_ ~websocket_handler:_ _ =
    fst (Lwt.task ())
  in
  let connect () =
    incr attempts;
    let* _ =
      Ws_client.connect_common ~default_port:80
        ~getaddrinfo:(fun _ _ _ -> lookup)
        ~socket ~do_connect ~sleep:(sleep clock) ~timeout:7.
        ~uri:"ws://127.0.0.1:9/socket" ()
    in
    Lwt.fail_with "unexpected connection"
  in
  let run =
    Slack_socket.run ~connect ~sleep:(sleep clock)
      ~handle_event:(fun _ -> Lwt.return_unit)
      ~on_health:(fun _ -> ())
      ()
  in
  Lwt_main.run
    (Lwt.finalize
       (fun () ->
         Lwt.cancel run;
         let* () = settle 20 in
         check_terminated clock run;
         Alcotest.(check bool)
           "worker job remains pending" true (Lwt.is_sleeping lookup);
         Lwt.wakeup_later resolve
           [
             Unix.
               {
                 ai_family = PF_INET;
                 ai_socktype = SOCK_STREAM;
                 ai_protocol = 0;
                 ai_addr = ADDR_INET (inet_addr_loopback, 9);
                 ai_canonname = "localhost";
               };
           ];
         let* () = settle 20 in
         Alcotest.(check int) "connector did not retry" 1 !attempts;
         Alcotest.(check int) "abandoned DNS did not allocate" 0 !allocations;
         check_terminated clock run;
         Lwt.return_unit)
       (fun () ->
         Lwt.cancel run;
         settle 20))

let suite =
  [
    Alcotest.test_case "connector cancel prevents work after delayed DNS" `Quick
      test_connector_cancel_during_uncancellable_dns;
    Alcotest.test_case "cancel disposes late TLS without runtime startup" `Quick
      (fun () -> test_ws_late_transport ~tls:true false);
    Alcotest.test_case "deadline disposes late TLS without runtime startup"
      `Quick (fun () -> test_ws_late_transport ~tls:true true);
    Alcotest.test_case "cancel before DNS prevents late socket allocation"
      `Quick (fun () -> test_ws_late_dns false);
    Alcotest.test_case "deadline before DNS prevents late socket allocation"
      `Quick (fun () -> test_ws_late_dns true);
    Alcotest.test_case "cancel disposes uncancellable late transport" `Quick
      (fun () -> test_ws_late_transport false);
    Alcotest.test_case "deadline disposes uncancellable late transport" `Quick
      (fun () -> test_ws_late_transport true);
    Alcotest.test_case "handler failure does not duplicate accepted work" `Quick
      test_failed_handler_is_not_dispatched_twice;
    Alcotest.test_case "production status rejects stale or dead health" `Quick
      test_status_rejects_stale_health;
    Alcotest.test_case "cancel during connect stops retries and timers" `Quick
      test_cancel_during_connect;
    Alcotest.test_case "failed ack can retry after reconnect" `Quick
      test_failed_ack_allows_retry;
    Alcotest.test_case "cancel cleans pending ack and timers" `Quick
      test_cancel_cleans_pending_ack;
    Alcotest.test_case "hello deadline closes unready transport" `Quick
      test_hello_deadline;
    Alcotest.test_case "WebSocket requires matching pong and echoes ping" `Quick
      test_ws_ping_requires_matching_pong;
    Alcotest.test_case "WebSocket handshake deadline releases descriptor" `Quick
      (fun () -> test_ws_handshake_cleanup false);
    Alcotest.test_case "WebSocket handshake cancellation releases descriptor"
      `Quick (fun () -> test_ws_handshake_cleanup true);
    Alcotest.test_case "WebSocket retains hello before handler registration"
      `Quick test_ws_buffers_early_hello;
    Alcotest.test_case "control disconnect reconnects and resumes delivery"
      `Quick test_control_disconnect_recovers;
    Alcotest.test_case "half-open reconnects and resumes delivery" `Quick
      test_half_open_recovers;
    Alcotest.test_case "transport drop deduplicates across reconnect" `Quick
      test_transport_drop_deduplicates_across_reconnect;
    Alcotest.test_case "connect deadline and cancellation stop retries" `Quick
      test_connect_deadline_and_cancel_retry;
    Alcotest.test_case "retry backoff is bounded" `Quick test_backoff_is_bounded;
    Alcotest.test_case "cancellation closes active transport" `Quick
      test_cancel_active_connection;
    Alcotest.test_case "accepted delivery survives reconnect" `Quick
      test_inflight_delivery_survives_reconnect;
    Alcotest.test_case "disconnect control without envelope id" `Quick
      test_disconnect_without_envelope_id;
    Alcotest.test_case "event identity survives extraction" `Quick
      test_extract_event_body_preserves_event_id;
    Alcotest.test_case "parse envelope valid" `Quick test_parse_envelope_valid;
    Alcotest.test_case "parse envelope disconnect" `Quick
      test_parse_envelope_disconnect;
    Alcotest.test_case "parse envelope no id" `Quick test_parse_envelope_no_id;
    Alcotest.test_case "parse envelope invalid json" `Quick
      test_parse_envelope_invalid_json;
    Alcotest.test_case "extract event body" `Quick test_extract_event_body;
    Alcotest.test_case "extract event body reusable by slack" `Quick
      test_extract_event_body_reusable_by_slack;
    Alcotest.test_case "parse envelope empty string id" `Quick
      test_parse_envelope_empty_string_id;
    Alcotest.test_case "extract event body non-object payload" `Quick
      test_extract_event_body_non_object_payload;
    Alcotest.test_case "parse envelope empty payload" `Quick
      test_parse_envelope_empty_payload;
  ]
