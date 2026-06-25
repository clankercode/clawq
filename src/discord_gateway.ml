let invalid_session_backoff_s () = 1.0 +. Random.float 4.0

type t = {
  mutable ws : Ws_client.t option;
  mutable session_id : string option;
  mutable resume_gateway_url : string option;
  mutable seq : int option;
  mutable heartbeat_interval : float;
  mutable heartbeat_ack_received : bool;
  mutable heartbeat_stop : unit Lwt.u option;
  bot_token : string;
  intents : int;
  on_dispatch : string -> Yojson.Safe.t -> unit Lwt.t;
  on_close : int option -> unit Lwt.t;
  backoff_s : unit -> float;
}

let send_json t json =
  match t.ws with
  | None -> Lwt.return_unit
  | Some ws -> Ws_client.send_text ws (Yojson.Safe.to_string json)

let send_heartbeat t =
  let d = match t.seq with Some s -> `Int s | None -> `Null in
  send_json t (`Assoc [ ("op", `Int 1); ("d", d) ])

let send_identify t =
  let payload =
    `Assoc
      [
        ("op", `Int 2);
        ( "d",
          `Assoc
            [
              ("token", `String t.bot_token);
              ("intents", `Int t.intents);
              ( "properties",
                `Assoc
                  [
                    ("os", `String "linux");
                    ("browser", `String "clawq");
                    ("device", `String "clawq");
                  ] );
            ] );
      ]
  in
  send_json t payload

let send_resume t =
  match t.session_id with
  | None -> send_identify t
  | Some sid ->
      let payload =
        `Assoc
          [
            ("op", `Int 6);
            ( "d",
              `Assoc
                [
                  ("token", `String t.bot_token);
                  ("session_id", `String sid);
                  ("seq", match t.seq with Some s -> `Int s | None -> `Null);
                ] );
          ]
      in
      send_json t payload

let start_heartbeat t =
  (match t.heartbeat_stop with
  | Some u ->
      Lwt.wakeup_later u ();
      t.heartbeat_stop <- None
  | None -> ());
  let stop_p, stop_u = Lwt.wait () in
  t.heartbeat_stop <- Some stop_u;
  Lwt.async (fun () ->
      let open Lwt.Syntax in
      let jitter = t.heartbeat_interval *. Random.float 1.0 in
      let* () = Lwt.pick [ Lwt_unix.sleep (jitter /. 1000.0); stop_p ] in
      if not (Lwt.is_sleeping stop_p) then Lwt.return_unit
      else
        let rec loop () =
          t.heartbeat_ack_received <- false;
          let* () = send_heartbeat t in
          let* () =
            Lwt.pick [ Lwt_unix.sleep (t.heartbeat_interval /. 1000.0); stop_p ]
          in
          if not (Lwt.is_sleeping stop_p) then Lwt.return_unit
          else if not t.heartbeat_ack_received then begin
            Logs.warn (fun m ->
                m "Discord: heartbeat ACK not received, zombie connection");
            (match t.ws with Some ws -> Ws_client.close ws | None -> ());
            Lwt.return_unit
          end
          else loop ()
        in
        loop ())

let handle_gateway_message t msg =
  let open Lwt.Syntax in
  match Yojson.Safe.from_string msg with
  | exception _ ->
      Logs.warn (fun m -> m "Discord: failed to parse gateway message");
      Lwt.return_unit
  | json -> (
      let open Yojson.Safe.Util in
      let op = try json |> member "op" |> to_int with _ -> -1 in
      match op with
      | 10 ->
          (* Hello *)
          let d = json |> member "d" in
          let interval =
            try d |> member "heartbeat_interval" |> to_float
            with _ -> (
              try d |> member "heartbeat_interval" |> to_int |> float_of_int
              with _ -> 41250.0)
          in
          t.heartbeat_interval <- interval;
          t.heartbeat_ack_received <- true;
          start_heartbeat t;
          Logs.info (fun m ->
              m "Discord: Hello received, heartbeat_interval=%.0fms" interval);
          if t.session_id <> None then send_resume t else send_identify t
      | 11 ->
          (* Heartbeat ACK *)
          t.heartbeat_ack_received <- true;
          Lwt.return_unit
      | 0 ->
          (* Dispatch *)
          let s = try Some (json |> member "s" |> to_int) with _ -> None in
          (match s with Some _ -> t.seq <- s | None -> ());
          let event_name = try json |> member "t" |> to_string with _ -> "" in
          let d = json |> member "d" in
          if event_name = "READY" then begin
            let sid =
              try Some (d |> member "session_id" |> to_string) with _ -> None
            in
            let rurl =
              try Some (d |> member "resume_gateway_url" |> to_string)
              with _ -> None
            in
            t.session_id <- sid;
            t.resume_gateway_url <- rurl;
            Logs.info (fun m ->
                m "Discord: READY, session_id=%s"
                  (Option.value sid ~default:"(none)"))
          end;
          t.on_dispatch event_name d
      | 1 ->
          (* Heartbeat request *)
          send_heartbeat t
      | 7 ->
          (* Reconnect *)
          Logs.info (fun m -> m "Discord: Reconnect requested by server");
          (match t.ws with Some ws -> Ws_client.close ws | None -> ());
          Lwt.return_unit
      | 9 ->
          (* Invalid Session *)
          let resumable = try json |> member "d" |> to_bool with _ -> false in
          if resumable then begin
            Logs.info (fun m ->
                m "Discord: Invalid Session (resumable), waiting before resume");
            let* () = Lwt_unix.sleep (t.backoff_s ()) in
            send_resume t
          end
          else begin
            Logs.info (fun m ->
                m "Discord: Invalid Session (not resumable), clearing state");
            t.session_id <- None;
            t.seq <- None;
            (* Close websocket and let the reconnect loop handle identification *)
            (match t.ws with
            | Some ws -> Ws_client.close ws
            | None -> ());
            Lwt.return_unit
          end
      | _ ->
          Logs.debug (fun m -> m "Discord: unhandled gateway opcode %d" op);
          Lwt.return_unit)

let get_gateway_url ~bot_token =
  let open Lwt.Syntax in
  let uri = "https://discord.com/api/v10/gateway/bot" in
  let headers = [ ("Authorization", "Bot " ^ bot_token) ] in
  let* status, body = Http_client.get ~uri ~headers in
  if status >= 200 && status < 300 then
    try
      let json = Yojson.Safe.from_string body in
      let url = Yojson.Safe.Util.(json |> member "url" |> to_string) in
      Lwt.return (url ^ "?v=10&encoding=json")
    with _ -> Lwt.fail_with "Discord: failed to parse gateway/bot response"
  else
    Lwt.fail_with
      (Printf.sprintf "Discord: gateway/bot returned HTTP %d: %s" status body)

let connect ~bot_token ~intents ?resume_session_id ?resume_seq ?resume_url
    ?(backoff_s = invalid_session_backoff_s) ~on_dispatch ~on_close () =
  let open Lwt.Syntax in
  let t =
    {
      ws = None;
      session_id = resume_session_id;
      resume_gateway_url = resume_url;
      seq = resume_seq;
      heartbeat_interval = 41250.0;
      heartbeat_ack_received = true;
      heartbeat_stop = None;
      bot_token;
      intents;
      on_dispatch;
      on_close;
      backoff_s;
    }
  in
  let* url =
    match resume_url with
    | Some u -> Lwt.return (u ^ "?v=10&encoding=json")
    | None -> get_gateway_url ~bot_token
  in
  Logs.info (fun m ->
      m "Discord: connecting to gateway %s"
        (if String.length url > 50 then String.sub url 0 50 ^ "..." else url));
  let* ws = Ws_client.connect_wss ~uri:url () in
  t.ws <- Some ws;
  Ws_client.on_message ws (fun msg -> handle_gateway_message t msg);
  Lwt.async (fun () ->
      let* () = Ws_client.closed ws in
      (match t.heartbeat_stop with
      | Some u ->
          Lwt.wakeup_later u ();
          t.heartbeat_stop <- None
      | None -> ());
      t.on_close (Ws_client.close_code ws));
  Lwt.return t

let session_id t = t.session_id
let last_seq t = t.seq
let resume_url t = t.resume_gateway_url

let disconnect t =
  (match t.heartbeat_stop with
  | Some u ->
      Lwt.wakeup_later u ();
      t.heartbeat_stop <- None
  | None -> ());
  match t.ws with Some ws -> Ws_client.close ws | None -> ()
