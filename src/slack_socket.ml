let get_wss_url ~app_token =
  let open Lwt.Syntax in
  let uri = "https://slack.com/api/apps.connections.open" in
  let headers = [ ("Authorization", "Bearer " ^ app_token) ] in
  let* status, body = Http_client.post_json ~uri ~headers ~body:"" in
  if status >= 200 && status < 300 then
    try
      let json = Yojson.Safe.from_string body in
      let open Yojson.Safe.Util in
      let ok = try json |> member "ok" |> to_bool with _ -> false in
      if ok then
        let url = json |> member "url" |> to_string in
        Lwt.return url
      else
        let err =
          try json |> member "error" |> to_string with _ -> "unknown"
        in
        Lwt.fail_with ("Slack Socket Mode: apps.connections.open error: " ^ err)
    with exn ->
      Lwt.fail_with
        ("Slack Socket Mode: failed to parse response: "
       ^ Printexc.to_string exn)
  else
    Lwt.fail_with
      (Printf.sprintf "Slack Socket Mode: HTTP %d from apps.connections.open"
         status)

type envelope = {
  envelope_id : string;
  payload_type : string;
  payload : Yojson.Safe.t;
}

let parse_envelope msg =
  try
    let json = Yojson.Safe.from_string msg in
    let open Yojson.Safe.Util in
    let envelope_id =
      try json |> member "envelope_id" |> to_string with _ -> ""
    in
    let payload_type = try json |> member "type" |> to_string with _ -> "" in
    let payload = try json |> member "payload" with _ -> `Null in
    if envelope_id = "" then None
    else Some { envelope_id; payload_type; payload }
  with _ -> None

let extract_event_body (payload : Yojson.Safe.t) : string =
  let open Yojson.Safe.Util in
  try
    let event = payload |> member "event" in
    let body =
      `Assoc [ ("type", `String "event_callback"); ("event", event) ]
    in
    Yojson.Safe.to_string body
  with _ -> Yojson.Safe.to_string payload

let run_connection ~(config : Runtime_config.slack_config)
    ~(session_manager : Session.t) =
  let open Lwt.Syntax in
  let* url = get_wss_url ~app_token:config.app_token in
  Logs.info (fun m -> m "Slack Socket Mode: connecting to WSS");
  let* ws = Ws_client.connect_wss ~uri:url () in
  let done_p, done_u = Lwt.wait () in
  Ws_client.on_message ws (fun msg ->
      match parse_envelope msg with
      | None ->
          Logs.debug (fun m ->
              m "Slack Socket Mode: non-envelope message ignored");
          Lwt.return_unit
      | Some env -> (
          let ack = `Assoc [ ("envelope_id", `String env.envelope_id) ] in
          let* () = Ws_client.send_text ws (Yojson.Safe.to_string ack) in
          match env.payload_type with
          | "disconnect" ->
              Logs.info (fun m -> m "Slack Socket Mode: disconnect requested");
              Ws_client.close ws;
              if Lwt.is_sleeping done_p then Lwt.wakeup_later done_u ();
              Lwt.return_unit
          | "events_api" ->
              let body = extract_event_body env.payload in
              let* _resp = Slack.handle_event ~config ~session_manager body in
              Lwt.return_unit
          | other ->
              Logs.debug (fun m ->
                  m "Slack Socket Mode: unhandled type %s" other);
              Lwt.return_unit));
  let* () = Lwt.pick [ done_p; Ws_client.closed ws ] in
  Lwt.return_unit

let start ~config ~session_manager =
  match config.Runtime_config.channels.slack with
  | None -> Lwt.return_unit
  | Some sc when (not sc.socket_mode) || sc.app_token = "" -> Lwt.return_unit
  | Some sc ->
      Logs.info (fun m -> m "Slack Socket Mode starting");
      let open Lwt.Syntax in
      let backoff = ref 1.0 in
      let rec loop () =
        let result =
          Lwt.catch
            (fun () ->
              let* () = run_connection ~config:sc ~session_manager in
              backoff := 1.0;
              Lwt.return (Ok ()))
            (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
        in
        let* outcome = result in
        match outcome with
        | Ok () ->
            Logs.info (fun m ->
                m "Slack Socket Mode: connection closed, reconnecting in 1s");
            let* () = Lwt_unix.sleep 1.0 in
            loop ()
        | Error err ->
            Logs.err (fun m -> m "Slack Socket Mode error: %s" err);
            let delay = !backoff in
            backoff := Float.min (!backoff *. 2.0) 30.0;
            Logs.info (fun m ->
                m "Slack Socket Mode: reconnecting in %.0fs" delay);
            let* () = Lwt_unix.sleep delay in
            loop ()
      in
      loop ()
