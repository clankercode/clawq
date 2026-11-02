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
    let payload =
      if payload_type = "disconnect" then json else json |> member "payload"
    in
    if
      envelope_id = ""
      && payload_type <> "disconnect"
      && payload_type <> "hello"
    then None
    else Some { envelope_id; payload_type; payload }
  with _ -> None

let extract_event_body (payload : Yojson.Safe.t) : string =
  let open Yojson.Safe.Util in
  match payload with
  | `Assoc fields when List.mem_assoc "type" fields ->
      Yojson.Safe.to_string payload
  | _ -> (
      try
        let event = payload |> member "event" in
        Yojson.Safe.to_string
          (`Assoc [ ("type", `String "event_callback"); ("event", event) ])
      with _ -> Yojson.Safe.to_string payload)

type state = Connecting | Connected | Reconnecting | Stopped

type health = {
  state : state;
  detail : string;
  retry_in : float option;
  reconnects : int;
  updated_at : float;
}

let state_name = function
  | Connecting -> "connecting"
  | Connected -> "connected"
  | Reconnecting -> "reconnecting"
  | Stopped -> "stopped"

let health_path () = Dot_dir.sub "slack_socket_status.json"

let publish_health h =
  try
    ignore (Dot_dir.ensure ());
    let pid = Unix.getpid () in
    let json =
      `Assoc
        [
          ("state", `String (state_name h.state));
          ("detail", `String h.detail);
          ( "retry_in",
            match h.retry_in with Some n -> `Float n | None -> `Null );
          ("reconnects", `Int h.reconnects);
          ("updated_at", `Float h.updated_at);
          ("pid", `Int pid);
          ( "start_ticks",
            match Daemon_status.proc_start_ticks pid with
            | Some s -> `String s
            | None -> `Null );
        ]
    in
    let path = health_path () in
    let tmp = path ^ ".tmp." ^ string_of_int pid in
    let oc = open_out_bin tmp in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> output_string oc (Yojson.Safe.to_string json));
    Unix.rename tmp path
  with exn ->
    Logs.warn (fun m ->
        m "Slack Socket Mode: health write failed: %s" (Printexc.to_string exn))

let status_line () =
  try
    let json = Yojson.Safe.from_file (health_path ()) in
    let open Yojson.Safe.Util in
    let pid = json |> member "pid" |> to_int in
    let start_ticks =
      match json |> member "start_ticks" with
      | `String ticks -> Some ticks
      | _ -> None
    in
    let state = json |> member "state" |> to_string in
    let updated = json |> member "updated_at" |> to_number in
    let process_alive =
      match start_ticks with
      | Some ticks -> Daemon_status.proc_start_ticks pid = Some ticks
      | None -> (
          try
            Unix.kill pid 0;
            true
          with
          | Unix.Unix_error (Unix.EPERM, _, _) -> true
          | _ -> false)
    in
    if not process_alive then
      "Slack Socket Mode: disconnected (connector process exited)"
    else if state <> "stopped" && Unix.gettimeofday () -. updated > 90.0 then
      "Slack Socket Mode: disconnected (health is stale)"
    else
      Printf.sprintf "Slack Socket Mode: %s (%s; reconnects=%d%s)" state
        (json |> member "detail" |> to_string)
        (json |> member "reconnects" |> to_int)
        (match json |> member "retry_in" with
        | `Float delay -> Printf.sprintf "; retry in %.0fs" delay
        | _ -> "")
  with _ -> "Slack Socket Mode: unknown (no current connection health)"

type transport = {
  on_message : (string -> unit Lwt.t) -> unit;
  send_text : string -> unit Lwt.t;
  ping : unit -> unit Lwt.t;
  closed : unit Lwt.t;
  close : unit -> unit Lwt.t;
}

let connect_transport ~(config : Runtime_config.slack_config) () =
  let open Lwt.Syntax in
  let* url = get_wss_url ~app_token:config.app_token in
  let* ws = Ws_client.connect_wss ~uri:url () in
  Lwt.return
    {
      on_message = Ws_client.on_message ws;
      send_text = Ws_client.send_text ws;
      ping = (fun () -> Ws_client.ping ws);
      closed = Ws_client.closed ws;
      close = (fun () -> Ws_client.shutdown ws);
    }

let with_deadline ~sleep ~seconds ~label f =
  let open Lwt.Syntax in
  Lwt.pick
    [
      f ();
      (let* () = sleep seconds in
       Lwt.fail_with label);
    ]

(* Delivery tasks belong to the connector, not a single transport. An already
   acknowledged event must survive a routine socket refresh. *)
let run ?(sleep = Lwt_unix.sleep) ?(now = Unix.gettimeofday)
    ?(connect_timeout = 30.0) ?(ping_interval = 15.0) ?(pong_timeout = 10.0)
    ~connect ~handle_event ~on_health () =
  let open Lwt.Syntax in
  let dedup = Channel_util.Lru_dedup.create 4096 in
  let in_flight = Hashtbl.create 16 in
  let deliveries = ref [] in
  let reconnects = ref 0 in
  let last_state = ref None in
  let backoff = Channel_util.Backoff.create ~max_val:30.0 () in
  let update state detail retry_in =
    if !last_state <> Some state then begin
      Logs.info (fun m ->
          m "Slack Socket Mode: %s (%s)" (state_name state) detail);
      last_state := Some state
    end;
    on_health
      { state; detail; retry_in; reconnects = !reconnects; updated_at = now () }
  in
  let dispatch key body =
    (* Once acknowledged, a handler may already have made visible changes even
       if it later fails. Retried deliveries must not run those changes twice. *)
    Channel_util.Lru_dedup.mark dedup key;
    Hashtbl.replace in_flight key ();
    let task =
      Lwt.catch
        (fun () ->
          let* () = handle_event body in
          Lwt.return_unit)
        (function
          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
          | exn ->
              Logs.err (fun m ->
                  m "Slack Socket Mode: event handling failed: %s"
                    (Printexc.to_string exn));
              Lwt.return_unit)
    in
    deliveries := task :: !deliveries;
    Lwt.on_any task
      (fun () ->
        Hashtbl.remove in_flight key;
        deliveries := List.filter (fun p -> p != task) !deliveries)
      (fun _ ->
        Hashtbl.remove in_flight key;
        deliveries := List.filter (fun p -> p != task) !deliveries)
  in
  let run_connection () =
    let* ws =
      with_deadline ~sleep ~seconds:connect_timeout ~label:"connection timeout"
        connect
    in
    let hello_p, hello_u = Lwt.task () in
    let stop_p, stop_u = Lwt.task () in
    let callbacks = ref [] in
    let connected_at = ref None in
    let stop reason =
      if Lwt.is_sleeping stop_p then Lwt.wakeup_later stop_u reason
    in
    let callback msg =
      Lwt.catch
        (fun () ->
          match parse_envelope msg with
          | None -> Lwt.return_unit
          | Some env when env.payload_type = "hello" ->
              connected_at := Some (now ());
              update Connected "hello received" None;
              if Lwt.is_sleeping hello_p then Lwt.wakeup_later hello_u ();
              Lwt.return_unit
          | Some env when env.payload_type = "disconnect" ->
              let reason =
                try
                  Yojson.Safe.Util.(env.payload |> member "reason" |> to_string)
                with _ -> "disconnect requested"
              in
              stop ("Slack disconnect: " ^ reason);
              Lwt.return_unit
          | Some env ->
              let* () =
                with_deadline ~sleep ~seconds:3.0 ~label:"ack timeout"
                  (fun () ->
                    ws.send_text
                      (Yojson.Safe.to_string
                         (`Assoc [ ("envelope_id", `String env.envelope_id) ])))
              in
              if
                env.payload_type = "events_api"
                || env.payload_type = "interactive"
              then begin
                let event_id =
                  try
                    Yojson.Safe.Util.(
                      env.payload |> member "event_id" |> to_string)
                  with _ -> ""
                in
                let key =
                  if event_id = "" then "envelope:" ^ env.envelope_id
                  else "event:" ^ event_id
                in
                if
                  (not (Channel_util.Lru_dedup.mem dedup key))
                  && not (Hashtbl.mem in_flight key)
                then dispatch key (extract_event_body env.payload)
              end;
              Lwt.return_unit)
        (function
          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
          | exn ->
              stop (Printexc.to_string exn);
              Lwt.return_unit)
    in
    let rec heartbeat () =
      let* () = sleep ping_interval in
      let* () =
        with_deadline ~sleep ~seconds:pong_timeout ~label:"pong timeout" ws.ping
      in
      (match !connected_at with
      | Some since ->
          if now () -. since >= 60.0 then Channel_util.Backoff.reset backoff;
          update Connected "pong received" None
      | None -> ());
      heartbeat ()
    in
    Lwt.finalize
      (fun () ->
        ws.on_message (fun msg ->
            let task = callback msg in
            callbacks := task :: !callbacks;
            let remove _ =
              callbacks := List.filter (fun p -> p != task) !callbacks
            in
            Lwt.on_any task remove remove;
            task);
        let* reason =
          Lwt.pick
            [
              stop_p;
              (let* () = Lwt.protected ws.closed in
               Lwt.return "transport closed");
              (let* () =
                 with_deadline ~sleep ~seconds:connect_timeout
                   ~label:"hello timeout" (fun () -> hello_p)
               in
               heartbeat ());
            ]
        in
        (match !connected_at with
        | Some since when now () -. since >= 60.0 ->
            Channel_util.Backoff.reset backoff
        | _ -> ());
        Lwt.return reason)
      (fun () ->
        ws.on_message (fun _ -> Lwt.return_unit);
        List.iter Lwt.cancel !callbacks;
        ws.close ())
  in
  let rec loop () =
    update Connecting "opening connection" None;
    let* reason =
      Lwt.catch run_connection (function
        | Lwt.Canceled -> Lwt.fail Lwt.Canceled
        | exn -> Lwt.return (Printexc.to_string exn))
    in
    incr reconnects;
    let delay = Channel_util.Backoff.current backoff in
    update Reconnecting reason (Some delay);
    Logs.warn (fun m ->
        m "Slack Socket Mode: %s; reconnecting in %.0fs" reason delay);
    Channel_util.Backoff.increase backoff;
    let* () = sleep delay in
    loop ()
  in
  Lwt.finalize loop (fun () ->
      List.iter Lwt.cancel !deliveries;
      update Stopped "connector stopped" None;
      Lwt.return_unit)

let start_with ?connect ?handle_event ?(on_health = publish_health) ?sleep ?now
    ?connect_timeout ?ping_interval ?pong_timeout ~config ~session_manager
    ~(event_limiter : Rate_limiter.t) () =
  match config.Runtime_config.channels.slack with
  | None -> Lwt.return_unit
  | Some sc
    when (not sc.socket_mode)
         || not (Runtime_config.slack_has_valid_credentials sc) ->
      Lwt.return_unit
  | Some sc ->
      let connect =
        match connect with Some f -> f | None -> connect_transport ~config:sc
      in
      let handle_event =
        match handle_event with
        | Some f -> f
        | None ->
            fun body ->
              let open Lwt.Syntax in
              let* _ =
                Slack.handle_event ~config:sc ~session_manager ~event_limiter
                  body
              in
              Lwt.return_unit
      in
      Logs.info (fun m -> m "Slack Socket Mode starting");
      run ?sleep ?now ?connect_timeout ?ping_interval ?pong_timeout ~connect
        ~handle_event ~on_health ()

let start ~config ~session_manager ~event_limiter =
  start_with ~config ~session_manager ~event_limiter ()
