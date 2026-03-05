let name = "discord"
let api_base = "https://discord.com/api/v10"

type message = {
  channel_id : string;
  guild_id : string option;
  author_id : string;
  author_bot : bool;
  content : string;
}

let is_allowed ~(config : Runtime_config.discord_config) ~guild_id ~user_id =
  let guild_ok =
    match config.allow_guilds with
    | [ "*" ] -> true
    | ids -> ( match guild_id with Some g -> List.mem g ids | None -> false)
  in
  let user_ok =
    match config.allow_users with
    | [ "*" ] -> true
    | ids -> List.mem user_id ids
  in
  guild_ok && user_ok

let session_key ~channel_id ~author_id =
  Printf.sprintf "discord:%s:%s" channel_id author_id

let chunk_text ?(max_len = 2000) text =
  let len = String.length text in
  if len <= max_len then [ text ]
  else
    let rec go off acc =
      if off >= len then List.rev acc
      else
        let remaining = len - off in
        let chunk_len = min max_len remaining in
        let chunk = String.sub text off chunk_len in
        go (off + chunk_len) (chunk :: acc)
    in
    go 0 []

(* --- Discord REST rate limit tracking --- *)

type route_bucket = { mutable remaining : int; mutable reset_at : float }

let route_buckets : (string, route_bucket) Hashtbl.t = Hashtbl.create 32
let route_mutex = Lwt_mutex.create ()
let global_rate_limit : float ref = ref 0.0

let wait_for_rate_limit ~route =
  let open Lwt.Syntax in
  let now = Unix.gettimeofday () in
  let* () =
    if !global_rate_limit > now then Lwt_unix.sleep (!global_rate_limit -. now)
    else Lwt.return_unit
  in
  match Hashtbl.find_opt route_buckets route with
  | Some bucket when bucket.remaining <= 0 && bucket.reset_at > now ->
      Lwt_unix.sleep (bucket.reset_at -. now)
  | _ -> Lwt.return_unit

let update_rate_limit ~route ~headers =
  let remaining =
    try
      Cohttp.Header.get headers "x-ratelimit-remaining"
      |> Option.map int_of_string
    with _ -> None
  in
  let reset_at =
    try
      Cohttp.Header.get headers "x-ratelimit-reset"
      |> Option.map float_of_string
    with _ -> None
  in
  let is_global =
    try
      Cohttp.Header.get headers "x-ratelimit-global"
      |> Option.map (fun s -> String.lowercase_ascii s = "true")
      |> Option.value ~default:false
    with _ -> false
  in
  if is_global then begin
    let retry_after =
      try
        Cohttp.Header.get headers "retry-after"
        |> Option.map float_of_string |> Option.value ~default:1.0
      with _ -> 1.0
    in
    global_rate_limit := Unix.gettimeofday () +. retry_after
  end;
  match (remaining, reset_at) with
  | Some r, Some ra ->
      let bucket =
        match Hashtbl.find_opt route_buckets route with
        | Some b -> b
        | None ->
            let b = { remaining = r; reset_at = ra } in
            Hashtbl.replace route_buckets route b;
            b
      in
      bucket.remaining <- r;
      bucket.reset_at <- ra
  | _ -> ()

let discord_rest_call ~route ~f =
  let open Lwt.Syntax in
  Lwt_mutex.with_lock route_mutex (fun () ->
      let* () = wait_for_rate_limit ~route in
      let* status, headers, body = f () in
      update_rate_limit ~route ~headers;
      if status = 429 then begin
        let retry_after =
          try
            Cohttp.Header.get headers "retry-after"
            |> Option.map float_of_string |> Option.value ~default:1.0
          with _ -> 1.0
        in
        Logs.warn (fun m ->
            m "Discord: rate limited on %s, retrying after %.1fs" route
              retry_after);
        let* () = Lwt_unix.sleep retry_after in
        let* () = wait_for_rate_limit ~route in
        let* status2, headers2, body2 = f () in
        update_rate_limit ~route ~headers:headers2;
        Lwt.return (status2, body2)
      end
      else Lwt.return (status, body))

let send_message ~bot_token ~channel_id ~text =
  let open Lwt.Syntax in
  let route = "POST /channels/" ^ channel_id ^ "/messages" in
  let uri = Printf.sprintf "%s/channels/%s/messages" api_base channel_id in
  let headers = [ ("Authorization", "Bot " ^ bot_token) ] in
  let chunks = chunk_text text in
  let* () =
    Lwt_list.iter_s
      (fun chunk ->
        let body =
          `Assoc [ ("content", `String chunk) ] |> Yojson.Safe.to_string
        in
        let* _status, _body =
          discord_rest_call ~route ~f:(fun () ->
              Http_client.post_json_with_headers ~uri ~headers ~body)
        in
        Lwt.return_unit)
      chunks
  in
  Lwt.return_unit

let parse_message_create json =
  let open Yojson.Safe.Util in
  try
    let t = json |> member "t" |> to_string in
    if t <> "MESSAGE_CREATE" then None
    else
      let d = json |> member "d" in
      let channel_id = d |> member "channel_id" |> to_string in
      let guild_id =
        try Some (d |> member "guild_id" |> to_string) with _ -> None
      in
      let author = d |> member "author" in
      let author_id = author |> member "id" |> to_string in
      let author_bot =
        try author |> member "bot" |> to_bool with _ -> false
      in
      let content = d |> member "content" |> to_string in
      Some { channel_id; guild_id; author_id; author_bot; content }
  with _ -> None

let parse_dispatch_message d =
  let open Yojson.Safe.Util in
  try
    let channel_id = d |> member "channel_id" |> to_string in
    let guild_id =
      try Some (d |> member "guild_id" |> to_string) with _ -> None
    in
    let author = d |> member "author" in
    let author_id = author |> member "id" |> to_string in
    let author_bot = try author |> member "bot" |> to_bool with _ -> false in
    let content = d |> member "content" |> to_string in
    Some { channel_id; guild_id; author_id; author_bot; content }
  with _ -> None

let is_bot_message json =
  let open Yojson.Safe.Util in
  try
    let d = json |> member "d" in
    let author = d |> member "author" in
    author |> member "bot" |> to_bool
  with _ -> false

let handle_message ~(discord_config : Runtime_config.discord_config)
    ~(session_mgr : Session.t) (msg : message) =
  let open Lwt.Syntax in
  if msg.author_bot then Lwt.return_unit
  else if
    not
      (is_allowed ~config:discord_config ~guild_id:msg.guild_id
         ~user_id:msg.author_id)
  then begin
    Logs.warn (fun m ->
        m "Discord: ignoring message from unauthorized user=%s guild=%s"
          msg.author_id
          (Option.value msg.guild_id ~default:"(DM)"));
    Lwt.return_unit
  end
  else if msg.content = "" then Lwt.return_unit
  else
    let key = session_key ~channel_id:msg.channel_id ~author_id:msg.author_id in
    let* result =
      Lwt.catch
        (fun () ->
          let* response = Session.turn session_mgr ~key ~message:msg.content in
          Lwt.return (Ok response))
        (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
    in
    match result with
    | Ok response ->
        send_message ~bot_token:discord_config.bot_token
          ~channel_id:msg.channel_id ~text:response
    | Error err ->
        Logs.err (fun m ->
            m "Discord agent error for channel=%s user=%s: %s" msg.channel_id
              msg.author_id err);
        send_message ~bot_token:discord_config.bot_token
          ~channel_id:msg.channel_id
          ~text:"Sorry, an error occurred processing your message."

(* Close code classification for reconnect behavior *)
let is_fatal_close_code code =
  match code with 4004 | 4010 | 4011 | 4012 | 4013 | 4014 -> true | _ -> false

let start ~config ~session_manager =
  match config.Runtime_config.channels.discord with
  | None ->
      Logs.info (fun m -> m "No Discord config found, skipping");
      Lwt.return_unit
  | Some discord_config ->
      if discord_config.bot_token = "" then begin
        Logs.warn (fun m -> m "Discord bot_token is empty, skipping");
        Lwt.return_unit
      end
      else begin
        Logs.info (fun m ->
            m "Discord channel starting (WebSocket gateway mode)");
        let open Lwt.Syntax in
        let resume_session_id = ref None in
        let resume_seq = ref None in
        let resume_url = ref None in
        let backoff = ref 1.0 in
        let rec connect_loop () =
          let close_p, close_u = Lwt.wait () in
          let on_dispatch event_name d =
            if event_name = "MESSAGE_CREATE" then
              match parse_dispatch_message d with
              | Some msg ->
                  handle_message ~discord_config ~session_mgr:session_manager
                    msg
              | None -> Lwt.return_unit
            else Lwt.return_unit
          in
          let on_close code =
            if Lwt.is_sleeping close_p then Lwt.wakeup_later close_u code;
            Lwt.return_unit
          in
          let result =
            Lwt.catch
              (fun () ->
                let* gw =
                  Discord_gateway.connect ~bot_token:discord_config.bot_token
                    ~intents:discord_config.intents
                    ?resume_session_id:!resume_session_id
                    ?resume_seq:!resume_seq ?resume_url:!resume_url ~on_dispatch
                    ~on_close ()
                in
                backoff := 1.0;
                let* code = close_p in
                resume_session_id := Discord_gateway.session_id gw;
                resume_seq := Discord_gateway.last_seq gw;
                resume_url := Discord_gateway.resume_url gw;
                Lwt.return (Ok code))
              (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
          in
          let* outcome = result in
          match outcome with
          | Error err ->
              Logs.err (fun m -> m "Discord: gateway connection error: %s" err);
              resume_session_id := None;
              resume_seq := None;
              resume_url := None;
              let delay = !backoff in
              backoff := Float.min (!backoff *. 2.0) 60.0;
              Logs.info (fun m -> m "Discord: reconnecting in %.0fs" delay);
              let* () = Lwt_unix.sleep delay in
              connect_loop ()
          | Ok code ->
              let code_int = match code with Some c -> c | None -> 0 in
              if is_fatal_close_code code_int then begin
                Logs.err (fun m ->
                    m "Discord: fatal close code %d, not reconnecting" code_int);
                Lwt.return_unit
              end
              else begin
                Logs.info (fun m ->
                    m "Discord: connection closed (code=%d), reconnecting"
                      code_int);
                let delay =
                  if !resume_session_id <> None then 1.0 else !backoff
                in
                backoff :=
                  if !resume_session_id = None then
                    Float.min (!backoff *. 2.0) 60.0
                  else 1.0;
                let* () = Lwt_unix.sleep delay in
                connect_loop ()
              end
        in
        connect_loop ()
      end
