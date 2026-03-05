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
  let guild_ok = match config.allow_guilds with
    | ["*"] -> true
    | ids -> (match guild_id with Some g -> List.mem g ids | None -> false)
  in
  let user_ok = match config.allow_users with
    | ["*"] -> true
    | ids -> List.mem user_id ids
  in
  guild_ok && user_ok

let session_key ~channel_id ~author_id =
  Printf.sprintf "discord:%s:%s" channel_id author_id

let chunk_text ?(max_len = 2000) text =
  let len = String.length text in
  if len <= max_len then [text]
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

let send_message ~bot_token ~channel_id ~text =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s/channels/%s/messages" api_base channel_id in
  let headers = [("Authorization", "Bot " ^ bot_token)] in
  let chunks = chunk_text text in
  let* () = Lwt_list.iter_s (fun chunk ->
    let body =
      `Assoc [("content", `String chunk)]
      |> Yojson.Safe.to_string
    in
    let* _status, _body = Http_client.post_json ~uri ~headers ~body in
    Lwt.return_unit
  ) chunks in
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
  else if not (is_allowed ~config:discord_config
                 ~guild_id:msg.guild_id ~user_id:msg.author_id) then begin
    Logs.warn (fun m ->
        m "Discord: ignoring message from unauthorized user=%s guild=%s"
          msg.author_id
          (Option.value msg.guild_id ~default:"(DM)"));
    Lwt.return_unit
  end else if msg.content = "" then Lwt.return_unit
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
          m "Discord agent error for channel=%s user=%s: %s"
            msg.channel_id msg.author_id err);
      send_message ~bot_token:discord_config.bot_token
        ~channel_id:msg.channel_id
        ~text:"Sorry, an error occurred processing your message."

(* Gateway connection stub.
   The Discord gateway requires a TLS WebSocket connection to
   wss://gateway.discord.gg/?v=10&encoding=json.
   httpun-ws-lwt-unix is installed but connecting over TLS requires
   additional plumbing (e.g. tls-lwt + gluten).
   For now, we use REST polling as a fallback. *)

let poll_messages ~bot_token ~channel_id ~after =
  let open Lwt.Syntax in
  let uri =
    Printf.sprintf "%s/channels/%s/messages?limit=100%s"
      api_base channel_id
      (match after with Some id -> "&after=" ^ id | None -> "")
  in
  let headers = [("Authorization", "Bot " ^ bot_token)] in
  let* status, body = Http_client.get ~uri ~headers in
  if status >= 200 && status < 300 then
    try
      let json = Yojson.Safe.from_string body in
      let messages = Yojson.Safe.Util.to_list json in
      let parsed = List.filter_map (fun msg_json ->
        let open Yojson.Safe.Util in
        try
          let channel_id = msg_json |> member "channel_id" |> to_string in
          let guild_id =
            try Some (msg_json |> member "guild_id" |> to_string) with _ -> None
          in
          let author = msg_json |> member "author" in
          let author_id = author |> member "id" |> to_string in
          let author_bot =
            try author |> member "bot" |> to_bool with _ -> false
          in
          let content = msg_json |> member "content" |> to_string in
          let id = msg_json |> member "id" |> to_string in
          Some (id, { channel_id; guild_id; author_id; author_bot; content })
        with _ -> None
      ) messages in
      Lwt.return parsed
    with _ -> Lwt.return []
  else begin
    Logs.warn (fun m ->
        m "Discord poll_messages error (HTTP %d) for channel=%s" status channel_id);
    Lwt.return []
  end

(* TODO: Replace REST polling with proper WebSocket gateway connection.
   The gateway approach would:
   1. GET /gateway/bot to obtain WSS URL
   2. Connect via TLS WebSocket
   3. Handle opcode 10 (Hello) -> start heartbeat
   4. Send opcode 2 (Identify) with token + intents
   5. Receive opcode 0 (Dispatch) events including MESSAGE_CREATE
   This is the preferred approach but requires TLS WebSocket integration. *)

let start ~config ~session_manager =
  match config.Runtime_config.channels.discord with
  | None ->
    Logs.info (fun m -> m "No Discord config found, skipping");
    Lwt.return_unit
  | Some discord_config ->
    if discord_config.bot_token = "" then begin
      Logs.warn (fun m -> m "Discord bot_token is empty, skipping");
      Lwt.return_unit
    end else begin
      Logs.info (fun m -> m "Discord channel starting (REST polling mode)");
      (* In REST polling mode, we need to know which channels to poll.
         Without gateway events, we cannot discover channels automatically.
         Log a warning and return -- gateway WebSocket is needed for
         production use. *)
      Logs.warn (fun m ->
          m "Discord REST polling requires WebSocket gateway for \
             message reception. Discord channel is not yet fully operational. \
             See TODO in discord.ml for gateway implementation plan.");
      let _discord_config = discord_config in
      let _session_manager = session_manager in
      (* Keep the Lwt thread alive so the daemon doesn't exit *)
      let waiter, _resolver = Lwt.wait () in
      waiter
    end
