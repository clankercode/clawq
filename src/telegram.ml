let api_base = "https://api.telegram.org/bot"

let redact_token token =
  let len = String.length token in
  if len <= 8 then "***"
  else String.sub token 0 4 ^ "..." ^ String.sub token (len - 4) 4

type update = {
  update_id : int;
  chat_id : string;
  text : string;
  voice_file_id : string option;
}

let get_updates ~bot_token ~offset ~timeout =
  let open Lwt.Syntax in
  let uri =
    Printf.sprintf "%s%s/getUpdates?offset=%d&timeout=%d" api_base bot_token
      offset timeout
  in
  let* status, body = Http_client.get ~uri ~headers:[] in
  if status >= 200 && status < 300 then
    let json =
      try Yojson.Safe.from_string body with _ -> `Assoc [ ("result", `List []) ]
    in
    let open Yojson.Safe.Util in
    let results =
      try json |> member "result" |> to_list with _ -> []
    in
    let updates =
      List.filter_map
        (fun u ->
          try
            let update_id = u |> member "update_id" |> to_int in
            let msg = u |> member "message" in
            let chat = msg |> member "chat" in
            let chat_id = chat |> member "id" |> to_int |> string_of_int in
            let text =
              try msg |> member "text" |> to_string with _ -> ""
            in
            let voice_file_id =
              try Some (msg |> member "voice" |> member "file_id" |> to_string)
              with _ -> None
            in
            Some { update_id; chat_id; text; voice_file_id }
          with _ -> None)
        results
    in
    Lwt.return updates
  else (
    Logs.warn (fun m -> m "Telegram getUpdates error (HTTP %d) for token=%s" status (redact_token bot_token));
    Lwt.return [])

let send_message ~bot_token ~chat_id ~text =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/sendMessage" api_base bot_token in
  let body =
    `Assoc [ ("chat_id", `String chat_id); ("text", `String text) ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body =
    Http_client.post_json ~uri ~headers:[] ~body
  in
  Lwt.return_unit

let is_allowed ~(account : Runtime_config.telegram_account) ~chat_id =
  match account.allow_from with
  | [ "*" ] -> true
  | ids -> List.mem chat_id ids

let handle_update ~bot_token ~(account : Runtime_config.telegram_account)
    ~(session_mgr : Session.t) update =
  let open Lwt.Syntax in
  if not (is_allowed ~account ~chat_id:update.chat_id) then (
    Logs.warn (fun m ->
        m "Telegram: ignoring message from unauthorized chat_id=%s"
          update.chat_id);
    Lwt.return_unit)
  else
    let key = "telegram:" ^ update.chat_id in
    let* user_text =
      match update.voice_file_id with
      | Some file_id -> (
        Lwt.catch
          (fun () ->
            let get_file_uri =
              Printf.sprintf "%s%s/getFile?file_id=%s" api_base bot_token
                file_id
            in
            let* _status, file_body =
              Http_client.get ~uri:get_file_uri ~headers:[]
            in
            let file_json = Yojson.Safe.from_string file_body in
            let file_path =
              Yojson.Safe.Util.(
                file_json |> member "result" |> member "file_path" |> to_string)
            in
            let download_uri =
              Printf.sprintf "https://api.telegram.org/file/bot%s/%s" bot_token
                file_path
            in
            let* _status, audio_data =
              Http_client.get ~uri:download_uri ~headers:[]
            in
            let filename = Filename.basename file_path in
            let content_type = Stt.content_type_of_ext filename in
            let config = Session.get_config session_mgr in
            let* result =
              Stt.transcribe ~config ~audio_data ~filename ~content_type ()
            in
            Lwt.return ("[Voice]: " ^ result.text))
          (fun exn ->
            Logs.err (fun m ->
                m "Voice transcription failed: %s" (Printexc.to_string exn));
            Lwt.return ""))
      | None -> Lwt.return update.text
    in
    if user_text = "" then Lwt.return_unit
    else
    match user_text with
    | "/start" | "/help" ->
      let text =
        "clawq bot ready. Send me a message and I'll respond using AI.\n\
         Commands: /new (reset session), /help"
      in
      send_message ~bot_token ~chat_id:update.chat_id ~text
    | "/new" ->
      Session.reset session_mgr ~key;
      send_message ~bot_token ~chat_id:update.chat_id
        ~text:"Session reset. Send a new message to start fresh."
    | msg -> (
      let* result =
        Lwt.catch
          (fun () ->
            let* response = Session.turn session_mgr ~key ~message:msg in
            Lwt.return (Ok response))
          (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
      in
      match result with
      | Ok response ->
        send_message ~bot_token ~chat_id:update.chat_id ~text:response
      | Error err ->
        Logs.err (fun m -> m "Agent error for chat_id=%s: %s" update.chat_id err);
        send_message ~bot_token ~chat_id:update.chat_id
          ~text:"Sorry, an error occurred processing your message. Please try again.")

let start_polling ~(config : Runtime_config.t)
    ~(session_manager : Session.t) =
  let open Lwt.Syntax in
  match config.channels.telegram with
  | None ->
    Logs.info (fun m -> m "No Telegram config found, skipping polling");
    Lwt.return_unit
  | Some tg_config -> (
    match tg_config.accounts with
    | [] ->
      Logs.info (fun m -> m "No Telegram accounts configured");
      Lwt.return_unit
    | (name, account) :: _ ->
      if account.bot_token = "" then (
        Logs.warn (fun m ->
            m "Telegram account '%s' has empty bot_token, skipping" name);
        Lwt.return_unit)
      else
        let bot_token = account.bot_token in
        Logs.info (fun m -> m "Starting Telegram polling for account '%s'" name);
        let offset = ref 0 in
        let poll_count = ref 0 in
        let rec poll () =
          incr poll_count;
          (if !poll_count <= 3 then
             Logs.info (fun m -> m "Telegram poll #%d for account '%s'" !poll_count name)
           else if !poll_count = 4 then
             Logs.info (fun m -> m "Telegram polling stable, suppressing routine poll logs"));
          let* updates =
            Lwt.catch
              (fun () -> get_updates ~bot_token ~offset:!offset ~timeout:30)
              (fun exn ->
                Logs.err (fun m ->
                    m "Telegram poll error: %s" (Printexc.to_string exn));
                let* () = Lwt_unix.sleep 5.0 in
                Lwt.return [])
          in
          let* () =
            Lwt_list.iter_s
              (fun update ->
                offset := update.update_id + 1;
                handle_update ~bot_token ~account ~session_mgr:session_manager
                  update)
              updates
          in
          poll ()
        in
        poll ())
