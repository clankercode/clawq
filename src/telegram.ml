let api_base = "https://api.telegram.org/bot"

let current_thinking_message current =
  Printf.sprintf "Current thinking level: %s"
    (Slash_commands.thinking_level_to_string current)

let set_thinking_level ~(session_mgr : Session.t) ~chat_id ~user_id level =
  let cfg = Session.get_config session_mgr in
  let previous = cfg.agent_defaults.reasoning_effort in
  match Config_set.set_reasoning_effort level with
  | Ok () ->
      let agent_defaults =
        { cfg.agent_defaults with reasoning_effort = level }
      in
      Session.update_config session_mgr { cfg with agent_defaults };
      Logs.info (fun m ->
          m
            "Telegram thinking level updated chat_id=%s user_id=%s from=%s \
             to=%s"
            chat_id user_id
            (Slash_commands.thinking_level_to_string previous)
            (Slash_commands.thinking_level_to_string level));
      Printf.sprintf "Thinking level changed from %s to %s."
        (Slash_commands.thinking_level_to_string previous)
        (Slash_commands.thinking_level_to_string level)
  | Error err ->
      Logs.err (fun m ->
          m "Telegram thinking level update failed chat_id=%s user_id=%s: %s"
            chat_id user_id err);
      "Failed to update thinking level: " ^ err

let redact_token token =
  let len = String.length token in
  if len <= 8 then "***"
  else String.sub token 0 4 ^ "..." ^ String.sub token (len - 4) 4

type update = {
  update_id : int;
  chat_id : string;
  user_id : string;
  text : string;
  voice_file_id : string option;
  photo_file_id : string option;
  document_file_id : string option;
  document_name : string option;
  caption : string option;
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
      try Yojson.Safe.from_string body
      with _ -> `Assoc [ ("result", `List []) ]
    in
    let open Yojson.Safe.Util in
    let results = try json |> member "result" |> to_list with _ -> [] in
    let updates =
      List.filter_map
        (fun u ->
          try
            let update_id = u |> member "update_id" |> to_int in
            let msg = u |> member "message" in
            let chat = msg |> member "chat" in
            let chat_id = chat |> member "id" |> to_int |> string_of_int in
            let user_id =
              try msg |> member "from" |> member "id" |> to_int |> string_of_int
              with _ -> chat_id
            in
            let text = try msg |> member "text" |> to_string with _ -> "" in
            let voice_file_id =
              try Some (msg |> member "voice" |> member "file_id" |> to_string)
              with _ -> None
            in
            (* Photos arrive as an array sorted by size; take the last (largest) *)
            let photo_file_id =
              try
                let photos = msg |> member "photo" |> to_list in
                let last = List.nth photos (List.length photos - 1) in
                Some (last |> member "file_id" |> to_string)
              with _ -> None
            in
            let document_file_id =
              try
                Some (msg |> member "document" |> member "file_id" |> to_string)
              with _ -> None
            in
            let document_name =
              try
                Some
                  (msg |> member "document" |> member "file_name" |> to_string)
              with _ -> None
            in
            let caption =
              try Some (msg |> member "caption" |> to_string) with _ -> None
            in
            Some
              {
                update_id;
                chat_id;
                user_id;
                text;
                voice_file_id;
                photo_file_id;
                document_file_id;
                document_name;
                caption;
              }
          with _ ->
            let update_id =
              try Yojson.Safe.Util.(u |> member "update_id" |> to_int)
              with _ -> -1
            in
            Logs.debug (fun m ->
                m "Telegram: dropping malformed update (update_id=%d)" update_id);
            None)
        results
    in
    Lwt.return updates
  else (
    Logs.warn (fun m ->
        m "Telegram getUpdates error (HTTP %d) for token=%s" status
          (redact_token bot_token));
    Lwt.return [])

let acknowledge_update ~bot_token ~update_id =
  let open Lwt.Syntax in
  let uri =
    Printf.sprintf "%s%s/getUpdates?offset=%d&timeout=0" api_base bot_token
      (update_id + 1)
  in
  Lwt.catch
    (fun () ->
      let* status, _body = Http_client.get ~uri ~headers:[] in
      if status >= 200 && status < 300 then Lwt.return (Ok ())
      else
        Lwt.return
          (Error
             (Printf.sprintf
                "Failed to acknowledge Telegram update %d before restart (HTTP \
                 %d). Restart aborted."
                update_id status)))
    (fun exn ->
      Lwt.return
        (Error
           (Printf.sprintf
              "Failed to acknowledge Telegram update %d before restart: %s"
              update_id (Printexc.to_string exn))))

let telegram_max_message_len = 4096

(* Split text into chunks no larger than max_len, preferring newline boundaries *)
let chunk_text ?(max_len = telegram_max_message_len) text =
  let len = String.length text in
  if len <= max_len then [ text ]
  else
    let rec go off acc =
      if off >= len then List.rev acc
      else
        let remaining = len - off in
        if remaining <= max_len then
          go len (String.sub text off remaining :: acc)
        else
          (* Try to find a newline to break on *)
          let limit = off + max_len in
          let break_at =
            let rec find i =
              if i <= off then limit
              else if text.[i] = '\n' then i + 1
              else find (i - 1)
            in
            find (limit - 1)
          in
          let chunk_len = break_at - off in
          go break_at (String.sub text off chunk_len :: acc)
    in
    go 0 []

let send_chat_action ~bot_token ~chat_id ~action =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/sendChatAction" api_base bot_token in
  let body =
    `Assoc [ ("chat_id", `String chat_id); ("action", `String action) ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body = Http_client.post_json ~uri ~headers:[] ~body in
  Lwt.return_unit

(* Send typing action repeatedly every ~4s until [p] resolves. *)
let with_typing ~bot_token ~chat_id p =
  let open Lwt.Syntax in
  let cancelled = ref false in
  let rec loop () =
    if !cancelled then Lwt.return_unit
    else
      let* () = send_chat_action ~bot_token ~chat_id ~action:"typing" in
      if !cancelled then Lwt.return_unit
      else
        let* () = Lwt_unix.sleep 4.0 in
        loop ()
  in
  Lwt.async (fun () -> loop ());
  Lwt.finalize
    (fun () -> p)
    (fun () ->
      cancelled := true;
      Lwt.return_unit)

let send_message ~bot_token ~chat_id ~text =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/sendMessage" api_base bot_token in
  let body =
    `Assoc [ ("chat_id", `String chat_id); ("text", `String text) ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body = Http_client.post_json ~uri ~headers:[] ~body in
  Lwt.return_unit

let send_chunked ~bot_token ~chat_id ~text =
  let open Lwt.Syntax in
  Lwt_list.iter_s
    (fun chunk -> send_message ~bot_token ~chat_id ~text:chunk)
    (chunk_text text)

let set_my_commands ~bot_token =
  let open Lwt.Syntax in
  let cmds =
    `List
      (List.map
         (fun (c : Slash_commands.command) ->
           `Assoc
             [
               ("command", `String c.name);
               ("description", `String c.description);
             ])
         Slash_commands.commands)
  in
  let uri = Printf.sprintf "%s%s/setMyCommands" api_base bot_token in
  let body = `Assoc [ ("commands", cmds) ] |> Yojson.Safe.to_string in
  let* status, _body = Http_client.post_json ~uri ~headers:[] ~body in
  if status >= 200 && status < 300 then
    Logs.info (fun m ->
        m "Telegram: registered %d slash commands"
          (List.length Slash_commands.commands))
  else
    Logs.warn (fun m ->
        m "Telegram: setMyCommands failed (HTTP %d) for token=%s" status
          (redact_token bot_token));
  Lwt.return_unit

let is_allowed ~(account : Runtime_config.telegram_account) ~chat_id =
  let coq_allowed = Clawq_core.is_allowed0 chat_id account.allow_from in
  let ocaml_allowed =
    match account.allow_from with
    | [ "*" ] -> true
    | ids -> List.mem chat_id ids
  in
  if coq_allowed <> ocaml_allowed then
    Logs.warn (fun m ->
        m "Telegram allowlist drift for chat_id=%s: Coq=%b OCaml=%b" chat_id
          coq_allowed ocaml_allowed);
  coq_allowed

(* TOTP pairing state: chat_id -> expiry timestamp *)
let _paired_sessions : (string, float) Hashtbl.t = Hashtbl.create 16

let is_totp_paired ~chat_id ~now =
  match Hashtbl.find_opt _paired_sessions chat_id with
  | Some expiry -> now < expiry
  | None -> false

let pair_session ~chat_id ~ttl_hours =
  let expiry = Unix.gettimeofday () +. (float_of_int ttl_hours *. 3600.0) in
  Hashtbl.replace _paired_sessions chat_id expiry

let cleanup_expired_sessions () =
  let now = Unix.gettimeofday () in
  let expired =
    Hashtbl.fold
      (fun k v acc -> if now >= v then k :: acc else acc)
      _paired_sessions []
  in
  List.iter (Hashtbl.remove _paired_sessions) expired

let _rate_limit_warnings : (string, float) Hashtbl.t = Hashtbl.create 16

let handle_pair_command ~bot_token ~(account : Runtime_config.telegram_account)
    ~chat_id ~code =
  match account.totp with
  | Some t when t.totp_enabled && t.totp_secret <> "" ->
      let time = Unix.gettimeofday () in
      if Totp.verify_totp ~secret:t.totp_secret ~code ~time then begin
        pair_session ~chat_id ~ttl_hours:t.session_ttl_hours;
        Logs.info (fun m ->
            m "Telegram: TOTP pairing successful for chat_id=%s" chat_id);
        send_message ~bot_token ~chat_id
          ~text:
            (Printf.sprintf "Pairing successful! Session valid for %d hours."
               t.session_ttl_hours)
      end
      else begin
        Logs.warn (fun m ->
            m "Telegram: TOTP pairing failed for chat_id=%s" chat_id);
        send_message ~bot_token ~chat_id
          ~text:
            "Invalid code. Please try again with a valid TOTP code from `clawq \
             otp-show`."
      end
  | _ ->
      send_message ~bot_token ~chat_id
        ~text:"TOTP pairing is not configured for this account."

let requires_totp_auth ~(account : Runtime_config.telegram_account) ~chat_id =
  match account.totp with
  | Some t when t.totp_enabled ->
      let now = Unix.gettimeofday () in
      if is_allowed ~account ~chat_id then false
      else not (is_totp_paired ~chat_id ~now)
  | _ -> false

let handle_update ~bot_token ~(account : Runtime_config.telegram_account)
    ~(session_mgr : Session.t) ?run_update_command ?chat_limiter update =
  let open Lwt.Syntax in
  (* Check /pair command first (before auth checks) *)
  let trimmed = String.trim update.text in
  let is_pair_cmd =
    String.length trimmed > 6
    && String.lowercase_ascii (String.sub trimmed 0 6) = "/pair "
  in
  if is_pair_cmd then
    let code = String.trim (String.sub trimmed 6 (String.length trimmed - 6)) in
    handle_pair_command ~bot_token ~account ~chat_id:update.chat_id ~code
  else if
    (not (is_allowed ~account ~chat_id:update.chat_id))
    && requires_totp_auth ~account ~chat_id:update.chat_id
  then (
    Logs.warn (fun m ->
        m "Telegram: unauthenticated chat_id=%s, requesting pairing"
          update.chat_id);
    send_message ~bot_token ~chat_id:update.chat_id
      ~text:
        "Please pair first: type `/pair <6-digit-code>`.\n\
         Get the code from `clawq otp-show` command.")
  else if
    (not (is_allowed ~account ~chat_id:update.chat_id))
    && not (is_totp_paired ~chat_id:update.chat_id ~now:(Unix.gettimeofday ()))
  then (
    Logs.warn (fun m ->
        m "Telegram: ignoring message from unauthorized chat_id=%s"
          update.chat_id);
    Lwt.return_unit)
  else
    let* rate_ok =
      match chat_limiter with
      | Some lim -> Rate_limiter.check_and_consume lim ~key:update.chat_id
      | None -> Lwt.return true
    in
    if not rate_ok then begin
      let now = Unix.gettimeofday () in
      let should_warn =
        match Hashtbl.find_opt _rate_limit_warnings update.chat_id with
        | Some last -> now -. last >= 60.0
        | None -> true
      in
      if should_warn then begin
        Hashtbl.replace _rate_limit_warnings update.chat_id now;
        let* () =
          send_message ~bot_token ~chat_id:update.chat_id
            ~text:
              "Please slow down, I can only process a limited number of \
               messages per minute."
        in
        Lwt.return_unit
      end
      else Lwt.return_unit
    end
    else
      let key = "telegram:" ^ update.chat_id ^ ":" ^ update.user_id in
      let* user_text =
        match update.voice_file_id with
        | Some file_id ->
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
                    file_json |> member "result" |> member "file_path"
                    |> to_string)
                in
                let download_uri =
                  Printf.sprintf "https://api.telegram.org/file/bot%s/%s"
                    bot_token file_path
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
                Lwt.return "")
        | None -> (
            match update.photo_file_id with
            | Some _ ->
                let cap =
                  match update.caption with Some c -> " — " ^ c | None -> ""
                in
                Lwt.return ("[Photo received" ^ cap ^ "]")
            | None -> (
                match update.document_file_id with
                | Some _ ->
                    let name =
                      match update.document_name with
                      | Some n -> ": " ^ n
                      | None -> ""
                    in
                    let cap =
                      match update.caption with
                      | Some c -> " — " ^ c
                      | None -> ""
                    in
                    Lwt.return ("[Document" ^ name ^ cap ^ "]")
                | None -> Lwt.return update.text))
      in
      if user_text = "" then Lwt.return_unit
      else if Update_tool.is_update_command user_text then
        let send_progress text =
          send_chunked ~bot_token ~chat_id:update.chat_id ~text
        in
        let run_update_command =
          match run_update_command with
          | Some run_update_command -> run_update_command
          | None ->
              fun ?prepare_restart ~send_progress () ->
                Update_tool.run_update ?prepare_restart
                  ~is_draining:(fun () -> Session.is_draining session_mgr)
                  ~send_progress ()
        in
        let* response =
          run_update_command
            ~prepare_restart:(fun () ->
              Restart_notify.write ~channel:"telegram"
                ~channel_id:update.chat_id;
              acknowledge_update ~bot_token ~update_id:update.update_id)
            ~send_progress ()
        in
        send_chunked ~bot_token ~chat_id:update.chat_id ~text:response
      else
        match Slash_commands.handle user_text with
        | Reply text -> send_message ~bot_token ~chat_id:update.chat_id ~text
        | Reset ->
            let* () = Session.reset session_mgr ~key in
            send_message ~bot_token ~chat_id:update.chat_id
              ~text:Slash_commands.reset_message
        | Thinking Slash_commands.ShowThinking ->
            let current =
              (Session.get_config session_mgr).agent_defaults.reasoning_effort
            in
            send_message ~bot_token ~chat_id:update.chat_id
              ~text:(current_thinking_message current)
        | Thinking (Slash_commands.SetThinking level) ->
            send_message ~bot_token ~chat_id:update.chat_id
              ~text:
                (set_thinking_level ~session_mgr ~chat_id:update.chat_id
                   ~user_id:update.user_id level)
        | NotACommand -> (
            let msg = user_text in
            let agent_defaults =
              (Session.get_config session_mgr).agent_defaults
            in
            let visibility = Stream_visibility.create () in
            let settings : Stream_visibility.settings =
              {
                show_thinking = agent_defaults.show_thinking;
                show_tool_calls = agent_defaults.show_tool_calls;
              }
            in
            let on_chunk chunk =
              Stream_visibility.on_chunk visibility ~settings
                ~notify:(fun text ->
                  send_chunked ~bot_token ~chat_id:update.chat_id ~text)
                chunk
            in
            let* result =
              Session.with_registered_notifier session_mgr ~key
                ~notify:(fun text ->
                  send_chunked ~bot_token ~chat_id:update.chat_id ~text)
                (fun () ->
                  Lwt.catch
                    (fun () ->
                      let turn_p =
                        Session.turn_stream session_mgr ~key ~message:msg
                          ~channel_name:"telegram" ~channel_type:"dm"
                          ~channel:"telegram" ~channel_id:update.chat_id
                          ~on_chunk ()
                      in
                      let* response =
                        with_typing ~bot_token ~chat_id:update.chat_id turn_p
                      in
                      Lwt.return (Ok response))
                    (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
            in
            match result with
            | Ok response ->
                let thinking = Stream_visibility.thinking_text visibility in
                let* () =
                  if thinking <> "" then
                    send_chunked ~bot_token ~chat_id:update.chat_id
                      ~text:("_" ^ thinking ^ "_")
                  else Lwt.return_unit
                in
                let* () =
                  send_chunked ~bot_token ~chat_id:update.chat_id ~text:response
                in
                Session.mark_response_sent session_mgr ~key;
                Lwt.return_unit
            | Error err ->
                Logs.err (fun m ->
                    m "Agent error for chat_id=%s: %s" update.chat_id err);
                let* () =
                  send_message ~bot_token ~chat_id:update.chat_id
                    ~text:
                      (Printf.sprintf
                         "Sorry, an error occurred processing your message: %s"
                         err)
                in
                Session.mark_response_sent session_mgr ~key;
                Lwt.return_unit)

let poll_account ~bot_token ~(account : Runtime_config.telegram_account) ~name
    ~(session_mgr : Session.t) ?run_update_command ?chat_limiter () =
  let open Lwt.Syntax in
  Logs.info (fun m -> m "Starting Telegram polling for account '%s'" name);
  let* () =
    Lwt.catch
      (fun () -> set_my_commands ~bot_token)
      (fun exn ->
        Logs.warn (fun m ->
            m "Telegram: setMyCommands failed for '%s': %s" name
              (Printexc.to_string exn));
        Lwt.return_unit)
  in
  let offset = ref 0 in
  let poll_count = ref 0 in
  let rec poll () =
    incr poll_count;
    if !poll_count <= 3 then
      Logs.info (fun m ->
          m "Telegram poll #%d for account '%s'" !poll_count name)
    else if !poll_count = 4 then
      Logs.info (fun m ->
          m "Telegram polling stable, suppressing routine poll logs for '%s'"
            name);
    let* updates =
      Lwt.catch
        (fun () -> get_updates ~bot_token ~offset:!offset ~timeout:30)
        (fun exn ->
          Logs.err (fun m ->
              m "Telegram poll error for '%s': %s" name (Printexc.to_string exn));
          let* () = Lwt_unix.sleep 5.0 in
          Lwt.return [])
    in
    let* () =
      Lwt_list.iter_s
        (fun update ->
          offset := update.update_id + 1;
          handle_update ~bot_token ~account ~session_mgr ?run_update_command
            ?chat_limiter update)
        updates
    in
    poll ()
  in
  poll ()

let start_polling ~(config : Runtime_config.t) ~(session_manager : Session.t)
    ?run_update_command ?chat_limiter () =
  match config.channels.telegram with
  | None ->
      Logs.info (fun m -> m "No Telegram config found, skipping polling");
      Lwt.return_unit
  | Some tg_config -> (
      match tg_config.accounts with
      | [] ->
          Logs.info (fun m -> m "No Telegram accounts configured");
          Lwt.return_unit
      | accounts -> (
          let poll_loops =
            List.filter_map
              (fun (name, (account : Runtime_config.telegram_account)) ->
                if account.bot_token = "" then (
                  Logs.warn (fun m ->
                      m "Telegram account '%s' has empty bot_token, skipping"
                        name);
                  None)
                else
                  Some
                    (poll_account ~bot_token:account.bot_token ~account ~name
                       ~session_mgr:session_manager ?run_update_command
                       ?chat_limiter ()))
              accounts
          in
          match poll_loops with
          | [] ->
              Logs.info (fun m -> m "No Telegram accounts with valid bot_token");
              Lwt.return_unit
          | loops -> Lwt.join loops))
