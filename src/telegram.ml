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
  message_id : int;
  chat_id : string;
  user_id : string;
  text : string;
  voice_file_id : string option;
  photo_file_id : string option;
  document_file_id : string option;
  document_name : string option;
  caption : string option;
}

type callback_query = {
  cb_bot_token : string;
  callback_query_id : string;
  cb_chat_id : string;
  cb_message_id : int;
  data : string;
}

let pending_callbacks : callback_query Queue.t = Queue.create ()

(* Keyed by "chat_id:tool_id" to prevent cross-chat data leakage *)
let tool_result_cache : (string, string) Hashtbl.t = Hashtbl.create 32
let recently_seen_updates : (string, float) Hashtbl.t = Hashtbl.create 256
let duplicate_update_ttl_seconds = 600.0

let update_dedupe_key (u : update) =
  Printf.sprintf "%s:%d" u.chat_id u.update_id

let cleanup_recently_seen_updates ~now =
  let expired = ref [] in
  Hashtbl.iter
    (fun key seen_at ->
      if now -. seen_at >= duplicate_update_ttl_seconds then
        expired := key :: !expired)
    recently_seen_updates;
  List.iter (Hashtbl.remove recently_seen_updates) !expired

let should_process_update (u : update) =
  let now = Unix.gettimeofday () in
  cleanup_recently_seen_updates ~now;
  let key = update_dedupe_key u in
  if Hashtbl.mem recently_seen_updates key then false
  else begin
    Hashtbl.replace recently_seen_updates key now;
    true
  end

type poll_error =
  | Conflict_webhook
  | Conflict_duplicate_poller
  | Other_error of int

type poll_result = Updates of int * update list | Poll_error of poll_error

let delete_webhook ~bot_token =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/deleteWebhook" api_base bot_token in
  let body = "{}" in
  let* status, _body = Http_client.post_json ~uri ~headers:[] ~body in
  if status >= 200 && status < 300 then
    Logs.info (fun m ->
        m "Telegram: deleteWebhook succeeded for token=%s"
          (redact_token bot_token))
  else
    Logs.warn (fun m ->
        m "Telegram: deleteWebhook failed (HTTP %d) for token=%s" status
          (redact_token bot_token));
  Lwt.return_unit

let parse_conflict_description body =
  try
    let json = Yojson.Safe.from_string body in
    let desc = Yojson.Safe.Util.(json |> member "description" |> to_string) in
    let desc_lower = String.lowercase_ascii desc in
    if
      try
        ignore (Str.search_forward (Str.regexp_string "webhook") desc_lower 0);
        true
      with Not_found -> false
    then Conflict_webhook
    else Conflict_duplicate_poller
  with _ -> Conflict_duplicate_poller

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
    (* Track max update_id across all results including non-message updates *)
    let max_update_id = ref 0 in
    let updates =
      List.filter_map
        (fun u ->
          (try
             let uid = u |> member "update_id" |> to_int in
             if uid > !max_update_id then max_update_id := uid
           with _ -> ());
          try
            let update_id = u |> member "update_id" |> to_int in
            let msg = u |> member "message" in
            let message_id =
              try msg |> member "message_id" |> to_int with _ -> 0
            in
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
                message_id;
                chat_id;
                user_id;
                text;
                voice_file_id;
                photo_file_id;
                document_file_id;
                document_name;
                caption;
              }
          with _ -> (
            let open Yojson.Safe.Util in
            try
              let cq = u |> member "callback_query" in
              if cq = `Null then raise Not_found;
              let callback_query_id = cq |> member "id" |> to_string in
              let msg = cq |> member "message" in
              let chat = msg |> member "chat" in
              let cb_chat_id = chat |> member "id" |> to_int |> string_of_int in
              let cb_message_id = msg |> member "message_id" |> to_int in
              let data = try cq |> member "data" |> to_string with _ -> "" in
              Queue.push
                {
                  cb_bot_token = bot_token;
                  callback_query_id;
                  cb_chat_id;
                  cb_message_id;
                  data;
                }
                pending_callbacks;
              None
            with _ ->
              let update_id =
                try u |> member "update_id" |> to_int with _ -> -1
              in
              Logs.debug (fun m ->
                  m "Telegram: dropping malformed update (update_id=%d)"
                    update_id);
              None))
        results
    in
    Lwt.return (Updates (!max_update_id, updates))
  else if status = 409 then (
    let conflict = parse_conflict_description body in
    (match conflict with
    | Conflict_webhook ->
        Logs.warn (fun m ->
            m
              "Telegram: 409 Conflict — webhook is active, will attempt \
               deleteWebhook for token=%s"
              (redact_token bot_token))
    | Conflict_duplicate_poller ->
        Logs.warn (fun m ->
            m
              "Telegram: 409 Conflict — another getUpdates instance is running \
               for token=%s"
              (redact_token bot_token))
    | Other_error _ -> ());
    Lwt.return (Poll_error conflict))
  else (
    Logs.warn (fun m ->
        m "Telegram getUpdates error (HTTP %d) for token=%s" status
          (redact_token bot_token));
    Lwt.return (Poll_error (Other_error status)))

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

let chat_action_for_tool name =
  match name with
  | "file_write" | "file_append" | "file_edit" | "file_edit_lines" | "doc_write"
    ->
      "upload_document"
  | "web_fetch" | "web_search" | "http_get" | "http_request" -> "find_location"
  | "transcribe" -> "record_voice"
  | _ -> "typing"

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

let send_message_with_id ?(disable_notification = false) ?parse_mode ~bot_token
    ~chat_id ~text () =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/sendMessage" api_base bot_token in
  let base_fields =
    [
      ("chat_id", `String chat_id);
      ("text", `String text);
      ("disable_notification", `Bool disable_notification);
    ]
  in
  let fields =
    match parse_mode with
    | Some mode -> ("parse_mode", `String mode) :: base_fields
    | None -> base_fields
  in
  let body = `Assoc fields |> Yojson.Safe.to_string in
  let* status, resp_body = Http_client.post_json ~uri ~headers:[] ~body in
  let* resp_body =
    if parse_mode = Some "MarkdownV2" && status >= 400 then
      let plain_body = `Assoc base_fields |> Yojson.Safe.to_string in
      let* _status, resp_body =
        Http_client.post_json ~uri ~headers:[] ~body:plain_body
      in
      Lwt.return resp_body
    else Lwt.return resp_body
  in
  let msg_id =
    try
      let json = Yojson.Safe.from_string resp_body in
      let result = json |> Yojson.Safe.Util.member "result" in
      result
      |> Yojson.Safe.Util.member "message_id"
      |> Yojson.Safe.Util.to_int |> string_of_int
    with _ -> "0"
  in
  Lwt.return msg_id

let send_message_with_keyboard ?(disable_notification = false) ?parse_mode
    ~bot_token ~chat_id ~text ~buttons () =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/sendMessage" api_base bot_token in
  let inline_buttons =
    List.map
      (fun (label, callback_data) ->
        `Assoc
          [ ("text", `String label); ("callback_data", `String callback_data) ])
      buttons
  in
  let reply_markup =
    `Assoc [ ("inline_keyboard", `List [ `List inline_buttons ]) ]
  in
  let base_fields =
    [
      ("chat_id", `String chat_id);
      ("text", `String text);
      ("disable_notification", `Bool disable_notification);
      ("reply_markup", reply_markup);
    ]
  in
  let fields =
    match parse_mode with
    | Some mode -> ("parse_mode", `String mode) :: base_fields
    | None -> base_fields
  in
  let body = `Assoc fields |> Yojson.Safe.to_string in
  let* _status, resp_body = Http_client.post_json ~uri ~headers:[] ~body in
  let msg_id =
    try
      let json = Yojson.Safe.from_string resp_body in
      json
      |> Yojson.Safe.Util.member "result"
      |> Yojson.Safe.Util.member "message_id"
      |> Yojson.Safe.Util.to_int |> string_of_int
    with _ -> "0"
  in
  Lwt.return msg_id

let answer_callback_query ~bot_token ~callback_query_id ?(text = "") () =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/answerCallbackQuery" api_base bot_token in
  let body =
    `Assoc
      [
        ("callback_query_id", `String callback_query_id); ("text", `String text);
      ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body = Http_client.post_json ~uri ~headers:[] ~body in
  Lwt.return_unit

let edit_message ?parse_mode ~bot_token ~chat_id ~message_id ~text () =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/editMessageText" api_base bot_token in
  let base_fields =
    [
      ("chat_id", `String chat_id);
      ("message_id", `Int (try int_of_string message_id with _ -> 0));
      ("text", `String text);
    ]
  in
  let fields =
    match parse_mode with
    | Some mode -> ("parse_mode", `String mode) :: base_fields
    | None -> base_fields
  in
  let body = `Assoc fields |> Yojson.Safe.to_string in
  let* status, _body = Http_client.post_json ~uri ~headers:[] ~body in
  if parse_mode = Some "MarkdownV2" && status >= 400 then
    let plain_body = `Assoc base_fields |> Yojson.Safe.to_string in
    let* _status, _body =
      Http_client.post_json ~uri ~headers:[] ~body:plain_body
    in
    Lwt.return_unit
  else Lwt.return_unit

let delete_message ~bot_token ~chat_id ~message_id () =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/deleteMessage" api_base bot_token in
  let body =
    `Assoc
      [
        ("chat_id", `String chat_id);
        ("message_id", `Int (try int_of_string message_id with _ -> 0));
      ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body = Http_client.post_json ~uri ~headers:[] ~body in
  Lwt.return_unit

let set_message_reaction ~bot_token ~chat_id ~message_id ~emoji () =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/setMessageReaction" api_base bot_token in
  let reaction =
    `Assoc [ ("type", `String "emoji"); ("emoji", `String emoji) ]
  in
  let body =
    `Assoc
      [
        ("chat_id", `String chat_id);
        ("message_id", `Int message_id);
        ("reaction", `List [ reaction ]);
      ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body = Http_client.post_json ~uri ~headers:[] ~body in
  Lwt.return_unit

let send_message ?(disable_notification = false) ?parse_mode ~bot_token ~chat_id
    ~text () =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/sendMessage" api_base bot_token in
  let base_fields =
    [
      ("chat_id", `String chat_id);
      ("text", `String text);
      ("disable_notification", `Bool disable_notification);
    ]
  in
  let fields =
    match parse_mode with
    | Some mode -> ("parse_mode", `String mode) :: base_fields
    | None -> base_fields
  in
  let body = `Assoc fields |> Yojson.Safe.to_string in
  let* status, _body = Http_client.post_json ~uri ~headers:[] ~body in
  if parse_mode <> None && status >= 400 then
    let plain_body = `Assoc base_fields |> Yojson.Safe.to_string in
    let* _status, _body =
      Http_client.post_json ~uri ~headers:[] ~body:plain_body
    in
    Lwt.return_unit
  else Lwt.return_unit

let send_chunked ?(disable_notification = false) ?parse_mode ~bot_token ~chat_id
    ~text () =
  let open Lwt.Syntax in
  Lwt_list.iter_s
    (fun chunk ->
      send_message ~disable_notification ?parse_mode ~bot_token ~chat_id
        ~text:chunk ())
    (chunk_text text)

type chunk_sender =
  ?disable_notification:bool ->
  ?parse_mode:string ->
  bot_token:string ->
  chat_id:string ->
  text:string ->
  unit ->
  unit Lwt.t

let send_silent_chunked (send_chunked : chunk_sender) ~bot_token ~chat_id ~text
    =
  send_chunked ~disable_notification:true ~bot_token ~chat_id ~text ()

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
          ()
      end
      else begin
        Logs.warn (fun m ->
            m "Telegram: TOTP pairing failed for chat_id=%s" chat_id);
        send_message ~bot_token ~chat_id
          ~text:
            "Invalid code. Please try again with a valid TOTP code from `clawq \
             otp-show`."
          ()
      end
  | _ ->
      send_message ~bot_token ~chat_id
        ~text:"TOTP pairing is not configured for this account." ()

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
         Get the code from `clawq otp-show` command."
      ())
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
            ()
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
        let send_first text =
          send_message_with_id ~disable_notification:true ~bot_token
            ~chat_id:update.chat_id ~text ()
        in
        let edit msg_id text =
          edit_message ~bot_token ~chat_id:update.chat_id ~message_id:msg_id
            ~text ()
        in
        let send_progress, _get_final =
          Update_tool.make_progress_sender ~send_first ~edit
            ~mode:Update_tool.Auto ()
        in
        let run_update_command =
          match run_update_command with
          | Some run_update_command -> run_update_command
          | None ->
              fun ?(mode = Update_tool.Auto)
                ?prepare_restart
                ~send_progress
                ()
              ->
                Update_tool.run_update ?prepare_restart ~mode
                  ~is_draining:(fun () -> Session.is_draining session_mgr)
                  ~send_progress ()
        in
        let* _response =
          run_update_command
            ~prepare_restart:(fun () ->
              Restart_notify.write ~channel:"telegram"
                ~channel_id:update.chat_id;
              acknowledge_update ~bot_token ~update_id:update.update_id)
            ~send_progress ()
        in
        Lwt.return_unit
      else
        match Slash_commands.handle user_text with
        | Reply text -> send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | Reset ->
            let* () = Session.reset session_mgr ~key in
            send_message ~bot_token ~chat_id:update.chat_id
              ~text:Slash_commands.reset_message ()
        | Compact ->
            let* compacted = Session.compact session_mgr ~key in
            let text =
              if compacted then
                "Session history compacted. Older messages have been \
                 summarized."
              else
                "Nothing to compact — session history is already short enough."
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | Thinking Slash_commands.ShowThinking ->
            let current =
              (Session.get_config session_mgr).agent_defaults.reasoning_effort
            in
            send_message ~bot_token ~chat_id:update.chat_id
              ~text:(current_thinking_message current)
              ()
        | Thinking (Slash_commands.SetThinking level) ->
            send_message ~bot_token ~chat_id:update.chat_id
              ~text:
                (set_thinking_level ~session_mgr ~chat_id:update.chat_id
                   ~user_id:update.user_id level)
              ()
        | Delegate prompt ->
            let* () =
              send_message ~bot_token ~chat_id:update.chat_id
                ~text:"Delegating to a temporary session..." ()
            in
            let* result = Session.delegate_turn session_mgr ~prompt in
            let text =
              match result with Ok response -> response | Error msg -> msg
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | NotACommand -> (
            let msg = user_text in
            let agent_defaults =
              (Session.get_config session_mgr).agent_defaults
            in
            let use_consolidated =
              agent_defaults.show_tool_calls
              && agent_defaults.tool_status_mode = "consolidated"
            in
            let current_turn_has_tools = ref false in
            let thinking_buf = Buffer.create 256 in
            let status_msg =
              if use_consolidated then
                let status_notifier : Status_message.notifier =
                  {
                    send =
                      (fun ?parse_mode text ->
                        let pm =
                          match parse_mode with
                          | Some m -> Some m
                          | None -> Some "MarkdownV2"
                        in
                        let text = Telegram_format.markdown_to_mdv2 text in
                        send_message_with_id ~disable_notification:true
                          ?parse_mode:pm ~bot_token ~chat_id:update.chat_id
                          ~text ());
                    edit =
                      (fun msg_id ?parse_mode text ->
                        let pm =
                          match parse_mode with
                          | Some m -> Some m
                          | None -> Some "MarkdownV2"
                        in
                        let text = Telegram_format.markdown_to_mdv2 text in
                        edit_message ?parse_mode:pm ~bot_token
                          ~chat_id:update.chat_id ~message_id:msg_id ~text ());
                    delete =
                      (fun msg_id ->
                        delete_message ~bot_token ~chat_id:update.chat_id
                          ~message_id:msg_id ());
                  }
                in
                Some
                  (Status_message.create ~notifier:status_notifier
                     ~parse_mode:"MarkdownV2" ())
              else None
            in
            let visibility = Stream_visibility.create () in
            let send_expandable ~name ~result ~is_error =
              if is_error then
                let formatted = Telegram_format.format_error_trace result in
                send_chunked ~disable_notification:true ~parse_mode:"MarkdownV2"
                  ~bot_token ~chat_id:update.chat_id ~text:formatted ()
              else
                match Telegram_format.format_sensitive_result ~name result with
                | Some formatted ->
                    send_chunked ~disable_notification:true
                      ~parse_mode:"MarkdownV2" ~bot_token
                      ~chat_id:update.chat_id ~text:formatted ()
                | None -> (
                    match
                      Telegram_format.format_verbose_result ~name result
                    with
                    | Some formatted ->
                        send_chunked ~disable_notification:true
                          ~parse_mode:"MarkdownV2" ~bot_token
                          ~chat_id:update.chat_id ~text:formatted ()
                    | None -> Lwt.return_unit)
            in
            let on_chunk chunk =
              match status_msg with
              | Some sm -> (
                  match chunk with
                  | Provider.ToolStart { id; name; arguments } ->
                      let summary =
                        Stream_visibility.summarize_tool_arguments ~name
                          arguments
                      in
                      let* () =
                        Status_message.tool_start sm ~id ~name ~summary
                      in
                      let action = chat_action_for_tool name in
                      Lwt.catch
                        (fun () ->
                          send_chat_action ~bot_token ~chat_id:update.chat_id
                            ~action)
                        (fun _exn -> Lwt.return_unit)
                  | Provider.ToolResult { id; name; result; is_error } ->
                      let open Lwt.Syntax in
                      let* () =
                        Status_message.tool_result sm ~id ~name ~result
                          ~is_error
                      in
                      (* Cap cache size to prevent unbounded growth *)
                      if Hashtbl.length tool_result_cache > 200 then
                        Hashtbl.clear tool_result_cache;
                      Hashtbl.replace tool_result_cache
                        (update.chat_id ^ ":" ^ id)
                        (Stream_visibility.truncate_text ~max_chars:2000 result);
                      current_turn_has_tools := true;
                      (* Only send inline messages for errors; non-error
                         output is available via "Show Details" button *)
                      if is_error then send_expandable ~name ~result ~is_error
                      else Lwt.return_unit
                  | Provider.ThinkingDelta text ->
                      if agent_defaults.show_thinking then
                        Buffer.add_string thinking_buf text;
                      Lwt.return_unit
                  | Provider.Delta _ | Provider.ToolCallDelta _
                  | Provider.ToolOutputDelta _ | Provider.Done ->
                      Lwt.return_unit)
              | None -> (
                  let open Lwt.Syntax in
                  let* () =
                    match chunk with
                    | Provider.ToolStart { name; _ } ->
                        let action = chat_action_for_tool name in
                        Lwt.catch
                          (fun () ->
                            send_chat_action ~bot_token ~chat_id:update.chat_id
                              ~action)
                          (fun _exn -> Lwt.return_unit)
                    | _ -> Lwt.return_unit
                  in
                  let settings : Stream_visibility.settings =
                    {
                      show_thinking = agent_defaults.show_thinking;
                      show_tool_calls = agent_defaults.show_tool_calls;
                      notify_tool_starts = true;
                      notify_tool_successes = false;
                    }
                  in
                  let* () =
                    Stream_visibility.on_chunk visibility ~settings
                      ~notify:(fun text ->
                        let text = Telegram_format.markdown_to_mdv2 text in
                        send_chunked ~disable_notification:true
                          ~parse_mode:"MarkdownV2" ~bot_token
                          ~chat_id:update.chat_id ~text ())
                      chunk
                  in
                  match chunk with
                  | Provider.ToolResult { name; result; is_error; _ } ->
                      send_expandable ~name ~result ~is_error
                  | _ -> Lwt.return_unit)
            in
            let* result =
              Session.with_registered_notifier session_mgr ~key
                ~notify:(fun text ->
                  send_chunked ~disable_notification:true
                    ~parse_mode:"MarkdownV2" ~bot_token ~chat_id:update.chat_id
                    ~text:(Telegram_format.markdown_to_mdv2 text)
                    ())
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
                let* () =
                  match status_msg with
                  | Some sm -> Status_message.finalize sm
                  | None -> Lwt.return_unit
                in
                if Session.is_queued_message_response response then
                  Lwt.return_unit
                else
                  let* () =
                    if status_msg <> None && !current_turn_has_tools then
                      let* _msg_id =
                        send_message_with_keyboard ~disable_notification:true
                          ~bot_token ~chat_id:update.chat_id
                          ~text:"\xF0\x9F\x93\x8B Tool output available"
                          ~buttons:[ ("Show Details", "show_details") ]
                          ()
                      in
                      Lwt.return_unit
                    else Lwt.return_unit
                  in
                  let thinking =
                    match status_msg with
                    | Some _ -> Buffer.contents thinking_buf
                    | None -> Stream_visibility.thinking_text visibility
                  in
                  let* () =
                    if thinking <> "" then
                      send_chunked ~parse_mode:"MarkdownV2" ~bot_token
                        ~chat_id:update.chat_id
                        ~text:("_" ^ Telegram_format.escape_mdv2 thinking ^ "_")
                        ()
                    else Lwt.return_unit
                  in
                  let* () =
                    send_chunked ~bot_token ~chat_id:update.chat_id
                      ~text:response ()
                  in
                  let* () =
                    Lwt.catch
                      (fun () ->
                        set_message_reaction ~bot_token ~chat_id:update.chat_id
                          ~message_id:update.message_id ~emoji:"\xE2\x9C\x85" ())
                      (fun _exn -> Lwt.return_unit)
                  in
                  if not (Session.take_response_deferred session_mgr ~key) then
                    Session.mark_response_sent session_mgr ~key;
                  Lwt.return_unit
            | Error err ->
                Logs.err (fun m ->
                    m "Agent error for chat_id=%s: %s" update.chat_id err);
                let* () =
                  match status_msg with
                  | Some sm -> Status_message.finalize sm
                  | None -> Lwt.return_unit
                in
                let* () =
                  send_message ~bot_token ~chat_id:update.chat_id
                    ~text:
                      (Printf.sprintf
                         "Sorry, an error occurred processing your message: %s"
                         err)
                    ()
                in
                let* () =
                  Lwt.catch
                    (fun () ->
                      set_message_reaction ~bot_token ~chat_id:update.chat_id
                        ~message_id:update.message_id ~emoji:"\xE2\x9A\xA0" ())
                    (fun _exn -> Lwt.return_unit)
                in
                if not (Session.take_response_deferred session_mgr ~key) then
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
  let conflict_backoff = ref 5.0 in
  let rec poll () =
    incr poll_count;
    if !poll_count <= 3 then
      Logs.info (fun m ->
          m "Telegram poll #%d for account '%s'" !poll_count name)
    else if !poll_count = 4 then
      Logs.info (fun m ->
          m "Telegram polling stable, suppressing routine poll logs for '%s'"
            name);
    let* poll_result =
      Lwt.catch
        (fun () -> get_updates ~bot_token ~offset:!offset ~timeout:30)
        (fun exn ->
          Logs.err (fun m ->
              m "Telegram poll error for '%s': %s" name (Printexc.to_string exn));
          let* () = Lwt_unix.sleep 5.0 in
          Lwt.return (Updates (0, [])))
    in
    let* max_uid, updates =
      match poll_result with
      | Updates (max_uid, updates) ->
          conflict_backoff := 5.0;
          Lwt.return (max_uid, updates)
      | Poll_error Conflict_webhook ->
          Logs.warn (fun m ->
              m
                "Telegram: clearing webhook for '%s' before resuming \
                 long-polling"
                name);
          let* () = delete_webhook ~bot_token in
          let* () = Lwt_unix.sleep 2.0 in
          Lwt.return (0, [])
      | Poll_error Conflict_duplicate_poller ->
          Logs.warn (fun m ->
              m "Telegram: another poller is active for '%s', backing off %.0fs"
                name !conflict_backoff);
          let* () = Lwt_unix.sleep !conflict_backoff in
          conflict_backoff := Float.min (!conflict_backoff *. 2.0) 60.0;
          Lwt.return (0, [])
      | Poll_error (Other_error _) ->
          let* () = Lwt_unix.sleep 5.0 in
          Lwt.return (0, [])
    in
    if max_uid + 1 > !offset then offset := max_uid + 1;
    List.iter
      (fun update ->
        offset := update.update_id + 1;
        if should_process_update update then
          Lwt.async (fun () ->
              Lwt.catch
                (fun () ->
                  handle_update ~bot_token ~account ~session_mgr
                    ?run_update_command ?chat_limiter update)
                (fun exn ->
                  Logs.err (fun m ->
                      m "Telegram: handle_update error for update_id=%d: %s"
                        update.update_id (Printexc.to_string exn));
                  Lwt.return_unit))
        else
          Logs.info (fun m ->
              m "Telegram: ignoring duplicate update update_id=%d chat_id=%s"
                update.update_id update.chat_id))
      updates;
    let* () =
      let rec drain_callbacks () =
        if Queue.is_empty pending_callbacks then Lwt.return_unit
        else
          let cb = Queue.pop pending_callbacks in
          if cb.cb_bot_token <> bot_token then begin
            (* Re-queue callbacks for other accounts *)
            Queue.push cb pending_callbacks;
            Lwt.return_unit
          end
          else
            let* () =
              Lwt.catch
                (fun () ->
                  match cb.data with
                  | "show_details" ->
                      let prefix = cb.cb_chat_id ^ ":" in
                      let details =
                        Hashtbl.fold
                          (fun key result acc ->
                            if String.starts_with ~prefix key then
                              acc
                              ^ Stream_visibility.truncate_text ~max_chars:300
                                  result
                              ^ "\n---\n"
                            else acc)
                          tool_result_cache ""
                      in
                      let text =
                        if details = "" then "No details available."
                        else details
                      in
                      let* () =
                        answer_callback_query ~bot_token
                          ~callback_query_id:cb.callback_query_id ()
                      in
                      let* () =
                        send_message ~disable_notification:true ~bot_token
                          ~chat_id:cb.cb_chat_id ~text ()
                      in
                      (* Only clear entries for this chat *)
                      Hashtbl.filter_map_inplace
                        (fun key v ->
                          if String.starts_with ~prefix key then None
                          else Some v)
                        tool_result_cache;
                      Lwt.return_unit
                  | _ ->
                      answer_callback_query ~bot_token
                        ~callback_query_id:cb.callback_query_id
                        ~text:"Unknown action" ())
                (fun exn ->
                  Logs.err (fun m ->
                      m "Telegram: callback handling error: %s"
                        (Printexc.to_string exn));
                  Lwt.return_unit)
            in
            drain_callbacks ()
      in
      drain_callbacks ()
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
                  Logs.info (fun m ->
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
