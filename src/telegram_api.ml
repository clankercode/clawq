let api_base = ref "https://api.telegram.org/bot"

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
      Session.update_config ~source:"telegram" session_mgr
        { cfg with agent_defaults };
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

let redact_token = String_util.redact_token

type update = {
  update_id : int;
  message_id : int;
  chat_id : string;
  user_id : string;
  text : string;
  voice_file_id : string option;
  voice_duration : int option;
  voice_file_size : int option;
  photo_file_id : string option;
  sticker_file_id : string option;
  document_file_id : string option;
  document_name : string option;
  document_mime_type : string option;
  caption : string option;
}

type callback_query = {
  cb_bot_token : string;
  callback_query_id : string;
  cb_chat_id : string;
  cb_user_id : string;
  cb_message_id : int;
  data : string;
}

type poll_answer = {
  pa_poll_id : string;
  pa_user_id : string;
  pa_option_ids : int list;
}

let pending_callbacks : callback_query Queue.t = Queue.create ()
let pending_poll_answers : poll_answer Queue.t = Queue.create ()

(* callback_id -> (session_key, label, created_at) *)
let callback_routing : (string, string * string * float) Hashtbl.t =
  Hashtbl.create 64

(* poll_id -> (session_key, chat_id, bot_token, options, created_at) *)
let poll_routing :
    (string, string * string * string * string list * float) Hashtbl.t =
  Hashtbl.create 16

type pending_text_update = {
  mutable update : update;
  mutable last_seen_at : float;
  mutable generation : int;
}

type tool_result_detail_entry = {
  chat_id : string;
  user_id : string;
  text : string;
}

(* callback_data -> scoped details text *)
let tool_result_details : (string, tool_result_detail_entry) Hashtbl.t =
  Hashtbl.create 64

let tool_result_details_order : string Queue.t = Queue.create ()

(* Tracks the highest message_id seen per chat_id — updated on every
   incoming update and every bot-sent message.  The consolidated status
   message uses this to detect when it has been displaced by newer chat
   activity and should be re-anchored at the bottom rather than edited
   in place. *)
let latest_chat_msg_id : (string, int) Hashtbl.t = Hashtbl.create 64
let recently_seen_updates : (string, float) Hashtbl.t = Hashtbl.create 256

(* Per-chat mutex for outbound message serialization - prevents reordering *)
let outbound_mutexes : (string, Lwt_mutex.t) Hashtbl.t = Hashtbl.create 64

let get_outbound_mutex chat_id =
  match Hashtbl.find_opt outbound_mutexes chat_id with
  | Some m -> m
  | None -> (
      let m = Lwt_mutex.create () in
      Hashtbl.replace outbound_mutexes chat_id m;
      (* F3: re-check after insertion to handle concurrent creation race.
         If two Lwt threads both passed find_opt=None, the second replace
         overwrites the first. Re-fetch to return the canonical mutex. *)
      match Hashtbl.find_opt outbound_mutexes chat_id with
      | Some existing -> existing
      | None -> m)

let with_outbound_lock ~chat_id f =
  let mutex = get_outbound_mutex chat_id in
  Lwt_util.with_lock_timeout
    ~label:(Printf.sprintf "tg_outbound[%s]" chat_id)
    mutex f

(* Per-chat 429 rate-limit cooldown: chat_id -> Unix time at which sends resume *)
let outbound_rate_limited_until : (string, float) Hashtbl.t = Hashtbl.create 16

let is_outbound_rate_limited chat_id =
  match Hashtbl.find_opt outbound_rate_limited_until chat_id with
  | None -> false
  | Some until ->
      if Unix.gettimeofday () >= until then (
        Hashtbl.remove outbound_rate_limited_until chat_id;
        false)
      else true

(* Parse retry_after seconds from a Telegram 429 response body.
   Body format: {"ok":false,"parameters":{"retry_after":N},...} *)
let parse_tg_retry_after body =
  try
    let json = Yojson.Safe.from_string body in
    match
      json
      |> Yojson.Safe.Util.member "parameters"
      |> Yojson.Safe.Util.member "retry_after"
    with
    | `Int n -> float_of_int n
    | `Float f -> f
    | _ -> 60.0
  with _ -> 60.0

let record_outbound_rate_limit ~chat_id ~body =
  let retry_after = parse_tg_retry_after body in
  Logs.warn (fun m ->
      m
        "Telegram 429 rate limit for chat_id=%s; suppressing outbound sends \
         for %.0fs"
        chat_id retry_after);
  Hashtbl.replace outbound_rate_limited_until chat_id
    (Unix.gettimeofday () +. retry_after)

let is_valid_message_id message_id =
  match int_of_string_opt message_id with
  | Some id when id > 0 -> true
  | _ -> false

let is_not_modified_error resp_body =
  try
    let json = Yojson.Safe.from_string resp_body in
    let desc =
      json
      |> Yojson.Safe.Util.member "description"
      |> Yojson.Safe.Util.to_string
    in
    let lower = String.lowercase_ascii desc in
    let re = Str.regexp_string "message is not modified" in
    try
      ignore (Str.search_forward re lower 0);
      true
    with Not_found -> false
  with _ -> false

let html_fallback_to_plain_text text =
  let with_newlines =
    text
    |> Str.global_replace (Str.regexp_case_fold "<br */?>") "\n"
    |> Str.global_replace (Str.regexp_case_fold "</p>") "\n"
    |> Str.global_replace (Str.regexp_case_fold "</div>") "\n"
    |> Str.global_replace (Str.regexp_case_fold "</li>") "\n"
  in
  let without_tags =
    Str.global_replace (Str.regexp {|<[^>]*>|}) "" with_newlines
  in
  without_tags
  |> Str.global_replace (Str.regexp "&lt;") "<"
  |> Str.global_replace (Str.regexp "&gt;") ">"
  |> Str.global_replace (Str.regexp "&quot;") "\""
  |> Str.global_replace (Str.regexp "&#39;") "'"
  |> Str.global_replace (Str.regexp "&amp;") "&"

let details_callback_prefix = "show_details:"

let fresh_details_callback_data () =
  let seed =
    Printf.sprintf "%f:%d:%d" (Unix.gettimeofday ()) (Random.bits ())
      (Hashtbl.length tool_result_details)
  in
  details_callback_prefix ^ String.sub (Digest.to_hex (Digest.string seed)) 0 16

let register_tool_result_details ~chat_id ~user_id text =
  let callback_data = fresh_details_callback_data () in
  Queue.push callback_data tool_result_details_order;
  Hashtbl.replace tool_result_details callback_data { chat_id; user_id; text };
  while Hashtbl.length tool_result_details > 256 do
    if Queue.is_empty tool_result_details_order then
      Hashtbl.clear tool_result_details
    else
      let oldest = Queue.pop tool_result_details_order in
      Hashtbl.remove tool_result_details oldest
  done;
  callback_data

let take_tool_result_details ~chat_id ~user_id callback_data =
  match Hashtbl.find_opt tool_result_details callback_data with
  | None -> None
  | Some { chat_id = detail_chat_id; user_id = detail_user_id; text }
    when detail_chat_id = chat_id && detail_user_id = user_id ->
      Hashtbl.remove tool_result_details callback_data;
      Some text
  | Some _ -> None

let format_tool_result_detail ~name ~result =
  let trimmed = String.trim result in
  let body =
    if trimmed = "" then "[empty output]"
    else Stream_visibility.truncate_text ~max_chars:300 trimmed
  in
  Printf.sprintf "%s\n%s" name body

let pending_text_updates : (string, pending_text_update) Hashtbl.t =
  Hashtbl.create 64

type typing_watcher = Typing_indicator.typing_watcher

let duplicate_update_ttl_seconds = 600.0
let text_coalesce_window_seconds = ref 0.15

(* Tracks message IDs whose reactions should be kept in sync per session key *)
let reactions : int Reaction_tracker.t = Reaction_tracker.create ()

(* Telegram Bot API allows only a preset set of emoji for setMessageReaction.
   Source: https://core.telegram.org/bots/api#reactiontypeemoji
   This list must be kept in sync with the Telegram API documentation. *)
let valid_reaction_emojis =
  [
    "\xF0\x9F\x91\x8D" (* 👍 *);
    "\xF0\x9F\x91\x8E" (* 👎 *);
    "\xE2\x9D\xA4" (* ❤ *);
    "\xF0\x9F\x94\xA5" (* 🔥 *);
    "\xF0\x9F\xA5\xB0" (* 🥰 *);
    "\xF0\x9F\x91\x8F" (* 👏 *);
    "\xF0\x9F\x98\x81" (* 😁 *);
    "\xF0\x9F\xA4\x94" (* 🤔 *);
    "\xF0\x9F\xA4\xAF" (* 🤯 *);
    "\xF0\x9F\x98\xB1" (* 😱 *);
    "\xF0\x9F\xA4\xAC" (* 🤬 *);
    "\xF0\x9F\x98\xA2" (* 😢 *);
    "\xF0\x9F\x8E\x89" (* 🎉 *);
    "\xF0\x9F\xA4\xA9" (* 🤩 *);
    "\xF0\x9F\xA4\xAE" (* 🤮 *);
    "\xF0\x9F\x92\xA9" (* 💩 *);
    "\xF0\x9F\x99\x8F" (* 🙏 *);
    "\xF0\x9F\x91\x8C" (* 👌 *);
    "\xF0\x9F\x95\x8A" (* 🕊 *);
    "\xF0\x9F\xA4\xA1" (* 🤡 *);
    "\xF0\x9F\xA5\xB1" (* 🥱 *);
    "\xF0\x9F\xA5\xB4" (* 🥴 *);
    "\xF0\x9F\x98\x8D" (* 😍 *);
    "\xF0\x9F\x90\xB3" (* 🐳 *);
    "\xF0\x9F\x8C\x9A" (* 🌚 *);
    "\xF0\x9F\x8C\xAD" (* 🌭 *);
    "\xF0\x9F\x92\xAF" (* 💯 *);
    "\xF0\x9F\xA4\xA3" (* 🤣 *);
    "\xE2\x9A\xA1" (* ⚡ *);
    "\xF0\x9F\x8D\x8C" (* 🍌 *);
    "\xF0\x9F\x8F\x86" (* 🏆 *);
    "\xF0\x9F\x92\x94" (* 💔 *);
    "\xF0\x9F\xA4\xA8" (* 🤨 *);
    "\xF0\x9F\x98\x90" (* 😐 *);
    "\xF0\x9F\x8D\x93" (* 🍓 *);
    "\xF0\x9F\x8D\xBE" (* 🍾 *);
    "\xF0\x9F\x92\x8B" (* 💋 *);
    "\xF0\x9F\x96\x95" (* 🖕 *);
    "\xF0\x9F\x98\x88" (* 😈 *);
    "\xF0\x9F\x98\xB4" (* 😴 *);
    "\xF0\x9F\x98\xAD" (* 😭 *);
    "\xF0\x9F\xA4\x93" (* 🤓 *);
    "\xF0\x9F\x91\xBB" (* 👻 *);
    "\xF0\x9F\x91\x80" (* 👀 *);
    "\xF0\x9F\x8E\x83" (* 🎃 *);
    "\xF0\x9F\x99\x88" (* 🙈 *);
    "\xF0\x9F\x98\x87" (* 😇 *);
    "\xF0\x9F\x98\xA8" (* 😨 *);
    "\xF0\x9F\xA4\x9D" (* 🤝 *);
    "\xE2\x9C\x8D" (* ✍ *);
    "\xF0\x9F\xA4\x97" (* 🤗 *);
    "\xF0\x9F\xAB\xA1" (* 🫡 *);
    "\xF0\x9F\x8E\x85" (* 🎅 *);
    "\xF0\x9F\x8E\x84" (* 🎄 *);
    "\xE2\x98\x83" (* ☃ *);
    "\xF0\x9F\x92\x85" (* 💅 *);
    "\xF0\x9F\xA4\xAA" (* 🤪 *);
    "\xF0\x9F\x97\xBF" (* 🗿 *);
    "\xF0\x9F\x86\x92" (* 🆒 *);
    "\xF0\x9F\x92\x98" (* 💘 *);
    "\xF0\x9F\x99\x89" (* 🙉 *);
    "\xF0\x9F\xA6\x84" (* 🦄 *);
    "\xF0\x9F\x98\x98" (* 😘 *);
    "\xF0\x9F\x92\x8A" (* 💊 *);
    "\xF0\x9F\x99\x8A" (* 🙊 *);
    "\xF0\x9F\x98\x8E" (* 😎 *);
    "\xF0\x9F\x91\xBE" (* 👾 *);
    "\xF0\x9F\x98\xA1" (* 😡 *);
  ]

(* Reaction emojis used by clawq for session state — aliases for backward compat *)
let reaction_emoji_received = Connector_status.Telegram.phase_emoji Received
let reaction_emoji_tools = Connector_status.Telegram.phase_emoji Processing
let reaction_emoji_done = Connector_status.Telegram.phase_emoji Completed
let reaction_emoji_error = Connector_status.Telegram.phase_emoji Failed

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

let text_coalesce_key ~bot_token (u : update) =
  String.concat ":" [ bot_token; u.chat_id; u.user_id ]

let has_media_or_caption (u : update) =
  u.voice_file_id <> None || u.photo_file_id <> None
  || u.sticker_file_id <> None || u.document_file_id <> None
  || u.caption <> None

let is_command_text text =
  let trimmed = String.trim text in
  String.length trimmed > 0 && trimmed.[0] = '/'

let is_text_coalescing_candidate (u : update) =
  u.text <> "" && (not (has_media_or_caption u)) && not (is_command_text u.text)

let can_coalesce_text_updates ~now older newer =
  is_text_coalescing_candidate older.update
  && is_text_coalescing_candidate newer
  && older.update.chat_id = newer.chat_id
  && older.update.user_id = newer.user_id
  && newer.message_id = older.update.message_id + 1
  && now -. older.last_seen_at <= !text_coalesce_window_seconds

let merge_text_updates older (newer : update) =
  {
    newer with
    text = older.update.text ^ newer.text;
    voice_file_id = None;
    voice_duration = None;
    voice_file_size = None;
    photo_file_id = None;
    sticker_file_id = None;
    document_file_id = None;
    document_name = None;
    document_mime_type = None;
    caption = None;
  }

type poll_error =
  | Conflict_webhook
  | Conflict_duplicate_poller
  | Other_error of int

type poll_result = Updates of int * update list | Poll_error of poll_error

let delete_webhook ~bot_token =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/deleteWebhook" !api_base bot_token in
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
  let request_timeout_s = float_of_int (timeout + 15) in
  let uri =
    Printf.sprintf "%s%s/getUpdates?offset=%d&timeout=%d&allowed_updates=%s"
      !api_base bot_token offset timeout
      "%5B%22message%22%2C%22callback_query%22%2C%22poll_answer%22%5D"
  in
  let* status, body =
    Http_client.get_with_timeout ~timeout_s:request_timeout_s ~uri ~headers:[]
  in
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
            let voice_duration =
              try Some (msg |> member "voice" |> member "duration" |> to_int)
              with _ -> None
            in
            let voice_file_size =
              try Some (msg |> member "voice" |> member "file_size" |> to_int)
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
            (* Static stickers only — skip animated and video stickers *)
            let sticker_file_id =
              try
                let sticker = msg |> member "sticker" in
                let is_animated =
                  try sticker |> member "is_animated" |> to_bool
                  with _ -> false
                in
                let is_video =
                  try sticker |> member "is_video" |> to_bool with _ -> false
                in
                if is_animated || is_video then None
                else Some (sticker |> member "file_id" |> to_string)
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
            let document_mime_type =
              try
                Some
                  (msg |> member "document" |> member "mime_type" |> to_string)
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
                voice_duration;
                voice_file_size;
                photo_file_id;
                sticker_file_id;
                document_file_id;
                document_name;
                document_mime_type;
                caption;
              }
          with _ -> (
            let open Yojson.Safe.Util in
            try
              let cq = u |> member "callback_query" in
              if cq = `Null then raise Not_found;
              let callback_query_id = cq |> member "id" |> to_string in
              let from = cq |> member "from" in
              let cb_user_id =
                try from |> member "id" |> to_int |> string_of_int
                with _ -> "0"
              in
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
                  cb_user_id;
                  cb_message_id;
                  data;
                }
                pending_callbacks;
              None
            with _ -> (
              (* Try to parse as poll_answer *)
              try
                let open Yojson.Safe.Util in
                let pa = u |> member "poll_answer" in
                if pa = `Null then raise Not_found;
                let pa_poll_id = pa |> member "poll_id" |> to_string in
                let pa_user_id =
                  pa |> member "user" |> member "id" |> to_int |> string_of_int
                in
                let pa_option_ids =
                  pa |> member "option_ids" |> to_list |> List.map to_int
                in
                Queue.push
                  { pa_poll_id; pa_user_id; pa_option_ids }
                  pending_poll_answers;
                None
              with _ ->
                let update_id =
                  try
                    let open Yojson.Safe.Util in
                    u |> member "update_id" |> to_int
                  with _ -> -1
                in
                Logs.debug (fun m ->
                    m "Telegram: dropping malformed update (update_id=%d)"
                      update_id);
                None)))
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
    Printf.sprintf "%s%s/getUpdates?offset=%d&timeout=0" !api_base bot_token
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

let telegram_delegate_prompt ~user_prompt =
  String.concat "\n"
    [
      user_prompt;
      "";
      "[Response format: Telegram HTML]";
      "Your response will be sent as a single Telegram message (max 4096 \
       chars). Use HTML formatting:";
      "- <b>bold</b> for headings/emphasis";
      "- <i>italic</i> for secondary emphasis";
      "- <code>inline code</code> for identifiers";
      "- <pre>code blocks</pre>";
      "- <blockquote expandable>long content</blockquote> for collapsible \
       sections";
      "";
      "Pattern: Lead with a concise 2-3 line summary, then put details in \
       <blockquote expandable>...</blockquote>. Example:";
      "";
      "<b>Result:</b> Task completed successfully.";
      "<blockquote expandable>";
      "1. Read the config file";
      "2. Applied changes to src/main.ml";
      "3. Ran tests — all passed";
      "</blockquote>";
      "";
      "Escape literal < > & as &lt; &gt; &amp; outside tags.";
    ]

(* Split text into chunks no larger than max_len, preferring newline boundaries *)
let chunk_text ?(max_len = telegram_max_message_len) text =
  Channel_util.chunk_text ~max_len text

let send_chat_action ~bot_token ~chat_id ~action =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/sendChatAction" !api_base bot_token in
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

(* Core typing-indicator loop, parameterised for testability.
   [send_action] is called every [interval] seconds until the wrapped
   promise resolves.  Individual [send_action] failures are caught so
   the loop survives transient network/rate-limit errors.
   Uses [Lwt.pick] so the typing loop is properly cancelled when [p]
   resolves, preventing stale background sleeps. *)
let typing_loop ~send_action ~interval p =
  let open Lwt.Syntax in
  let rec loop () =
    let* () =
      Lwt.catch (fun () -> send_action ()) (fun _exn -> Lwt.return_unit)
    in
    let* () = Lwt_unix.sleep interval in
    loop ()
  in
  let* result =
    Lwt.pick
      [
        (let* v = p in
         Lwt.return v);
        (let* () = loop () in
         (* loop never resolves on its own; this branch is unreachable
            but typed to match *)
         Lwt.fail_with "typing_loop: unreachable");
      ]
  in
  Lwt.return result

(* Typing-indicator loop with refresh support.
   Like [typing_loop] but accepts a [Lwt_condition.t] trigger: signalling
   the condition causes an immediate typing re-send, which prevents gaps
   after outbound messages (Telegram clears typing when a message is sent).
   Returns [(result Lwt.t * refresh_fn)]. *)
let typing_loop_refreshable ~send_action ~interval p =
  let open Lwt.Syntax in
  let trigger = Lwt_condition.create () in
  let rec loop () =
    let* () =
      Lwt.catch (fun () -> send_action ()) (fun _exn -> Lwt.return_unit)
    in
    let* () =
      Lwt.pick [ Lwt_unix.sleep interval; Lwt_condition.wait trigger ]
    in
    loop ()
  in
  let result_p =
    Lwt.pick
      [
        (let* v = p in
         Lwt.return v);
        (let* () = loop () in
         Lwt.fail_with "typing_loop: unreachable");
      ]
  in
  let refresh () = Lwt_condition.signal trigger () in
  (result_p, refresh)

let with_typing ~bot_token ~chat_id p =
  typing_loop
    ~send_action:(fun () ->
      send_chat_action ~bot_token ~chat_id ~action:"typing")
    ~interval:3.0 p

(* Typing wrapper that returns a refresh function.
   Call [refresh ()] after any outbound message to immediately re-assert
   the typing indicator (Telegram clears it whenever a message is sent). *)
let with_typing_refreshable ~bot_token ~chat_id p =
  typing_loop_refreshable
    ~send_action:(fun () ->
      send_chat_action ~bot_token ~chat_id ~action:"typing")
    ~interval:3.0 p

(* Typing loop with a grace period before the indicator appears.
   If [p] resolves within [grace] seconds, no typing is shown at all.
   Parameterised like [typing_loop] so it can be tested without HTTP. *)
let typing_loop_deferred ~send_action ~interval ~grace p =
  let open Lwt.Syntax in
  let grace_timer = Lwt_unix.sleep grace in
  let p_resolved =
    let* _ = p in
    Lwt.return_unit
  in
  let* () = Lwt.choose [ grace_timer; p_resolved ] in
  if not (Lwt.is_sleeping p) then p else typing_loop ~send_action ~interval p

(* Typing wrapper with a grace period before the indicator appears.
   If [p] resolves within [grace] seconds, no typing is shown at all.
   Useful for autonomous continuation turns that often resolve instantly
   with STAY_IDLE — avoids a stale 5-second typing flash on Telegram. *)
let with_typing_deferred ~bot_token ~chat_id ~grace p =
  typing_loop_deferred
    ~send_action:(fun () ->
      send_chat_action ~bot_token ~chat_id ~action:"typing")
    ~interval:3.0 ~grace p

let typing_loop_live_activity = Typing_indicator.typing_loop_live_activity

let ensure_session_typing_watcher ~(session_mgr : Session.t) ~key ~bot_token
    ~chat_id =
  Typing_indicator.ensure_session_typing_watcher ~session_mgr ~key
    ~send_action:(fun () ->
      send_chat_action ~bot_token ~chat_id ~action:"typing")
    ~interval:3.0 ~idle_timeout:300.0

let send_message_with_id ?(disable_notification = true) ?parse_mode ~bot_token
    ~chat_id ~text () =
  let open Lwt.Syntax in
  if is_outbound_rate_limited chat_id then Lwt.return "0"
  else
    with_outbound_lock ~chat_id (fun () ->
        let uri = Printf.sprintf "%s%s/sendMessage" !api_base bot_token in
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
        if status = 429 then (
          record_outbound_rate_limit ~chat_id ~body:resp_body;
          Lwt.return "0")
        else
          let* status, resp_body =
            if
              parse_mode <> None && status = 400
              && not (is_not_modified_error resp_body)
            then (
              let plain_text =
                match parse_mode with
                | Some "HTML" -> html_fallback_to_plain_text text
                | _ -> text
              in
              Logs.warn (fun m ->
                  m
                    "Telegram sendMessage failed (HTTP %d, parse_mode=%s), \
                     retrying without parse_mode"
                    status
                    (Option.value parse_mode ~default:"none"));
              let plain_fields =
                [
                  ("chat_id", `String chat_id);
                  ("text", `String plain_text);
                  ("disable_notification", `Bool disable_notification);
                ]
              in
              let plain_body = `Assoc plain_fields |> Yojson.Safe.to_string in
              Http_client.post_json ~uri ~headers:[] ~body:plain_body)
            else Lwt.return (status, resp_body)
          in
          let msg_id =
            try
              let json = Yojson.Safe.from_string resp_body in
              let result = json |> Yojson.Safe.Util.member "result" in
              result
              |> Yojson.Safe.Util.member "message_id"
              |> Yojson.Safe.Util.to_int |> string_of_int
            with _ ->
              Logs.warn (fun m ->
                  m
                    "Telegram sendMessage did not return a message_id (HTTP \
                     %d, chat_id=%s, body=%s)"
                    status chat_id
                    (if String.length resp_body > 300 then
                       String.sub resp_body 0 300 ^ "..."
                     else resp_body));
              "0"
          in
          (match int_of_string_opt msg_id with
          | Some id ->
              let cur =
                Option.value ~default:0
                  (Hashtbl.find_opt latest_chat_msg_id chat_id)
              in
              if id > cur then Hashtbl.replace latest_chat_msg_id chat_id id
          | None -> ());
          Lwt.return msg_id)

let send_message_with_keyboard ?(disable_notification = true) ?parse_mode
    ~bot_token ~chat_id ~text ~buttons () =
  let open Lwt.Syntax in
  if is_outbound_rate_limited chat_id then Lwt.return "0"
  else
    with_outbound_lock ~chat_id (fun () ->
        let uri = Printf.sprintf "%s%s/sendMessage" !api_base bot_token in
        let inline_buttons =
          List.map
            (fun (label, callback_data) ->
              `Assoc
                [
                  ("text", `String label);
                  ("callback_data", `String callback_data);
                ])
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
        let* status, resp_body = Http_client.post_json ~uri ~headers:[] ~body in
        if status = 429 then (
          record_outbound_rate_limit ~chat_id ~body:resp_body;
          Lwt.return "0")
        else
          let msg_id =
            try
              let json = Yojson.Safe.from_string resp_body in
              json
              |> Yojson.Safe.Util.member "result"
              |> Yojson.Safe.Util.member "message_id"
              |> Yojson.Safe.Util.to_int |> string_of_int
            with _ ->
              Logs.warn (fun m ->
                  m
                    "Telegram sendMessage with keyboard did not return a \
                     message_id (HTTP %d, chat_id=%s)"
                    status chat_id);
              "0"
          in
          (match int_of_string_opt msg_id with
          | Some id ->
              let cur =
                Option.value ~default:0
                  (Hashtbl.find_opt latest_chat_msg_id chat_id)
              in
              if id > cur then Hashtbl.replace latest_chat_msg_id chat_id id
          | None -> ());
          Lwt.return msg_id)

let answer_callback_query ~bot_token ~callback_query_id ?(text = "") () =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/answerCallbackQuery" !api_base bot_token in
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
  (* Guard: message_id "0" means a prior send failed; skip to avoid
     a permanent silent-failure loop with the Telegram API. *)
  if message_id = "0" || is_outbound_rate_limited chat_id then Lwt.return_unit
  else
    let open Lwt.Syntax in
    with_outbound_lock ~chat_id (fun () ->
        let uri = Printf.sprintf "%s%s/editMessageText" !api_base bot_token in
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
        let* status, resp_body = Http_client.post_json ~uri ~headers:[] ~body in
        if status = 429 then (
          record_outbound_rate_limit ~chat_id ~body:resp_body;
          Lwt.return_unit)
        else if
          parse_mode <> None && status = 400
          && not (is_not_modified_error resp_body)
        then
          let plain_text =
            match parse_mode with
            | Some "HTML" -> html_fallback_to_plain_text text
            | _ -> text
          in
          let plain_fields =
            [
              ("chat_id", `String chat_id);
              ("message_id", `Int (try int_of_string message_id with _ -> 0));
              ("text", `String plain_text);
            ]
          in
          let plain_body = `Assoc plain_fields |> Yojson.Safe.to_string in
          let* _status, _body =
            Http_client.post_json ~uri ~headers:[] ~body:plain_body
          in
          Lwt.return_unit
        else Lwt.return_unit)

let delete_message ~bot_token ~chat_id ~message_id () =
  let open Lwt.Syntax in
  with_outbound_lock ~chat_id (fun () ->
      let uri = Printf.sprintf "%s%s/deleteMessage" !api_base bot_token in
      let body =
        `Assoc
          [
            ("chat_id", `String chat_id);
            ("message_id", `Int (try int_of_string message_id with _ -> 0));
          ]
        |> Yojson.Safe.to_string
      in
      let* _status, _body = Http_client.post_json ~uri ~headers:[] ~body in
      Lwt.return_unit)

let default_parse_mode parse_mode =
  match parse_mode with Some mode -> Some mode | None -> Some "MarkdownV2"

type status_transport = {
  send_with_id :
    ?disable_notification:bool ->
    ?parse_mode:string ->
    bot_token:string ->
    chat_id:string ->
    text:string ->
    unit ->
    string Lwt.t;
  edit_text :
    ?parse_mode:string ->
    bot_token:string ->
    chat_id:string ->
    message_id:string ->
    text:string ->
    unit ->
    unit Lwt.t;
  delete_message :
    bot_token:string ->
    chat_id:string ->
    message_id:string ->
    unit ->
    unit Lwt.t;
}

let default_status_transport =
  {
    send_with_id = send_message_with_id;
    edit_text = edit_message;
    delete_message;
  }

let make_status_notifier_with_transport transport ~bot_token ~chat_id :
    Status_message.notifier =
  {
    send =
      (fun ?parse_mode text ->
        let open Lwt.Syntax in
        let parse_mode = default_parse_mode parse_mode in
        let text =
          if parse_mode = Some "HTML" then text
          else Telegram_format.markdown_to_mdv2 text
        in
        let* message_id =
          transport.send_with_id ~disable_notification:true ?parse_mode
            ~bot_token ~chat_id ~text ()
        in
        if is_valid_message_id message_id then Lwt.return message_id
        else begin
          Logs.warn (fun m ->
              m
                "Telegram status send returned an invalid message_id for \
                 chat_id=%s; suppressing poisoned status id"
                chat_id);
          Lwt.return "0"
        end);
    edit =
      (fun message_id ?parse_mode text ->
        let open Lwt.Syntax in
        let parse_mode = default_parse_mode parse_mode in
        let text =
          if parse_mode = Some "HTML" then text
          else Telegram_format.markdown_to_mdv2 text
        in
        let* () =
          transport.edit_text ?parse_mode ~bot_token ~chat_id ~message_id ~text
            ()
        in
        Lwt.return None);
    delete =
      (fun message_id ->
        transport.delete_message ~bot_token ~chat_id ~message_id ());
  }

let make_status_notifier ~bot_token ~chat_id =
  make_status_notifier_with_transport default_status_transport ~bot_token
    ~chat_id

let set_message_reaction ~bot_token ~chat_id ~message_id ~emoji () =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/setMessageReaction" !api_base bot_token in
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
  let* status, _body = Http_client.post_json ~uri ~headers:[] ~body in
  if status < 200 || status >= 300 then
    Logs.warn (fun m ->
        m
          "Telegram setMessageReaction failed: status=%d chat_id=%s \
           message_id=%d"
          status chat_id message_id);
  Lwt.return_unit

let clear_message_reaction ~bot_token ~chat_id ~message_id () =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/setMessageReaction" !api_base bot_token in
  let body =
    `Assoc
      [
        ("chat_id", `String chat_id);
        ("message_id", `Int message_id);
        ("reaction", `List []);
      ]
    |> Yojson.Safe.to_string
  in
  let* status, _body = Http_client.post_json ~uri ~headers:[] ~body in
  if status < 200 || status >= 300 then
    Logs.warn (fun m ->
        m "Telegram clear reaction failed: status=%d chat_id=%s message_id=%d"
          status chat_id message_id);
  Lwt.return_unit

let send_message ?(disable_notification = true) ?parse_mode ~bot_token ~chat_id
    ~text () =
  let open Lwt.Syntax in
  if is_outbound_rate_limited chat_id then Lwt.return_unit
  else
    let mutex = get_outbound_mutex chat_id in
    Lwt_util.with_lock_timeout
      ~label:(Printf.sprintf "tg_outbound/sendMessage[%s]" chat_id) mutex
      (fun () ->
        let uri = Printf.sprintf "%s%s/sendMessage" !api_base bot_token in
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
        if status = 429 then (
          record_outbound_rate_limit ~chat_id ~body:resp_body;
          Lwt.return_unit)
        else if
          parse_mode <> None && status = 400
          && not (is_not_modified_error resp_body)
        then
          let plain_text =
            match parse_mode with
            | Some "HTML" -> html_fallback_to_plain_text text
            | _ -> text
          in
          let plain_fields =
            [
              ("chat_id", `String chat_id);
              ("text", `String plain_text);
              ("disable_notification", `Bool disable_notification);
            ]
          in
          let plain_body = `Assoc plain_fields |> Yojson.Safe.to_string in
          let* _status, _body =
            Http_client.post_json ~uri ~headers:[] ~body:plain_body
          in
          Lwt.return_unit
        else Lwt.return_unit)

let send_chunked ?(disable_notification = true) ?parse_mode ~bot_token ~chat_id
    ~text () =
  let open Lwt.Syntax in
  Lwt_list.iter_s
    (fun chunk ->
      send_message ~disable_notification ?parse_mode ~bot_token ~chat_id
        ~text:chunk ())
    (chunk_text text)

let send_chunked_html_with_fallback_using
    (sender :
      ?disable_notification:bool ->
      ?parse_mode:string ->
      bot_token:string ->
      chat_id:string ->
      text:string ->
      unit ->
      unit Lwt.t) ?(disable_notification = true) ~bot_token ~chat_id ~text () =
  let open Lwt.Syntax in
  let chunks = chunk_text text in
  Lwt_list.iter_s
    (fun chunk ->
      Lwt.catch
        (fun () ->
          sender ~disable_notification ~parse_mode:"HTML" ~bot_token ~chat_id
            ~text:chunk ())
        (fun _exn ->
          let plain = html_fallback_to_plain_text chunk in
          sender ~disable_notification ~bot_token ~chat_id ~text:plain ()))
    chunks

let send_chunked_html_with_fallback ?(disable_notification = true) ~bot_token
    ~chat_id ~text () =
  send_chunked_html_with_fallback_using send_message ~disable_notification
    ~bot_token ~chat_id ~text ()

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

let send_poll_api ?(disable_notification = true) ~bot_token ~chat_id ~question
    ~options ~allows_multiple () =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/sendPoll" !api_base bot_token in
  let body =
    `Assoc
      [
        ("chat_id", `String chat_id);
        ("question", `String question);
        ( "options",
          `List (List.map (fun o -> `Assoc [ ("text", `String o) ]) options) );
        ("is_anonymous", `Bool false);
        ("allows_multiple_answers", `Bool allows_multiple);
        ("disable_notification", `Bool disable_notification);
      ]
    |> Yojson.Safe.to_string
  in
  let* _status, resp_body = Http_client.post_json ~uri ~headers:[] ~body in
  let msg_id, poll_id =
    try
      let json = Yojson.Safe.from_string resp_body in
      let result = json |> Yojson.Safe.Util.member "result" in
      let mid =
        result
        |> Yojson.Safe.Util.member "message_id"
        |> Yojson.Safe.Util.to_int |> string_of_int
      in
      let pid =
        result
        |> Yojson.Safe.Util.member "poll"
        |> Yojson.Safe.Util.member "id"
        |> Yojson.Safe.Util.to_string
      in
      (mid, pid)
    with _ -> ("0", "0")
  in
  Lwt.return (msg_id, poll_id)

let cleanup_stale_routing () =
  let now = Unix.gettimeofday () in
  let max_age = 3600.0 in
  let stale_cbs = ref [] in
  Hashtbl.iter
    (fun key (_, _, created_at) ->
      if now -. created_at >= max_age then stale_cbs := key :: !stale_cbs)
    callback_routing;
  List.iter (Hashtbl.remove callback_routing) !stale_cbs;
  let stale_polls = ref [] in
  Hashtbl.iter
    (fun key (_, _, _, _, created_at) ->
      if now -. created_at >= max_age then stale_polls := key :: !stale_polls)
    poll_routing;
  List.iter (Hashtbl.remove poll_routing) !stale_polls

let set_my_commands ~bot_token =
  let open Lwt.Syntax in
  let sorted = Slash_commands.sorted_by_priority () in
  let cmds =
    `List
      (List.map
         (fun (c : Slash_commands.command) ->
           `Assoc
             [
               ("command", `String c.name);
               ("description", `String c.description);
             ])
         sorted)
  in
  let uri = Printf.sprintf "%s%s/setMyCommands" !api_base bot_token in
  let body = `Assoc [ ("commands", cmds) ] |> Yojson.Safe.to_string in
  let* status, resp_body =
    Http_client.post_json_with_timeout ~timeout_s:45.0 ~uri ~headers:[] ~body
  in
  if status >= 200 && status < 300 then
    Logs.info (fun m ->
        m "Telegram: registered %d slash commands" (List.length sorted))
  else
    Logs.warn (fun m ->
        m "Telegram: setMyCommands failed (HTTP %d) for token=%s: %s" status
          (redact_token bot_token)
          (if String.length resp_body > 500 then
             String.sub resp_body 0 500 ^ "..."
           else resp_body));
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

let download_telegram_file ~bot_token ~file_id =
  let open Lwt.Syntax in
  let get_file_uri =
    Printf.sprintf "%s%s/getFile?file_id=%s" !api_base bot_token file_id
  in
  let* _status, file_body = Http_client.get ~uri:get_file_uri ~headers:[] in
  let file_json = Yojson.Safe.from_string file_body in
  let file_path =
    Yojson.Safe.Util.(
      file_json |> member "result" |> member "file_path" |> to_string)
  in
  let download_uri =
    Printf.sprintf "https://api.telegram.org/file/bot%s/%s" bot_token file_path
  in
  let* _status, data = Http_client.get ~uri:download_uri ~headers:[] in
  Lwt.return data

let detect_mime_type data =
  let len = String.length data in
  if
    len >= 3
    && Char.code data.[0] = 0xFF
    && Char.code data.[1] = 0xD8
    && Char.code data.[2] = 0xFF
  then "image/jpeg"
  else if
    len >= 4
    && Char.code data.[0] = 0x89
    && data.[1] = 'P'
    && data.[2] = 'N'
    && data.[3] = 'G'
  then "image/png"
  else if
    len >= 4
    && data.[0] = 'G'
    && data.[1] = 'I'
    && data.[2] = 'F'
    && data.[3] = '8'
  then "image/gif"
  else if
    len >= 12
    && data.[0] = 'R'
    && data.[1] = 'I'
    && data.[2] = 'F'
    && data.[3] = 'F'
    && data.[8] = 'W'
    && data.[9] = 'E'
    && data.[10] = 'B'
    && data.[11] = 'P'
  then "image/webp"
  else if len >= 2 && data.[0] = 'B' && data.[1] = 'M' then "image/bmp"
  else "image/jpeg"

let send_document ?(disable_notification = true) ~bot_token ~chat_id ~filename
    ~content () =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/sendDocument" !api_base bot_token in
  let parts =
    [
      Http_client.Field { name = "chat_id"; value = chat_id };
      Http_client.Field
        {
          name = "disable_notification";
          value = (if disable_notification then "true" else "false");
        };
      Http_client.File
        {
          name = "document";
          filename;
          content_type = "application/octet-stream";
          data = content;
        };
    ]
  in
  let* status, body = Http_client.post_multipart ~uri ~headers:[] ~parts in
  if status >= 200 && status < 300 then (
    Logs.info (fun m ->
        m "Telegram: sent document filename=%s chat_id=%s" filename chat_id);
    Lwt.return (Ok (Yojson.Safe.from_string body)))
  else (
    Logs.warn (fun m ->
        m "Telegram: sendDocument failed (HTTP %d) chat_id=%s: %s" status
          chat_id body);
    Lwt.return (Error (Printf.sprintf "sendDocument HTTP %d: %s" status body)))
