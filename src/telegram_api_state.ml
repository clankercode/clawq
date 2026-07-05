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

let is_success_status status = status >= 200 && status < 300

let log_edit_message_failure ~chat_id ~message_id ~status ~body =
  Logs.warn (fun m ->
      m
        "Telegram editMessageText failed (HTTP %d, chat_id=%s, message_id=%s): \
         %s"
        status chat_id message_id
        (Stream_visibility.truncate_text ~max_chars:300 body))

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
let reaction_emoji_interrupt_ack = Connector_status.Telegram.interrupt_ack_emoji

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

let pending_text_update_ttl_seconds = 300.0

let cleanup_pending_text_updates ~now =
  let expired = ref [] in
  Hashtbl.iter
    (fun key (entry : pending_text_update) ->
      if now -. entry.last_seen_at >= pending_text_update_ttl_seconds then
        expired := key :: !expired)
    pending_text_updates;
  List.iter (Hashtbl.remove pending_text_updates) !expired

let should_process_update_counter = ref 0

let should_process_update (u : update) =
  let now = Unix.gettimeofday () in
  incr should_process_update_counter;
  if !should_process_update_counter mod 50 = 0 then begin
    cleanup_recently_seen_updates ~now;
    cleanup_pending_text_updates ~now
  end;
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
