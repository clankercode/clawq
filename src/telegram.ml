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

let redact_token = Tui_input.redact

type update = {
  update_id : int;
  message_id : int;
  chat_id : string;
  user_id : string;
  text : string;
  voice_file_id : string option;
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
  | None ->
      let m = Lwt_mutex.create () in
      Hashtbl.add outbound_mutexes chat_id m;
      m

let with_outbound_lock ~chat_id f =
  let mutex = get_outbound_mutex chat_id in
  Lwt_mutex.with_lock mutex f

let is_valid_message_id message_id =
  match int_of_string_opt message_id with
  | Some id when id > 0 -> true
  | _ -> false

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

type typing_watcher = { refresh : unit -> unit }

let typing_watchers : (string, typing_watcher) Hashtbl.t = Hashtbl.create 64
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
    Printf.sprintf "%s%s/getUpdates?offset=%d&timeout=%d&allowed_updates=%s"
      api_base bot_token offset timeout
      "%5B%22message%22%2C%22callback_query%22%2C%22poll_answer%22%5D"
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

let rec typing_loop_live_activity ~current_activity ~wait_for_change
    ~wait_for_refresh ~send_action ~interval ~idle_timeout () =
  let open Lwt.Syntax in
  let rec wait_until_active snapshot =
    if snapshot.Session.active then keep_active snapshot
    else
      let* next =
        Lwt.pick
          [
            (let* snapshot =
               wait_for_change ~after_generation:snapshot.Session.generation
             in
             Lwt.return (`Changed snapshot));
            (let* () = Lwt_unix.sleep idle_timeout in
             Lwt.return `Idle_timeout);
          ]
      in
      match next with
      | `Changed snapshot -> wait_until_active snapshot
      | `Idle_timeout -> Lwt.return_unit
  and keep_active snapshot =
    if not snapshot.Session.active then wait_until_active snapshot
    else
      let* () =
        Lwt.catch (fun () -> send_action ()) (fun _exn -> Lwt.return_unit)
      in
      let* next =
        Lwt.pick
          [
            (let* snapshot =
               wait_for_change ~after_generation:snapshot.Session.generation
             in
             Lwt.return (`Changed snapshot));
            (let* () =
               Lwt.pick [ Lwt_unix.sleep interval; wait_for_refresh () ]
             in
             let* snapshot = current_activity () in
             Lwt.return (`Tick snapshot));
          ]
      in
      match next with
      | `Changed snapshot -> keep_active snapshot
      | `Tick snapshot -> keep_active snapshot
  in
  let* snapshot = current_activity () in
  wait_until_active snapshot

let ensure_session_typing_watcher ~(session_mgr : Session.t) ~key ~bot_token
    ~chat_id =
  match Hashtbl.find_opt typing_watchers key with
  | Some watcher -> watcher
  | None ->
      let refresh_trigger = Lwt_condition.create () in
      let watcher =
        { refresh = (fun () -> Lwt_condition.broadcast refresh_trigger ()) }
      in
      Hashtbl.replace typing_watchers key watcher;
      Lwt.async (fun () ->
          Lwt.finalize
            (fun () ->
              typing_loop_live_activity
                ~current_activity:(fun () ->
                  Session.current_live_activity session_mgr ~key)
                ~wait_for_change:(fun ~after_generation ->
                  Session.wait_for_live_activity_change session_mgr ~key
                    ~after_generation)
                ~wait_for_refresh:(fun () -> Lwt_condition.wait refresh_trigger)
                ~send_action:(fun () ->
                  send_chat_action ~bot_token ~chat_id ~action:"typing")
                ~interval:3.0 ~idle_timeout:300.0 ())
            (fun () ->
              Hashtbl.remove typing_watchers key;
              Lwt.return_unit));
      watcher

let send_message_with_id ?(disable_notification = false) ?parse_mode ~bot_token
    ~chat_id ~text () =
  let open Lwt.Syntax in
  with_outbound_lock ~chat_id (fun () ->
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
      let* status, resp_body =
        if parse_mode <> None && status >= 400 then (
          Logs.warn (fun m ->
              m
                "Telegram sendMessage failed (HTTP %d, parse_mode=%s), \
                 retrying without parse_mode"
                status
                (Option.value parse_mode ~default:"none"));
          let plain_body = `Assoc base_fields |> Yojson.Safe.to_string in
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
                "Telegram sendMessage did not return a message_id (HTTP %d, \
                 chat_id=%s)"
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

let send_message_with_keyboard ?(disable_notification = false) ?parse_mode
    ~bot_token ~chat_id ~text ~buttons () =
  let open Lwt.Syntax in
  with_outbound_lock ~chat_id (fun () ->
      let uri = Printf.sprintf "%s%s/sendMessage" api_base bot_token in
      let inline_buttons =
        List.map
          (fun (label, callback_data) ->
            `Assoc
              [
                ("text", `String label); ("callback_data", `String callback_data);
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
  (* Guard: message_id "0" means a prior send failed; skip to avoid
     a permanent silent-failure loop with the Telegram API. *)
  if message_id = "0" then Lwt.return_unit
  else
    let open Lwt.Syntax in
    with_outbound_lock ~chat_id (fun () ->
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
        if parse_mode <> None && status >= 400 then
          let plain_body = `Assoc base_fields |> Yojson.Safe.to_string in
          let* _status, _body =
            Http_client.post_json ~uri ~headers:[] ~body:plain_body
          in
          Lwt.return_unit
        else Lwt.return_unit)

let delete_message ~bot_token ~chat_id ~message_id () =
  let open Lwt.Syntax in
  with_outbound_lock ~chat_id (fun () ->
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
        (* If newer messages exist in the chat, the status message has
           scrolled off the screen. Send a fresh one at the bottom first, then
           delete the prior one so status visibility is preserved even if the
           replacement send fails. *)
        let should_reanchor =
          match int_of_string_opt message_id with
          | None -> false
          | Some mid ->
              let latest =
                Option.value ~default:0
                  (Hashtbl.find_opt latest_chat_msg_id chat_id)
              in
              latest > mid
        in
        if should_reanchor then
          let* new_id =
            transport.send_with_id ~disable_notification:true ?parse_mode
              ~bot_token ~chat_id ~text ()
          in
          if is_valid_message_id new_id then begin
            let* () =
              Lwt.catch
                (fun () ->
                  transport.delete_message ~bot_token ~chat_id ~message_id ())
                (fun _exn -> Lwt.return_unit)
            in
            Lwt.return (Some new_id)
          end
          else begin
            Logs.warn (fun m ->
                m
                  "Telegram status reanchor failed to obtain a replacement \
                   message_id for chat_id=%s; keeping prior status message"
                  chat_id);
            Lwt.return None
          end
        else
          let* () =
            transport.edit_text ?parse_mode ~bot_token ~chat_id ~message_id
              ~text ()
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
  let* status, _body = Http_client.post_json ~uri ~headers:[] ~body in
  if status < 200 || status >= 300 then
    Logs.warn (fun m ->
        m
          "Telegram setMessageReaction failed: status=%d chat_id=%s \
           message_id=%d"
          status chat_id message_id);
  Lwt.return_unit

let send_message ?(disable_notification = false) ?parse_mode ~bot_token ~chat_id
    ~text () =
  let open Lwt.Syntax in
  let mutex = get_outbound_mutex chat_id in
  Lwt_mutex.with_lock mutex (fun () ->
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
      else Lwt.return_unit)

let send_chunked ?(disable_notification = false) ?parse_mode ~bot_token ~chat_id
    ~text () =
  let open Lwt.Syntax in
  Lwt_list.iter_s
    (fun chunk ->
      send_message ~disable_notification ?parse_mode ~bot_token ~chat_id
        ~text:chunk ())
    (chunk_text text)

let send_chunked_html_with_fallback ~bot_token ~chat_id ~text () =
  let open Lwt.Syntax in
  let chunks = chunk_text text in
  Lwt_list.iter_s
    (fun chunk ->
      Lwt.catch
        (fun () ->
          send_message ~parse_mode:"HTML" ~bot_token ~chat_id ~text:chunk ())
        (fun _exn -> send_message ~bot_token ~chat_id ~text:chunk ()))
    chunks

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

let send_poll_api ~bot_token ~chat_id ~question ~options ~allows_multiple () =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s%s/sendPoll" api_base bot_token in
  let body =
    `Assoc
      [
        ("chat_id", `String chat_id);
        ("question", `String question);
        ( "options",
          `List (List.map (fun o -> `Assoc [ ("text", `String o) ]) options) );
        ("is_anonymous", `Bool false);
        ("allows_multiple_answers", `Bool allows_multiple);
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
  let* status, resp_body = Http_client.post_json ~uri ~headers:[] ~body in
  if status >= 200 && status < 300 then
    Logs.info (fun m ->
        m "Telegram: registered %d slash commands"
          (List.length Slash_commands.commands))
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
    Printf.sprintf "%s%s/getFile?file_id=%s" api_base bot_token file_id
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

let handle_update ~bot_token ~(account : Runtime_config.telegram_account)
    ~(session_mgr : Session.t) ?run_update_command ?chat_limiter
    (update : update) =
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
      let typing_watcher =
        ensure_session_typing_watcher ~session_mgr ~key ~bot_token
          ~chat_id:update.chat_id
      in
      let refresh_typing () = typing_watcher.refresh () in
      (* Register a persistent channel notifier so autonomous continuation
         responses can reach the Telegram chat *)
      let send_to_chat text =
        let open Lwt.Syntax in
        let* () =
          send_chunked ~parse_mode:"MarkdownV2" ~bot_token
            ~chat_id:update.chat_id
            ~text:(Telegram_format.markdown_to_mdv2 text)
            ()
        in
        refresh_typing ();
        Lwt.return_unit
      in
      if Option.is_none (Session.find_registered_notifier session_mgr ~key) then begin
        Session.register_channel_notifier session_mgr ~key send_to_chat;
        Session.register_status_message_factory session_mgr ~key (fun () ->
            Status_message.create
              ~notifier:
                (make_status_notifier ~bot_token ~chat_id:update.chat_id)
              ~parse_mode:"HTML" ())
      end;
      (* Register rich notifier for inline keyboards and polls *)
      if Option.is_none (Session.find_rich_notifier session_mgr ~key) then
        Session.register_rich_notifier session_mgr ~key (fun msg ->
            let open Lwt.Syntax in
            match msg with
            | Rich_message.Text text ->
                let* () =
                  send_chunked ~parse_mode:"MarkdownV2" ~bot_token
                    ~chat_id:update.chat_id
                    ~text:(Telegram_format.markdown_to_mdv2 text)
                    ()
                in
                refresh_typing ();
                Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] }
            | Rich_message.TextWithButtons { text; button_rows } ->
                let now = Unix.gettimeofday () in
                let buttons =
                  List.concat_map
                    (fun row ->
                      List.map
                        (fun (btn : Rich_message.button) ->
                          (btn.label, btn.callback_id))
                        row)
                    button_rows
                in
                let callback_ids =
                  List.map
                    (fun (label, cb_id) ->
                      Hashtbl.replace callback_routing cb_id (key, label, now);
                      cb_id)
                    buttons
                in
                let* msg_id =
                  send_message_with_keyboard ~bot_token ~chat_id:update.chat_id
                    ~text ~buttons ()
                in
                refresh_typing ();
                Lwt.return Rich_message.{ message_id = msg_id; callback_ids }
            | Rich_message.Poll { question; options; allows_multiple } ->
                let* msg_id, poll_id =
                  send_poll_api ~bot_token ~chat_id:update.chat_id ~question
                    ~options ~allows_multiple ()
                in
                Hashtbl.replace poll_routing poll_id
                  (key, update.chat_id, bot_token, options, Unix.gettimeofday ());
                refresh_typing ();
                Lwt.return
                  Rich_message.{ message_id = msg_id; callback_ids = [] });
      let image_content_parts = ref [] in
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
            (* Determine image file_id from photo, sticker, or image document *)
            let image_file_id =
              match update.photo_file_id with
              | Some fid -> Some fid
              | None -> (
                  match update.sticker_file_id with
                  | Some fid -> Some fid
                  | None -> (
                      match
                        (update.document_file_id, update.document_mime_type)
                      with
                      | Some fid, Some mt
                        when String.length mt >= 6
                             && String.sub mt 0 6 = "image/" ->
                          Some fid
                      | _ -> None))
            in
            match image_file_id with
            | Some file_id ->
                Lwt.catch
                  (fun () ->
                    let* image_data =
                      download_telegram_file ~bot_token ~file_id
                    in
                    let media_type = detect_mime_type image_data in
                    let b64 = Base64.encode_exn image_data in
                    let text =
                      match update.caption with
                      | Some c -> c
                      | None -> "[Image]"
                    in
                    image_content_parts :=
                      [ Provider.Image_base64 { data = b64; media_type } ];
                    Lwt.return text)
                  (fun exn ->
                    Logs.err (fun m ->
                        m "Image download failed: %s" (Printexc.to_string exn));
                    let cap =
                      match update.caption with
                      | Some c -> " — " ^ c
                      | None -> ""
                    in
                    if update.photo_file_id <> None then
                      Lwt.return ("[Photo received" ^ cap ^ "]")
                    else if update.sticker_file_id <> None then
                      Lwt.return ("[Sticker received" ^ cap ^ "]")
                    else Lwt.return ("[Image document received" ^ cap ^ "]"))
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
      else if Update_tool.is_update_command user_text then (
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
        (* Eagerly acknowledge this update before starting the build.
           Without this, if a concurrent /update is rejected by claim_update and
           exec-restart then races with the normal poll-advance cycle, the rejected
           message can be re-delivered to the new daemon, triggering a redundant
           build.  Ignore failures — the prepare_restart path below is the safety
           valve that will abort the restart if the final ack fails. *)
        let* _ =
          Lwt.catch
            (fun () ->
              acknowledge_update ~bot_token ~update_id:update.update_id)
            (fun _ -> Lwt.return (Ok ()))
        in
        Logs.info (fun m ->
            m "Telegram: /update command from chat_id=%s, initiating update"
              update.chat_id);
        let* _response =
          run_update_command
            ~prepare_restart:(fun () ->
              Restart_notify.write ~channel:"telegram"
                ~channel_id:update.chat_id;
              acknowledge_update ~bot_token ~update_id:update.update_id)
            ~send_progress ()
        in
        Lwt.return_unit)
      else
        match Slash_commands.handle user_text with
        | Reply text -> send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | Reset ->
            let* () = Session.reset session_mgr ~key in
            send_message ~bot_token ~chat_id:update.chat_id
              ~text:Slash_commands.reset_message ()
        | Compact -> (
            let notifier =
              make_status_notifier ~bot_token ~chat_id:update.chat_id
            in
            let* compact_result =
              Session.compact session_mgr ~key ~notifier ()
            in
            match compact_result with
            | Ok _ ->
                (* Progress/result message handled by session.compact via notifier *)
                Lwt.return_unit
            | Error err ->
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:(Printf.sprintf "Compaction failed: %s" err)
                  ())
        | RuntimeCtx ->
            let* text = Session.runtime_context_block session_mgr ~key in
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
        | ShowThinking action ->
            let cfg = Session.get_config session_mgr in
            let current = cfg.agent_defaults.show_thinking in
            let text =
              match action with
              | Slash_commands.ShowThinkingStatus ->
                  Printf.sprintf "Show thinking: %s"
                    (if current then "on" else "off")
              | Slash_commands.ToggleShowThinking -> (
                  let new_val = not current in
                  match Config_set.set_show_thinking new_val with
                  | Ok () ->
                      let agent_defaults =
                        { cfg.agent_defaults with show_thinking = new_val }
                      in
                      Session.update_config ~source:"telegram" session_mgr
                        { cfg with agent_defaults };
                      Logs.info (fun m ->
                          m
                            "Telegram show_thinking toggled chat_id=%s \
                             user_id=%s from=%b to=%b"
                            update.chat_id update.user_id current new_val);
                      Printf.sprintf "Show thinking: %s"
                        (if new_val then "on" else "off")
                  | Error err -> "Failed to update show_thinking: " ^ err)
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | Delegate prompt ->
            let* () =
              send_message ~bot_token ~chat_id:update.chat_id
                ~text:"Delegating to a temporary session..." ()
            in
            let tg_prompt = telegram_delegate_prompt ~user_prompt:prompt in
            Session.delegate_turn session_mgr ~prompt:tg_prompt
              ~send_reply:(fun text ->
                send_chunked_html_with_fallback ~bot_token
                  ~chat_id:update.chat_id ~text ());
            Lwt.return_unit
        | Tools ->
            let text =
              match Session.get_tool_registry session_mgr with
              | Some reg ->
                  Slash_commands.format_tools_telegram (Tool_registry.list reg)
              | None -> "Tools are not enabled."
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text
              ~parse_mode:"HTML" ()
        | Tasks ->
            let text =
              match Session.get_db session_mgr with
              | Some db ->
                  Task_tree.init_schema db;
                  Task_tree.render_tree_with_legend ~db ~session_key:key
              | None -> "Tasks are not available (no database)."
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | Model action -> (
            let open Slash_commands in
            match action with
            | ModelShow ->
                let current =
                  (Session.get_config session_mgr).agent_defaults.primary_model
                in
                let prefs = Model_preferences.load () in
                let usage_ranked =
                  List.filter_map
                    (fun (m, c) ->
                      if List.mem m prefs.favorites then None else Some (m, c))
                    prefs.usage_counts
                in
                let text =
                  format_model_show_telegram ~current ~favorites:prefs.favorites
                    ~usage_ranked
                in
                send_message ~bot_token ~chat_id:update.chat_id ~text
                  ~parse_mode:"HTML" ()
            | ModelSet name -> (
                let provider, model_id, fmt = Models_catalog.split_name name in
                match fmt with
                | Models_catalog.Canonical | Models_catalog.Legacy ->
                    let hint =
                      match fmt with
                      | Models_catalog.Legacy ->
                          Printf.sprintf
                            "\nHint: use %s:%s format instead of %s/%s."
                            provider model_id provider model_id
                      | _ -> ""
                    in
                    let cfg = Session.get_config session_mgr in
                    let agent_defaults =
                      { cfg.agent_defaults with primary_model = name }
                    in
                    Session.update_config ~source:"telegram" session_mgr
                      { cfg with agent_defaults };
                    let _ = Model_preferences.increment_usage name in
                    let provider_in_config =
                      List.mem_assoc provider cfg.providers
                    in
                    let warn =
                      if not provider_in_config then
                        Printf.sprintf
                          "\n\
                           Warning: provider '%s' not found in config. Add it \
                           to your config.json to use this model."
                          provider
                      else ""
                    in
                    send_message ~bot_token ~chat_id:update.chat_id
                      ~text:
                        (Printf.sprintf
                           "Model set to: %s (provider: %s)%s%s\n\
                            Session-only change; use /model set-default to \
                            persist for new sessions and restarts."
                           model_id provider hint warn)
                      ()
                | Models_catalog.Plain -> (
                    let model_info = Models_catalog.find_by_full_name name in
                    match model_info with
                    | None ->
                        let text =
                          Printf.sprintf
                            "Warning: '%s' not found in model catalog. Setting \
                             anyway."
                            name
                        in
                        let cfg = Session.get_config session_mgr in
                        let agent_defaults =
                          { cfg.agent_defaults with primary_model = name }
                        in
                        Session.update_config session_mgr
                          { cfg with agent_defaults };
                        let _ = Model_preferences.increment_usage name in
                        send_message ~bot_token ~chat_id:update.chat_id ~text ()
                    | Some m ->
                        let cfg = Session.get_config session_mgr in
                        let agent_defaults =
                          { cfg.agent_defaults with primary_model = name }
                        in
                        Session.update_config session_mgr
                          { cfg with agent_defaults };
                        let _ = Model_preferences.increment_usage name in
                        let display =
                          if m.Models_catalog.provider <> "" then
                            Printf.sprintf "Model set to: %s (provider: %s)"
                              m.Models_catalog.id m.Models_catalog.provider
                          else Printf.sprintf "Model set to: %s" name
                        in
                        send_message ~bot_token ~chat_id:update.chat_id
                          ~text:display ()))
            | ModelSetDefault name -> (
                let provider, model_id, fmt = Models_catalog.split_name name in
                let hint =
                  match fmt with
                  | Models_catalog.Legacy ->
                      Printf.sprintf "\nHint: use %s:%s format instead."
                        provider model_id
                  | _ -> ""
                in
                let result =
                  Config_set.set_json_value "agent_defaults.primary_model"
                    (`String name)
                in
                match result with
                | Error e ->
                    send_message ~bot_token ~chat_id:update.chat_id
                      ~text:(Printf.sprintf "Error writing config: %s" e)
                      ()
                | Ok () ->
                    let msg =
                      match fmt with
                      | Models_catalog.Canonical | Models_catalog.Legacy ->
                          Printf.sprintf
                            "Default model set to: %s (provider: %s)%s\n\
                             Applies to new sessions."
                            model_id provider hint
                      | Models_catalog.Plain ->
                          Printf.sprintf
                            "Default model set to: %s\nApplies to new sessions."
                            name
                    in
                    send_message ~bot_token ~chat_id:update.chat_id ~text:msg ()
                )
            | ModelFav name ->
                let prefs = Model_preferences.toggle_favorite name in
                let status =
                  if List.mem name prefs.favorites then "added to"
                  else "removed from"
                in
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:(Printf.sprintf "%s %s favorites" name status)
                  ()
            | ModelUnfav name ->
                let _ = Model_preferences.remove_favorite name in
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:(Printf.sprintf "Removed from favorites: %s" name)
                  ()
            | ModelList provider ->
                let db_extras =
                  match Session.get_db session_mgr with
                  | None -> []
                  | Some db ->
                      Model_discovery.get_db_only_models ~db
                        ~provider_filter:provider
                in
                let models =
                  Models_catalog.to_plain_list ~provider_filter:provider
                    ~db_extras ()
                  |> String.split_on_char '\n'
                  |> List.filter (fun s -> s <> "")
                in
                let text = format_model_list_telegram ~models ~provider in
                send_message ~bot_token ~chat_id:update.chat_id ~text
                  ~parse_mode:"HTML" ()
            | ModelUsage ->
                let cfg = Session.get_config session_mgr in
                Provider_quota.set_cache_ttl cfg.quota_cache_ttl_s;
                let results = Provider_quota.get_all_cached () in
                let lines =
                  List.map
                    (fun (name, pq) ->
                      let summary = Provider_quota.to_summary_string pq in
                      let threshold =
                        match List.assoc_opt name cfg.providers with
                        | Some pc ->
                            Option.value ~default:0.85 pc.quota_threshold
                        | None -> 0.85
                      in
                      let label = Provider_quota.status_label ~threshold pq in
                      summary ^ "  " ^ label)
                    results
                in
                let text =
                  if lines = [] then "No providers configured."
                  else
                    "<b>Provider Quota/Usage</b>\n\n" ^ String.concat "\n" lines
                in
                send_message ~bot_token ~chat_id:update.chat_id ~text
                  ~parse_mode:"HTML" ())
        | ForkAnd prompt ->
            let* () =
              send_message ~bot_token ~chat_id:update.chat_id
                ~text:"Forking session..." ()
            in
            let tg_prompt = telegram_delegate_prompt ~user_prompt:prompt in
            Session.fork_and_run session_mgr ~parent_key:key ~prompt:tg_prompt
              ~send_reply:(fun text ->
                send_chunked_html_with_fallback ~bot_token
                  ~chat_id:update.chat_id ~text ());
            Lwt.return_unit
        | NotACommand -> (
            let msg = user_text in
            (* Early busy-session fast path: if the session is already busy,
               enqueue immediately without the reaction HTTP call or UI setup.
               This avoids ~1s+ of latency from the setMessageReaction API call
               and other setup that is unnecessary for queued messages. *)
            let normalized_msg =
              if String.length msg > 0 && msg.[0] = '!' then
                let raw = String.sub msg 1 (String.length msg - 1) in
                if String.trim raw = "" then "[interrupted]" else raw
              else msg
            in
            let* () =
              if String.length msg > 0 && msg.[0] = '!' then
                Session.set_interrupt_if_present session_mgr ~key normalized_msg
              else Lwt.return_unit
            in
            let* early_queued =
              Session.enqueue_message_if_busy session_mgr ~key
                ({
                   message = normalized_msg;
                   content_parts = !image_content_parts;
                   attachments = [];
                   channel_name = Some "telegram";
                   channel_type = Some "dm";
                   sender_id = None;
                   sender_name = None;
                   channel = Some "telegram";
                   channel_id = Some update.chat_id;
                   message_id = Some (string_of_int update.message_id);
                 }
                  : Session.queued_message)
            in
            if early_queued then Lwt.return_unit
            else
              let agent_defaults =
                (Session.get_config session_mgr).agent_defaults
              in
              let use_consolidated =
                agent_defaults.show_tool_calls
                && agent_defaults.tool_status_mode = "consolidated"
              in
              let current_turn_has_tools = ref false in
              let current_turn_tool_details = ref [] in
              let tool_reaction_set = ref false in
              let peers =
                Reaction_tracker.get_or_create_peers reactions ~key
                  ~initial:update.message_id
              in
              Reaction_tracker.add_peer reactions ~key
                ~message_id:update.message_id;
              let set_reaction emoji =
                Reaction_tracker.set_reaction_all reactions ~peers_ref:peers
                  ~set_one:(fun mid e ->
                    Lwt.catch
                      (fun () ->
                        set_message_reaction ~bot_token ~chat_id:update.chat_id
                          ~message_id:mid ~emoji:e ())
                      (fun _exn -> Lwt.return_unit))
                  ~emoji
              in
              let set_reaction_on mid emoji =
                Lwt.catch
                  (fun () ->
                    set_message_reaction ~bot_token ~chat_id:update.chat_id
                      ~message_id:mid ~emoji ())
                  (fun _exn -> Lwt.return_unit)
              in
              let thinking_buf = Buffer.create 256 in
              let status_msg =
                if use_consolidated then
                  let status_notifier =
                    make_status_notifier ~bot_token ~chat_id:update.chat_id
                  in
                  Some
                    (Status_message.create ~notifier:status_notifier
                       ~parse_mode:"HTML" ())
                else None
              in
              let visibility = Stream_visibility.create () in
              let tool_start_times : (string, float * string option) Hashtbl.t =
                Hashtbl.create 8
              in
              let send_expandable ~name ~result ~is_error =
                if is_error then
                  let formatted = Telegram_format.format_error_trace result in
                  send_chunked ~disable_notification:true
                    ~parse_mode:"MarkdownV2" ~bot_token ~chat_id:update.chat_id
                    ~text:formatted ()
                else
                  match
                    Telegram_format.format_sensitive_result ~name result
                  with
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
                        let* () =
                          if not !tool_reaction_set then begin
                            tool_reaction_set := true;
                            set_reaction reaction_emoji_tools
                          end
                          else Lwt.return_unit
                        in
                        let summary =
                          Stream_visibility.summarize_tool_arguments ~name
                            arguments
                        in
                        let* () =
                          Status_message.tool_start sm ~id ~name ~summary
                        in
                        let action = chat_action_for_tool name in
                        let* () =
                          Lwt.catch
                            (fun () ->
                              send_chat_action ~bot_token
                                ~chat_id:update.chat_id ~action)
                            (fun _exn -> Lwt.return_unit)
                        in
                        refresh_typing ();
                        Lwt.return_unit
                    | Provider.ToolResult { id; name; result; is_error } ->
                        let open Lwt.Syntax in
                        let* () =
                          Status_message.tool_result sm ~id ~name ~result
                            ~is_error
                        in
                        refresh_typing ();
                        current_turn_tool_details :=
                          format_tool_result_detail ~name ~result
                          :: !current_turn_tool_details;
                        current_turn_has_tools := true;
                        (* Only send inline messages for errors; non-error
                         output is available via "Show Details" button *)
                        if is_error then (
                          let info = Status_message.get_tool_info sm ~id in
                          let emoji =
                            Option.fold ~none:"\xE2\x9C\x97"
                              ~some:(fun (e : Status_message.tool_entry) ->
                                e.emoji)
                              info
                          in
                          let summary =
                            Option.bind info
                              (fun (e : Status_message.tool_entry) -> e.summary)
                          in
                          let duration_secs =
                            Option.bind info
                              (fun (e : Status_message.tool_entry) ->
                                Option.map
                                  (fun fin -> fin -. e.started_at)
                                  e.finished_at)
                          in
                          let formatted =
                            Telegram_format.format_error_standalone ~emoji ~name
                              ~summary ~duration_secs ~result
                          in
                          let* () =
                            send_chunked ~disable_notification:true
                              ~parse_mode:"MarkdownV2" ~bot_token
                              ~chat_id:update.chat_id ~text:formatted ()
                          in
                          refresh_typing ();
                          Lwt.return_unit)
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
                      if not !tool_reaction_set then
                        match chunk with
                        | Provider.ToolStart _ ->
                            tool_reaction_set := true;
                            set_reaction reaction_emoji_tools
                        | _ -> Lwt.return_unit
                      else Lwt.return_unit
                    in
                    let* () =
                      match chunk with
                      | Provider.ToolStart { id; name; arguments } ->
                          let summary =
                            Stream_visibility.summarize_tool_arguments ~name
                              arguments
                          in
                          Hashtbl.replace tool_start_times id
                            (Unix.gettimeofday (), summary);
                          let action = chat_action_for_tool name in
                          let* () =
                            Lwt.catch
                              (fun () ->
                                send_chat_action ~bot_token
                                  ~chat_id:update.chat_id ~action)
                              (fun _exn -> Lwt.return_unit)
                          in
                          refresh_typing ();
                          Lwt.return_unit
                      | _ -> Lwt.return_unit
                    in
                    let settings : Stream_visibility.settings =
                      {
                        show_thinking = agent_defaults.show_thinking;
                        show_tool_calls = agent_defaults.show_tool_calls;
                        notify_tool_starts = true;
                        notify_tool_successes = true;
                      }
                    in
                    let* () =
                      Stream_visibility.on_chunk visibility ~settings
                        ~notify:(fun text ->
                          let text = Telegram_format.markdown_to_mdv2 text in
                          let open Lwt.Syntax in
                          let* () =
                            send_chunked ~disable_notification:true
                              ~parse_mode:"MarkdownV2" ~bot_token
                              ~chat_id:update.chat_id ~text ()
                          in
                          refresh_typing ();
                          Lwt.return_unit)
                        chunk
                    in
                    match chunk with
                    | Provider.ToolResult { id; name; result; is_error; _ } ->
                        let* () =
                          if is_error then
                            let emoji = Stream_visibility.tool_emoji name in
                            let duration_secs, summary =
                              match Hashtbl.find_opt tool_start_times id with
                              | Some (t0, s) ->
                                  (Some (Unix.gettimeofday () -. t0), s)
                              | None -> (None, None)
                            in
                            let formatted =
                              Telegram_format.format_error_standalone ~emoji
                                ~name ~summary ~duration_secs ~result
                            in
                            send_chunked ~disable_notification:true
                              ~parse_mode:"MarkdownV2" ~bot_token
                              ~chat_id:update.chat_id ~text:formatted ()
                          else send_expandable ~name ~result ~is_error
                        in
                        refresh_typing ();
                        Lwt.return_unit
                    | _ -> Lwt.return_unit)
              in
              (* See reaction_emoji_* constants and valid_reaction_emojis *)
              Lwt.async (fun () ->
                  Lwt.catch
                    (fun () -> set_reaction reaction_emoji_received)
                    (fun _exn -> Lwt.return_unit));
              let drain_progress_msg_id = ref None in
              let on_drain_progress : Session.drain_progress =
                {
                  before_turn =
                    (fun queued_msg_id ->
                      let* () =
                        match queued_msg_id with
                        | Some mid -> (
                            match int_of_string_opt mid with
                            | Some mid_int ->
                                set_reaction_on mid_int reaction_emoji_received
                            | None -> Lwt.return_unit)
                        | None -> Lwt.return_unit
                      in
                      let* () =
                        match !drain_progress_msg_id with
                        | Some mid ->
                            Lwt.catch
                              (fun () ->
                                delete_message ~bot_token
                                  ~chat_id:update.chat_id ~message_id:mid ())
                              (fun _exn -> Lwt.return_unit)
                        | None -> Lwt.return_unit
                      in
                      let* mid =
                        send_message_with_id ~disable_notification:true
                          ~bot_token ~chat_id:update.chat_id
                          ~text:
                            "\xe2\x8f\xb3 Processing queued message\xe2\x80\xa6"
                          ()
                      in
                      drain_progress_msg_id := Some mid;
                      refresh_typing ();
                      Lwt.return_unit);
                  after_turn =
                    (fun queued_msg_id ->
                      match queued_msg_id with
                      | Some mid -> (
                          match int_of_string_opt mid with
                          | Some mid_int ->
                              set_reaction_on mid_int reaction_emoji_done
                          | None -> Lwt.return_unit)
                      | None -> Lwt.return_unit);
                  after_all =
                    (fun () ->
                      match !drain_progress_msg_id with
                      | Some mid ->
                          drain_progress_msg_id := None;
                          let open Lwt.Syntax in
                          let* () =
                            Lwt.catch
                              (fun () ->
                                delete_message ~bot_token
                                  ~chat_id:update.chat_id ~message_id:mid ())
                              (fun _exn -> Lwt.return_unit)
                          in
                          refresh_typing ();
                          Lwt.return_unit
                      | None -> Lwt.return_unit);
                }
              in
              let response_sent = ref false in
              let* result =
                Session.with_registered_notifier session_mgr ~key
                  ~notify:(fun text ->
                    let open Lwt.Syntax in
                    let* () =
                      send_chunked ~disable_notification:true
                        ~parse_mode:"MarkdownV2" ~bot_token
                        ~chat_id:update.chat_id
                        ~text:(Telegram_format.markdown_to_mdv2 text)
                        ()
                    in
                    refresh_typing ();
                    Lwt.return_unit)
                  (fun () ->
                    Lwt.catch
                      (fun () ->
                        let before_drain response =
                          if Session.is_queued_message_response response then
                            Lwt.return_unit
                          else
                            let open Lwt.Syntax in
                            let* () =
                              match status_msg with
                              | Some sm -> Status_message.finalize sm
                              | None -> Lwt.return_unit
                            in
                            let* () =
                              if status_msg <> None && !current_turn_has_tools
                              then (
                                let details_text =
                                  List.rev !current_turn_tool_details
                                  |> String.concat "\n---\n"
                                in
                                let details_callback =
                                  register_tool_result_details
                                    ~chat_id:update.chat_id
                                    ~user_id:update.user_id details_text
                                in
                                let* _msg_id =
                                  send_message_with_keyboard
                                    ~disable_notification:true ~bot_token
                                    ~chat_id:update.chat_id
                                    ~text:
                                      "\xF0\x9F\x93\x8B Tool output available"
                                    ~buttons:
                                      [ ("Show Details", details_callback) ]
                                    ()
                                in
                                refresh_typing ();
                                Lwt.return_unit)
                              else Lwt.return_unit
                            in
                            let thinking =
                              match status_msg with
                              | Some _ -> Buffer.contents thinking_buf
                              | None ->
                                  Stream_visibility.thinking_text visibility
                            in
                            let* () =
                              if thinking <> "" then (
                                let* () =
                                  send_chunked ~parse_mode:"MarkdownV2"
                                    ~bot_token ~chat_id:update.chat_id
                                    ~text:
                                      ("_"
                                      ^ Telegram_format.escape_mdv2 thinking
                                      ^ "_")
                                    ()
                                in
                                refresh_typing ();
                                Lwt.return_unit)
                              else Lwt.return_unit
                            in
                            let* () =
                              let* () =
                                send_chunked ~parse_mode:"MarkdownV2" ~bot_token
                                  ~chat_id:update.chat_id
                                  ~text:
                                    (Telegram_format.markdown_to_mdv2 response)
                                  ()
                              in
                              refresh_typing ();
                              Lwt.return_unit
                            in
                            let* () = set_reaction reaction_emoji_done in
                            if
                              not
                                (Session.take_response_deferred session_mgr ~key)
                            then Session.mark_response_sent session_mgr ~key;
                            response_sent := true;
                            Lwt.return_unit
                        in
                        let turn_p =
                          Session.turn_stream session_mgr ~key ~message:msg
                            ~content_parts:!image_content_parts
                            ~channel_name:"telegram" ~channel_type:"dm"
                            ~channel:"telegram" ~channel_id:update.chat_id
                            ~message_id:(string_of_int update.message_id)
                            ~on_drain_progress ~before_drain ~on_chunk ()
                        in
                        let* response = turn_p in
                        Lwt.return (Ok response))
                      (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
              in
              match result with
              | Ok response ->
                  if Session.is_queued_message_response response then
                    Lwt.return_unit
                  else if !response_sent then begin
                    ignore (Reaction_tracker.cleanup reactions ~key);
                    Lwt.async (fun () ->
                        Session.process_autonomous_turn_result
                          ~on_response:send_to_chat session_mgr ~key ~response);
                    Lwt.return_unit
                  end
                  else
                    let* () =
                      match status_msg with
                      | Some sm -> Status_message.finalize sm
                      | None -> Lwt.return_unit
                    in
                    let* () =
                      if status_msg <> None && !current_turn_has_tools then (
                        let details_text =
                          List.rev !current_turn_tool_details
                          |> String.concat "\n---\n"
                        in
                        let details_callback =
                          register_tool_result_details ~chat_id:update.chat_id
                            ~user_id:update.user_id details_text
                        in
                        let* _msg_id =
                          send_message_with_keyboard ~disable_notification:true
                            ~bot_token ~chat_id:update.chat_id
                            ~text:"\xF0\x9F\x93\x8B Tool output available"
                            ~buttons:[ ("Show Details", details_callback) ]
                            ()
                        in
                        refresh_typing ();
                        Lwt.return_unit)
                      else Lwt.return_unit
                    in
                    let thinking =
                      match status_msg with
                      | Some _ -> Buffer.contents thinking_buf
                      | None -> Stream_visibility.thinking_text visibility
                    in
                    let* () =
                      if thinking <> "" then (
                        let* () =
                          send_chunked ~parse_mode:"MarkdownV2" ~bot_token
                            ~chat_id:update.chat_id
                            ~text:
                              ("_" ^ Telegram_format.escape_mdv2 thinking ^ "_")
                            ()
                        in
                        refresh_typing ();
                        Lwt.return_unit)
                      else Lwt.return_unit
                    in
                    let* () =
                      send_chunked ~parse_mode:"MarkdownV2" ~bot_token
                        ~chat_id:update.chat_id
                        ~text:(Telegram_format.markdown_to_mdv2 response)
                        ()
                    in
                    let* () = set_reaction reaction_emoji_done in
                    ignore (Reaction_tracker.cleanup reactions ~key);
                    if not (Session.take_response_deferred session_mgr ~key)
                    then Session.mark_response_sent session_mgr ~key;
                    Lwt.async (fun () ->
                        Session.process_autonomous_turn_result
                          ~on_response:send_to_chat session_mgr ~key ~response);
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
                           "Sorry, an error occurred processing your message: \
                            %s"
                           err)
                      ()
                  in
                  let* () = set_reaction reaction_emoji_error in
                  ignore (Reaction_tracker.cleanup reactions ~key);
                  if not (Session.take_response_deferred session_mgr ~key) then
                    Session.mark_response_sent session_mgr ~key;
                  Lwt.return_unit)

let dispatch_update ~bot_token ~(account : Runtime_config.telegram_account)
    ~(session_mgr : Session.t) ?run_update_command ?chat_limiter update =
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          handle_update ~bot_token ~account ~session_mgr ?run_update_command
            ?chat_limiter update)
        (fun exn ->
          Logs.err (fun m ->
              m "Telegram: handle_update error for update_id=%d: %s"
                update.update_id (Printexc.to_string exn));
          Lwt.return_unit))

let flush_pending_text_update ~key ~bot_token
    ~(account : Runtime_config.telegram_account) ~(session_mgr : Session.t)
    ?run_update_command ?chat_limiter () =
  match Hashtbl.find_opt pending_text_updates key with
  | None -> ()
  | Some pending ->
      Hashtbl.remove pending_text_updates key;
      dispatch_update ~bot_token ~account ~session_mgr ?run_update_command
        ?chat_limiter pending.update

let schedule_pending_text_flush ~key ~bot_token
    ~(account : Runtime_config.telegram_account) ~(session_mgr : Session.t)
    ?run_update_command ?chat_limiter generation =
  Lwt.async (fun () ->
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep !text_coalesce_window_seconds in
      match Hashtbl.find_opt pending_text_updates key with
      | Some pending
        when pending.generation = generation
             && Unix.gettimeofday () -. pending.last_seen_at
                >= !text_coalesce_window_seconds ->
          Hashtbl.remove pending_text_updates key;
          Lwt.catch
            (fun () ->
              handle_update ~bot_token ~account ~session_mgr ?run_update_command
                ?chat_limiter pending.update)
            (fun exn ->
              Logs.err (fun m ->
                  m "Telegram: handle_update error for update_id=%d: %s"
                    pending.update.update_id (Printexc.to_string exn));
              Lwt.return_unit)
      | _ -> Lwt.return_unit)

let buffer_or_dispatch_update ~bot_token
    ~(account : Runtime_config.telegram_account) ~(session_mgr : Session.t)
    ?run_update_command ?chat_limiter update =
  let now = Unix.gettimeofday () in
  let key = text_coalesce_key ~bot_token update in
  if
    (not (is_text_coalescing_candidate update))
    || !text_coalesce_window_seconds <= 0.0
  then begin
    flush_pending_text_update ~key ~bot_token ~account ~session_mgr
      ?run_update_command ?chat_limiter ();
    dispatch_update ~bot_token ~account ~session_mgr ?run_update_command
      ?chat_limiter update
  end
  else
    match Hashtbl.find_opt pending_text_updates key with
    | Some pending when can_coalesce_text_updates ~now pending update ->
        pending.update <- merge_text_updates pending update;
        pending.last_seen_at <- now;
        pending.generation <- pending.generation + 1;
        schedule_pending_text_flush ~key ~bot_token ~account ~session_mgr
          ?run_update_command ?chat_limiter pending.generation
    | Some _ ->
        flush_pending_text_update ~key ~bot_token ~account ~session_mgr
          ?run_update_command ?chat_limiter ();
        let pending = { update; last_seen_at = now; generation = 0 } in
        Hashtbl.replace pending_text_updates key pending;
        schedule_pending_text_flush ~key ~bot_token ~account ~session_mgr
          ?run_update_command ?chat_limiter pending.generation
    | None ->
        let pending = { update; last_seen_at = now; generation = 0 } in
        Hashtbl.replace pending_text_updates key pending;
        schedule_pending_text_flush ~key ~bot_token ~account ~session_mgr
          ?run_update_command ?chat_limiter pending.generation

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
    let poll_start = Unix.gettimeofday () in
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
    let update_count = List.length updates in
    List.iter
      (fun update ->
        offset := update.update_id + 1;
        if update.message_id > 0 then begin
          let cur =
            Option.value ~default:0
              (Hashtbl.find_opt latest_chat_msg_id update.chat_id)
          in
          if update.message_id > cur then
            Hashtbl.replace latest_chat_msg_id update.chat_id update.message_id
        end;
        if should_process_update update then
          buffer_or_dispatch_update ~bot_token ~account ~session_mgr
            ?run_update_command ?chat_limiter update
        else
          Logs.info (fun m ->
              m "Telegram: ignoring duplicate update update_id=%d chat_id=%s"
                update.update_id update.chat_id))
      updates;
    (if !poll_count <= 3 || !poll_count mod 100 = 0 then
       let poll_elapsed_ms = (Unix.gettimeofday () -. poll_start) *. 1000.0 in
       Logs.info (fun m ->
           m "Telegram poll #%d for '%s': %.0fms elapsed, %d update(s) received"
             !poll_count name poll_elapsed_ms update_count));
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
                  | data
                    when String.starts_with ~prefix:details_callback_prefix data
                    ->
                      let text =
                        match
                          take_tool_result_details ~chat_id:cb.cb_chat_id
                            ~user_id:cb.cb_user_id data
                        with
                        | Some details when String.trim details <> "" -> details
                        | _ -> "No details available."
                      in
                      let* () =
                        answer_callback_query ~bot_token
                          ~callback_query_id:cb.callback_query_id ()
                      in
                      send_message ~disable_notification:true ~bot_token
                        ~chat_id:cb.cb_chat_id ~text ()
                  | data -> (
                      match Hashtbl.find_opt callback_routing data with
                      | Some (session_key, label, _created) ->
                          Hashtbl.remove callback_routing data;
                          let* () =
                            answer_callback_query ~bot_token
                              ~callback_query_id:cb.callback_query_id
                              ~text:(Printf.sprintf "Selected: %s" label)
                              ()
                          in
                          Lwt.async (fun () ->
                              Lwt.catch
                                (fun () ->
                                  let message =
                                    Printf.sprintf "[Button: %s]" label
                                  in
                                  let* response =
                                    Session.turn session_mgr ~key:session_key
                                      ~message ~channel:"telegram"
                                      ~channel_id:cb.cb_chat_id ()
                                  in
                                  if
                                    not
                                      (Session.is_queued_message_response
                                         response)
                                  then
                                    send_chunked ~parse_mode:"MarkdownV2"
                                      ~bot_token ~chat_id:cb.cb_chat_id
                                      ~text:
                                        (Telegram_format.markdown_to_mdv2
                                           response)
                                      ()
                                  else Lwt.return_unit)
                                (fun exn ->
                                  Logs.err (fun m ->
                                      m
                                        "Telegram: button callback routing \
                                         error: %s"
                                        (Printexc.to_string exn));
                                  Lwt.return_unit));
                          Lwt.return_unit
                      | None ->
                          answer_callback_query ~bot_token
                            ~callback_query_id:cb.callback_query_id
                            ~text:"Unknown action" ()))
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
    (* Drain poll answers *)
    let* () =
      let rec drain_poll_answers () =
        if Queue.is_empty pending_poll_answers then Lwt.return_unit
        else
          let pa = Queue.pop pending_poll_answers in
          let* () =
            match Hashtbl.find_opt poll_routing pa.pa_poll_id with
            | Some (session_key, chat_id, poll_bot_token, options, _created_at)
              ->
                let selected =
                  List.filter_map
                    (fun idx ->
                      if idx >= 0 && idx < List.length options then
                        Some (List.nth options idx)
                      else None)
                    pa.pa_option_ids
                in
                if selected = [] then Lwt.return_unit
                else begin
                  Lwt.async (fun () ->
                      Lwt.catch
                        (fun () ->
                          let message =
                            Printf.sprintf "[Poll vote: %s]"
                              (String.concat ", " selected)
                          in
                          let* response =
                            Session.turn session_mgr ~key:session_key ~message
                              ~channel:"telegram" ~channel_id:chat_id ()
                          in
                          if not (Session.is_queued_message_response response)
                          then
                            send_chunked ~bot_token:poll_bot_token ~chat_id
                              ~text:response ()
                          else Lwt.return_unit)
                        (fun exn ->
                          Logs.err (fun m ->
                              m "Telegram: poll answer routing error: %s"
                                (Printexc.to_string exn));
                          Lwt.return_unit));
                  Lwt.return_unit
                end
            | None ->
                Logs.debug (fun m ->
                    m "Telegram: ignoring poll_answer for unknown poll_id=%s"
                      pa.pa_poll_id);
                Lwt.return_unit
          in
          drain_poll_answers ()
      in
      drain_poll_answers ()
    in
    (* Periodic cleanup of stale routing entries *)
    if !poll_count mod 100 = 0 then cleanup_stale_routing ();
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
      text_coalesce_window_seconds :=
        float_of_int tg_config.text_coalesce_ms /. 1000.0;
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
