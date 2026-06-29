type edit_support = Edit_in_place | Delete_and_resend | No_edit

type thread_reply_support =
  | Native_thread_replies
  | Thread_like_replies
  | No_thread_replies

type thread_reply_strategy =
  | Use_native_thread
  | Use_thread_like_reply
  | Use_room_fallback

type progress_delivery =
  | Edit_progress_in_place
  | Delete_and_resend_progress
  | Buffered_progress

type card_strategy = Use_cards | Use_buttons | Use_text_fallback
type history_capture_support = Ambient_history_capture | No_history_capture
type history_capture_strategy = Capture_ambient_history | Skip_history_capture

type t = {
  can_edit : edit_support;
  can_delete : bool;
  can_react : bool;
  can_type : bool;
  can_show_status : bool;
  can_send_files : bool;
  can_send_cards : bool;
  can_send_buttons : bool;
  thread_replies : thread_reply_support;
  history_capture : history_capture_support;
  max_message_length : int;
  connector : Format_adapter.connector;
  parse_mode : string;
  debounce_interval : float;
}

let make ?(can_edit = No_edit) ?(can_delete = false) ?(can_react = false)
    ?(can_type = false) ?(can_show_status = false) ?(can_send_files = false)
    ?(can_send_cards = false) ?(can_send_buttons = false)
    ?(thread_replies = No_thread_replies)
    ?(history_capture = No_history_capture) ~max_message_length ~connector
    ~parse_mode ~debounce_interval () =
  {
    can_edit;
    can_delete;
    can_react;
    can_type;
    can_show_status;
    can_send_files;
    can_send_cards;
    can_send_buttons;
    thread_replies;
    history_capture;
    max_message_length;
    connector;
    parse_mode;
    debounce_interval;
  }

let telegram =
  make ~can_edit:Edit_in_place ~can_delete:true ~can_type:true
    ~can_show_status:true ~can_send_files:true ~can_send_buttons:true
    ~thread_replies:Thread_like_replies ~max_message_length:4096
    ~connector:Format_adapter.Telegram_html ~parse_mode:"HTML"
    ~debounce_interval:0.5 ()

let discord =
  make ~can_edit:Edit_in_place ~can_delete:true ~can_react:true
    ~can_show_status:true ~thread_replies:Native_thread_replies
    ~history_capture:Ambient_history_capture ~max_message_length:2000
    ~connector:Format_adapter.Discord ~parse_mode:"Markdown"
    ~debounce_interval:0.5 ()

let slack =
  make ~can_edit:Edit_in_place ~can_delete:true ~can_react:true
    ~can_show_status:true ~thread_replies:Native_thread_replies
    ~history_capture:Ambient_history_capture ~max_message_length:4000
    ~connector:Format_adapter.Slack ~parse_mode:"mrkdwn" ~debounce_interval:0.5
    ()

let teams =
  make ~can_edit:Edit_in_place ~can_delete:true ~can_type:true
    ~can_show_status:true ~can_send_files:true ~can_send_cards:true
    ~can_send_buttons:true ~thread_replies:Thread_like_replies
    ~history_capture:Ambient_history_capture ~max_message_length:28672
    ~connector:Format_adapter.Teams ~parse_mode:"Markdown"
    ~debounce_interval:1.0 ()

let matrix =
  make ~can_edit:Edit_in_place ~can_delete:true ~can_show_status:true
    ~thread_replies:Native_thread_replies ~max_message_length:4000
    ~connector:Format_adapter.Plain ~parse_mode:"Markdown"
    ~debounce_interval:0.5 ()

let irc =
  make ~max_message_length:512 ~connector:Format_adapter.Plain
    ~parse_mode:"Markdown" ~debounce_interval:0.0 ()

let mattermost =
  make ~can_edit:Edit_in_place ~can_delete:true ~can_react:true
    ~can_show_status:true ~thread_replies:Native_thread_replies
    ~max_message_length:16383 ~connector:Format_adapter.Discord
    ~parse_mode:"Markdown" ~debounce_interval:0.5 ()

let lark =
  make ~max_message_length:4096 ~connector:Format_adapter.Plain
    ~parse_mode:"Markdown" ~debounce_interval:0.0 ()

let line =
  make ~max_message_length:5000 ~connector:Format_adapter.Plain
    ~parse_mode:"Markdown" ~debounce_interval:0.0 ()

let dingtalk =
  make ~max_message_length:20000 ~connector:Format_adapter.Plain
    ~parse_mode:"Markdown" ~debounce_interval:0.0 ()

let onebot =
  make ~can_delete:true ~max_message_length:4500 ~connector:Format_adapter.Plain
    ~parse_mode:"Markdown" ~debounce_interval:0.0 ()

let nostr =
  make ~max_message_length:8000 ~connector:Format_adapter.Plain
    ~parse_mode:"Markdown" ~debounce_interval:0.0 ()

let imessage =
  make ~max_message_length:4096 ~connector:Format_adapter.Plain
    ~parse_mode:"Markdown" ~debounce_interval:0.0 ()

let email =
  make ~thread_replies:Thread_like_replies ~max_message_length:65536
    ~connector:Format_adapter.Plain ~parse_mode:"Markdown"
    ~debounce_interval:0.0 ()

let github =
  make ~can_edit:Edit_in_place ~can_delete:true ~can_react:true
    ~can_show_status:true ~thread_replies:Thread_like_replies
    ~max_message_length:65536 ~connector:Format_adapter.Discord
    ~parse_mode:"Markdown" ~debounce_interval:0.5 ()

let signal =
  make ~can_delete:true ~can_react:true ~max_message_length:6000
    ~connector:Format_adapter.Plain ~parse_mode:"Markdown"
    ~debounce_interval:0.0 ()

let whatsapp =
  make ~can_react:true ~max_message_length:4096 ~connector:Format_adapter.Plain
    ~parse_mode:"Markdown" ~debounce_interval:0.0 ()

let web_channel =
  make ~max_message_length:65536 ~connector:Format_adapter.Plain
    ~parse_mode:"Markdown" ~debounce_interval:0.0 ()

let plain =
  make ~max_message_length:4096 ~connector:Format_adapter.Plain
    ~parse_mode:"Markdown" ~debounce_interval:0.0 ()

let thread_reply_strategy (caps : t) =
  match caps.thread_replies with
  | Native_thread_replies -> Use_native_thread
  | Thread_like_replies -> Use_thread_like_reply
  | No_thread_replies -> Use_room_fallback

let progress_delivery (caps : t) =
  match caps.can_edit with
  | Edit_in_place -> Edit_progress_in_place
  | Delete_and_resend -> Delete_and_resend_progress
  | No_edit -> Buffered_progress

let card_strategy (caps : t) =
  if caps.can_send_cards then Use_cards
  else if caps.can_send_buttons then Use_buttons
  else Use_text_fallback

let history_capture_strategy (caps : t) =
  match caps.history_capture with
  | Ambient_history_capture -> Capture_ambient_history
  | No_history_capture -> Skip_history_capture

let should_capture_history ~enabled caps =
  enabled
  &&
  match history_capture_strategy caps with
  | Capture_ambient_history -> true
  | Skip_history_capture -> false

let supports_rich_questions (caps : t) =
  caps.can_send_buttons
  && (caps.connector = Format_adapter.Telegram_html
     || caps.connector = Format_adapter.Teams)

(** Look up the capability profile for a named connector. Returns [None] for
    unrecognised names. *)
let of_name = function
  | "telegram" -> Some telegram
  | "discord" -> Some discord
  | "slack" -> Some slack
  | "teams" -> Some teams
  | "matrix" -> Some matrix
  | "irc" -> Some irc
  | "mattermost" -> Some mattermost
  | "lark" -> Some lark
  | "line" -> Some line
  | "dingtalk" -> Some dingtalk
  | "onebot" -> Some onebot
  | "nostr" -> Some nostr
  | "imessage" -> Some imessage
  | "email" -> Some email
  | "github" -> Some github
  | "signal" -> Some signal
  | "whatsapp" -> Some whatsapp
  | "web" -> Some web_channel
  | "plain" -> Some plain
  | _ -> None
