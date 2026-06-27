(** Typed room session key construction and parsing.

    Session keys encode a channel, channel-specific identifiers, and a session
    kind (Personal / Room / Thread / Routine). This module provides typed
    accessors so callers don't need to hand-parse colon-separated strings.

    Key formats recognised:
    - Slack: slack:<channel_id>:<user_id>[:…]
    - Teams: teams:<team_id>:<conversation_id>[:…]
    - Discord: discord:<channel_id>:<user_id>[:…]
    - Telegram: telegram:<chat_id>:<user_id>[:…]
    - Web: web:<session_id>
    - Generic: anything else (e.g. "__main__", "cli:…") *)

type channel = Slack | Teams | Discord | Telegram | Web | Generic of string
type session_kind = Personal | Room | Thread | Routine

type session = {
  channel : channel;
  kind : session_kind;
  channel_id : string;
  sender_id : string;
}

let channel_to_string = function
  | Slack -> "slack"
  | Teams -> "teams"
  | Discord -> "discord"
  | Telegram -> "telegram"
  | Web -> "web"
  | Generic s -> s

let kind_to_string = function
  | Personal -> "personal"
  | Room -> "room"
  | Thread -> "thread"
  | Routine -> "routine"

let kind_of_string = function
  | "personal" -> Some Personal
  | "room" -> Some Room
  | "thread" -> Some Thread
  | "routine" -> Some Routine
  | _ -> None

let is_thread_conversation_id conv_id =
  let len = String.length conv_id in
  len >= String.length "@thread.v2"
  && String.sub conv_id
       (len - String.length "@thread.v2")
       (String.length "@thread.v2")
     = "@thread.v2"

let detect_teams_kind team_id conv_id =
  if is_thread_conversation_id conv_id then Thread
  else if team_id = "personal" then Personal
  else Room

(** [parse key] returns a typed [session] for known key formats. Returns [None]
    for unparseable keys (empty, "__main__", etc.). *)
let parse key =
  if key = "" || key = "__main__" then None
  else
    let parts = String.split_on_char ':' key in
    match parts with
    | [ "slack"; ch_id; uid ] ->
        Some
          { channel = Slack; kind = Room; channel_id = ch_id; sender_id = uid }
    | "slack" :: ch_id :: uid :: _rest ->
        Some
          {
            channel = Slack;
            kind = Room;
            channel_id = ch_id;
            sender_id = String.concat ":" (uid :: _rest);
          }
    | [ "teams"; team_id; conv_id ] ->
        let kind = detect_teams_kind team_id conv_id in
        Some
          { channel = Teams; kind; channel_id = team_id; sender_id = conv_id }
    | "teams" :: team_id :: conv_id :: rest ->
        let full_conv =
          match rest with
          | [] -> conv_id
          | _ -> conv_id ^ ":" ^ String.concat ":" rest
        in
        let kind = detect_teams_kind team_id full_conv in
        Some
          { channel = Teams; kind; channel_id = team_id; sender_id = full_conv }
    | [ "discord"; ch_id; uid ] ->
        Some
          {
            channel = Discord;
            kind = Room;
            channel_id = ch_id;
            sender_id = uid;
          }
    | "discord" :: ch_id :: uid :: _rest ->
        Some
          {
            channel = Discord;
            kind = Room;
            channel_id = ch_id;
            sender_id = String.concat ":" (uid :: _rest);
          }
    | [ "telegram"; ch_id; uid ] ->
        Some
          {
            channel = Telegram;
            kind = Room;
            channel_id = ch_id;
            sender_id = uid;
          }
    | "telegram" :: ch_id :: uid :: _rest ->
        Some
          {
            channel = Telegram;
            kind = Room;
            channel_id = ch_id;
            sender_id = String.concat ":" (uid :: _rest);
          }
    | [ "web"; sid ] ->
        Some { channel = Web; kind = Room; channel_id = sid; sender_id = "" }
    | _ ->
        Some
          {
            channel = Generic key;
            kind = Room;
            channel_id = key;
            sender_id = "";
          }

(** [to_key session] reconstructs the canonical session key string. *)
let to_key s =
  match s.channel with
  | Slack -> Printf.sprintf "slack:%s:%s" s.channel_id s.sender_id
  | Teams -> Printf.sprintf "teams:%s:%s" s.channel_id s.sender_id
  | Discord -> Printf.sprintf "discord:%s:%s" s.channel_id s.sender_id
  | Telegram -> Printf.sprintf "telegram:%s:%s" s.channel_id s.sender_id
  | Web -> Printf.sprintf "web:%s" s.channel_id
  | Generic _ -> s.channel_id

(** [channel_and_id key] extracts (channel_name, channel_id) for callers that
    only need the raw channel string and the rest-of-key identifier. This is
    compatible with [Restart_notify.parse_channel_from_key]. *)
let channel_and_id key =
  match parse key with
  | Some s -> Some (channel_to_string s.channel, s.sender_id)
  | None -> None

let child_thread_prefix = "__room_child_thread"
let routine_prefix = "__room_routine"

type child_thread = {
  connector : string;
  profile_id : string;
  room_id : string;
  thread_id : string option;
  source_message_id : string option;
}

type routine = { profile_id : string; routine_id : string }

let is_unreserved = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' -> true
  | _ -> false

let percent_encode value =
  let buf = Buffer.create (String.length value) in
  String.iter
    (fun c ->
      if is_unreserved c then Buffer.add_char buf c
      else Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    value;
  Buffer.contents buf

let hex_value = function
  | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' as c -> Some (10 + Char.code c - Char.code 'a')
  | 'A' .. 'F' as c -> Some (10 + Char.code c - Char.code 'A')
  | _ -> None

let percent_decode value =
  let len = String.length value in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len then Some (Buffer.contents buf)
    else
      match value.[i] with
      | '%' when i + 2 < len -> (
          match (hex_value value.[i + 1], hex_value value.[i + 2]) with
          | Some hi, Some lo ->
              Buffer.add_char buf (Char.chr ((hi * 16) + lo));
              loop (i + 3)
          | _ -> None)
      | '%' -> None
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop 0

let nonempty_option = function
  | Some "" | None -> None
  | Some _ as value -> value

let option_to_key_part = function
  | None -> ""
  | Some value -> percent_encode value

let key_part_to_option value =
  if value = "" then Some None
  else Option.map Option.some (percent_decode value)

let child_thread_key ?thread_id ?source_message_id ~profile_id ~connector
    ~room_id () =
  let thread_id = nonempty_option thread_id in
  let source_message_id = nonempty_option source_message_id in
  if thread_id = None && source_message_id = None then
    invalid_arg "child_thread_key: thread_id or source_message_id is required";
  String.concat ":"
    [
      child_thread_prefix;
      percent_encode connector;
      percent_encode profile_id;
      percent_encode room_id;
      option_to_key_part thread_id;
      option_to_key_part source_message_id;
    ]

let make_child_thread_key = child_thread_key

let parse_child_thread_key key =
  match String.split_on_char ':' key with
  | [ prefix; connector; profile_id; room_id; thread_id; source_message_id ]
    when prefix = child_thread_prefix -> (
      match
        ( percent_decode connector,
          percent_decode profile_id,
          percent_decode room_id,
          key_part_to_option thread_id,
          key_part_to_option source_message_id )
      with
      | ( Some connector,
          Some profile_id,
          Some room_id,
          Some thread_id,
          Some source_message_id ) ->
          Some { connector; profile_id; room_id; thread_id; source_message_id }
      | _ -> None)
  | _ -> None

let routine_key ~profile_id ~routine_id () =
  String.concat ":"
    [ routine_prefix; percent_encode profile_id; percent_encode routine_id ]

let make_routine_key = routine_key

let parse_routine_key key =
  match String.split_on_char ':' key with
  | [ prefix; profile_id; routine_id ] when prefix = routine_prefix -> (
      match (percent_decode profile_id, percent_decode routine_id) with
      | Some profile_id, Some routine_id -> Some { profile_id; routine_id }
      | _ -> None)
  | _ -> None
