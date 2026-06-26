(** Typed room session key construction and parsing.

    Session keys encode a channel, channel-specific identifiers, and a session
    kind (Personal / Room / Thread / Routine).  This module provides typed
    accessors so callers don't need to hand-parse colon-separated strings.

    Key formats recognised:
    - Slack:    slack:<channel_id>:<user_id>[:…]
    - Teams:    teams:<team_id>:<conversation_id>[:…]
    - Discord:  discord:<channel_id>:<user_id>[:…]
    - Telegram: telegram:<chat_id>:<user_id>[:…]
    - Web:      web:<session_id>
    - Generic:  anything else (e.g. "__main__", "cli:…") *)

type channel =
  | Slack
  | Teams
  | Discord
  | Telegram
  | Web
  | Generic of string

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
  && String.sub conv_id (len - String.length "@thread.v2")
       (String.length "@thread.v2") = "@thread.v2"

let detect_teams_kind team_id conv_id =
  if is_thread_conversation_id conv_id then Thread
  else if team_id = "personal" then Personal
  else Room

(** [parse key] returns a typed [session] for known key formats.
    Returns [None] for unparseable keys (empty, "__main__", etc.). *)
let parse key =
  if key = "" || key = "__main__" then None
  else
    let parts = String.split_on_char ':' key in
    match parts with
    | [ "slack"; ch_id; uid ] ->
        Some
          {
            channel = Slack;
            kind = Room;
            channel_id = ch_id;
            sender_id = uid;
          }
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
          {
            channel = Teams;
            kind;
            channel_id = team_id;
            sender_id = conv_id;
          }
    | "teams" :: team_id :: conv_id :: rest ->
        let full_conv =
          match rest with
          | [] -> conv_id
          | _ -> conv_id ^ ":" ^ String.concat ":" rest
        in
        let kind = detect_teams_kind team_id full_conv in
        Some
          {
            channel = Teams;
            kind;
            channel_id = team_id;
            sender_id = full_conv;
          }
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
        Some
          {
            channel = Web;
            kind = Room;
            channel_id = sid;
            sender_id = "";
          }
    | _ -> Some { channel = Generic key; kind = Room; channel_id = key; sender_id = "" }

(** [to_key session] reconstructs the canonical session key string. *)
let to_key s =
  match s.channel with
  | Slack ->
      Printf.sprintf "slack:%s:%s" s.channel_id s.sender_id
  | Teams ->
      Printf.sprintf "teams:%s:%s" s.channel_id s.sender_id
  | Discord ->
      Printf.sprintf "discord:%s:%s" s.channel_id s.sender_id
  | Telegram ->
      Printf.sprintf "telegram:%s:%s" s.channel_id s.sender_id
  | Web ->
      Printf.sprintf "web:%s" s.channel_id
  | Generic _ -> s.channel_id

(** [channel_and_id key] extracts (channel_name, channel_id) for callers that
    only need the raw channel string and the rest-of-key identifier.
    This is compatible with [Restart_notify.parse_channel_from_key]. *)
let channel_and_id key =
  match parse key with
  | Some s -> Some (channel_to_string s.channel, s.sender_id)
  | None -> None
