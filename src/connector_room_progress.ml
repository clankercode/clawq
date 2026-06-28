(** Connector-agnostic dispatch for room progress delivery.

    When a room-origin background task changes state, progress updates need to
    be delivered to the originating room. Each connector (Slack, Teams, Discord)
    has different APIs for sending and editing messages. This module provides a
    uniform dispatch that returns [progress_callbacks] for the connector
    identified in the task's origin metadata.

    This replaces the previous Slack-only branching in [deliver_room_progress]
    with a capability-based dispatch: connectors that support send + edit get
    edit-in-place progress; others are skipped. *)

type progress_callbacks = {
  send :
    room_id:string -> ?thread_id:string -> text:string -> unit -> string Lwt.t;
      (** Send a new message to [room_id], optionally in a thread. Returns the
          connector-assigned message identifier (e.g. Slack [ts], Discord
          [message_id], Teams [activity_id]). Returns ["0"] or [""] if the ID is
          unavailable. *)
  edit : room_id:string -> msg_id:string -> text:string -> unit Lwt.t;
      (** Edit an existing message in place. The [msg_id] is the identifier
          returned by a prior [send] call. Should raise on failure so callers
          can fall back to a fresh send. *)
}
(** Callbacks for delivering progress messages to a specific connector. *)

(** {1 Slack callbacks} *)

let slack_callbacks (slack_config : Runtime_config.slack_config) :
    progress_callbacks =
  let send ~room_id ?thread_id ~text () =
    match thread_id with
    | Some thread_ts ->
        let open Lwt.Syntax in
        let uri = "https://slack.com/api/chat.postMessage" in
        let headers =
          [ ("Authorization", "Bearer " ^ slack_config.bot_token) ]
        in
        let body =
          `Assoc
            [
              ("channel", `String room_id);
              ("text", `String text);
              ("thread_ts", `String thread_ts);
            ]
          |> Yojson.Safe.to_string
        in
        let* _status, resp_body = Http_client.post_json ~uri ~headers ~body in
        let ts =
          try
            let json = Yojson.Safe.from_string resp_body in
            json |> Yojson.Safe.Util.member "ts" |> Yojson.Safe.Util.to_string
          with _ -> "0"
        in
        Lwt.return ts
    | None ->
        Slack.send_message_with_id ~bot_token:slack_config.bot_token
          ~channel_id:room_id ~text
  in
  let edit ~room_id ~msg_id ~text =
    Slack.edit_message ~bot_token:slack_config.bot_token ~channel_id:room_id
      ~ts:msg_id ~text
  in
  { send; edit }

(** {1 Teams callbacks} *)

(** [compose_teams_channel_id ~service_url ~conversation_id
     ~fallback_service_url] builds the [service_url|conversation_id] channel ID
    that Teams APIs expect. Uses the provided [service_url] if non-empty,
    otherwise falls back to [fallback_service_url] (typically from the Teams
    config). *)
let compose_teams_channel_id ~service_url ~conversation_id ~fallback_service_url
    =
  let effective_url =
    if service_url <> "" then service_url else fallback_service_url
  in
  if effective_url <> "" then
    Teams.encode_channel_id ~service_url:effective_url ~conversation_id
  else conversation_id

let teams_callbacks ~(teams_config : Runtime_config.teams_config)
    ~(service_url : string) : progress_callbacks =
  let send ~room_id ?thread_id ~text () =
    let _ = thread_id in
    let channel_id =
      compose_teams_channel_id ~service_url ~conversation_id:room_id
        ~fallback_service_url:teams_config.service_url
    in
    Teams.send_message ~config:teams_config ~channel_id ~text
  in
  let edit ~room_id ~msg_id ~text =
    let channel_id =
      compose_teams_channel_id ~service_url ~conversation_id:room_id
        ~fallback_service_url:teams_config.service_url
    in
    let decoded_service_url, conversation_id =
      Teams.decode_channel_id channel_id
    in
    Teams.edit_activity ~config:teams_config ~service_url:decoded_service_url
      ~conversation_id ~activity_id:msg_id ~text ()
  in
  { send; edit }

(** {1 Discord callbacks} *)

let discord_callbacks (discord_config : Runtime_config.discord_config) :
    progress_callbacks =
  let send ~room_id ?thread_id ~text () =
    let _ = thread_id in
    Discord.send_message_with_id ~bot_token:discord_config.bot_token
      ~channel_id:room_id ~text ()
  in
  let edit ~room_id ~msg_id ~text =
    Discord.edit_message ~bot_token:discord_config.bot_token ~channel_id:room_id
      ~message_id:msg_id ~text
  in
  { send; edit }

(** {1 Dispatch} *)

(** [dispatch ~config ~connector ?service_url ()] returns [Some callbacks] if
    the connector supports room progress delivery (send + edit-in-place), or
    [None] if it does not. The connector name is matched case-insensitively.

    [?service_url] is the connector service URL from the task's origin metadata.
    For Teams, this is required to compose the proper channel ID
    ([service_url|conversation_id]). For other connectors it is ignored.

    Supported connectors:
    - Slack (requires configured slack channel with valid credentials)
    - Teams (requires configured teams channel)
    - Discord (requires configured discord channel with valid credentials) *)
let dispatch ~(config : Runtime_config.t) ~(connector : string)
    ?(service_url = "") () : progress_callbacks option =
  match String.lowercase_ascii connector with
  | "slack" -> (
      match config.channels.slack with
      | Some sc when Runtime_config.slack_has_valid_credentials sc ->
          Some (slack_callbacks sc)
      | _ -> None)
  | "teams" -> (
      match config.channels.teams with
      | Some tc when Runtime_config.teams_has_valid_credentials tc ->
          Some (teams_callbacks ~teams_config:tc ~service_url)
      | _ -> None)
  | "discord" -> (
      match config.channels.discord with
      | Some dc when Runtime_config.discord_has_valid_credentials dc ->
          Some (discord_callbacks dc)
      | _ -> None)
  | _ -> None
