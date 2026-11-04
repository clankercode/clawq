(** Pure huddle event classifier (spec §6.2, P0-observed payload shapes).

    Reads fixed top-level JSON paths only: never a recursive search, never
    metadata, blocks or attachments text. Every observation is a hint;
    provenance is proven elsewhere. No Lwt, no DB, no Slack calls. *)

open Slack_huddles

type room = {
  room_id : string;
  has_ended : bool;
  date_start : int option;
  date_end : int option;
  thread_root_ts : string option;
  canvas_thread_ts : string option;
  attached_file_ids : string list;
  transcript_file_id : string option;
      (** [room.transcript_file_id] when it is an [F…] id: Slack's
          room-to-notes-canvas link (P0: equals the canvas id), a hint only. *)
}

type notice = Rate_limited | Spend_limit | Disabled | Not_delivered
type text_kind = Notice of notice | Reply  (** Matches no known notice. *)
type notes_variant = Notes_and_transcript | Notes_only | Variant_unknown

type observation =
  | Huddle_seen of {
      channel : string;
      root_ts : string;
      file_ids : string list;
          (** The root's [files[]]: the canvas is here (P0). *)
      room : room;
    }
  | Notes_ready of {
      channel : string;
      thread_ts : string;
      ts : string;
      file_ids : string list;
      canvas_file_id : string;
      variant : notes_variant;
    }
  | Own_relay of {
      channel : string;
      ts : string;
      file_ids : string list;
      nonce : string;
    }
  | Target_reaction of {
      channel : string option;
          (** [None]: Slack documents the file item without a channel. *)
      user : string;
      item_ts : string option;
      item_file : string option;
    }
  | Target_reply of {
      channel : string;
      thread_ts : string;
      ts : string;
      bot_id : string;
      app_id : string option;
      username : string option;
      kind_if_first : text_kind;
      kind_if_later : text_kind;
    }
  | Posture_change of { channel : string }
  | Irrelevant

type target = {
  target_user_id : string;
  target_bot_id : string;
  target_app_id : string;
}

type matchers = {
  first_reply : (string * notice) list;
  any_reply : (string * notice) list;
}
(** Exact notice texts. [first_reply] applies to the first target reply only
    (rate_limited, not_delivered); [any_reply] to every reply (spend_limit,
    disabled). *)

type context = {
  self_user_id : string option;
  targets : target list;
  matchers : matchers;
}

(* Empty until Claude's notice wordings are captured: every reply is Reply. *)
let no_matchers = { first_reply = []; any_reply = [] }

let empty_context =
  { self_user_id = None; targets = []; matchers = no_matchers }

let posture_event_types =
  [
    "member_joined_channel";
    "member_left_channel";
    "channel_shared";
    "channel_unshared";
  ]

let notes_variant_to_string = function
  | Notes_and_transcript -> "notes_and_transcript"
  | Notes_only -> "notes_only"
  | Variant_unknown -> "unknown"

let text_kind matchers ~first text =
  let t = String.trim text in
  match List.assoc_opt t matchers.any_reply with
  | Some notice -> Notice notice
  | None -> (
      match if first then List.assoc_opt t matchers.first_reply else None with
      | Some notice -> Notice notice
      | None -> Reply)

let string_opt key json =
  match get key json with `String s -> Some s | _ -> None

let is_object = function `Assoc _ -> true | _ -> false
let absent key json = get key json = `Null

let ts_opt key json =
  match string_opt key json with Some s when valid_ts s -> Some s | _ -> None

let id_opt prefixes key json =
  match string_opt key json with
  | Some s when valid_id prefixes s -> Some s
  | _ -> None

let digits s =
  let n = String.length s in
  n >= 1 && n <= 18
  && String.for_all (function '0' .. '9' -> true | _ -> false) s

(* Unix seconds: no signs, exponents or fractions in any encoding. *)
let seconds = function
  | `Int n when n >= 0 -> Some n
  | `Float f when Float.is_integer f && f >= 0. && f < 1e18 ->
      Some (Float.to_int f)
  | (`String s | `Intlit s) when digits s -> int_of_string_opt s
  | _ -> None

(* [None] unless [json] is an object with an [R…] id. *)
let room_of_json json =
  match id_opt "R" "id" json with
  | None -> None
  | Some room_id ->
      let date_end =
        match seconds (get "date_end" json) with Some 0 -> None | d -> d
      in
      let attached_file_ids =
        List.fold_left
          (fun ids v ->
            match v with
            | `String id when valid_id "F" id && not (List.mem id ids) ->
                id :: ids
            | _ -> ids)
          []
          (list (get "attached_file_ids" json))
        |> List.rev
      in
      Some
        {
          room_id;
          has_ended =
            get "has_ended" json = `Bool true || Option.is_some date_end;
          date_start = seconds (get "date_start" json);
          date_end;
          thread_root_ts = ts_opt "thread_root_ts" json;
          canvas_thread_ts = ts_opt "canvas_thread_ts" json;
          attached_file_ids;
          transcript_file_id = id_opt "F" "transcript_file_id" json;
        }

let canvas_mimetype = "application/vnd.slack-docs"

let huddle ~channel m =
  let room = get "room" m in
  let is_huddle =
    field "call_family" room = "huddle" || field "subtype" m = "huddle_thread"
  in
  match (room_of_json room, ts_opt "ts" m) with
  | Some room, Some root_ts when is_huddle ->
      Some
        (Huddle_seen
           { channel; root_ts; file_ids = Slack_relay_guard.file_ids m; room })
  | _ -> None

let variant_of_text text =
  let t = String.lowercase_ascii text in
  if String_util.contains t "notes and transcript are ready" then
    Notes_and_transcript
  else if String_util.contains t "notes are ready" then Notes_only
  else Variant_unknown

(* File ownership is never read: the canvas belongs to the huddle starter. *)
let notes_ready ~channel m =
  match (ts_opt "ts" m, ts_opt "thread_ts" m, Slack_relay_guard.file_ids m) with
  | Some ts, Some thread_ts, (first :: _ as file_ids)
    when field "user" m = slackbot_user_id
         && absent "bot_id" m && thread_ts <> ts ->
      let canvas =
        List.find_map
          (fun file ->
            match id_opt "F" "id" file with
            | Some id when field "mimetype" file = canvas_mimetype -> Some id
            | _ -> None)
          (list (get "files" m))
      in
      Some
        (Notes_ready
           {
             channel;
             thread_ts;
             ts;
             file_ids;
             canvas_file_id = Option.value canvas ~default:first;
             variant = variant_of_text (field "text" m);
           })
  | _ -> None

let own_relay ctx ~channel m =
  match (ctx.self_user_id, ts_opt "ts" m) with
  | Some bot, Some ts when Slack_relay_guard.is_own_relay_message ~bot m -> (
      let top_level =
        match get "thread_ts" m with
        | `Null -> true
        | `String thread_ts -> thread_ts = ts
        | _ -> false
      in
      match Slack_relay_guard.nonce_of_text (field "text" m) with
      | Some nonce when top_level ->
          Some
            (Own_relay
               { channel; ts; file_ids = Slack_relay_guard.file_ids m; nonce })
      | _ -> None)
  | _ -> None

(* Every present app id must be the target's; an absent one is not evidence. *)
let target_reply ctx ~channel m =
  let bot_id = field "bot_id" m in
  let profile_app = get "app_id" (get "bot_profile" m) in
  let top_app = get "app_id" m in
  let app_agrees t = function
    | `Null -> true
    | `String app -> app = t.target_app_id
    | _ -> false
  in
  let is_target t =
    bot_id <> "" && t.target_bot_id = bot_id && app_agrees t profile_app
    && app_agrees t top_app
  in
  match (ts_opt "ts" m, ts_opt "thread_ts" m) with
  | Some ts, Some thread_ts
    when thread_ts <> ts && List.exists is_target ctx.targets ->
      let text = field "text" m in
      let app_id =
        match profile_app with
        | `String app -> Some app
        | _ -> string_opt "app_id" m
      in
      Some
        (Target_reply
           {
             channel;
             thread_ts;
             ts;
             bot_id;
             app_id;
             username = string_opt "username" m;
             kind_if_first = text_kind ctx.matchers ~first:true text;
             kind_if_later = text_kind ctx.matchers ~first:false text;
           })
  | _ -> None

(* [event.subtype] picks the message once; after that only [m] is read. *)
let message ctx event =
  match id_opt "CG" "channel" event with
  | None -> Irrelevant
  | Some channel -> (
      let m =
        match get "subtype" event with
        | `String "message_changed" ->
            let inner = get "message" event in
            if is_object inner then Some inner else None
        | `String "message_deleted" -> None
        | _ -> Some event
      in
      match m with
      | None -> Irrelevant
      | Some m ->
          let rules =
            [
              huddle ~channel;
              notes_ready ~channel;
              own_relay ctx ~channel;
              target_reply ctx ~channel;
            ]
          in
          Option.value
            (List.find_map (fun rule -> rule m) rules)
            ~default:Irrelevant)

let reaction ctx event =
  let user = field "user" event in
  let item = get "item" event in
  let from_target =
    user <> "" && List.exists (fun t -> t.target_user_id = user) ctx.targets
  in
  if not (from_target && is_object item) then Irrelevant
  else
    let channel = id_opt "CG" "channel" item in
    match field "type" item with
    | "message" -> (
        match (channel, ts_opt "ts" item) with
        | Some _, (Some _ as item_ts) ->
            Target_reaction { channel; user; item_ts; item_file = None }
        | _ -> Irrelevant)
    | "file" -> (
        match id_opt "F" "file" item with
        | Some _ as item_file ->
            Target_reaction { channel; user; item_ts = None; item_file }
        | None -> Irrelevant)
    | _ -> Irrelevant

let classify ctx envelope =
  let event = get "event" envelope in
  if field "type" envelope <> "event_callback" || not (is_object event) then
    Irrelevant
  else
    match field "type" event with
    | "reaction_added" -> reaction ctx event
    | kind when List.mem kind posture_event_types -> (
        match id_opt "CG" "channel" event with
        | Some channel -> Posture_change { channel }
        | None -> Irrelevant)
    | "message" -> message ctx event
    | _ -> Irrelevant

let event_channel envelope =
  let event = get "event" envelope in
  match string_opt "channel" event with
  | Some _ as channel -> channel
  | None -> string_opt "channel" (get "item" event)

let channel_of = function
  | Huddle_seen { channel; _ }
  | Notes_ready { channel; _ }
  | Own_relay { channel; _ }
  | Target_reply { channel; _ }
  | Posture_change { channel } ->
      Some channel
  | Target_reaction { channel; _ } -> channel
  | Irrelevant -> None
