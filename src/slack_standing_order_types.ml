(** Standing-order values (spec §6.1): pure types, string and JSON codecs,
    instruction validation and the per-channel [SO-n] display names. Every
    decoder is total and fail-closed: an unknown value is [None] or [Error],
    never a default. *)

type state = Pending_confirmation | Active | Paused | Suspended | Cancelled

let state_to_string = function
  | Pending_confirmation -> "pending_confirmation"
  | Active -> "active"
  | Paused -> "paused"
  | Suspended -> "suspended"
  | Cancelled -> "cancelled"

let state_of_string = function
  | "pending_confirmation" -> Some Pending_confirmation
  | "active" -> Some Active
  | "paused" -> Some Paused
  | "suspended" -> Some Suspended
  | "cancelled" -> Some Cancelled
  | _ -> None

type trigger = Huddle_notes_ready

let trigger_to_string Huddle_notes_ready = "huddle_notes_ready"

let trigger_of_string = function
  | "huddle_notes_ready" -> Some Huddle_notes_ready
  | _ -> None

type when_no_transcript = Relay_notes | Skip

let when_no_transcript_to_string = function
  | Relay_notes -> "relay_notes"
  | Skip -> "skip"

let when_no_transcript_of_string = function
  | "relay_notes" -> Some Relay_notes
  | "skip" -> Some Skip
  | _ -> None

type target = {
  user_id : string;
  app_id : string;
  bot_id : string;
  label : string;
      (** The operator-configured label: the only target name shown. *)
}

type action =
  | Relay_to_app_mention of {
      target : target;
      instruction : string;
      when_no_transcript : when_no_transcript;
    }

let action_target (Relay_to_app_mention { target; _ }) = target
let max_instruction_chars = 1000

(* C0 bytes never occur inside a multi-byte UTF-8 sequence, so a byte scan is
   exact once the text is known to be valid UTF-8. *)
let validate_instruction raw =
  if not (String.is_valid_utf_8 raw) then
    Error "the instruction is not valid UTF-8 text"
  else
    let text = String.trim raw in
    if text = "" then Error "the instruction is empty"
    else if String.exists (fun c -> c < ' ' && c <> '\n' && c <> '\t') text then
      Error
        "the instruction contains a control character; only plain text, \
         newlines and tabs are allowed"
    else if Slack_config_types.code_points text > max_instruction_chars then
      Error
        (Printf.sprintf "the instruction is longer than %d characters"
           max_instruction_chars)
    else Ok text

let valid_label label =
  label <> ""
  && String.is_valid_utf_8 label
  && Slack_config_types.code_points label
     <= Slack_config_types.max_target_label_chars

let action_kind = "relay_to_app_mention"

let action_to_json
    (Relay_to_app_mention { target; instruction; when_no_transcript }) :
    Yojson.Safe.t =
  `Assoc
    [
      ("kind", `String action_kind);
      ("target_user_id", `String target.user_id);
      ("target_app_id", `String target.app_id);
      ("target_bot_id", `String target.bot_id);
      ("target_label", `String target.label);
      ("instruction_text", `String instruction);
      ( "when_no_transcript",
        `String (when_no_transcript_to_string when_no_transcript) );
    ]

(* Error texts name the field, never its value. *)
let action_of_json (json : Yojson.Safe.t) =
  let ( let* ) = Result.bind in
  let field key =
    match json with
    | `Assoc fields -> (
        match List.assoc_opt key fields with
        | Some (`String s) -> Ok s
        | _ -> Error (key ^ " is missing or not a string"))
    | _ -> Error "action is not a JSON object"
  in
  let id key prefixes =
    let* s = field key in
    if Slack_config_types.valid_id prefixes s then Ok s
    else Error (key ^ " is not a valid Slack id")
  in
  let* kind = field "kind" in
  if kind <> action_kind then Error "unknown action kind"
  else
    let* user_id = id "target_user_id" "UW" in
    let* app_id = id "target_app_id" "A" in
    let* bot_id = id "target_bot_id" "B" in
    let* label = field "target_label" in
    let* label =
      if valid_label label then Ok label else Error "target_label is invalid"
    in
    let* text = field "instruction_text" in
    let* instruction =
      Result.map_error
        (fun e -> "instruction_text is invalid: " ^ e)
        (validate_instruction text)
    in
    let* wnt = field "when_no_transcript" in
    let* when_no_transcript =
      Option.to_result ~none:"when_no_transcript is unknown"
        (when_no_transcript_of_string wnt)
    in
    Ok
      (Relay_to_app_mention
         {
           target = { user_id; app_id; bot_id; label };
           instruction;
           when_no_transcript;
         })

type order = {
  id : string;  (** Opaque random key; users see [display_name] only. *)
  display_no : int;
  team_id : string;
  enterprise_id : string option;
  api_app_id : string;
  channel_id : string;
  grantor_user_id : string;
  principal_id : string option;
  trigger : trigger;
  action : action;
  state : state;
  active_since : string option;
      (** Slack ts of the confirm or latest resume click. *)
  epoch : int;
  revision : int;
  management_channel_id : string;
  management_thread_ts : string;
  max_fires_per_day : int;
  pending_expires_at : float option;
  created_at : string;
  confirmed_at : string option;
  confirmed_by : string option;
  expires_at : string option;
  paused_by : string option;
  suspended_reason : string option;
  cancelled_by : string option;
  updated_at : string;
  bot_user_id : string option;
  bot_id : string option;
  spoken_requests : bool;
  card_version : int;  (** The card copy the grantor confirmed. *)
}

let current_card_version = 1
let display_name (o : order) = "SO-" ^ string_of_int o.display_no

let parse_display s =
  let n = String.length s in
  if n < 4 || n > 9 || String.lowercase_ascii (String.sub s 0 3) <> "so-" then
    None
  else
    let digits = String.sub s 3 (n - 3) in
    if String.for_all (function '0' .. '9' -> true | _ -> false) digits then
      match int_of_string_opt digits with
      | Some v when v >= 1 -> Some v
      | _ -> None
    else None

type fire_state =
  | Detected
  | Fetching
  | Uploading
  | Posting
  | Posting_unknown
  | Posted
  | Acknowledged
  | Picked_up
  | No_pickup
  | Not_delivered
  | No_notes
  | Unavailable
  | Skipped_no_transcript
  | Denied
  | Skipped_inactive
  | Skipped_cap
  | Failed

let all_fire_states =
  [
    Detected;
    Fetching;
    Uploading;
    Posting;
    Posting_unknown;
    Posted;
    Acknowledged;
    Picked_up;
    No_pickup;
    Not_delivered;
    No_notes;
    Unavailable;
    Skipped_no_transcript;
    Denied;
    Skipped_inactive;
    Skipped_cap;
    Failed;
  ]

let fire_state_to_string = function
  | Detected -> "detected"
  | Fetching -> "fetching"
  | Uploading -> "uploading"
  | Posting -> "posting"
  | Posting_unknown -> "posting_unknown"
  | Posted -> "posted"
  | Acknowledged -> "acknowledged"
  | Picked_up -> "picked_up"
  | No_pickup -> "no_pickup"
  | Not_delivered -> "not_delivered"
  | No_notes -> "no_notes"
  | Unavailable -> "unavailable"
  | Skipped_no_transcript -> "skipped_no_transcript"
  | Denied -> "denied"
  | Skipped_inactive -> "skipped_inactive"
  | Skipped_cap -> "skipped_cap"
  | Failed -> "failed"

let fire_state_of_string s =
  List.find_opt (fun st -> fire_state_to_string st = s) all_fire_states

let pre_dispatch_states = [ Detected; Fetching; Uploading ]

let dispatched_states =
  [ Posting; Posting_unknown; Posted; Acknowledged; Picked_up ]

type fire = {
  id : string;
  order_id : string;
  room_id : string;
  date_start : string;
  state : fire_state;
  epoch : int;  (** The order's epoch when the fire was created. *)
  relay_ts : string option;
  reason : string option;
  repost_forbidden : bool;
  cancel_requested : bool;
  reposts : int;
  dispatched_at : float option;
  created_at : string;
  updated_at : string;
}

type card_kind = Confirm_card | Resume_card | Pause_button | Change_card

let card_kind_to_string = function
  | Confirm_card -> "confirm"
  | Resume_card -> "resume"
  | Pause_button -> "pause"
  | Change_card -> "change"

let card_kind_of_string = function
  | "confirm" -> Some Confirm_card
  | "resume" -> Some Resume_card
  | "pause" -> Some Pause_button
  | "change" -> Some Change_card
  | _ -> None

type card = {
  id : string;  (** The button value: an opaque random [soc_…] key. *)
  kind : card_kind;
  order_id : string;
  team_id : string;
  app_id : string;
  channel_id : string;
  reply_thread_ts : string option;
  message_ts : string option;
  plan_id : string option;
  plan_digest : string option;
  order_epoch : int;
  expires_at : float option;
  used_at : string option;
  created_by : string;
}

let valid_slack_ts s =
  Slack_huddles.valid_ts s
  && match float_of_string_opt s with Some v -> v > 0. | None -> false
