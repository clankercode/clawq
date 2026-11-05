(** Slack config values shared by core config and the Slack integration. *)

(** The one Slack id rule: 2..64 chars, first in [prefixes], all [A-Z0-9]. *)
let valid_id prefixes s =
  String.length s > 1
  && String.length s <= 64
  && String.contains prefixes s.[0]
  && String.for_all (function 'A' .. 'Z' | '0' .. '9' -> true | _ -> false) s

type event_log_text_rule = { text_channel_id : string; text_bot_id : string }

type event_log = {
  enabled : bool;
  capture_text_from : event_log_text_rule list;
      (** Exact channel and bot id pairs whose message text is logged too. No
          wildcards. *)
}

let default_event_log = { enabled = false; capture_text_from = [] }

let warn reason =
  Logs.warn (fun m -> m "Config: 'channels.slack.event_log': %s" reason)

let text_rule_of_json = function
  | `Assoc fields -> (
      match
        (List.assoc_opt "channel" fields, List.assoc_opt "bot_id" fields)
      with
      | Some (`String channel), Some (`String bot_id)
        when valid_id "CG" channel && valid_id "B" bot_id ->
          Some { text_channel_id = channel; text_bot_id = bot_id }
      | _ -> None)
  | _ -> None

(* Rule values are never printed: they are ids, but a malformed one may be
   anything an operator pasted. *)
let event_log_of_json = function
  | `Null -> default_event_log
  | `Assoc fields ->
      let enabled =
        match List.assoc_opt "enabled" fields with
        | Some (`Bool b) -> b
        | _ -> false
      in
      let rules =
        match List.assoc_opt "capture_text_from" fields with
        | Some (`List rules) -> rules
        | None | Some `Null -> []
        | Some _ ->
            warn "capture_text_from is not a list; no text is captured";
            []
      in
      let capture_text_from =
        List.mapi
          (fun i rule ->
            match text_rule_of_json rule with
            | Some _ as kept -> kept
            | None ->
                warn
                  (Printf.sprintf
                     "dropped capture_text_from rule %d (needs an exact C/G \
                      channel id and B bot id)"
                     i);
                None)
          rules
        |> List.filter_map Fun.id
      in
      { enabled; capture_text_from }
  | _ ->
      warn "not an object; the event log stays off";
      default_event_log

let event_log_to_json e : Yojson.Safe.t =
  `Assoc
    [
      ("enabled", `Bool e.enabled);
      ( "capture_text_from",
        `List
          (List.map
             (fun r ->
               `Assoc
                 [
                   ("channel", `String r.text_channel_id);
                   ("bot_id", `String r.text_bot_id);
                 ])
             e.capture_text_from) );
    ]

(** Standing orders: which bots a channel may grant, who may create an order,
    and a kill switch. *)
type standing_order_creator_policy = So_creator_member | So_creator_admin

type standing_order_target_config = {
  app_id : string;
  label : string;
      (** Operator-chosen name; the only target name cards and notices show. *)
}

type slack_standing_orders_config = {
  orders_enabled : bool;
      (** JSON key ["enabled"]. Named apart from [event_log.enabled] so that
          unannotated [x.enabled] keeps resolving to [event_log]. *)
  allowed_targets : standing_order_target_config list;
      (** Empty denies every order. *)
  creator_policy : standing_order_creator_policy;
}

let default_standing_orders =
  {
    orders_enabled = true;
    allowed_targets = [];
    creator_policy = So_creator_member;
  }

let max_target_label_chars = 80

let standing_order_creator_policy_to_string = function
  | So_creator_member -> "member"
  | So_creator_admin -> "admin"

let standing_order_creator_policy_of_string = function
  | "member" -> Some So_creator_member
  | "admin" -> Some So_creator_admin
  | _ -> None

let warn_orders reason =
  Logs.warn (fun m -> m "Config: 'channels.slack.standing_orders': %s" reason)

(* Callers check [String.is_valid_utf_8] first, so this is exact. *)
let code_points s =
  let rec go i n =
    if i >= String.length s then n
    else go (i + Uchar.utf_decode_length (String.get_utf_8_uchar s i)) (n + 1)
  in
  go 0 0

let target_of_json = function
  | `String app_id when valid_id "A" app_id -> Some { app_id; label = app_id }
  | `Assoc fields -> (
      let label =
        match List.assoc_opt "label" fields with
        | None | Some `Null -> `Missing
        | Some (`String l) -> `Label (String.trim l)
        | Some _ -> `Invalid
      in
      match (List.assoc_opt "app_id" fields, label) with
      | Some (`String app_id), `Missing when valid_id "A" app_id ->
          Some { app_id; label = app_id }
      | Some (`String app_id), `Label label
        when valid_id "A" app_id && label <> ""
             && String.is_valid_utf_8 label
             && code_points label <= max_target_label_chars ->
          Some { app_id; label }
      | _ -> None)
  | _ -> None

(* Values are never printed: an entry is identified by its index only. *)
let targets_of_json items =
  List.mapi (fun i item -> (i, item)) items
  |> List.fold_left
       (fun kept (i, item) ->
         match target_of_json item with
         | None ->
             warn_orders
               (Printf.sprintf
                  "dropped allowed_targets entry %d (needs an A app id and a \
                   non-empty UTF-8 label of at most %d characters)"
                  i max_target_label_chars);
             kept
         | Some t when List.exists (fun k -> k.app_id = t.app_id) kept ->
             warn_orders
               (Printf.sprintf
                  "dropped allowed_targets entry %d (repeats an earlier app id)"
                  i);
             kept
         | Some t -> t :: kept)
       []
  |> List.rev

let standing_orders_of_json = function
  | `Null -> default_standing_orders
  | `Assoc fields ->
      let orders_enabled =
        match List.assoc_opt "enabled" fields with
        | None | Some `Null -> default_standing_orders.orders_enabled
        | Some (`Bool b) -> b
        | Some _ ->
            warn_orders "enabled is not a bool; standing orders are off";
            false
      in
      let allowed_targets =
        match List.assoc_opt "allowed_targets" fields with
        | None | Some `Null -> []
        | Some (`List items) -> targets_of_json items
        | Some _ ->
            warn_orders "allowed_targets is not a list; every target is denied";
            []
      in
      let creator_policy =
        match List.assoc_opt "creator_policy" fields with
        | None | Some `Null -> default_standing_orders.creator_policy
        | Some (`String s) -> (
            match standing_order_creator_policy_of_string s with
            | Some p -> p
            | None ->
                warn_orders "unknown creator_policy; using admin";
                So_creator_admin)
        | Some _ ->
            warn_orders "creator_policy is not a string; using admin";
            So_creator_admin
      in
      { orders_enabled; allowed_targets; creator_policy }
  | _ ->
      warn_orders "not an object; using the defaults (no targets allowed)";
      default_standing_orders

let standing_orders_to_json o : Yojson.Safe.t =
  `Assoc
    [
      ("enabled", `Bool o.orders_enabled);
      ( "allowed_targets",
        `List
          (List.map
             (fun t ->
               `Assoc
                 [ ("app_id", `String t.app_id); ("label", `String t.label) ])
             o.allowed_targets) );
      ( "creator_policy",
        `String (standing_order_creator_policy_to_string o.creator_policy) );
    ]

(** Channel reply admission; DMs retain their existing gates. *)
type reply_mode = Mention | All

let reply_mode_to_string = function Mention -> "mention" | All -> "all"

let reply_mode_of_json = function
  | `Null | `String "mention" -> Mention
  | `String "all" -> All
  | _ ->
      Logs.warn (fun m ->
          m
            "Config: 'channels.slack.reply_mode': expected mention or all; \
             using mention");
      Mention

(** Final-answer presentation policy. Values are read from current config at
    delivery entry, rather than cached in a connector. *)
type long_reply_mode = Canvas | File | Off

type long_reply = { mode : long_reply_mode; threshold : int }

let default_long_reply = { mode = Canvas; threshold = 2500 }

let long_reply_mode_to_string = function
  | Canvas -> "canvas"
  | File -> "file"
  | Off -> "off"

let long_reply_mode_of_json = function
  | `String "canvas" -> Some Canvas
  | `String "file" -> Some File
  | `String "off" -> Some Off
  | _ -> None

let long_reply_threshold_of_json = function
  | `Int n when n >= 1 && n <= 40000 -> Some n
  | _ -> None

let long_reply_error leaf =
  let key = "channels.slack.long_reply" in
  let key, requirement, example =
    match leaf with
    | "mode" -> (key ^ ".mode", "must be canvas, file or off", "mode canvas")
    | "threshold" ->
        ( key ^ ".threshold",
          "must be an integer from 1 to 40000",
          "threshold 2500" )
    | _ ->
        ( key,
          "must be an object containing only mode and threshold",
          "threshold 2500" )
  in
  Error
    (Printf.sprintf
       "Error: %s %s. Example: clawq config set channels.slack.long_reply.%s"
       key requirement example)

let validate_long_reply = function
  | `Assoc fields ->
      let rec check = function
        | [] -> Ok ()
        | ("mode", v) :: rest ->
            if Option.is_some (long_reply_mode_of_json v) then check rest
            else long_reply_error "mode"
        | ("threshold", v) :: rest ->
            if Option.is_some (long_reply_threshold_of_json v) then check rest
            else long_reply_error "threshold"
        | _ -> long_reply_error ""
      in
      check fields
  | _ -> long_reply_error ""

let warn_long_reply field reason =
  Logs.warn (fun m ->
      m "Config: 'channels.slack.long_reply%s': %s" field reason)

let long_reply_of_json = function
  | `Null -> default_long_reply
  | `Assoc fields ->
      let field key parse fallback =
        match List.assoc_opt key fields with
        | None -> fallback
        | Some v -> (
            match parse v with
            | Some value -> value
            | None ->
                warn_long_reply ("." ^ key) "invalid value; using default";
                fallback)
      in
      if List.exists (fun (k, _) -> k <> "mode" && k <> "threshold") fields then
        warn_long_reply "" "unknown fields ignored";
      {
        mode = field "mode" long_reply_mode_of_json default_long_reply.mode;
        threshold =
          field "threshold" long_reply_threshold_of_json
            default_long_reply.threshold;
      }
  | _ ->
      warn_long_reply "" "expected an object; using defaults";
      default_long_reply

let long_reply_to_json p : Yojson.Safe.t =
  `Assoc
    [
      ("mode", `String (long_reply_mode_to_string p.mode));
      ("threshold", `Int p.threshold);
    ]
