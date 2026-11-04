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
