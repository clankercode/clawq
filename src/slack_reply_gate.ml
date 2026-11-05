(** Channel reply admission under [channels.slack.reply_mode] (B843). Ingress
    pre-checks top-level messages before history and peer lookups, then applies
    the full gate after the relay and peer gates; DMs keep only those. *)

open Slack_huddles

let continuation_window_us = 15 * 60 * 1_000_000

(* A Slack ts in integer microseconds. Float arithmetic can place a reply that
   is exactly fifteen minutes later on either side of the boundary. *)
let micros ts =
  if not (valid_ts ts) then None
  else
    let whole, frac =
      match String.index_opt ts '.' with
      | None -> (ts, "")
      | Some i ->
          (String.sub ts 0 i, String.sub ts (i + 1) (String.length ts - i - 1))
    in
    if
      whole = ""
      || String.length whole > 12
      || String.length frac > 6
      || String.contains frac '.'
    then None
    else
      let frac = frac ^ String.make (6 - String.length frac) '0' in
      match (int_of_string_opt whole, int_of_string_opt frac) with
      | Some s, Some us -> Some ((s * 1_000_000) + us)
      | _ -> None

(* Broadcast and user-group mentions address people other than clawq. Other
   [<!...>] tokens, such as [<!date^...>], are formatting. *)
let addresses_group text =
  List.exists
    (fun token -> String_util.contains text token)
    [ "<!here"; "<!channel"; "<!everyone"; "<!subteam^" ]

let newest_earlier ~current messages =
  List.fold_left
    (fun newest message ->
      match micros (field "ts" message) with
      | Some ts when ts < current -> (
          match newest with
          | Some (prior, _) when prior >= ts -> newest
          | _ -> Some (ts, message))
      | _ -> newest)
    None messages

(* A relay post speaks to a peer assistant, not to the humans in the thread. *)
let own_reply ~bot ~untrusted message =
  field "user" message = bot
  && (not (Slack_relay_guard.is_own_relay_message ~bot message))
  && not (untrusted message)

(** Top-level channel messages cannot be continuations. Keep interrupts
    addressed to clawq even when their text contains no mention. *)
let silent_before_fetch ~reply_mode ~channel ~bot ~reply ~interrupt ~text =
  reply_mode = Slack_config_types.Mention
  && (not (String.starts_with ~prefix:"D" channel))
  && (not reply) && (not interrupt)
  && not (Slack_relay_guard.mentions ~bot text)

(** True when the message must start no turn. [interrupt]: the sender owns the
    thread's active run and wrote a [!] interrupt, which addresses clawq.
    [owner] is the persisted person last answered in this thread. [thread] is
    the fetched thread ([[]] when unread); [untrusted] marks recorded relay
    posts. *)
let silent ~reply_mode ~channel ~bot ~reply ~interrupt ~owner ~sender ~current
    ~text ~thread ~untrusted =
  match reply_mode with
  | Slack_config_types.All -> false
  | Mention -> (
      if
        String.starts_with ~prefix:"D" channel
        || Slack_relay_guard.mentions ~bot text
        || interrupt
      then false
      else if
        (not reply) || owner <> Some sender
        || Slack_peer_thread.mentioned_users text <> []
        || addresses_group text
      then true
      else
        match micros current with
        | None -> true
        | Some current -> (
            match newest_earlier ~current thread with
            | Some (ts, message) ->
                not
                  (own_reply ~bot ~untrusted message
                  && current - ts <= continuation_window_us)
            | None -> true))
