(** Standing-order relay recognition and the relay-thread silence gate.

    Clawq's own relay posts are recognised structurally: the own bot user,
    attached files and the fixed footer as the last line. The registry answers
    the DB-backed checks across both retained attempts of every fire. *)

open Slack_huddles

let footer_prefix = "clawq-relay/1 "

let valid_nonce n =
  String.length n = 32
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) n

let footer ~nonce =
  if valid_nonce nonce then footer_prefix ^ nonce
  else invalid_arg "Slack_relay_guard.footer"

let nonce_of_text text =
  let lines =
    String.split_on_char '\n' text
    |> List.map String.trim
    |> List.filter (fun line -> line <> "")
  in
  match List.rev lines with
  | last :: _ when String.starts_with ~prefix:footer_prefix last ->
      let start = String.length footer_prefix in
      let nonce = String.sub last start (String.length last - start) in
      if valid_nonce nonce then Some nonce else None
  | _ -> None

let file_ids message =
  List.fold_left
    (fun ids file ->
      let id = field "id" file in
      if valid_id "F" id && not (List.mem id ids) then id :: ids else ids)
    []
    (list (get "files" message))
  |> List.rev

(* Never reads [subtype] or [bot_id]: Slack sets [user] and [bot_id] together
   from the posting token, so [bot_id] adds no proof. *)
let is_own_relay_message ~bot message =
  valid_id "UW" bot
  && field "user" message = bot
  && file_ids message <> []
  && nonce_of_text (field "text" message) <> None

let mentions ~bot text =
  String_util.contains text ("<@" ^ bot ^ ">")
  || String_util.contains text ("<@" ^ bot ^ "|")

type registry = {
  recorded_relay : channel:string -> ts:string -> bool;
      (** A fire's recorded [relay_ts]. *)
  in_flight_file : channel:string -> file_id:string -> bool;
      (** One of an in-flight fire's [uploaded_file_ids]. *)
  unconfirmed_since : channel:string -> float option;
      (** Earliest upload start among [posting_unknown] fires and fires awaiting
          their [relay_ts]. *)
}

let no_relays =
  {
    recorded_relay = (fun ~channel:_ ~ts:_ -> false);
    in_flight_file = (fun ~channel:_ ~file_id:_ -> false);
    unconfirmed_since = (fun ~channel:_ -> None);
  }

(* Each lookup's answer on doubt: the thread is a relay thread and the message
   is untrusted. An [unconfirmed_since] of [neg_infinity] counts every own file
   post as posted after an unconfirmed upload. *)
let fail_closed =
  {
    recorded_relay = (fun ~channel:_ ~ts:_ -> true);
    in_flight_file = (fun ~channel:_ ~file_id:_ -> true);
    unconfirmed_since = (fun ~channel:_ -> Some Float.neg_infinity);
  }

let guarded name ~closed lookup =
  try lookup ()
  with exn ->
    Logs.warn (fun m ->
        m "Slack relay registry %s raised (%s); failing closed" name
          (Printexc.to_string exn));
    closed ()

(** [registry] with every lookup total: one that raises logs a warning and
    answers as [fail_closed] does. Every reader of a registry goes through this,
    so a raising lookup (phase 3's DB) never escapes ingress. *)
let total registry =
  {
    recorded_relay =
      (fun ~channel ~ts ->
        guarded "recorded_relay"
          (fun () -> registry.recorded_relay ~channel ~ts)
          ~closed:(fun () -> fail_closed.recorded_relay ~channel ~ts));
    in_flight_file =
      (fun ~channel ~file_id ->
        guarded "in_flight_file"
          (fun () -> registry.in_flight_file ~channel ~file_id)
          ~closed:(fun () -> fail_closed.in_flight_file ~channel ~file_id));
    unconfirmed_since =
      (fun ~channel ->
        guarded "unconfirmed_since"
          (fun () -> registry.unconfirmed_since ~channel)
          ~closed:(fun () -> fail_closed.unconfirmed_since ~channel));
  }

let phase1_registry manager =
  match Session.get_db manager with
  | None -> fail_closed
  | Some db ->
      {
        recorded_relay = Slack_standing_order_store.recorded_relay db;
        in_flight_file = Slack_standing_order_store.in_flight_file db;
        unconfirmed_since = Slack_standing_order_store.unconfirmed_since db;
      }

(* Test seam only; production never assigns it. *)
let registry_source = ref phase1_registry

let current_registry manager =
  guarded "source"
    (fun () -> !registry_source manager)
    ~closed:(fun () -> fail_closed)

(** History's secondary relay check: [message] is a recorded relay post.
    [Slack_thread_api.render_history] applies the structural check itself. *)
let untrusted_in_history ~registry ~channel message =
  (total registry).recorded_relay ~channel ~ts:(field "ts" message)

let posted_after_unconfirmed_upload ~registry ~bot ~channel root =
  field "user" root = bot
  && file_ids root <> []
  &&
  match (total registry).unconfirmed_since ~channel with
  | None -> false
  | Some since ->
      let ts = field "ts" root in
      valid_ts ts && float_of_string ts >= since

(** True when a thread reply must start no turn: the thread is rooted at one of
    clawq's relay posts and the reply does not mention clawq. [root] is [None]
    when the thread could not be read; then only the registry decides. *)
let silent ~registry ~bot ~channel ~root_ts ~root ~text =
  let registry = total registry in
  (not (mentions ~bot text))
  && (registry.recorded_relay ~channel ~ts:root_ts
     ||
     match root with
     | None -> false
     | Some m ->
         is_own_relay_message ~bot m
         || List.exists
              (fun file_id -> registry.in_flight_file ~channel ~file_id)
              (file_ids m)
         || posted_after_unconfirmed_upload ~registry ~bot ~channel m)
