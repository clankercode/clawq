(** Huddle observer tap (spec §6.2). Ingress calls [observe] on every event
    after the event-id dedup mark.

    It returns unit, catches every exception and never changes routing. It never
    calls Slack and never runs an LLM. D channels never reach the watch, and
    [slack_huddles] rows are written only for channels the watch observes. It
    must not depend on [Slack] or [Slack_thread_ingress]. *)

module H = Slack_huddles
module C = Slack_huddle_classify
module L = Slack_huddle_ledger

let wake : (unit -> unit) ref = ref (fun () -> ())
let posture_changed : (channel:string -> unit) ref = ref (fun ~channel:_ -> ())

let skipped exn =
  Logs.warn (fun m ->
      m "Slack huddle observer skipped an event: %s" (Printexc.to_string exn))

module Store = Slack_standing_order_store
module Pickup = Slack_standing_order_pickup

(* Classification deliberately omits bot_id. Check the original message against
   every matching order before letting the typed hint reach the durable queue. *)
let verified_own_relay ~db ~team ~app ~channel envelope observation =
  let event = H.get "event" envelope in
  let message =
    if H.field "subtype" event = "message_changed" then H.get "message" event
    else event
  in
  let bot_id = H.field "bot_id" message in
  let orders =
    Slack_huddle_watch.orders db ~team ~app ~channel
    |> List.filter (fun (o : Slack_standing_order_types.order) ->
        Store.list_fires db ~order_id:o.id ~limit:max_int
        |> List.exists (fun (f : Slack_standing_order_types.fire) ->
            match Store.runtime db ~fire_id:f.id with
            | None -> false
            | Some r ->
                List.exists
                  (fun a -> Pickup.matches o r a observation)
                  r.attempts))
  in
  bot_id <> "" && orders <> []
  && List.for_all
       (fun (o : Slack_standing_order_types.order) ->
         o.bot_id = Some bot_id && o.bot_user_id = Some (H.field "user" message))
       orders

(* [channel] and envelope installation are approved by the watch. Fire effects
   enqueue hints only; a worker applies them under its existing DB lease. *)
let apply ~db ~team ~app ~channel ~now = function
  | C.Huddle_seen { root_ts; file_ids; room; channel = _ } ->
      L.record_seen ~db ~team ~channel ~source:"event" ~now ~root_ts ~file_ids
        room;
      !wake ()
  | C.Notes_ready
      { thread_ts; ts; file_ids; canvas_file_id; variant; channel = _ } ->
      ignore
        (L.record_notes_ready ~db ~team ~channel ~source:"event" ~now ~thread_ts
           ~ts ~file_ids ~canvas_file_id ~variant
          : L.notes_attachment);
      Slack_standing_order_schedule.note_ready ~db ~team ~app ~channel
        ~thread_ts ~canvas_file_id ~ready_ts:ts ~now;
      !wake ()
  | (C.Own_relay _ | C.Target_reaction _ | C.Target_reply _) as observation ->
      Pickup.enqueue ~db ~team_id:team ~api_app_id:app ~now observation;
      !wake ()
  | C.Posture_change _ ->
      !posture_changed ~channel;
      !wake ()
  | C.Irrelevant -> ()

let reaction_channel ~db ~team ~app envelope =
  let event = H.get "event" envelope in
  let item = H.get "item" event in
  let file_id = H.field "file" item in
  if
    H.field "type" event = "reaction_added"
    && H.field "type" item = "file"
    && H.get "channel" item = `Null
    && H.valid_id "F" file_id
  then Store.channel_for_file db ~team_id:team ~api_app_id:app ~file_id
  else None

let observe_db ~db ~(watch : Slack_huddle_watch.t) ~now envelope =
  try
    let team = H.field "team_id" envelope
    and app = H.field "api_app_id" envelope in
    if
      H.field "type" envelope = "event_callback"
      && H.valid_id "T" team && H.valid_id "A" app
    then
      let channel =
        match C.event_channel envelope with
        | Some _ as channel -> channel
        | None -> reaction_channel ~db ~team ~app envelope
      in
      match channel with
      | Some channel
        when H.valid_id "CG" channel && watch.observed ~team ~app ~channel ->
          let observation =
            C.classify (watch.context ~team ~app ~channel) envelope
          in
          let permitted =
            match observation with
            | C.Huddle_seen _ | C.Notes_ready _ ->
                watch.huddles ~team ~app ~channel
            | C.Own_relay _ ->
                verified_own_relay ~db ~team ~app ~channel envelope observation
            | _ -> true
          in
          if permitted then apply ~db ~team ~app ~channel ~now observation
      | _ -> ()
  with exn -> skipped exn

let observe ~manager envelope =
  try
    match Session.get_db manager with
    | None -> ()
    | Some db ->
        observe_db ~db
          ~watch:(!Slack_huddle_watch.source db)
          ~now:(Store.now ()) envelope
  with exn -> skipped exn
