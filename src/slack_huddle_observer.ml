(** Huddle observer tap (spec §6.2). Ingress calls [observe] on every event
    after the event-id dedup mark.

    It returns unit, catches every exception and never changes routing. It never
    calls Slack and never runs an LLM. D channels never reach the watch, and
    [slack_huddles] rows are written only for channels the watch observes. It
    must not depend on [Slack] or [Slack_thread_ingress]. *)

module H = Slack_huddles
module C = Slack_huddle_classify
module L = Slack_huddle_ledger

let skipped exn =
  Logs.warn (fun m ->
      m "Slack huddle observer skipped an event: %s" (Printexc.to_string exn))

(* [channel] is the one the watch approved; the classifier reads the same
   path. *)
let apply ~db ~team ~channel ~now = function
  | C.Huddle_seen { root_ts; file_ids; room; channel = _ } ->
      L.record_seen ~db ~team ~channel ~source:"event" ~now ~root_ts ~file_ids
        room
  | C.Notes_ready
      { thread_ts; ts; file_ids; canvas_file_id; variant; channel = _ } ->
      ignore
        (L.record_notes_ready ~db ~team ~channel ~source:"event" ~now ~thread_ts
           ~ts ~file_ids ~canvas_file_id ~variant
          : L.notes_attachment)
  (* Phase 3 adds these effects: relay_ts, fire progress, posture cache. *)
  | C.Own_relay _ | C.Target_reaction _ | C.Target_reply _ | C.Posture_change _
  | C.Irrelevant ->
      ()

(* Phase 1 drops a file reaction without a channel. Phase 3 must resolve its
   [item.file] through the fire ledger to a channel, then apply the watch to
   that channel; otherwise the [item.file] in [uploaded_file_ids] pickup is
   unreachable. *)
let observe_db ~db ~(watch : Slack_huddle_watch.t) ~now envelope =
  try
    let team = H.field "team_id" envelope
    and app = H.field "api_app_id" envelope in
    match C.event_channel envelope with
    | Some channel
      when H.field "type" envelope = "event_callback"
           && H.valid_id "T" team && H.valid_id "A" app
           && H.valid_id "CG" channel
           && watch.observed ~team ~app ~channel ->
        apply ~db ~team ~channel ~now
          (C.classify (watch.context ~team ~app ~channel) envelope)
    | _ -> ()
  with exn -> skipped exn

let observe ~manager envelope =
  try
    match Session.get_db manager with
    | None -> ()
    | Some db ->
        observe_db ~db
          ~watch:(!Slack_huddle_watch.source db)
          ~now:(Unix.gettimeofday ()) envelope
  with exn -> skipped exn
