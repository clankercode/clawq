(** Route comment-mode effects for item projections (P19.M3.E1.T003). *)

module E = Github_event_envelope
module S = Github_route_store
module P = Github_item_projection

type comment_effect =
  | Drop
  | Summary of {
      comment_count_delta : int;
      latest_actor : string option;
      latest_at : string option;
    }
  | Threaded of {
      comment_count_delta : int;
      latest_actor : string option;
      latest_at : string option;
      thread_ref : string option;
    }

(** Prefer [event_at], then [received_at], for latest-comment timestamps. *)
let latest_at_of (env : E.t) =
  match env.event_at with Some _ as t -> t | None -> env.received_at

let latest_actor_of (env : E.t) = env.actor.login

(** Thread identity without bodies: delivery id is the durable webhook key. *)
let thread_ref_of (env : E.t) = env.delivery_id

let effect_for ~(mode : S.comment_mode) ~(envelope : E.t) : comment_effect =
  match envelope.family with
  | E.Comment -> (
      match mode with
      | S.Off -> Drop
      | S.Summary ->
          Summary
            {
              comment_count_delta = 1;
              latest_actor = latest_actor_of envelope;
              latest_at = latest_at_of envelope;
            }
      | S.Threaded ->
          Threaded
            {
              comment_count_delta = 1;
              latest_actor = latest_actor_of envelope;
              latest_at = latest_at_of envelope;
              thread_ref = thread_ref_of envelope;
            })
  | E.Lifecycle | E.Review | E.Commit | E.Ci | E.State_update | E.Other _ ->
      Drop

let apply_count (p : P.projection) ~delta ~latest_at : P.projection =
  let last_event_at =
    match latest_at with Some _ as t -> t | None -> p.last_event_at
  in
  {
    p with
    comment_count = p.comment_count + delta;
    last_event_at;
    last_family = Some E.Comment;
  }

let apply_to_projection ~(projection : P.projection) ~(effect : comment_effect)
    : P.projection =
  match effect with
  | Drop -> projection
  | Summary { comment_count_delta; latest_at; latest_actor = _ } ->
      apply_count projection ~delta:comment_count_delta ~latest_at
  | Threaded
      { comment_count_delta; latest_at; latest_actor = _; thread_ref = _ } ->
      apply_count projection ~delta:comment_count_delta ~latest_at
