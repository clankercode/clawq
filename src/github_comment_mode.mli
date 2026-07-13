(** Route comment-mode effects for item projections (P19.M3.E1.T003).

    Maps [Github_route_store.comment_mode] against a normalized envelope family
    into a secret-safe effect: never forward raw comment bodies for [off] or
    [summary]; [threaded] carries only metadata (and optional delivery/thread
    ref) without body text.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

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
          (** delivery or comment id when present on the envelope *)
    }

val effect_for :
  mode:Github_route_store.comment_mode ->
  envelope:Github_event_envelope.t ->
  comment_effect
(** Non-Comment families always [Drop] for comment handling. [off] → [Drop] for
    comments. [summary] → [Summary] without body. [threaded] → [Threaded]
    without raw body fields. *)

val apply_to_projection :
  projection:Github_item_projection.projection ->
  effect:comment_effect ->
  Github_item_projection.projection
(** Apply count/latest metadata; never store bodies. [Drop] is a no-op. *)
