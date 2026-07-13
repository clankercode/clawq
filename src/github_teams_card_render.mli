(** Render GitHub delivery intents as Teams Adaptive Cards (P19.M3.E2.T002).

    Pure JSON construction only — no network, credentials, or comment bodies.
    Create/Update intents produce full lifecycle/update cards; Reply_in_thread
    and Plain_message produce compact cards. Secret-free by construction. *)

val render_adaptive_card : Github_delivery_intent.intent -> Yojson.Safe.t
(** Adaptive Card JSON (schema 1.4) wrapped as a Bot Framework message
    attachment. Includes title, state, labels, and [Action.OpenUrl] when
    [html_url] is set. Create_lifecycle_card and Update_card produce full cards;
    Reply_in_thread / Plain_message produce compact TextBlock cards. *)

val render_update_card : Github_delivery_intent.intent -> Yojson.Safe.t
(** Same Adaptive Card structure used when editing an existing Teams card in
    place (identical envelope/content shape to [render_adaptive_card]). *)

val card_supports_edit : Github_delivery_intent.intent -> bool
(** [true] for [Create_lifecycle_card] and [Update_card]; [false] for thread
    replies and plain messages (those are append-only deliveries). *)
