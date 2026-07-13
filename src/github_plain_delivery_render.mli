(** Plain-text and editless fallbacks for GitHub delivery intents
    (P19.M3.E2.T003).

    Teams-capable destinations may use Adaptive Cards; other Connectors and
    Direct Sessions use capability-selected plain or editless text. All
    renderers consume the same secret-free [Github_delivery_intent.intent]. *)

val render_plain : Github_delivery_intent.intent -> string
(** Markdown-ish plain text for Telegram and other text Connectors that can edit
    messages in place. Compact deterministic projection of title, state, labels,
    summary, and optional link. *)

val render_editless : Github_delivery_intent.intent -> string
(** Full replacement message text when the connector cannot edit. Same shape as
    [render_plain] plus an explicit degraded/weaker-continuity note so clients
    never silently pretend in-place card continuity. *)

val select_renderer :
  supports_adaptive_cards:bool ->
  supports_edit:bool ->
  Github_delivery_intent.intent ->
  [ `Adaptive_card | `Plain | `Editless_plain ]
(** Centralized capability selection for route/delivery code. Prefer Adaptive
    Cards when supported; otherwise plain text when edit is available; else
    editless full-replacement text. Not Teams-only. *)
