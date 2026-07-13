(** Connector-neutral delivery intents for GitHub item cards (P19.M3.E2.T001).

    Lifecycle cards, card updates, and thread replies are expressed without
    Teams/Adaptive-Card specifics so any Connector renderer (or plain-text
    fallback) can consume the same intent. Payloads are secret-free structured
    bodies only — never raw comment bodies or credentials.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type intent_kind =
  | Create_lifecycle_card
  | Update_card
  | Reply_in_thread
  | Plain_message

type intent = {
  id : string;
  room_id : string;
  item_key : string;
  kind : intent_kind;
  title : string option;
  summary : string;
  html_url : string option;
  state : string option;
  labels : string list;
  comment_mode : Github_route_store.comment_mode option;
  projection_revision : int option;
  payload : Yojson.Safe.t;  (** secret-free structured body for renderers *)
  created_at : string;
}

val of_projection :
  room_id:string ->
  projection:Github_item_projection.projection ->
  ?comment_mode:Github_route_store.comment_mode ->
  ?prior:Github_item_projection.projection option ->
  ?now:float ->
  unit ->
  intent
(** Lifecycle card if [prior=None] or [projection.card_kind=Lifecycle]; else
    [Update_card]. *)

val of_comment_effect :
  room_id:string ->
  item_key:string ->
  effect:Github_comment_mode.comment_effect ->
  ?now:float ->
  unit ->
  intent option
(** [Drop] → [None]; [Summary] → [Update_card] (summary metadata only);
    [Threaded] → [Reply_in_thread]. Never includes comment bodies. *)

val to_json : intent -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> (intent, string) result
