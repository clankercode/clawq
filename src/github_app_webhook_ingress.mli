(** Verified shared GitHub App webhook ingress with durable delivery identity
    (P19.M2.E1.T004).

    Sits in front of normalization: checks path, HMAC signature, delivery ID,
    App/installation identity, subscribed event class, and current repository
    scope. Only [Accepted] outcomes should be handed to downstream work;
    [Rejected] and [Duplicate] never produce work items.

    HTTP acknowledgement is independent of Connector/room delivery: the ledger
    reserves [delivery_id] on [Accepted] so GitHub retries after an early HTTP
    200 become [Duplicate]. This module does not call Connector delivery.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md and
    docs/adr/0002-use-unified-live-github-app-routes.md. *)

type headers = {
  delivery_id : string option;  (** X-GitHub-Delivery *)
  event : string option;  (** X-GitHub-Event *)
  signature_header : string option;  (** X-Hub-Signature-256 *)
  user_agent : string option;
}

type request = { body : string; headers : headers; path : string }

type reject_reason =
  | Bad_signature
  | Missing_delivery_id
  | Duplicate_delivery
  | Unknown_or_suspended_installation
  | Repo_not_in_scope
  | Event_not_subscribed
  | Invalid_payload
  | Wrong_path
  | App_id_mismatch
  | Missing_app_id
  | Missing_installation_id

type accepted = {
  delivery_id : string;
  event : string;
  installation_id : int option;
  app_id : int option;
  repo_full_name : string option;
  action : string option;
  payload : Yojson.Safe.t;
}

type outcome =
  | Accepted of accepted
  | Rejected of { reason : reject_reason; message : string }
  | Duplicate of { delivery_id : string }
      (** Already reserved/ack'd; safe no-op for HTTP 200. *)

val default_path : string
(** Shared App webhook path: [/github/app/webhook]. *)

val default_allowed_events : string list
(** [Github_app_setup_tx.default_events] (includes installation events). *)

val ensure_schema : Sqlite3.db -> unit
(** Durable delivery ledger: delivery_id PRIMARY KEY, received_at, event,
    status. Idempotent. *)

val verify_and_accept :
  db:Sqlite3.db ->
    webhook_secret:string ->
    ?expected_path:string ->
    ?allowed_events:string list ->
    expected_app_id:int ->
  ?now:float ->
  request ->
  outcome
(** Validate and, on success, reserve [delivery_id] in the ledger before
    returning [Accepted]. A configured [expected_app_id] is mandatory. Every
    non-ping event must carry a matching App and installation id before
    normalization; installation events may establish a previously unseen local
    scope, while ping alone may omit both identities. Never invokes
    Connector/delivery. *)

val record_ack : db:Sqlite3.db -> delivery_id:string -> (unit, string) result
(** Optional status update after processing; acceptance already reserved the id.
*)

val was_seen : db:Sqlite3.db -> delivery_id:string -> bool
val reject_reason_to_string : reject_reason -> string
