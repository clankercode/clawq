(** Versioned GitHub PR/Issue event envelopes for room routing (P19.M2.E2.T001).

    Normalizes verified webhook payloads into a safe, journal-friendly envelope:
    repo identity, item kind/number, delivery/installation, actor, lifecycle/
    update family, safe before/after metadata (no bodies/secrets), URLs, head
    SHA, and timestamps. Unsupported actions are explicit [Unsupported] results
    rather than silent corruption.

    Independent of [Github_webhook] parse types so routing can consume envelopes
    without the legacy PR-dispatch path. Canonical contract:
    docs/plans/2026-07-12-github-item-room-routing.md. *)

val envelope_version : int
(** Schema version of [t]; starts at 1. *)

type item_kind = Pull_request | Issue

type family =
  | Lifecycle  (** open/close/merge/reopen/ready/draft/transfer *)
  | Review
  | Comment
  | Commit
  | Ci
  | State_update  (** labels, assignees, milestone, title, etc. *)
  | Other of string

type actor = { login : string option; id : int option; type_ : string option }

type safe_state = {
  title : string option;
  state : string option;  (** open/closed *)
  draft : bool option;
  merged : bool option;
  labels : string list;
  assignees : string list;
  milestone : string option;
  head_sha : string option;
  base_ref : string option;  (** No bodies/secrets — safe for journal/cards. *)
  head_ref : string option;
      (** PR [pull_request.head.ref], safe for routing predicates. *)
}

type transfer_info = { from_repo : string option; to_repo : string option }

type t = {
  version : int;
  delivery_id : string option;
  installation_id : int option;
  event : string;  (** X-GitHub-Event name *)
  action : string option;
  repo_full_name : string;
  org : string option;
  item_kind : item_kind option;
  item_number : int option;
  item_node_id : string option;
  item_url : string option;
  html_url : string option;
  family : family;
  actor : actor;
  item_author : string option;
      (** PR/Issue [user.login], distinct from webhook [actor]. *)
  before : safe_state option;
  after : safe_state option;
  transfer : transfer_info option;
  received_at : string option;
  event_at : string option;
      (** created_at/updated_at from payload when present *)
  head_sha : string option;
  unsupported : bool;
  skip_reason : string option;
}

type normalize_result =
  | Ok_envelope of t
  | Unsupported of { event : string; action : string option; reason : string }
  | Error of string

val normalize :
  ?delivery_id:string ->
  ?installation_id:int ->
  ?received_at:string ->
  event:string ->
  payload:Yojson.Safe.t ->
  unit ->
  normalize_result
(** Map a GitHub webhook [event] name + JSON [payload] into a versioned
    envelope.

    - Requires [repository.full_name] (or name+owner) for repo-bound events.
    - Pure installation / non-item events return [Unsupported].
    - Unknown event+action pairs return [Unsupported] with a reason (never
      invent a lifecycle state).
    - Missing optional fields become [None]; never crashes on partial payloads.
    - Bodies, review text, and tokens are never copied into the envelope. *)

val string_of_item_kind : item_kind -> string
val string_of_family : family -> string
val empty_safe_state : safe_state
val empty_actor : actor

val to_safe_json : t -> Yojson.Safe.t
(** Serialize an envelope to JSON suitable for the room event journal. Contains
    only safe metadata already present on [t] (no bodies/secrets). *)

val of_safe_json : Yojson.Safe.t -> (t, string) result
(** Inverse of [to_safe_json]. Used by journal projection reduce. *)

val envelope_of_json : Yojson.Safe.t -> (t, string) result
(** Alias of [of_safe_json]. *)
