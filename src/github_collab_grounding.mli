(** Ground thread and main-Room questions in journal plus optional live GitHub
    state for agent context (P19.M4.E1.T001).

    Combines [Github_item_context_resolve] (room-scoped journal / projection
    history) with an injectable live snapshot fetcher. Never wakes the agent;
    never posts secrets. Under ambiguity, [item_key] stays [None] and
    [prompt_block] asks the caller/agent to clarify — never guess.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type live_snapshot = {
  title : string option;
  state : string option;
  labels : string list;
  head_sha : string option;
  body_excerpt : string option;  (** already redacted by fetcher *)
}

type grounded = {
  room_id : string;
  item_key : string option;
  resolved : Github_item_context_resolve.resolved;
  live : live_snapshot option;
  prompt_block : string;
}

type live_fetch = item_key:string -> (live_snapshot, string) result

val ground :
  db:Sqlite3.db ->
  source:Github_item_context_resolve.source ->
  ?live_fetch:live_fetch ->
  unit ->
  (grounded, string) result
(** Resolve context; optionally merge live snapshot; build [prompt_block]
    secret-free. When [live_fetch] is omitted or returns [Error], grounding
    still succeeds with journal context only ([live = None]). Live-fetch errors
    are not echoed into [prompt_block] (may contain secrets). *)
