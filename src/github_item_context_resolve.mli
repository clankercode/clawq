(** Resolve human card actions, thread replies, and Room mentions to bounded
    item context for agent invocation (P19.M3.E2.T004).

    Webhook delivery never wakes the agent; only human interaction paths call
    [resolve]. Access is room-scoped: only that room's journal and projections
    are visible. Under ambiguity, [item_key] stays [None] and [ambiguity] lists
    candidates — never guess.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type source =
  | Card_action of { action : string; item_key : string; room_id : string }
  | Thread_reply of {
      room_id : string;
      thread_ref : string option;
      text : string;
    }
  | Room_mention of {
      room_id : string;
      text : string;
      item_key_hint : string option;
    }

type resolved = {
  room_id : string;
  item_key : string option;
  projection : Github_item_projection.projection option;
  history : Github_room_event_journal.journal_entry list;
  context_block : string;
  ambiguity : string list;  (** candidate item_keys if multiple match *)
}

val resolve :
  db:Sqlite3.db -> source:source -> unit -> (resolved, string) result
(** [Card_action]: use [item_key] directly (room-scoped load). [Thread_reply]:
    match [thread_ref] against journal delivery_ids / session_message_ids /
    item_keys; fall back to refs in [text]. [Room_mention]: parse PR/issue refs
    from [text] or use [item_key_hint]; if multiple candidates → [ambiguity]
    nonempty and [item_key = None]. Never invokes the agent. *)

val parse_item_refs : text:string -> string list
(** Extract [owner/repo#N] or [#N] patterns as candidate refs (order of
    appearance, unique). Does not invent full [pr:]/[issue:] keys. *)
