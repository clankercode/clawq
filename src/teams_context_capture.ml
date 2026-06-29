(* teams_context_capture.ml — Bounded automatic room context injection
   for Teams room-agent turns.  Gathers recent connector history for
   profile-bound rooms and formats it for injection as a skill context
   block, respecting privacy (profile binding required), retention
   (max_messages / max_age_days), and connector-history policy
   (enabled flag). *)

(** Maximum number of history messages to auto-inject. Separate from
    [connector_history.max_messages] (which caps the stored buffer) so prompt
    size stays bounded even when the buffer is large. *)
let max_auto_context_messages = 30

(** [capture_room_context ~session_manager ~has_binding ~session_key
     ~conversation_id] returns [Some formatted_context] when the room has a
    profile binding, connector history is enabled, and entries exist. [None]
    otherwise. Accepts [has_binding] with a labeled [~conversation_id] parameter
    to avoid a dependency cycle with [Teams]. *)
let capture_room_context ~(session_manager : Session.t) ~has_binding
    ~session_key ~conversation_id =
  let cfg = Session.get_config session_manager in
  if not cfg.connector_history.enabled then None
  else if not (has_binding ~conversation_id) then None
  else
    let db =
      if cfg.connector_history.persist_to_db then Session.get_db session_manager
      else None
    in
    let count =
      min cfg.connector_history.max_messages max_auto_context_messages
    in
    match
      Connector_history.get_formatted_for_key ?db ~key:session_key ~count ()
    with
    | Some (context, n) ->
        Some
          (Printf.sprintf
             "[Room context: %d recent messages from channel history]\n%s" n
             context)
    | None -> None
