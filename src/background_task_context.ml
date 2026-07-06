type context_origin = {
  connector : string option;
  workspace_id : string option;
  room_id : string option;
  requester_id : string option;
  source_message_id : string option;
  thread_id : string option;
}

let nonempty = function Some "" | None -> None | Some _ as value -> value

let make_context_origin ?connector ?workspace_id ?room_id ?requester_id
    ?source_message_id ?thread_id () =
  {
    connector;
    workspace_id;
    room_id;
    requester_id;
    source_message_id;
    thread_id;
  }

let context_origin_of_session_key key =
  let parts = String.split_on_char ':' key in
  match Room_session.parse_child_thread_key key with
  | Some child ->
      make_context_origin ~connector:child.connector ~room_id:child.room_id
        ?source_message_id:child.source_message_id ?thread_id:child.thread_id ()
  | None -> (
      match parts with
      | [ (("slack" | "discord" | "telegram") as connector); room_id ] ->
          make_context_origin ~connector ~room_id ()
      | (("slack" | "discord" | "telegram") as connector)
        :: room_id :: requester_parts ->
          make_context_origin ~connector ~room_id
            ?requester_id:(nonempty (Some (String.concat ":" requester_parts)))
            ()
      | [ "teams"; room_id ] ->
          make_context_origin ~connector:"teams" ~room_id ()
      | "teams" :: team_id :: conversation_parts when conversation_parts <> []
        ->
          make_context_origin ~connector:"teams" ~workspace_id:team_id
            ~room_id:(String.concat ":" conversation_parts)
            ()
      | _ -> (
          match Room_session.parse key with
          | None -> make_context_origin ()
          | Some session ->
              make_context_origin
                ~connector:(Room_session.channel_to_string session.channel)
                ~room_id:session.channel_id
                ?requester_id:(nonempty (Some session.sender_id))
                ()))

let room_binding_candidates ~session_key origin =
  let add seen value =
    match value with
    | None | Some "" -> seen
    | Some value when List.mem value seen -> seen
    | Some value -> value :: seen
  in
  let seen = add [] origin.room_id in
  let seen =
    match (origin.connector, origin.room_id) with
    | Some connector, Some room_id ->
        add seen (Some (connector ^ ":" ^ room_id))
    | _ -> seen
  in
  List.rev (add seen (Some session_key))

(** Derive room-origin fields from a tool invoke context's session key and the
    DB room-profile binding when available. Returns
    [(profile_id, origin_json, thread_id, requester)] suitable for passing to
    {!Background_task.enqueue}. *)
let origin_fields_from_context ~db ?context () =
  let session_key =
    match context with
    | Some (c : Tool.invoke_context) -> c.session_key
    | None -> None
  in
  match session_key with
  | None -> (None, None, None, None)
  | Some key ->
      let origin_fields = context_origin_of_session_key key in
      let profile_id =
        room_binding_candidates ~session_key:key origin_fields
        |> List.find_map (fun room_id ->
            match Memory.get_room_profile_binding ~db ~room_id with
            | Some b -> Some b.profile_id
            | None -> None)
      in
      let origin =
        Room_origin.make ?connector:origin_fields.connector
          ?workspace_id:origin_fields.workspace_id
          ?room_id:origin_fields.room_id
          ?requester_id:origin_fields.requester_id
          ?source_message_id:origin_fields.source_message_id
          ?thread_id:origin_fields.thread_id ?profile_id ()
      in
      let origin_json =
        if Room_origin.is_empty origin then None
        else Some (Room_origin.to_compact_json_string origin)
      in
      let requester =
        match Room_origin.display_summary origin with
        | s when s <> "CLI room=- requester=-" -> Some s
        | _ -> None
      in
      (profile_id, origin_json, origin_fields.thread_id, requester)

let routing_from_context ?context ?notify_cfg () =
  let session_key =
    match context with
    | Some c -> c.Tool.session_key
    | None ->
        let value =
          try Some (Sys.getenv "CLAWQ_SESSION_ID") with Not_found -> None
        in
        Option.bind value (fun raw ->
            let trimmed = String.trim raw in
            if trimmed = "" then None else Some trimmed)
  in
  match session_key with
  | Some key -> (
      match Restart_notify.parse_channel_from_key key with
      | Some (channel, channel_id) ->
          (session_key, Some channel, Some channel_id)
      | None -> (session_key, None, None))
  | None -> (
      match notify_cfg with
      | Some notify ->
          let notify : Runtime_config.notify_config = notify in
          (None, Some notify.channel, Some notify.target)
      | None -> (None, None, None))

let build_delegate_prompt ~automerge:_ ~goal =
  let commit_line =
    "- CRITICAL: You MUST `git add` and `git commit` all changes before \
     reporting completion. Verify with `git status` that the worktree is \
     clean. Tasks with uncommitted changes are marked as dirty-worktree \
     failures regardless of exit code."
  in
  String.concat "\n"
    [
      "You are a delegated background coding agent running in the target \
       directory.";
      "";
      "Goal:";
      goal;
      "";
      "Execution contract:";
      commit_line;
      "- Before reporting completion, rebase your branch against the master \
       branch (e.g., `git rebase master`) to ensure your changes are up to \
       date. If the rebase has conflicts, resolve straightforward ones and \
       continue.";
      "- Work only inside this directory/worktree.";
      "- Do not inspect or modify the original source repo path directly; use \
       only the files available in the current worktree.";
      "- Make the smallest focused change that completes the task well.";
      "- Run relevant verification when practical and mention what you ran.";
      "- Summarize the changes, results, and any follow-up concerns at the end.";
      "- Do not push or perform destructive git history edits.";
    ]
