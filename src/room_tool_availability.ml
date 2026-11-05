(** B844: room-scoped tools that cannot work in a Room turn's room.

    A Room turn leaves these out of its tool catalog and system prompt, so the
    model never spends calls on tools that can only fail there with a
    room-not-configured error. Each check reuses the lookup the tools themselves
    make, so a tool is hidden exactly when it would fail for lack of room setup.
    A failed lookup keeps the tools: the tool then reports the error itself. *)

let room_memory_tools = Tools_builtin_room_memory.tool_names
let github_item_tools = Github_room_tools.item_tool_names

let lookup_or_available ~what f =
  try f ()
  with exn ->
    Logs.warn (fun m ->
        m "Room tool availability: %s lookup failed (%s); keeping the tools"
          what (Printexc.to_string exn));
    true

(* The room_memory_* tools resolve their room from the session key, not from
   the snapshot, so availability does the same. *)
let room_memory_available ~db ?session_key () =
  lookup_or_available ~what:"room memory" (fun () ->
      match
        Tools_builtin_room_memory.resolve_room_id_for_context ~db
          { Tool.default_context with session_key }
      with
      | None -> false
      | Some room_id ->
          Tools_builtin_room_memory.room_scope_available ~db ~room_id)

(* The same emptiness gate the github_room_* item tools apply to the
   snapshot's room. *)
let github_items_available ~db ~room_id =
  lookup_or_available ~what:"GitHub room items" (fun () ->
      match Github_room_tools.room_has_any_data ~db ~room_id with
      | Ok has_data -> has_data
      | Error _ -> true)

let slack_canvas_available ~context =
  match context with
  | Some { Slack_reply_context.connector = "slack"; conversation = Channel } ->
      true
  | _ -> false

(* What the model gets if it still calls a hidden tool (for example, one it
   saw in an earlier turn). It is an ordinary tool error, not a configuration
   error, so it never trips the B778 abort. *)
let unavailable_error name =
  let needs =
    if name = "slack_canvas_create" then
      "it needs a verified Slack channel turn; reply normally here (long Slack \
       DM answers become file attachments)"
    else if List.mem name room_memory_tools then
      "room memory needs a room profile binding, and this room has none (an \
       admin can add one with `clawq rooms bind <room_id> <profile_id>`)"
    else
      "it reads the GitHub issues and PRs a GitHub feed routes to this room, \
       and no feed has routed any here"
  in
  Printf.sprintf
    "Error: Tool '%s' is not available in this room: %s. Do not retry it; \
     answer with the tools you have."
    name needs

let unavailable_tools ~db ?session_key ~room_id () =
  (if room_memory_available ~db ?session_key () then [] else room_memory_tools)
  @ if github_items_available ~db ~room_id then [] else github_item_tools
