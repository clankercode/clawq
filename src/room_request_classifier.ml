(** Room request classifier.

    Categorises an incoming room message so the caller can select the right
    execution path *before* acquiring the long-running session mutex.

    Three categories:
    - [QuickReply] — slash commands dispatched synchronously through
      [Connector_dispatch.dispatch] (model, thinking, status, help, etc.).
    - [AsyncCommand] — commands that spawn background work (update, delegate,
      agent, debate, compact, fork, rig, bash).
    - [MentionToTask] — free-form messages ([NotACommand]) that will be
      processed through an agent turn. *)

type classification = QuickReply | AsyncCommand | MentionToTask

let rec classify (result : Slash_commands.result) : classification =
  match result with
  (* Async commands — each spawns background Lwt work *)
  | Compact | Delegate _ | ForkAnd _ | AgentInvoke _ | Debate _ | BashRun _
  | Rig _ | Model _ ->
      AsyncCommand
  (* Quick replies — handled synchronously by Connector_dispatch.dispatch *)
  | Reply _ | FormattedReply _ | Help | Reset | RuntimeCtx | Uptime | Status
  | Thinking _ | ShowThinking _ | Heartbeat _ | Debug _ | Tools | Tasks
  | TasksFull | Costs _ | Usage _ | Active | Bg _ | WorkflowRun _ | Cron _
  | Bl _ | Session _ | Menu _ | AgentMenu _ | ModelMenu _ | ThinkingMenu
  | ConfigMenu _ | SkillsMenu _ | CostsMenu | BgMenu | Repo _ | HeldItems _
  | Memories _ | RoomsMemory _ | ExplainAccess | WhatCanDo
  | RegisterAsAdminOtc _ | InjectConnectorHistory _ | SkillInvoke _ | Context
  | Followup _ ->
      QuickReply
  (* AdminRequired wraps another result — classify the inner result *)
  | AdminRequired inner -> classify inner
  (* Free-form messages that go to the agent turn *)
  | DebugDumpChat | NotACommand -> MentionToTask

(** Derive a human-readable title for an [AsyncCommand] result. Returns
    [Some title] for async commands and [None] for everything else. Used to
    create task-tree records when async commands arrive from profiled rooms. *)
let rec title_of_async_cmd (result : Slash_commands.result) : string option =
  let open Slash_commands in
  match result with
  | Compact -> Some "Compact session"
  | Delegate (Some name, _) -> Some (Printf.sprintf "Delegate: %s" name)
  | Delegate (None, _) -> Some "Delegate"
  | ForkAnd (Some name, _) -> Some (Printf.sprintf "Fork: %s" name)
  | ForkAnd (None, _) -> Some "Fork"
  | AgentInvoke (name, _) -> Some (Printf.sprintf "Agent: %s" name)
  | Debate _ -> Some "Debate"
  | BashRun cmd ->
      let short =
        if String.length cmd > 60 then String.sub cmd 0 57 ^ "..." else cmd
      in
      Some (Printf.sprintf "Bash: %s" short)
  | Rig RigList -> Some "Rig: list"
  | Rig (RigInstall name) -> Some (Printf.sprintf "Rig: install %s" name)
  | Rig (RigAdjust name) -> Some (Printf.sprintf "Rig: adjust %s" name)
  | Rig (RigRemove name) -> Some (Printf.sprintf "Rig: remove %s" name)
  | Model ModelShow -> Some "Model: show"
  | Model (ModelSet m) -> Some (Printf.sprintf "Model: set %s" m)
  | Model (ModelSetForce m) -> Some (Printf.sprintf "Model: force-set %s" m)
  | Model (ModelSetDefault m) -> Some (Printf.sprintf "Model: set-default %s" m)
  | Model (ModelFav m) -> Some (Printf.sprintf "Model: fav %s" m)
  | Model (ModelUnfav m) -> Some (Printf.sprintf "Model: unfav %s" m)
  | Model (ModelList _) -> Some "Model: list"
  | Model ModelUsage -> Some "Model: usage"
  | AdminRequired inner -> title_of_async_cmd inner
  | _ -> None

(** Map an [AsyncCommand] result to background-task launch parameters. Returns
    [Some (goal, preferred_runner, agent_name)] for commands that should be
    launched as background tasks, or [None] for commands that should remain
    inline (Compact, Model, RigList).

    The caller passes [preferred_runner] as a string option:
    - [Some "local"] for native/in-process execution
    - [None] for auto-runner selection via [delegate_enqueue]

    @param goal is the prompt/goal text for the background task.
    @param runner is ["local"] for native runner or [None] for auto.
    @param agent_name is the agent template name when applicable. *)
let rec async_cmd_to_bg_launch (result : Slash_commands.result) :
    (string * string option * string option) option =
  let open Slash_commands in
  match result with
  | Delegate (agent_name, prompt) -> Some (prompt, None, agent_name)
  | AgentInvoke (agent_name, prompt) ->
      Some (prompt, Some "local", Some agent_name)
  | ForkAnd (agent_name, prompt) -> Some (prompt, None, agent_name)
  | Debate prompt -> Some (prompt, None, None)
  | BashRun cmd ->
      let goal = Printf.sprintf "Run the following bash command:\n\n%s" cmd in
      Some (goal, None, None)
  | Rig RigList -> None
  | Rig (RigInstall name) -> (
      match Rig.prompt_for ~name ~action:`Install with
      | Ok prompt -> Some (prompt, None, None)
      | Error _ -> None)
  | Rig (RigAdjust name) -> (
      match Rig.prompt_for ~name ~action:`Adjust with
      | Ok prompt -> Some (prompt, None, None)
      | Error _ -> None)
  | Rig (RigRemove name) -> (
      match Rig.prompt_for ~name ~action:`Remove with
      | Ok prompt -> Some (prompt, None, None)
      | Error _ -> None)
  | AdminRequired inner -> async_cmd_to_bg_launch inner
  | Compact | Model _ -> None
  | _ -> None

(** Check whether an [AsyncCommand] result is allowed for a guest (non-admin)
    caller in a room async context. Returns [Ok ()] if allowed, or
    [Error message] with an actionable denial message if denied.

    Guest-allowed commands are limited to read-only or delegated agent
    interactions: Delegate, AgentInvoke, and Debate. State-mutating or
    privileged commands (BashRun, ForkAnd, Rig install/adjust/remove) are denied
    for guests. *)
let rec guest_async_policy (result : Slash_commands.result) :
    (unit, string) result =
  let open Slash_commands in
  match result with
  | Delegate _ | AgentInvoke _ | Debate _ -> Ok ()
  | ForkAnd _ ->
      Error
        "Error: forking sessions requires admin privileges. Ask an admin to \
         run this command or register as admin with /register_as_admin_otc."
  | BashRun _ ->
      Error
        "Error: running shell commands requires admin privileges. Ask an admin \
         to run this command or register as admin with /register_as_admin_otc."
  | Rig (RigInstall _) ->
      Error
        "Error: installing rigs requires admin privileges. Ask an admin to run \
         this command or register as admin with /register_as_admin_otc."
  | Rig (RigAdjust _) ->
      Error
        "Error: adjusting rigs requires admin privileges. Ask an admin to run \
         this command or register as admin with /register_as_admin_otc."
  | Rig (RigRemove _) ->
      Error
        "Error: removing rigs requires admin privileges. Ask an admin to run \
         this command or register as admin with /register_as_admin_otc."
  | AdminRequired inner -> guest_async_policy inner
  | _ -> Ok ()

(** Launch an async room command as a background task using the room's profile
    policy. Combines [async_cmd_to_bg_launch] with
    [Background_task.launch_room_bg_task].

    Guest callers (non-admin) are checked against [guest_async_policy] before
    launch. Denied requests produce an actionable error message.

    Returns [Ok (Some bg_task_id)] if a background task was launched, [Ok None]
    if the command should remain inline, or [Error msg] on launch failure or
    guest policy denial. *)
let launch_room_async_bg ~db ~session_key ~connector ~room_id ~requester_id
    ~is_admin ?thread_id ?model_override ?notify_cfg ?config
    (result : Slash_commands.result) =
  let ( let* ) = Result.bind in
  let* () =
    if is_admin then Ok ()
    else (
      Logs.info (fun m ->
          m "Guest policy check for %s in room %s: %s" requester_id room_id
            (Option.value (title_of_async_cmd result) ~default:"unknown"));
      guest_async_policy result)
  in
  match async_cmd_to_bg_launch result with
  | None -> Ok None
  | Some (goal, runner_s, agent_name) -> (
      let preferred_runner =
        match runner_s with
        | Some "local" -> Some Background_task.Local
        | _ -> None
      in
      match
        Background_task.launch_room_bg_task ~db ~session_key ~connector ~room_id
          ~requester_id ~goal ?preferred_runner ?agent_name ?thread_id
          ?model_override ?notify_cfg ?config ()
      with
      | Ok id -> Ok (Some id)
      | Error msg -> Error msg)
