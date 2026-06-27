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
  | TasksFull | Costs _ | Usage _ | Active | Bg _ | Cron _ | Bl _ | Session _
  | Menu _ | AgentMenu _ | ModelMenu _ | ThinkingMenu | ConfigMenu _
  | SkillsMenu _ | CostsMenu | BgMenu | Repo _ | HeldItems _ | Memories _
  | RegisterAsAdminOtc _ | InjectConnectorHistory _ | SkillInvoke _ ->
      QuickReply
  (* AdminRequired wraps another result — classify the inner result *)
  | AdminRequired inner -> classify inner
  (* Free-form messages that go to the agent turn *)
  | DebugDumpChat | NotACommand -> MentionToTask

(** Derive a human-readable title for an [AsyncCommand] result. Returns
    [Some title] for async commands and [None] for everything else. Used to
    create task-tree records when async commands arrive from profiled rooms. *)
let title_of_async_cmd (result : Slash_commands.result) : string option =
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
        if String.length cmd > 60 then String.sub cmd 0 57 ^ "..."
        else cmd
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
