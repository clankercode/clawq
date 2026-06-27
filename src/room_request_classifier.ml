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
