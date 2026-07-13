# Clawq CLI Reference

This document covers the full public `clawq` CLI command surface. Internal,
debug, and operator-only commands are listed in the [Internal/Debug
Commands](#internaldebug-commands) appendix.

## Admin Gating

Commands marked with **[admin]** require the `CLAWQ_ADMIN=1` environment variable:

```bash
export CLAWQ_ADMIN=1
```

## Getting Started

### `clawq onboard`

Create a starter config file interactively (or as a template when not in a TTY).

```
clawq onboard                                    Launch interactive setup wizard
```

### `clawq version`

Print version and build info.

```
clawq version                                    Print version, git hash, and build date
```

## Configuration

### `clawq config`

Manage Clawq configuration.

```
clawq config wizard                              Interactive configuration wizard
clawq config set KEY VALUE                       Set a config value by dot-path
clawq config set KEY                             Prompt for value (secret keys only, hidden input)
clawq config get KEY                             Get a config value by dot-path (secrets redacted)
clawq config show [SECTION]                      Display current config (secrets redacted)
clawq config tree [SECTION]                      Render config as a tree (secrets redacted)
clawq config tree keys                           Render config tree, structure only (no values)
clawq config search QUERY                        Search config keys matching QUERY
```

### `clawq setup`

Interactive setup wizards for individual clawq features.

```
clawq setup                                      Launch setup wizard menu
clawq setup <wizard-name>                        Run a specific setup wizard
```

### `clawq models`

Manage model configuration.

```
clawq models list [--provider P] [--json] [--availability available|unavailable|all]
    List available models (--availability also accepts --available/--unavailable/--all)
clawq models set-default MODEL [--skip-validation]
    Set default model (canonical: provider:model; --skip-validation alias: --no-test)
clawq models refresh [--force]                   Refresh model list from provider APIs
clawq models refresh --provider P [--force]      Refresh models for a specific provider
```

### `clawq status`

Show current daemon and system status.

```
clawq status                                     Display system status
```

### `clawq doctor`

Run system diagnostics.

```
clawq doctor                                     Check configuration for common issues
```

## Authentication

### `clawq auth`

Manage provider authentication, including Codex subscription login.

```
clawq auth                                       Print redacted provider auth status
clawq auth set-key PROVIDER [API_KEY]            Set API key for a provider (prompts if key omitted)
clawq auth providers                             List configured providers
clawq auth encrypt                               Encrypt plaintext secrets in config
clawq auth pair [OTP]                            Pair with a running gateway using OTP
clawq auth codex-login [PROVIDER]                Start ChatGPT/Codex OAuth login flow
clawq auth codex-status [PROVIDER]               Show Codex OAuth status
clawq auth codex-logout [PROVIDER]               Clear stored Codex OAuth credentials
```

### `clawq provider`

Inspect LLM provider configuration and live quota state.

```
clawq provider quota [NAME]                      Fetch and display live quota/usage
clawq provider list                              List configured providers
```

## Channels and Memory

### `clawq channel`

List configured channels or manage per-channel model overrides.

```
clawq channel                                    List configured channels
clawq channel test teams                         Test Teams channel connection
clawq channel set-model <channel> <model>        Set per-channel default model
clawq channel show-model <channel>               Show per-channel default model
clawq channel clear-model <channel>              Clear per-channel model (inherits global)
```

### `clawq memory`

Show memory backend configuration.

```
clawq memory                                     Display memory backend and search settings
```

## Session Management

### `clawq session`

Manage agent sessions, chat log epochs, and message injection.

**Registered CLI subcommands:**

```
clawq session list [--channel C] [--prefix P] [--active|--inactive] [--main|--non-main]
    List persisted sessions with optional filters
clawq session epochs <session>                   List current and archived chat-log epochs
clawq session show <session> [--epoch current|ID] [--offset N] [--limit N]
    Print raw chat log for a session epoch
clawq session pending <session>                  Show pending inbound queue rows
clawq session events <session> [--epoch E] [--type TYPE]
    Show event, system, and compaction messages
clawq session inject [--cwd PATH] <session> <message...>
    Inject a live inbound message through the daemon session manager
clawq session send [--cwd PATH] <session> <message...>
    Send an inbound message to another live or queued session
clawq session compact <session>                  Compact session history (summarize older messages)
clawq session model <session> [get|set MODEL [--skip-validation]|clear]
    Get/set/clear per-session model override
```

**Bridge-only subcommands** (accessible via gateway API, not direct CLI):

```
clawq session archives [SESSION]                 List archived session epochs
clawq session archive show <id> [--offset N] [--limit N]
    Show messages from a specific archive
clawq session keepalive <session> [on|off|status] Manage session keepalive
clawq session heartbeat <session> [on|off|status] Manage session heartbeat
clawq session postmortems [SESSION] [--limit N]  List session postmortems
```

### `clawq workspace`

Print the current workspace directory.

```
clawq workspace                                  Print current workspace path
```

> Note: Workspace backup/versions/restore/delete subcommands are implemented in
> `command_bridge.ml` but not exposed as top-level CLI arguments. Use the
> gateway API or daemon interface for workspace version management.

## Daemon and Service

### `clawq agent`

Start the Clawq agent daemon (agent loop, gateway, and all configured channels).

```
clawq agent                                      Start the daemon
```

### `clawq service`

Manage the Clawq system service.

```
clawq service start                              Start the service
clawq service stop                               Stop the service
clawq service status                             Show service status
clawq service restart                            Restart the service
clawq service signal-restart                     Send SIGUSR1 for graceful restart
clawq service install                            Install system service (systemd/launchd)
clawq service uninstall                          Uninstall system service
clawq service systemd-unit                       Generate systemd unit file
clawq service launchd-plist                      Generate launchd plist file
```

### `clawq update`

Update Clawq to the latest version.

```
clawq update [--mode auto|git|binary|pkg]        Request daemon update (offline fallback when no daemon running)
```

### `clawq mcp`

Start Clawq as an MCP server.

```
clawq mcp                                        Start MCP server (requires mcp.enabled=true)
```

## Tunnel

### `clawq tunnel`

Manage a public tunnel to the local gateway (Cloudflare supported).

```
clawq tunnel start                               Start the tunnel
clawq tunnel stop                                Stop the tunnel
clawq tunnel status                              Show tunnel status
clawq tunnel apply                               Trigger live tunnel reconfiguration
clawq tunnel restart                             Stop and restart tunnel with current config
clawq tunnel daemon-status                       Show tunnel manager state from daemon
```

## Cron Jobs

### `clawq cron`

Manage scheduled cron jobs.

```
clawq cron list [--prompt|-p]                    List all jobs (--prompt shows prompt text)
clawq cron show <name>                           Show job details
clawq cron add <name> <session> <schedule> <msg> [--ephemeral] [--ttl <duration>]
clawq cron remove <name>                         Remove a job
clawq cron enable <name>                         Enable a paused job
clawq cron disable <name>                        Pause job (keeps schedule + prompt)
clawq cron trigger <name>                        Trigger a job immediately
clawq cron history <name>                        Show run history
clawq cron runs [name]                           Show all run history
```

**Schedule format:**
- Interval: `"every 5m"` (supports m, h, d)
- Cron: Standard 5-field cron expression (e.g., `"0 9 * * 1-5"` for weekdays at 9am)

**TTL duration:** e.g., `24h`, `7d`, `30m` (job auto-disables after this time)

## Background Tasks

### `clawq background`

Manage background coding tasks that run a coding agent in git worktrees or
local sessions.

```
clawq background list                            List queued, running, and completed tasks
clawq background show <id>                       Show detailed task status (incl. session host kind/identity)
clawq background add <runner> <repo> [--model M] [--branch B] [--agent A] [--host <direct|herdr>] <prompt...>
    Queue a background coding task
clawq background start <runner> <repo> ...       Alias for add
clawq background wait <id> [--timeout S]         Wait for a task to finish
clawq background logs <id> [--lines N] [--offset L] [--follow|-f]
    Show task log output
clawq background transcript <id> [--regex R] [--max-lines N] [--export]
    Show bounded task transcript
clawq background resume <id>                     Resume a previously started task
clawq background message <id> <message...>       Send a follow-up message to a task
clawq background send <id> <message...>          Alias for message
clawq background cancel <id>                     Cancel a queued or running task
clawq background stop <id>                       Alias for cancel
clawq background retry <id>                      Re-queue a failed task
clawq background recover <id> [--runner R] [--model M]
    Recover a failed or stuck task with full context
clawq background finalize <id>                   Rebase and fast-forward task worktree
```

**Bridge-only subcommand** (accessible via gateway API, not direct CLI):

```
clawq background export-acp <id>                 Export task as ACP artifact
```

**Local task restart policy (B736):** Local runner tasks support automatic
re-enqueue on daemon restart. By default (`restart_policy=reenqueue`), a Local
task that was running when the daemon shut down is re-queued on the next
startup with a fresh agent history. Tasks are capped at `max_restarts=2`
attempts. Use `restart_policy=fail` for tasks with non-idempotent side effects
to prevent re-execution after a crash. If the room's budget is exceeded at
restart time, the task is marked as failed rather than re-enqueued.

### `clawq subagents`

Manage native/local subagents backed by background tasks.

```
clawq subagents list                             List native/local subagent tasks
clawq subagents start <repo> [--model M] [--agent A] <prompt...>
    Start a native/local subagent task
clawq subagents stop <id>                        Stop a native subagent task
clawq subagents send <id> <message...>           Send a follow-up message
clawq subagents transcript <id> [--regex R] [--max-lines N] [--export]
    Show bounded native subagent transcript
```

### `clawq delegate`

High-level workflow for delegating coding tasks to background runners.

```
clawq delegate [--runner R] [--model M] [--repo PATH] [--branch B] <goal...>
    Queue a coding task (auto-selects runner if not specified)
```

Runner selection: `auto` tries kimi, cursor, opencode, zai-coding-plan, glm-5,
claude, codex, gemini in order.

## Planning

### `clawq plan`

Run multi-stage planning pipelines: planner, plan-review loop, coder,
code-review loop.

```
clawq plan list                                  List all pipelines
clawq plan start [--repo PATH] [--runner R] [--planner-model M] [--reviewer-model M]
                   [--coder-model M] [--max-plan-review-iters N] [--max-code-review-iters N]
                   [--no-plan-review] [--no-code-review] <prompt...>
    Start a new planning pipeline (foreground, blocking)
clawq plan show <id>                             Show pipeline status and details
clawq plan logs <id> [--lines N]                 Show logs for the current stage
clawq plan cancel <id>                           Cancel a running pipeline
```

## Pipelines

### `clawq pipeline`

Define and run structured output pipelines with validated JSON Schema outputs.

```
clawq pipeline list                              List available pipelines
clawq pipeline show <name>                       Show pipeline definition details
clawq pipeline run <name> [--input k=v ...]      Execute a pipeline synchronously
clawq pipeline validate <name>                   Validate a pipeline definition
clawq pipeline create <name>                     Scaffold a new pipeline YAML file
clawq pipeline wizard                            Interactive pipeline builder
clawq pipeline history [--pipeline <name>]       List past runs
clawq pipeline result <run-id>                   Show run results
clawq pipeline workflow-result <run-id>          Show workflow run status
clawq pipeline workflow-runs [--room <id>]       List workflow runs
```

## Agents and Rigs

### `clawq agents`

Manage agent templates and bindings.

```
clawq agents list                                List all agent templates
clawq agents show <name>                         Show full template details
clawq agents create <name>                       Create a new template in ~/.clawq/agents/
clawq agents edit <name>                         Edit template (copies builtin to user dir)
clawq agents delete <name>                       Delete a user template
clawq agents bind <pattern> <agent> [--priority N]  Bind a routing pattern to an agent
clawq agents unbind <pattern>                    Remove a routing pattern binding
clawq agents bindings                            List current agent bindings
clawq agents setup                               Launch interactive setup wizard
clawq agents path                                Show template search directories
```

### `clawq rig`

Manage agent-driven setup rigs (install, adjust, remove).

```
clawq rig list                                   List available rigs and install status
clawq rig install <name>                         Install a rig via background task
clawq rig adjust <name>                          Reconfigure an installed rig
clawq rig remove <name>                          Remove an installed rig and clean up
```

## Room Management

### `clawq rooms`

Manage room profiles and bindings.

```
clawq rooms                                      List all room profiles and bindings
clawq rooms list                                 List all room profiles and bindings
clawq rooms show <room_id>                       Show room details including profile, model, and grants
clawq rooms workspace <room_id>                  Show workspace path for a room
clawq rooms bind <room_id> <profile_id> [--preserve|--reset]  Bind a room to a profile [admin]
clawq rooms unbind <room_id>                     Remove a room binding (profile preserved) [admin]
clawq rooms rename <profile_id> <display_name>   Rename a room profile [admin]
clawq rooms delete <profile_id> [--force]        Soft-delete a room profile [admin]
```

### `clawq rooms inspect`

Inspect room configuration and state. **[admin]**

```
clawq rooms inspect <room_id>                    Inspect ambient watcher state
```

### `clawq rooms readiness`

Check room readiness for agent operations.

```
clawq rooms readiness [--room-id R] [--profile-id P] [--json]
    Show room-agent readiness report
```

### `clawq rooms deliveries`

View delivery failure events. **[admin]**

```
clawq rooms deliveries list [--room-id ID] [--connector C] [--from TS] [--limit N] [--json]
```

### `clawq rooms session`

View room session records. **[admin]**

```
clawq rooms session list [--room-id ID] [--session-key KEY] [--snapshot-id ID] [--limit N] [--json]
clawq rooms session show <id> [--json]           Show a session record by ID
clawq rooms session get-latest <room_id> [--json] Get latest session record for a room
```

### `clawq rooms ledger`

View room activity ledger. **[admin]**

```
clawq rooms ledger list [--room-id ID] [--event-type TYPE] [--from TS] [--to TS] [--actor ACTOR]
                        [--profile-id ID] [--thread-id ID] [--task-id ID] [--background-id ID]
                        [--requester ID] [--status STATUS] [--format json|jsonl]
clawq rooms ledger export [filters]              Export ledger entries (add --format jsonl for JSON Lines)
clawq rooms ledger retention-cleanup [--retention-days N]  Clean up old ledger entries [admin]
```

### `clawq rooms gc`

Garbage collect room workspaces. **[admin]**

```
clawq rooms gc [--retention-days N]              Purge old room workspaces (preserves active ones)
```

### `clawq rooms routine`

Manage room routines (scheduled prompts for room profiles). **[admin]**

```
clawq rooms routine create <profile> <schedule> <message> [--thread-id ID]
clawq rooms routine list                         List all room routines
clawq rooms routine show <name>                  Show room routine details
clawq rooms routine edit <name> [--schedule S] [--message M]
clawq rooms routine remove <name>                Remove a room routine
clawq rooms routine enable <name>                Enable a room routine
clawq rooms routine disable <name>               Disable a room routine
clawq rooms routine trigger <name>               Trigger a room routine immediately
```

## Memory

### `clawq rooms memory`

Manage room-scoped memories.

```
clawq rooms memory list <room_id>                List memories in a room scope
clawq rooms memory show <room_id> <id>           Show details of a specific memory
clawq rooms memory save <room_id> <ref> <content> [--visibility public|private|team]
clawq rooms memory correct <room_id> <id> <new_content>
clawq rooms memory forget <room_id> <id> [-- --hard] [reason]
```

**Visibility levels:**
- `public` — Visible to all rooms sharing the scope
- `private` — Visible only to the creating room
- `team` — Visible to rooms with explicit team grants

**Forget behavior:**
- Default: redacts content (keeps metadata, content hidden)
- `--hard`: permanently deletes (admin-only, requires `CLAWQ_ADMIN=1`)

### Team Grants

Manage team-level access to team-visible memories. **[admin]**

```
clawq rooms memory team-grant add <room_id> <memory_id> <principal_kind> <principal_id>
clawq rooms memory team-grant remove <room_id> <memory_id> <principal_kind> <principal_id>
clawq rooms memory team-grant list <room_id> <memory_id>
```

### Scope Grants

Manage scope-level access grants. **[admin]**

```
clawq rooms memory grant add <room_id> <principal_kind> <principal_id> <capability>
clawq rooms memory grant remove <room_id> <principal_kind> <principal_id> <capability>
clawq rooms memory grant list <room_id>
```

**Principal kinds:** `room`, `profile`

**Capabilities:** `read`, `write`, `list`

When a room holds a `read` grant over a sibling room's scope, the agent's
turn-time retrieval automatically searches the granted scope and injects
relevant context (FTS + vector, budget-gated). This enables Claude-Tag-style
cross-channel learning — the agent learns from granted sibling rooms without
manual recall. Results are labelled with `[granted:room/<sibling>]` provenance.

See `docs/room-agent-architecture.md` section 8.5 for details.

## Access Control

### `clawq rooms explain-access`

Explain access configuration for a room. **[admin]**

```
clawq rooms explain-access <room_id> [--json]
```

Shows:
- Room profile binding status
- GitHub repository grants
- Codebase grants
- Memory scope grants
- **Inbound memory grants** (who can learn from this room)
- Blocked grants and reasons

## GitHub Subscriptions

### `clawq subscriptions`

Manage GitHub PR notification subscriptions. **[admin]** These commands are
compatibility aliases over GitHub Item routes: existing legacy subscriptions
are migrated idempotently before use and new commands write only the route
store. Listed route IDs are preferred; legacy numeric IDs remain accepted.

```
clawq subscriptions list [--room ROOM | --repo REPO]
clawq subscriptions show <id>                    Show subscription details
clawq subscriptions add <room> <repo> <pr#> [--profile P]
    [--on-open true|false] [--on-close true|false]
    [--on-comment true|false] [--on-review true|false]
    [--on-status true|false] [--on-merge true|false]
clawq subscriptions disable <id>                 Disable a subscription
clawq subscriptions enable <id>                  Enable a subscription
clawq subscriptions remove <id>
clawq subscriptions remove <room> <repo> <pr#>
```

## Pairing

### `clawq pair`

Manage device pairing sessions. **[bridge-only]**

> Note: `pair` is implemented in `command_bridge.ml` but not registered as a
> top-level CLI command in `main.ml`. It is accessible via the gateway API
> or daemon dispatch, not directly from the CLI.

```
clawq pair start                                 Start pairing flow
clawq pair status [id]                           Show pairing status
clawq pair list                                  List pair coding sessions
clawq pair stop <id>                             Stop a pair coding session
clawq pair report <id>                           Show a pair session report
clawq pair notes <id>                            Show notes for a pair session
```

> **`/pipeline-designer` skill vs `clawq pipeline wizard`:** Both produce the same pipeline YAML files. The `/pipeline-designer` skill is an in-conversation authoring flow guided by the agent (useful when you want the agent to help design schemas and step sequences interactively). `clawq pipeline wizard` is a standalone interactive CLI setup. Use whichever fits your workflow.

## Setup

### `clawq rooms wizard`

Interactive room-agent pilot wizard with plan/apply flow. **[admin]**

```
clawq rooms wizard                               Launch interactive wizard
clawq rooms -- wizard plan --profile-id ID       Show plan without applying changes
CLAWQ_ADMIN=1 clawq rooms -- wizard apply --profile-id ID
                                                 Apply wizard configuration
clawq rooms -- wizard rerun --profile-id ID      Rerun wizard to repair configuration
clawq rooms -- wizard validate-delivery --profile-id ID
                                                 Validate audit and delivery paths
```

### `clawq rooms audit-export`

Export room audit data. **[admin]**

```
clawq rooms audit-export <room_id> [--json|--jsonl]  Export audit data for a room
```

## Audit

### `clawq audit`

Manage the audit trail. **[admin]**

```
clawq audit list [--limit N]                     List audit entries (default: 20)
clawq audit verify                               Verify audit chain integrity
clawq audit export [path]                        Export audit entries to JSON
clawq audit import <path> [--anchor PATH.anchor.json]  Import audit entries
clawq audit purge                                Purge old entries based on retention policy
```

## Held Items

### `clawq held-items`

Manage held items (deferred feature proposals).

```
clawq held-items list [--status pending|approved|rejected|all]
clawq held-items show <id>
clawq held-items save --name NAME --desc DESC --plan-file FILE --layer N [--requestor ID] [--channel CH]
clawq held-items approve <id> [--by ADMIN] [--notes TEXT]
clawq held-items reject <id> [--by ADMIN] [--notes TEXT]
```

## Skills

### `clawq skills`

Manage agent skills (shell-script tool extensions).

```
clawq skills list                                List all available skills
clawq skills path                                Print the skills directory path
clawq skills init                                Create an example skill file
```

## Cost and Usage

### `clawq costs`

View cumulative LLM costs and token usage.

```
clawq costs [--json]                             Show cost summary (today / 7d / 30d / all)
clawq costs session [--json]                     Per-session cost breakdown
clawq costs model [--json]                       Per-model cost breakdown
clawq costs provider [--json]                    Per-provider cost breakdown
```

### `clawq usage`

View provider quota/usage status.

```
clawq usage [--refresh|-r]                       Show current quota (force fetch with --refresh)
clawq usage history [--provider P] [--since PERIOD] [--limit N] [--json]
    Show historical quota snapshots
clawq usage purge [PERIOD]                       Delete old history (default: 90d)
```

### `clawq active`

Show active 5-hour window usage.

```
clawq active                                     Show active 5-hour window usage (cost, tokens, quota)
```

## Debate

### `clawq debate`

Route a prompt to multiple models and synthesize a consensus.

```
clawq debate [--models m1,m2,m3] [--judge model] [--no-judge] [--format json|text] <prompt...>
clawq debate --history                           List past debate rounds
clawq debate --show <id>                         Show a specific past debate round
```

## Transcription

### `clawq transcribe`

Transcribe audio files using the configured STT provider.

```
clawq transcribe <audio_file>                    Transcribe an audio file to text
```

## Capabilities

### `clawq capabilities`

List all active runtime capabilities.

```
clawq capabilities                               Display providers, channels, tools, and integrations
```

## Shell Completions

### `clawq completions`

Generate shell tab-completion scripts.

```
clawq completions print [--shell bash|zsh|fish]  Print completion script
clawq completions install [--shell bash|zsh|fish]  Install completion script
```

---

## Internal/Debug Commands

The following commands are intended for developers, operators, and automated
systems. They are not part of the standard user-facing surface.

### `clawq debug`

Internal debugging utilities.

```
clawq debug html-preview [PORT]                  Serve Html_page test pages (default port 8099)
clawq debug prompt [MESSAGE]                     Print normalized logical messages for a turn
clawq debug context [SESSION]                    Dump runtime context for a session
clawq debug http {on|off|status|clear|tail [N]}  Manage HTTP debug logging
```

### `clawq benchmark`

Measure tool invocation latency.

```
clawq benchmark [--iterations N] [--tool NAME]   Benchmark tool invocation latency
```

### `clawq runtime`

Manage native and Docker runtimes for the clawq daemon.

```
clawq runtime status                             Show runtime status (default)
clawq runtime native {start|stop|health}         Control or health-check native runtime
clawq runtime docker {start|stop|health}         Control or health-check Docker runtime
```

### `clawq watcher`

Manage the error correction watcher.

```
clawq watcher status                             Show watcher config and EC process status
clawq watcher enable                             Enable the error correction watcher
clawq watcher disable                            Disable the error correction watcher
clawq watcher reports                            List recent EC reports
clawq watcher report <id>                        Show a specific EC report
```

### `clawq ec-run`

Internal: run the error correction process.

```
clawq ec-run [--daemon-mode]                     Run the error correction process
```

### `clawq manifest`

Generate connector command manifests.

```
clawq manifest teams [--output FILE] [-n COUNT]  Generate Teams bot manifest commands JSON
clawq manifest telegram                          Generate Telegram setMyCommands JSON payload
```

### `clawq reset-agent`

Wipe all session history, cron jobs, and workspace files, then redeploy
workspace defaults. Prompts for confirmation. Does NOT touch config.json.

```
clawq reset-agent                                Wipe and redeploy agent
```

### `clawq reset-workspace`

Wipe conversation history and workspace identity files, then redeploy
workspace defaults. Leaves cron jobs and config.json intact.

```
clawq reset-workspace                            Reset workspace without clearing sessions
```

### `clawq otp-show`

Show the current browser pairing code and any Telegram TOTP codes.

```
clawq otp-show                                   Show current pairing codes
```

### `clawq migrate`

Run database migrations.

```
clawq migrate <command>                          Run migration commands
```

### `clawq hardware`

Hardware integration (deferred to Phase 2).

```
clawq hardware                                   Hardware integration placeholder
```

### `clawq phase2`

Show Phase 2 feature status.

```
clawq phase2                                     Display Phase 2 information
```

### `clawq runner`

Generate runner authentication tokens. **[bridge-only]**

> Note: `runner` is implemented in `command_bridge.ml` but not registered as a
> top-level CLI command in `main.ml`. It is accessible via the gateway API
> or daemon dispatch.

```
clawq runner token --session <session_key> [--ttl-hours N]
    Generate a runner auth token for MCP access
```
