# Clawq CLI Reference

This document covers the common, user-facing `clawq` CLI commands organized by category.

## Admin Gating

Commands marked with **[admin]** require the `CLAWQ_ADMIN=1` environment variable:

```bash
export CLAWQ_ADMIN=1
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

Inspect room configuration and state.

```
clawq rooms inspect <room_id>                    Inspect room configuration details
```

### `clawq rooms readiness`

Check room readiness for agent operations.

```
clawq rooms readiness <room_id>                  Run readiness checks for a room
```

### `clawq rooms deliveries`

View delivery failure events. **[admin]**

```
clawq rooms deliveries list [--room-id ID] [--connector C] [--from TS] [--limit N] [--json]
```

Flags:
- `--room-id` / `--room` — Filter by room ID
- `--connector` — Filter by connector type
- `--from` / `--since` — Filter from timestamp
- `--limit` — Max results (default: 20)
- `--json` — Output as JSON

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

Manage GitHub PR notification subscriptions. **[admin]**

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

## Pipelines

### `clawq pipeline`

Manage structured pipelines.

```
clawq pipeline list                              List available pipelines
clawq pipeline show <name>                       Show pipeline definition
clawq pipeline run <name> [--input k=v ...]      Execute a pipeline synchronously
clawq pipeline trigger <name> [--input k=v ...]  Trigger pipeline as background task
clawq pipeline validate <name>                   Validate pipeline definition
clawq pipeline create <name>                     Scaffold a new pipeline YAML
clawq pipeline wizard                            Interactive pipeline builder
clawq pipeline history [--pipeline <name>]       List past runs
clawq pipeline result <run-id>                   Show run results
clawq pipeline workflow-result <run-id>          Show workflow run status
clawq pipeline workflow-runs [--room <id>]       List workflow runs
```

> **`/pipeline-designer` skill vs `clawq pipeline wizard`:** Both produce the same pipeline YAML files. The `/pipeline-designer` skill is an in-conversation authoring flow guided by the agent (useful when you want the agent to help design schemas and step sequences interactively). `clawq pipeline wizard` is a standalone interactive CLI setup. Use whichever fits your workflow.

## Setup

### `clawq rooms wizard`

Interactive room-agent pilot wizard with plan/apply flow. **[admin]**

```
clawq rooms wizard                               Launch interactive wizard
clawq rooms wizard --plan                        Show plan without applying changes
clawq rooms wizard --apply                       Apply wizard configuration
clawq rooms wizard --rerun                       Rerun wizard to repair configuration
```

The wizard configures:
- Room profile (model, system prompt, tool restrictions)
- Access bundle binding
- Memory scope
- Budget limits
- Connector binding
- Readiness checks

### `clawq rooms audit-export`

Export room audit data. **[admin]**

```
clawq rooms audit-export [room_id]               Export audit data for a room or all rooms
```

## Agents

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

## System Commands

### `clawq agent`

Start the Clawq agent daemon.

```
clawq agent                                      Start the agent daemon
```

### `clawq service`

Manage the Clawq system service.

```
clawq service start                              Start the service
clawq service stop                               Stop the service
clawq service status                             Show service status
clawq service signal-restart                     Signal the service to restart
clawq service restart                            Restart the service
clawq service install                            Install system service (systemd/launchd)
clawq service uninstall                          Uninstall system service
clawq service systemd-unit                       Generate systemd unit file
clawq service launchd-plist                      Generate launchd plist file
```

### `clawq update`

Update Clawq to the latest version.

```
clawq update [--mode auto|git|binary|pkg]        Update Clawq
```

### `clawq doctor`

Run system diagnostics.

```
clawq doctor                                     Check system health and configuration
```

### `clawq mcp`

Run Clawq as an MCP server.

```
clawq mcp                                        Start MCP server (requires mcp.enabled=true)
```

### `clawq completions`

Generate shell completions.

```
clawq completions print [--shell bash|zsh|fish]  Print completion script
clawq completions install [--shell bash|zsh|fish]  Install completion script
```

> Note: `runner` is an internal command surface (handler exists but no top-level command is registered); it is not yet user-facing.

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

Manage background tasks.

```
clawq background list                            List background tasks
clawq background show <id>                       Show task details
clawq background logs <id> [--lines N] [--offset L] [--follow|-f]  Show task log output
clawq background transcript <id> [--regex R] [--max-lines N] [--export]  Show bounded task transcript
clawq background wait <id> [--timeout S]         Wait for a task to finish
clawq background resume <id>                     Resume a previously started task
clawq background retry <id>                      Re-queue a failed task
clawq background send <id> <message...>          Send a follow-up message to a task
clawq background cancel <id>                     Cancel a queued or running task
clawq background stop <id>                       Alias of cancel
```

**Local task restart policy (B736):** Local runner tasks support automatic
re-enqueue on daemon restart. By default (`restart_policy=reenqueue`), a Local
task that was running when the daemon shut down is re-queued on the next
startup with a fresh agent history. Tasks are capped at `max_restarts=2`
attempts. Use `restart_policy=fail` for tasks with non-idempotent side effects
to prevent re-execution after a crash. If the room's budget is exceeded at
restart time, the task is marked as failed rather than re-enqueued.

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

## Other Commands

### `clawq skills`

List available skills.

```
clawq skills list                                List all available skills
clawq skills show <name>                         Show skill details
```

### `clawq pair`

Manage device pairing.

```
clawq pair start                                 Start pairing flow
clawq pair status [id]                           Show pairing status
clawq pair list                                  List pair coding sessions
clawq pair stop <id>                             Stop a pair coding session
clawq pair report <id>                           Show a pair session report
clawq pair notes <id>                            Show notes for a pair session
```

### `clawq auth`

Manage authentication.

```
clawq auth set-key PROVIDER [API_KEY]            Set API key for a provider (prompts if key omitted)
clawq auth providers                             List configured providers (alias: list-providers)
clawq auth encrypt                               Encrypt secrets at rest
clawq auth pair                                  Pair a provider account
clawq auth codex-login [PROVIDER]                Login via Codex OAuth
clawq auth codex-status [PROVIDER]               Show Codex OAuth status
clawq auth codex-logout [PROVIDER]               Logout from Codex OAuth
```

> Note: a bare `clawq auth` (or `clawq auth status`) prints a provider-status overview; there is no generic non-codex `auth login`/`auth logout`.

### `clawq transcribe`

Transcribe audio files.

```
clawq transcribe <audio_file>                    Transcribe an audio file to text
```

### `clawq costs`

View cost information.

```
clawq costs [--json]                             Show cost summary (today / 7d / 30d / all)
clawq costs session [--json]                     Per-session cost breakdown
```

### `clawq usage`

View usage statistics.

```
clawq usage                                      Show usage summary
```

### `clawq active`

Show active sessions and tasks.

```
clawq active                                     List active sessions and tasks
```

### `clawq capabilities`

Show system capabilities.

```
clawq capabilities                               Display available capabilities
```

### `clawq phase2`

Show Phase 2 features and status.

```
clawq phase2                                     Display Phase 2 information
```

### `clawq migrate`

Run database migrations.

```
clawq migrate <command>                          Run migration commands
```

### `clawq debate`

Start or manage debates.

```
clawq debate <command>                           Debate commands
```
