# What's Changed Since 0.3.0

## New Features

### Major: Room Agents Campaign

This release introduces the room-agents system: shared, policy-controlled agent sessions for rooms across Slack, Discord, Telegram, and Teams.

- **Room profile config + DB binding model** — deterministic config-to-DB reconciliation; per-room profile bindings with validation and repair.
- **Per-room persistent workspaces** — deterministic slug+hash paths under `~/.clawq/`; lifecycle management (create, rebind, rename, delete) with config/DB contract.
- **Scoped memory** — per-profile memory isolation (create, import, export, search); grant resolution and enforcement; budget tracking with DB ledger.
- **Room privacy guard** — limits what room participants can send to the agent; request classifier with deterministic tests.
- **Room lifecycle** — admin stubs for room management; child thread sessions with session keys.
- **Progress states** — sparse room progress tracking; delivery of progress updates to Slack/Discord.
- **Guest policy** — enforcement for room async requests.
- **Background task launch** — room-launched background tasks under profile policy.
- **Completion/failure messages** — final room completion and failure delivery.
- **Concurrent room messages** — verification and restart replay.
- **Scheduler cron** — cron metadata, cron-as-room-sessions, manual trigger path.
- **Room routines** — create/list/show/edit/remove/enable/disable/trigger commands with admin audit.
- **Connector capabilities** — Telegram capabilities, Teams commands, consent cards.
- **Ambient watcher** — policy engine, stale query engine, watcher decisions, material-change gate, safe delivery pipeline.
- **Ambient inspection** — admin inspection surface for ambient watcher state.
- **Documentation** — comprehensive room-agents docs in `docs/room-agents.mdx`.

### CLI, Cron, Update, and Release Tooling

- Added package-manager-aware self-update routing.
- Added npm package scaffolding and OIDC trusted publishing workflow.
- Added `RELEASE.md` with npm release process docs.
- Added cron enable/disable CLI subcommands.
- Improved `/feature` progress directives.
- Expanded tool descriptions with cross-references and usage constraints.

### Website and Packaging

- Added Open Graph/social metadata and browser-rendered preview card assets.
- Updated README/package metadata for `clawq.org` and the scoped npm package.

## Bug Fixes

### Provider and Model Compatibility

- Fixed MiniMax tool-call/result ordering, streaming empty-argument handling, transient 500 retry behavior, intermittent 404 surfacing, and oversized error logging.
- Fixed Z.ai session resume failures by enforcing tool-group integrity and resume-message shape.
- Fixed Kimi/OpenAI-compatible resume failures from orphan tool-call IDs and missing `reasoning_content`.
- Fixed provider timeout behavior, including stream idle timeout and dead 30s timeout paths.
- Fixed Ollama usage handling, Cohere `Done` handling, `maxOutputTokens`, quota double-store behavior, and an API-key leak.
- Fixed model discovery refresh failures and silent fallback logging.

### Connectors and Messaging

- Fixed Discord REST route mutexing so one rate-limited route does not block others.
- Fixed Mattermost DM/group channel-type detection.
- Fixed Teams send failure visibility, empty Teams payload handling, cron delivery reporting, and status reset on mid-turn injection.
- Fixed Telegram stale tool status and outbound mutex race handling.
- Fixed forced message splitting and connector dispatch edge behavior across Teams, Telegram, and WhatsApp.

### Daemon, Sessions, and Background Work

- Fixed admin stop handling during active and queued turns.
- Fixed duplicate resume prompt persistence and Z.ai resume prompt failures.
- Fixed session model preservation across update restarts.
- Added watchdog/circuit-breaker behavior for stuck sessions, repeated invalid tool calls, repeated identical parameter-validation errors.
- Fixed session mutex deadlock and restart-resume history sanitization issues.

### Cron, Search, and Briefings

- Fixed Brave/DDG/web-search fallback behavior and startup health checks.
- Fixed briefing cron rate-limit behavior by avoiding unbounded parallel searches.
- Fixed fragile briefing prompts by routing through deterministic skills and persistent sessions.

### Security, Safety, and Reliability

- Fixed ACP security/correctness issues and converted blocking `Sys.command` usage to `Lwt_process`.
- Fixed unsafe `pkill -f` cleanup by using process-group-scoped cleanup.
- Fixed multiple resource leaks around files, SQLite statements, process handles, IRC/IMAP/SMTP connections.
- Improved tool execution safety, sandbox PATH exposure, config parse logging.

### Memory, Task Tree, Pipeline, and Tests

- Fixed memory FTS5 colon escaping and empty-string channel handling.
- Fixed task-tree dependency cycle and task-not-found guidance.
- Improved restart, provider, migration, replay, and connector test stability.

## Breaking Changes

No explicit breaking changes. Potential upgrade note:

- Legacy `default_provider` config is now migrated into `agent_defaults.primary_model`. Existing configs should continue to load, but users may see deprecation warnings until the old field is removed.

## Documentation

- Added room-agent documentation and updated `llms-full`.
- Added npm release documentation in `RELEASE.md`.
- Updated README/package metadata for the public website.
- Added a Dune memory-usage warning to repo instructions.
