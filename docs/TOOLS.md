# Connector Abstraction & Slash Command Registration

> Scope: this document covers **slash-command and connector registration** (the
> `Slash_commands` registry and per-platform manifest/menu rendering). It does
> **not** document the agent *tools* registry (the runtime tools agents call,
> such as read/write/edit, bash, browser, memory, messaging) —
> see `src/tool_registry.ml`, `src/tools_builtin*.ml`, `src/tool.ml`.
>
> TODO(follow-up): the agent tools registry has no dedicated doc; add one and
> cross-link.

## Overview

clawq supports multiple messaging connectors (Telegram, Teams, Discord, Slack) via a shared slash command system. Each connector dispatches commands through `Slash_commands.handle` and formats output using `Format_adapter`.

## Slash Command Registration

### Command Type

```ocaml
type command = { name : string; description : string; priority : int }
```

Priority (0–100) determines ordering for platform command autocomplete menus. Higher = more prominent.

### Platform Registration

| Platform | Mechanism | When | Limit |
|----------|-----------|------|-------|
| **Telegram** | `setMyCommands` API | Daemon startup (per account) | ~100 |
| **Teams** | App manifest `bots.commandLists` | Build time via `clawq manifest teams` | 10 per scope |
| **Discord** | N/A (no native slash registration yet) | — | — |
| **Slack** | N/A (no native slash registration yet) | — | — |

### Telegram Runtime Registration

On startup, each Telegram polling account calls `set_my_commands` which registers all commands sorted by priority via the Telegram Bot API `setMyCommands` endpoint. This happens automatically in `poll_account` before the polling loop begins.

### Teams Manifest Generation

Teams requires a static app manifest. Use the CLI to generate the command fragment:

```bash
clawq manifest teams              # Print to stdout
clawq manifest teams --output manifest.json  # Write to file
clawq manifest teams -n 20        # Override default 10-command limit
```

This outputs the top 10 commands (by priority, override with `-n COUNT`) in Teams `bots.commandLists` format. Paste the `commandLists` array into your Teams app manifest.

For full command discoverability beyond the 10-command limit, use `/menu` in Teams to get an Adaptive Card with all commands as clickable buttons.

### Telegram Manifest Generation

```bash
clawq manifest telegram              # Print setMyCommands payload to stdout
clawq manifest telegram --output cmds.json  # Write to file
clawq manifest telegram > cmds.json  # Redirect to a file with your shell
```

Generates the full `setMyCommands` JSON payload sorted by priority.

## Skills as Slash Commands

SKILL.md skills (from `.claude-p/skills/`, `.claude/skills/`, `~/.clawq/skills/`) are automatically included in slash command manifests and menus. Each skill appears as `/skill-name` with its description from YAML frontmatter. Skills are appended after built-in commands at priority 100.

Skills can also be referenced via `@skill-name` in messages (ensured in context) or invoked via the `use_skill` tool. No-argument skills are de-duplicated against the current agent context: if the skill has been retained since session start or the last compaction, clawq skips reloading it and does not emit another load notification. If compaction removes the skill instructions from retained context, the next invocation loads them again.

### Command Injection in SKILL.md

SKILL.md bodies support command injection via `` !`command` `` syntax. When the skill actually loads, each `` !`...` `` expression is replaced with the command's stdout output. De-duplicated no-op invocations do not rerun command injections. This enables dynamic content in skill instructions.

Example SKILL.md body:
```
Current git status:
!`git status --short`

Recent commits:
!`git log --oneline -5`
```

Scripts in the skill's directory are automatically added to PATH, so a skill at `~/.clawq/skills/my-skill/SKILL.md` can reference `~/.clawq/skills/my-skill/helper.sh` as just `helper.sh` in injections.

When `workspace_only` security is enabled, command injections are subject to the same shell safety checks as JSON skills (allowlist, no unsafe syntax).

### JSON Skills (Deprecated)

Legacy JSON skills (`.json` files in `~/.clawq/skills/`) are still loaded but deprecated. A warning is emitted when JSON skills are loaded. Every `{{param_name}}` placeholder used in a JSON skill command must be declared under `parameters.properties`; otherwise the skill is refused at load time with a warning. The `skill_create` tool now creates SKILL.md skills instead of JSON. Convert existing JSON skills to SKILL.md format by creating a `<name>/SKILL.md` file with the command embedded as `` !`command` ``.

## Adding a New Command

1. Add an entry to the `commands` list in `src/slash_commands_fmt.ml` with appropriate priority (`src/slash_commands.ml` re-exports it via `include`)
2. Add a result variant if needed
3. Add handler case in `Slash_commands.handle`
4. Handle the new variant in each connector: `telegram.ml`, `teams.ml`, `discord.ml`, `slack.ml`, `http_server.ml`
5. Add tests in `test/test_slash_commands.ml`

## `/menu` Command

The `/menu` command renders a full command listing:
- **Teams**: Adaptive Card with a flat, paginated list of command buttons (9 per page, sorted by priority, with prev/next navigation)
  > TODO(follow-up): tier grouping (Core / Info & Config / Advanced) is not yet
  > implemented; the current `/menu` card is flat-paginated
  > (`src/slash_commands_manifest.ml:menu_adaptive_card_json`).
- **Telegram**: HTML-formatted list with `<code>` command names
- **Other connectors**: Plain text list sorted by priority

## `/agent menu` Command

The `/agent menu [page]` command renders a paginated agent template browser (8 per page):
- **Teams**: Adaptive Card with template buttons (each sends `/agent <name> `) and prev/next navigation actions
- **Other connectors**: Formatted text list with bold template names, descriptions, and `/agent menu N` prev/next links
- Page is optional (defaults to 1); out-of-range pages are clamped to valid range
- Implementation: `src/slash_commands_fmt.ml` (`format_agent_menu`), `src/slash_commands_manifest.ml` (`agent_menu_adaptive_card_json`)
