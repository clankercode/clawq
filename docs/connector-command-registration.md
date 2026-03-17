# Connector Command Registration

Slash commands are defined centrally in `src/slash_commands.ml` with a `priority` field that controls ordering across all connectors. Registration and discovery vary by connector.

## Command Priority

Each command has a `priority : int` field. Higher values appear first in menus, manifests, and autocomplete. `Slash_commands.sorted_by_priority ()` returns commands in descending priority order.

## Connector Patterns

### Telegram

Telegram uses the Bot API `setMyCommands` call at startup. This registers all commands sorted by priority so the most important appear first in the Telegram command autocomplete menu.

- Implementation: `src/telegram_api.ml` (`set_my_commands`)
- Called from: `src/telegram.ml` at polling loop startup
- Manifest preview: `clawq manifest telegram`

### Teams

Teams has two mechanisms:

1. **Static manifest**: The bot manifest JSON (`bots.commandLists[].commands[]`) is generated via `clawq manifest teams`. This produces the top 10 commands by priority for Teams autocomplete. The output is a JSON fragment to paste into the Teams app manifest.

2. **Runtime `/menu` command**: Sends an Adaptive Card with paginated command buttons (9 per page). Each button uses `imBack` to send the command text into the conversation. Navigation buttons allow paging through all commands.

3. **Runtime `/agent menu` command**: Sends an Adaptive Card with paginated agent template buttons (8 per page). Each button uses `messageBack` to send `/agent <name> ` into the compose box. Navigation buttons allow paging through all templates.

- Manifest generation: `src/slash_commands_manifest.ml` (`teams_json`, `menu_adaptive_card_json`, `agent_menu_adaptive_card_json`)
- Adaptive Card sending: `src/teams.ml` (`send_adaptive_card`)

### Discord

No automatic command registration yet. Discord Application Commands could be registered via REST API in the future. Currently, `/menu` falls back to the standard help text.

### Slack

No automatic command registration yet. Slack app manifests could be generated in the future. Currently, `/menu` falls back to the standard help text.

## CLI Commands

- `clawq manifest teams` — Print Teams manifest JSON (top 10 commands) to stdout
- `clawq manifest teams --output FILE` — Write to file
- `clawq manifest teams -n COUNT` — Customize command count
- `clawq manifest telegram` — Print Telegram setMyCommands payload to stdout

## Adding New Commands

When adding a slash command:

1. Add the entry to `Slash_commands.commands` with an appropriate `priority` value
2. Commands with priority 60+ appear in Teams autocomplete (top 10 default)
3. All commands appear in Telegram autocomplete and `/menu` pages
4. Run `clawq manifest teams` to regenerate the Teams manifest if needed
