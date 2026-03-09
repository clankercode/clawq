# I012: Comprehensive Models Suite

**Task ID:** I012
**Status:** Planning
**Created:** 2026-03-10
**Related Files:**
- `.backlog/ideas/I012-add-comprehensive-suite-of-mod.todo`

## Summary

Add a comprehensive suite of model management commands across CLI, slash commands, tools, and HTTP gateway. Includes favorites, usage tracking, and quota monitoring.

## Requirements Summary (from user)

1. **CLI Commands:**
   - `clawq models [list]` - List known models (plaintext like `opencode models`)
   - `clawq models set-default <model>` - Set default model (alias to config set)
   - `clawq usage` - Show provider quota/usage with cache/refresh

2. **Slash Command `/model`:**
   - Full management in messaging channels
   - Ranked by usage frequency + favorites
   - Collapsible table format (like `/tools`)
   - Rich Telegram UI with favorites support

3. **Tools:**
   - `models` tool with actions: list, set, get
   - `provider_usage` tool for quota access

4. **HTTP Gateway Routes:**
   - `GET /models` - List known models
   - `GET /usage` - Provider quota/usage
   - `POST /model/set` - Set session model
   - `GET /model/preferences` - Get favorites/usage

5. **Data Persistence:**
   - Model favorites (per-user)
   - Usage counts for ranking
   - Model catalog for tab-completion prep

## Architecture

### New Modules

| Module | Purpose |
|--------|---------|
| `models_catalog.ml` | Known models list (provider, id, capabilities) |
| `model_preferences.ml` | Favorites + usage tracking (JSON file) |

### Modified Files

| File | Changes |
|------|---------|
| `src/command_bridge.ml` | Add `cmd_usage`, extend `cmd_models` |
| `src/command_bridge_min.ml` | Add minimal versions |
| `src/main.ml` | Add `usage_cmd`, extend `models_cmd` |
| `src/main_min.ml` | Add minimal versions |
| `src/slash_commands.ml` | Add `Model` result type, `format_models_telegram` |
| `src/telegram.ml` | Handle `Model` slash command |
| `src/discord.ml` | Handle `Model` slash command |
| `src/slack.ml` | Handle `Model` slash command |
| `src/tools_builtin.ml` | Add `models` and `provider_usage` tools |
| `src/http_server.ml` | Add `/models`, `/usage`, `/model/*` routes |
| `src/dune` | Add new modules |
| `test/test_slash_commands.ml` | Add `/model` tests |

### Data Storage

**Model Preferences:** `~/.clawq/model_prefs.json`
```json
{
  "favorites": ["anthropic/claude-sonnet-4-5", "openai/gpt-5.4"],
  "usage_counts": {
    "anthropic/claude-sonnet-4-5": 42,
    "openai/gpt-4o": 15
  }
}
```

## Detailed Design

### 1. Models Catalog (`models_catalog.ml`)

Comprehensive list of known models with metadata:
- `provider`: Provider name (anthropic, openai, gemini, etc.)
- `id`: Model ID (claude-sonnet-4-5, gpt-5.4, etc.)
- `context_window`: Token limit
- `supports_vision`: Boolean
- `supports_tools`: Boolean
- `supports_thinking`: Boolean (extended thinking/reasoning)
- `deprecated`: Boolean

**Output format for CLI:**
```
anthropic/claude-opus-4-6 (200K vision thinking)
anthropic/claude-sonnet-4-5 (200K vision)
openai/gpt-5.4 (272K vision thinking)
...
```

### 2. Model Preferences (`model_preferences.ml`)

Manages favorites and usage tracking:
- `load ()` / `save prefs` - JSON persistence
- `add_favorite model` / `remove_favorite model` / `toggle_favorite model`
- `increment_usage model` - Called when model is used
- `ranked_models ()` - Returns models sorted by favorites first, then usage
- `format_for_telegram ()` - HTML formatted with collapsible blocks
- `format_for_cli ()` - Plain text summary

### 3. CLI Commands

#### `clawq models [list] [--provider P]`
Lists known models, optionally filtered by provider.
Output: Plain text, one model per line with metadata.

#### `clawq models set-default <model>`
Alias for `clawq config set agent_defaults.primary_model <model>`.
Validates model against catalog (warns if unknown, allows anyway).

#### `clawq usage [--refresh]`
Shows quota/usage for all configured providers.
- Uses cached values if < 60 seconds old
- `--refresh` / `-r` forces fresh fetch
- Handles HTTP errors (400/401/403/429) gracefully

**Output format:**
```
Provider    Session    Weekly     Monthly    Status
anthropic   45% (2h)   30%        --         ok
codex       80% (1h)   55%        --         constrained
kimi        --         --         20%        ok
```

### 4. Slash Command `/model`

**Subcommands:**
- `/model` - Show current model + ranked list (favorites first)
- `/model set <name>` - Set model for current session
- `/model fav <name>` - Toggle favorite
- `/model unfav <name>` - Remove from favorites
- `/model list [--provider P]` - Full model catalog
- `/model usage` - Show provider quotas

**Telegram Formatting:**
```html
<b>Current Model</b>
<code>anthropic/claude-sonnet-4-5</code>

<b>Favorites</b>
⭐ <code>anthropic/claude-sonnet-4-5</code>
⭐ <code>openai/gpt-5.4</code>

<b>Recent</b>
<blockquote expandable>
<code>anthropic/claude-3-5-haiku</code> (15)
<code>gemini/gemini-2.5-flash</code> (8)
</blockquote>

<b>Usage</b>
anthropic: 45% session, 30% weekly
```

### 5. Tools

#### `models` Tool
```json
{
  "name": "models",
  "description": "List, get, or set the current model",
  "parameters": {
    "action": { "enum": ["list", "get", "set"] },
    "model": { "description": "Model name for set action" },
    "provider": { "description": "Filter by provider for list" }
  }
}
```

Actions:
- `list` - Returns plain text list of models
- `get` - Returns current model
- `set` - Sets model for current session (not persistent)

#### `provider_usage` Tool
```json
{
  "name": "provider_usage",
  "description": "Get quota/usage for LLM providers",
  "parameters": {
    "action": { "enum": ["list", "get"] },
    "provider": { "description": "Provider name for get action" },
    "refresh": { "type": "boolean" }
  }
}
```

### 6. HTTP Gateway Routes

| Route | Method | Auth | Description |
|-------|--------|------|-------------|
| `/models` | GET | No | List known models (JSON) |
| `/models/list` | GET | No | List models (text, for scripts) |
| `/usage` | GET | Yes | Get provider quotas |
| `/usage/refresh` | POST | Yes | Force refresh all quotas |
| `/model/current` | GET | Yes | Get current session model |
| `/model/set` | POST | Yes | Set session model |
| `/model/preferences` | GET | Yes | Get favorites/usage |
| `/model/favorite` | POST | Yes | Toggle favorite |

## Implementation Order

1. **Phase 1: Core Data**
   - Create `models_catalog.ml` ✅ (already created)
   - Create `model_preferences.ml` ✅ (already created)
   - Add to `src/dune`
   - Write tests for both modules

2. **Phase 2: CLI Commands**
   - Extend `cmd_models` in `command_bridge.ml`
   - Add `cmd_usage` in `command_bridge.ml`
   - Update `main.ml` with subcommands
   - Update `main_min.ml` / `command_bridge_min.ml`

3. **Phase 3: Slash Command**
   - Add `Model` variant to `slash_commands.ml` result type
   - Add parsing for `/model` subcommands
   - Add `format_models_telegram` function
   - Update `telegram.ml`, `discord.ml`, `slack.ml` handlers
   - Add tests

4. **Phase 4: Tools**
   - Add `models` tool to `tools_builtin.ml`
   - Add `provider_usage` tool to `tools_builtin.ml`

5. **Phase 5: HTTP Routes**
   - Add routes to `http_server.ml`

6. **Phase 6: Integration**
   - Call `Model_preferences.increment_usage` when model is used
   - Ensure usage tracking works across all paths

## Files Already Created (Premature)

These were created before plan approval:
- `src/models_catalog.ml` - May need adjustment based on plan review
- `src/model_preferences.ml` - May need adjustment based on plan review

## Design Decisions (Confirmed)

1. **Usage tracking trigger:** ✅ On request send
   - Call `increment_usage` when a request is sent to the provider
   - Captures user intent, includes failed attempts

2. **Favorites scope:** ✅ Global
   - Single favorites list in `~/.clawq/model_prefs.json`
   - Shared across all channels (Telegram, Discord, Slack, CLI)

3. **Unknown model handling:** ✅ Warn but allow
   - Show warning if model not in catalog
   - Still allow setting - future-proof for new model releases

## Testing Strategy

- Unit tests for `models_catalog.ml` and `model_preferences.ml`
- Integration tests for `/model` slash command
- CLI smoke tests for `clawq models` and `clawq usage`
- HTTP route tests for new endpoints

## Verification Commands

```bash
# Build
make build

# Run tests
make test

# Manual testing
./_build/default/src/main.exe models
./_build/default/src/main.exe models set-default anthropic/claude-sonnet-4-5
./_build/default/src/main.exe usage
./_build/default/src/main.exe usage --refresh
```
