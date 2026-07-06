# Pilot Setup Wizard

The pilot setup wizard (`clawq rooms wizard`) configures room-agent profiles with a plan/apply flow. It is designed for Teams-first pilots but supports Slack, Discord, and Telegram connectors.

## Quick Start

```bash
# Interactive wizard (requires CLAWQ_ADMIN=1)
CLAWQ_ADMIN=1 clawq rooms wizard

# Non-interactive plan (read-only, no side effects)
# Note: use '--' before wizard options to avoid Cmdliner parsing conflicts
clawq rooms -- wizard plan --profile-id my-agent --connector teams --room "19:abc@thread.tacv2"

# Non-interactive apply (requires CLAWQ_ADMIN=1)
CLAWQ_ADMIN=1 clawq rooms -- wizard apply --profile-id my-agent --connector teams --room "19:abc@thread.tacv2"

# Rerun report (compare desired state vs current config)
clawq rooms -- wizard rerun --profile-id my-agent --connector teams --room "19:abc@thread.tacv2"

# Rerun with auto-apply (requires CLAWQ_ADMIN=1)
CLAWQ_ADMIN=1 clawq rooms -- wizard rerun --profile-id my-agent --connector teams --room "19:abc@thread.tacv2" --apply
```

## Teams-First Rollout

The wizard defaults to Teams as the connector type. In **interactive mode**, it auto-detects configured connectors and defaults to Teams if available, otherwise uses the first configured connector. In **non-interactive modes** (plan/apply/rerun), the `--connector` flag defaults to `teams` regardless of configuration.

Teams is the recommended path for pilots because Teams supports:

- **Adaptive Cards** for rich interactive messages
- **Rich questions** with structured responses
- **File consent** flows for document handling
- **Typing indicators** for real-time feedback
- **Native thread replies** for conversation threading

When both Teams and Slack are configured, the wizard displays a capability comparison table to help you choose the right connector for your use case.

## Slack Baseline

Slack is supported as a baseline connector with these capabilities:

- **Reactions** for message acknowledgement
- **Native thread replies** for conversation threading
- **Ambient history capture** for context awareness
- **Channel name support** (e.g., `#general` in addition to channel IDs like `C12345`)

Slack channel ID formats:
- `C12345` — public channel
- `G67890` — private channel
- `D12345` — direct message
- `#general` — channel name (resolved at runtime)

## Modes

### Interactive Mode

The default mode when no subcommand is specified. Prompts for all configuration options interactively.

```bash
CLAWQ_ADMIN=1 clawq rooms wizard
```

### Plan Mode

Shows what would happen without making changes. Safe to run without admin privileges.

```bash
clawq rooms -- wizard plan --profile-id my-agent [options]
```

### Apply Mode

Applies the configuration changes. Requires `CLAWQ_ADMIN=1`.

```bash
CLAWQ_ADMIN=1 clawq rooms -- wizard apply --profile-id my-agent [options]
```

### Rerun Mode

Compares the desired state against the current config and generates a report with status categories:

- **Changed** — value differs from current config; will be updated
- **Already valid** — value matches current config; no action needed
- **Blocked** — cannot be applied due to missing dependencies (e.g., missing access bundles, unconfigured connector)
- **Manual repair** — needs human intervention (e.g., invalid room format, inconsistent budget)

```bash
clawq rooms -- wizard rerun --profile-id my-agent [options]
```

Use `--apply` to automatically apply changes when there are no blocked or manual-repair items.

## Options

| Option | Description | Default |
|--------|-------------|---------|
| `--profile-id ID` | Room profile ID (required) | — |
| `--model M` | Model identifier | `openai:gpt-5.4` |
| `--system-prompt P` | System prompt text | (empty) |
| `--max-iters N` | Max tool iterations (1-1000) | `25` |
| `--allowed-tools T1,T2` | Allowed tools (comma-separated) | (empty) |
| `--denied-tools T1,T2` | Denied tools (comma-separated) | (empty) |
| `--access-bundles B1,B2` | Access bundle IDs (comma-separated) | (empty) |
| `--token-limit N` | Token budget limit | `0` (disabled) |
| `--cost-limit F` | Cost budget limit (USD) | `0` (disabled) |
| `--reset-period P` | Budget reset period | `monthly` |
| `--connector C` | Connector type | `teams` |
| `--room R` | Room/channel ID | (empty) |
| `--inactive` | Create binding as inactive | (false) |
| `--apply` | (rerun only) Apply changes if no blocked items | (false) |

## Connector Detection

The wizard automatically detects which connectors have usable credentials configured:

| Connector | Required Credentials |
|-----------|---------------------|
| Teams | `app_id`, `app_secret` |
| Slack | `bot_token`, `signing_secret` |
| Discord | Discord config stanza present |
| Telegram | Telegram config stanza present |

In **interactive mode**, when no connector is specified, the wizard defaults to Teams if configured, otherwise uses the first available connector. In **non-interactive modes** (plan/apply/rerun), the `--connector` flag defaults to `teams` regardless of configuration.

## Room ID Validation

Each connector has specific room ID format requirements:

### Teams
- Must start with `19:` or end with `@thread.tacv2`
- Example: `19:abc123@thread.tacv2`

### Slack
- Must start with `C` (public), `G` (private), `D` (DM), or `#` (channel name)
- Examples: `C12345`, `#general`

### Discord/Telegram
- Must be non-empty

## Readiness Checks

Before applying changes, the wizard runs readiness checks:

- **Profile ID** — valid format (lowercase alphanumeric with hyphens/underscores, max 64 chars)
- **Model** — non-empty
- **Access Bundles** — all referenced bundles exist in config
- **Connector Available** — connector has usable credentials configured
- **Connector Room** — room ID matches connector format requirements
- **Budget** — limits are non-negative
- **Budget State** — current usage vs limits when a database profile exists
- **Budget Denial Msg** — redacted budget denial message does not leak configured limits (only when a budget is configured)
- **Max Tool Iterations** — between 1 and 1000
- **Budget Reset Period** — one of: daily, weekly, monthly, yearly
- **GitHub App** — app ID, private key, and installations are valid
- **Repo Grants** — access bundles have valid `owner/repo` grants with capabilities
- **Webhook Config** — webhook secrets/paths are configured and a gateway or tunnel endpoint is present
- **Room Backlink** — profile has access bundles with repo grants
- **Activity Ledger** — SQLite schema is accessible
- **Egress Audit** — SQLite schema is accessible

## Merge Semantics

When updating an existing profile, the wizard uses merge semantics:

- **Default values preserve existing**: If you pass the default model (`openai:gpt-5.4`) and the existing profile has a different model, the existing model is preserved
- **Explicit values override**: If you pass a specific model (e.g., `anthropic:claude-opus-4-6`), it overrides the existing value
- **Empty values preserve existing**: If you don't specify a system prompt, the existing one is preserved

This means you can safely re-run the wizard with only the fields you want to change.

## Troubleshooting

### "This command requires admin privileges"

Set the `CLAWQ_ADMIN=1` environment variable:

```bash
CLAWQ_ADMIN=1 clawq rooms -- wizard [subcommand]
```

### Cmdliner parsing conflicts

If you see unexpected flag errors, use `--` before wizard options:

```bash
clawq rooms -- wizard plan --profile-id my-agent
```

### "Connector not configured"

The connector you specified doesn't have usable credentials. Check your config:

```bash
clawq config show channels
```

### "Room ID format is invalid"

Each connector has specific room ID requirements. See [Room ID Validation](#room-id-validation).

### "Access bundle not found"

The specified access bundle ID doesn't exist in your config. Create it first:

```bash
clawq access-bundles create <bundle-id>
```

### "Readiness checks failed"

Review the readiness check output for specific failures. Common issues:
- Invalid profile ID format
- Missing connector credentials
- Invalid room ID format
- Non-existent access bundles

### Rerun shows "Blocked" or "Manual repair" items

- **Blocked**: Missing dependencies (e.g., access bundles not created, connector not configured). Resolve the dependency first.
- **Manual repair**: Needs human intervention (e.g., invalid room format, inconsistent budget). Fix the issue manually before re-running.

## Minimal Build Behavior

The pilot setup wizard is **not available** in the minimal build (`clawq-min`). It requires the full `clawq` binary with runtime integrations.

If you're using the minimal binary, use the full `clawq` binary for room wizard operations:

```bash
clawq rooms wizard [subcommand]
```

## Post-Setup Steps

After applying the wizard configuration:

1. **Restart the daemon** to pick up the new configuration:
   ```bash
   clawq service restart
   ```

2. **Verify the binding**:
   ```bash
   clawq rooms show <room-id>
   ```

3. **Test the room** by sending a message to the configured room/channel

## Examples

### Teams Pilot Setup

```bash
# Plan first to see what would happen
clawq rooms -- wizard plan \
  --profile-id pilot-agent \
  --model "openai:gpt-5.4" \
  --system-prompt "You are a helpful assistant for the engineering team." \
  --max-iters 50 \
  --allowed-tools "web_search,read_file,write_file" \
  --token-limit 1000000 \
  --cost-limit 100.00 \
  --reset-period monthly \
  --connector teams \
  --room "19:abc123@thread.tacv2"

# Apply after reviewing the plan
CLAWQ_ADMIN=1 clawq rooms -- wizard apply \
  --profile-id pilot-agent \
  --model "openai:gpt-5.4" \
  --system-prompt "You are a helpful assistant for the engineering team." \
  --max-iters 50 \
  --allowed-tools "web_search,read_file,write_file" \
  --token-limit 1000000 \
  --cost-limit 100.00 \
  --reset-period monthly \
  --connector teams \
  --room "19:abc123@thread.tacv2"
```

### Slack Channel Setup

```bash
clawq rooms -- wizard plan \
  --profile-id slack-agent \
  --connector slack \
  --room "#general"
```

### Re-run with Auto-Apply

```bash
# Check what would change
clawq rooms -- wizard rerun --profile-id pilot-agent --connector teams --room "19:abc123@thread.tacv2"

# Auto-apply if no blockers
CLAWQ_ADMIN=1 clawq rooms -- wizard rerun --profile-id pilot-agent --connector teams --room "19:abc123@thread.tacv2" --apply
```
