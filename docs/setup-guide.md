# Clawq Setup Guide

A comprehensive guide to installing, configuring, and operating Clawq — an AI assistant for work and life.

## Table of Contents

1. [Installation](#1-installation)
2. [Initial Configuration](#2-initial-configuration)
3. [GitHub App Setup](#3-github-app-setup)
4. [Room Configuration](#4-room-configuration)
5. [Teams Setup](#5-teams-setup)
6. [Slack Setup](#6-slack-setup)
7. [Credential Management](#7-credential-management)
8. [Egress Policy](#8-egress-policy)
9. [Budget Configuration](#9-budget-configuration)
10. [Monitoring](#10-monitoring)
11. [Troubleshooting](#11-troubleshooting)

---

## 1. Installation

### Prerequisites

- **OCaml 5.1** via opam
- **SQLite3** development headers (`libsqlite3-dev` on Debian/Ubuntu)
- **opam** package manager

### Install Dependencies

```bash
# Install project dependencies
opam install . --deps-only --with-test

# System package (Debian/Ubuntu)
sudo apt install libsqlite3-dev
```

### Build Variants

Clawq ships two binaries:

| Binary | Description | Use Case |
|--------|-------------|----------|
| `clawq` | Full build with all integrations | Production, Teams/Slack/GitHub, web UI |
| `clawq-min` | Minimal core CLI | Lightweight environments, no network integrations |

### Build Commands

```bash
# Standard build
make build

# Minimal build
make build-minimal

# Optimized builds (for deployment)
make build-opt-speed        # -O3 optimization
make build-opt-size         # -O2 -compact
make build-opt-speed-stripped
make build-opt-size-stripped
make build-opt-minimal
```

### Verify Installation

```bash
# Run CLI help
make run

# Run quick tests (skips Slow-tagged tests)
make test

# Run all tests
make test-all

# Check formatting
make fmt-check
```

### Important Build Notes

- **Never run `dune` commands in parallel** — Dune locks `_build`; concurrent runs hang. Wait for any running dune process to finish before starting another.
- The default Makefile shell runs through `opam exec --switch=clawq-5.1`. If commands fail due to environment mismatch, prefix with `opam exec --switch=clawq-5.1 -- <command>`.

---

## 2. Initial Configuration

Clawq uses a single JSON configuration file located at `~/.clawq/config.json` (or `$CLAWQ_HOME/config.json`).

### Config File Structure

```jsonc
{
  "workspace": "~/.clawq/workspace",
  "default_temperature": 0.7,

  // Provider configurations (OpenAI, Anthropic, etc.)
  "providers": [],

  // Agent defaults
  "agent_defaults": {
    "primary_model": "openai-codex:gpt-5.4",
    "max_tool_iterations": 10,
    "autonomous_continuation_delay": 90.0,
    "tool_status_mode": "consolidated"
  },

  // Channel connectors
  "channels": {
    "teams": { /* ... */ },
    "slack": { /* ... */ },
    "github": { /* ... */ },
    "discord": { /* ... */ },
    "telegram": { /* ... */ }
    // 17 channel types total
  },

  // Gateway (web UI, API endpoints)
  "gateway": {
    "host": "127.0.0.1",
    "port": 13451,
    "require_pairing": true
  },

  // Tunnel for webhook reachability
  "tunnel": {
    "provider": "cloudflare",
    "enabled": false
  },

  // Memory/storage
  "memory": {
    "backend": "sqlite",
    "search_enabled": false
  },

  // Security
  "security": {
    "workspace_only": true
  },

  // Room profiles and bindings
  "room_profiles": [],
  "room_profile_bindings": [],
  "access_bundles": [],
  "access_scopes": [],

  // Credential handles
  "credential_handles": [],

  // Resilience
  "resilience": {
    "timeout_s": 120,
    "retries": 2
  },

  // Heartbeat (context refresh)
  "heartbeat": {
    "enabled": true,
    "interval_s": 250,
    "quiet_start": 23,
    "quiet_end": 8
  }
}
```

### Model Format Convention

The canonical format for model identifiers is `provider:model` (with a colon):

```
openai:gpt-5.4
anthropic:claude-opus-4-6
openai-codex:gpt-5.4
```

Legacy `provider/model` (slash) and bare `model` formats are accepted but deprecated and produce warnings.

### Provider Configuration

Each provider in the `providers` array supports these fields:

| Field | Description | Example |
|-------|-------------|---------|
| `api_key` | API key (or env var reference) | `"$OPENAI_API_KEY"` |
| `kind` | Provider type | `"openai"`, `"anthropic"`, `"google"` |
| `base_url` | Custom API base URL | `"https://api.openai.com/v1"` |
| `default_model` | Default model for this provider | `"gpt-5.4"` |
| `thinking_budget_tokens` | Reasoning token budget | `10000` |
| `prompt_cache_retention` | Cache TTL | `"24h"` |
| `http_timeout_s` | Per-request timeout | `120` |

### Config Reload

The daemon polls the config file every 10 seconds for changes. For immediate reload:

```bash
# Send SIGHUP to the daemon process
kill -HUP $(pgrep -f "clawq daemon")
```

---

## 3. GitHub App Setup

Clawq supports GitHub App authentication for tighter integration with repositories. The setup involves creating a GitHub App, installing it, and configuring the private key.

### Step 1: Create a GitHub App

1. Go to **GitHub > Settings > Developer settings > GitHub Apps > New GitHub App**.
2. Fill in the required fields:
   - **GitHub App name**: e.g., `clawq-bot`
   - **Homepage URL**: your organization or project URL
3. Set **Webhook**:
   - **Active**: checked
   - **Webhook URL**: `https://<your-domain>/webhook/github` (or use a tunnel — see [Gateway & Tunnel](#gateway--tunnel))
   - **Webhook Secret**: generate a strong random string (you will need this later)
4. Set **Repository permissions**:
   - **Contents**: Read & Write (for branch creation)
   - **Issues**: Read & Write (for comments)
   - **Pull requests**: Read & Write (for PR creation)
   - **Actions**: Read (for workflow triggers)
5. Subscribe to events: **Issues**, **Pull requests**, **Issue comment**, **Pull request review comment**
6. Click **Create GitHub App**.
7. **Note the App ID** shown on the app settings page.

### Step 2: Generate a Private Key

1. On the GitHub App settings page, scroll to **Private keys**.
2. Click **Generate a private key**.
3. A `.pem` file will be downloaded. Store it securely:
   ```bash
   mkdir -p ~/.clawq/secrets
   mv ~/Downloads/clawq-bot.*.private-key.pem ~/.clawq/secrets/github-app.pem
   chmod 600 ~/.clawq/secrets/github-app.pem
   ```

### Step 3: Install the App on Repositories

1. On the GitHub App settings page, click **Install App**.
2. Select the organization/user and repositories.
3. **Note the Installation ID** from the URL: `https://github.com/organizations/<org>/settings/installations/<INSTALLATION_ID>`

### Step 4: Configure Clawq

Add the GitHub App configuration to `config.json`:

```jsonc
{
  "channels": {
    "github": {
      "auth": {
        "type": "github_app",
        "app_id": 123456,
        "private_key_path": "~/.clawq/secrets/github-app.pem",
        "webhook_secret": "your-webhook-secret-here",
        "installations": [
          {
            "installation_id": 78901234,
            "repos": ["your-org/your-repo", "your-org/other-repo"]
          }
        ]
      },
      "repos": [
        {
          "name": "your-org/your-repo",
          "webhook_secret": "your-webhook-secret-here",
          "webhook_path": "/webhook/github"
        }
      ]
    }
  }
}
```

### Step 5: Validate with the Wizard

```bash
# Run the setup wizard
clawq setup github
```

The wizard performs these checks automatically (see `github_wizard_checks.ml`):

- **GitHub App token**: verifies `app_id > 0`, private key path is non-empty, PEM file is valid RSA, installations have valid IDs
- **Repo grants**: validates access bundles have properly formatted `owner/repo` grants with non-empty capabilities
- **Webhook reachability**: checks webhook secrets/paths are configured and a gateway port or tunnel URL exists
- **Room backlink**: verifies the room profile has access bundles with repo grants

### Token Lifecycle

The GitHub App token flow (handled by `github_app_token.ml`):

1. **PEM key** is loaded from disk (PKCS#8 or PKCS#1 format)
2. A **short-lived JWT** (RS256, max 10 minutes) is generated and signed
3. The JWT is exchanged for an **installation access token** via the GitHub API
4. The token is **cached for ~50 minutes** (GitHub tokens expire after 60 minutes)
5. Tokens are scoped to an `installation_id` and optionally specific repos

### Gateway & Tunnel

For GitHub webhooks to reach Clawq, you need either a public gateway or a tunnel:

```bash
# Option A: Start the gateway directly (if you have a public IP/domain)
clawq gateway start

# Option B: Use a tunnel provider
clawq setup tunnel          # Interactive tunnel setup
clawq tunnel start          # Start the configured tunnel
```

Supported tunnel providers: `cloudflare` (default), `tailscale`, `ngrok`, `custom`.

Custom tunnels use `CLAWQ_TUNNEL_COMMAND` and `CLAWQ_TUNNEL_URL_REGEX` environment variables.

---

## 4. Room Configuration

Room profiles define how Clawq behaves in a specific chat room. Use the setup wizard for guided configuration.

### Using the Room Wizard

```bash
# Interactive wizard (requires admin privileges)
CLAWQ_ADMIN=1 clawq rooms wizard

# Non-interactive plan (read-only, safe to run without admin)
clawq rooms -- wizard plan --profile-id my-agent --connector teams --room "19:abc@thread.tacv2"

# Non-interactive apply
CLAWQ_ADMIN=1 clawq rooms -- wizard apply --profile-id my-agent --connector teams --room "19:abc@thread.tacv2"

# Compare desired state vs current config
clawq rooms -- wizard rerun --profile-id my-agent --connector teams --room "19:abc@thread.tacv2"

# Auto-apply if no blockers
CLAWQ_ADMIN=1 clawq rooms -- wizard rerun --profile-id my-agent --connector teams --room "19:abc@thread.tacv2" --apply
```

### Room Profile Fields

| Field | Description | Default |
|-------|-------------|---------|
| `id` | Profile identifier (lowercase alphanumeric, hyphens, underscores, max 64 chars) | required |
| `display_name` | Human-readable name | none |
| `model` | Model identifier | `"openai:gpt-5.4"` |
| `system_prompt` | Custom system prompt text | empty |
| `max_tool_iterations` | Max tool call iterations (1–1000) | `25` |
| `allowed_tools` | Whitelist of tools | empty (all allowed) |
| `denied_tools` | Blacklist of tools | empty |
| `access_bundle_ids` | Bound access bundle IDs | empty |
| `status` | Profile status | `"active"` |

### Access Bundles

Access bundles group permissions and grants. Create one via config:

```jsonc
{
  "access_bundles": [
    {
      "id": "engineering-bundle",
      "display_name": "Engineering Team Bundle",
      "status": "active",
      "allowed_tools": ["web_search", "read_file", "write_file"],
      "repo_grants": [
        {
          "repo": "your-org/your-repo",
          "capabilities": ["read", "comment", "branch", "pr"]
        }
      ],
      "egress_rules": [
        { "host": "api.github.com", "action": "allow", "log_policy": "log" },
        { "host": "*", "action": "deny", "log_policy": "log" }
      ]
    }
  ]
}
```

### Access Scopes (Resolution Hierarchy)

Access scopes define which bundles apply to which rooms. Scopes are resolved in priority order:

```
Default (0) < Workspace (1) < Channel (2) < Room (3)
```

Higher-priority scopes override lower ones. Each scope references bundles by ID:

```jsonc
{
  "access_scopes": [
    {
      "level": "workspace",
      "access_bundle_ids": ["engineering-bundle"]
    },
    {
      "level": "room",
      "selector": { "room": "19:abc@thread.tacv2" },
      "access_bundle_ids": ["engineering-bundle", "admin-bundle"]
    }
  ]
}
```

### Room Profile Binding

Bind a profile to a specific chat room:

```jsonc
{
  "room_profile_bindings": [
    {
      "profile_id": "pilot-agent",
      "room": "19:abc123@thread.tacv2",
      "active": true
    }
  ]
}
```

### Wizard Options Reference

| Option | Description | Default |
|--------|-------------|---------|
| `--profile-id ID` | Room profile ID (required) | — |
| `--model M` | Model identifier | `openai:gpt-5.4` |
| `--system-prompt P` | System prompt text | empty |
| `--max-iters N` | Max tool iterations (1–1000) | `25` |
| `--allowed-tools T1,T2` | Allowed tools (comma-separated) | empty |
| `--denied-tools T1,T2` | Denied tools (comma-separated) | empty |
| `--access-bundles B1,B2` | Access bundle IDs (comma-separated) | empty |
| `--token-limit N` | Token budget limit | `0` (disabled) |
| `--cost-limit F` | Cost budget limit (USD) | `0` (disabled) |
| `--reset-period P` | Budget reset period | `monthly` |
| `--connector C` | Connector type | `teams` |
| `--room R` | Room/channel ID | empty |
| `--inactive` | Create binding as inactive | false |
| `--apply` | (rerun only) Apply if no blockers | false |

### Merge Semantics

When updating an existing profile, the wizard uses merge semantics:
- **Default values preserve existing**: If you pass the default model (`openai:gpt-5.4`) and the existing profile has a different model, the existing model is preserved.
- **Explicit values override**: If you pass a specific model (e.g., `anthropic:claude-opus-4-6`), it overrides.
- **Empty values preserve existing**: If you don't specify a system prompt, the existing one is kept.

This means you can safely re-run the wizard with only the fields you want to change.

### Rerun Report Status Categories

When using `clawq rooms wizard rerun`, each config item is classified:

- **Changed** — value differs from current config; will be updated
- **Already valid** — value matches current config; no action needed
- **Blocked** — cannot be applied due to missing dependencies (e.g., missing access bundles, unconfigured connector)
- **Manual repair** — needs human intervention (e.g., invalid room format, inconsistent budget)

---

## 5. Teams Setup

Teams is the recommended connector for rich interactive experiences. It supports Adaptive Cards, rich questions, file consent flows, and typing indicators.

### Step 1: Register a Bot in Azure

1. Go to **Azure Portal > App registrations > New registration**.
2. Name: e.g., `Clawq Bot`.
3. Supported account types: single tenant or multi-tenant as needed.
4. Register and note the **Application (client) ID** and **Directory (tenant) ID**.

### Step 2: Create a Client Secret

1. In the app registration, go to **Certificates & secrets > New client secret**.
2. Note the **secret value** (not the secret ID).

### Step 3: Configure Bot Framework

1. Go to **Azure Portal > Bot Channels Registration** (or create via Teams Developer Portal).
2. Set the messaging endpoint: `https://<your-domain>/webhook/teams`
3. Link the App registration from Step 1.

### Step 4: Enable Teams Channel

1. In the Bot resource, go to **Channels**.
2. Add the **Microsoft Teams** channel.

### Step 5: Configure Clawq

```jsonc
{
  "channels": {
    "teams": {
      "app_id": "your-app-id",
      "app_secret": "your-client-secret",
      "tenant_id": "your-tenant-id",
      "webhook_path": "/webhook/teams",
      "service_url": "https://smba.trafficmanager.net",
      "allow_teams": [],           // empty = all teams
      "allow_users": [],           // empty = all users
      "mention_mode": "entity",    // "entity", "text", or "none"
      "file_consent_cards": true   // OneDrive file upload flow
    }
  }
}
```

### Teams Room ID Format

Teams conversation IDs follow specific formats:
- Must start with `19:` — e.g., `19:abc123@thread.tacv2`
- Thread conversations end with `@thread.tacv2`

### Teams Capabilities

| Feature | Support |
|---------|---------|
| Edit messages | In-place |
| Delete messages | Yes |
| Reactions | No |
| Typing indicator | Yes |
| Status updates | Yes |
| File sending | Yes |
| Adaptive Cards | Yes |
| Buttons | Yes |
| Thread replies | Native |
| Rich questions | Yes |
| Max message length | 28,672 chars |

---

## 6. Slack Setup

Slack is supported as a baseline connector with reactions, native threads, and ambient history capture.

### Step 1: Create a Slack App

1. Go to **https://api.slack.com/apps > Create New App**.
2. Choose **From scratch**.
3. Name: e.g., `Clawq`.
4. Select your workspace.

### Step 2: Configure Bot Permissions

Under **OAuth & Permissions**, add these Bot Token Scopes:
- `app_mentions:read`
- `chat:write`
- `channels:history`
- `channels:read`
- `groups:history`
- `groups:read`
- `im:history`
- `im:read`
- `im:write`
- `reactions:write`
- `files:write`

### Step 3: Enable Socket Mode (Recommended)

1. Go to **Socket Mode** and enable it.
2. Generate an **App-Level Token** with `connections:write` scope.
3. Note the `xapp-...` token.

### Step 4: Configure Event Subscriptions

Under **Event Subscriptions**, subscribe to:
- `app_mention`
- `message.channels`
- `message.groups`
- `message.im`

### Step 5: Install to Workspace

Under **Install App**, click **Install to Workspace** and note the **Bot User OAuth Token** (`xoxb-...`).

### Step 6: Configure Clawq

```jsonc
{
  "channels": {
    "slack": {
      "bot_token": "xoxb-your-bot-token",
      "signing_secret": "your-signing-secret",
      "events_path": "/slack/events",
      "app_token": "xapp-your-app-token",
      "socket_mode": true,
      "allow_channels": [],    // empty = all channels
      "allow_users": []        // empty = all users
    }
  }
}
```

### Private Channels

By default, Clawq **will not read or operate in Slack private channels**, even if their IDs are listed in `allow_channels`. This is a defense-in-depth safety measure.

To allow a private channel:
```json
{
  "channels": {
    "slack": {
      "allow_channels": ["C-public", "G-private"],
      "allow_private_channels": ["G-private"]
    }
  }
}
```

For backward-compatible behaviour (pre-B735), set `private_channel_policy: "allow_if_listed"`.

Refusals are logged to the room activity ledger under `private_channel_refused`.

### Slack Channel ID Formats

| Prefix | Type | Example |
|--------|------|---------|
| `C` | Public channel | `C12345` |
| `G` | Private channel | `G67890` |
| `D` | Direct message | `D12345` |
| `#` | Channel name | `#general` |

### Slack Capabilities

| Feature | Support |
|---------|---------|
| Edit messages | Delete+resend |
| Delete messages | Yes |
| Reactions | Yes |
| Typing indicator | No |
| Status updates | No |
| File sending | No |
| Adaptive Cards | No |
| Buttons | No |
| Thread replies | Thread-like |
| Rich questions | No |
| Max message length | 4,000 chars |

### Teams vs Slack Comparison

When both connectors are configured, the wizard displays a capability comparison:

```
=== Teams vs Slack Capability Comparison ===

  Feature                 Teams           Slack
  ----------------------  --------------- ---------------
  Edit messages           In-place        Delete+resend
  Delete messages         Yes             Yes
  Reactions               No              Yes
  Typing indicator        Yes             No
  Status updates          Yes             No
  File sending            Yes             No
  Adaptive Cards          Yes             No
  Buttons                 Yes             No
  Thread replies          Native          Thread-like
  Rich questions          Yes             No
  Max message length      28672           4000
```

**Recommendation**: Use Teams for rich interactions (cards, buttons, file consent). Use Slack for conversations where reactions and native threads matter.

---

## 7. Credential Management

Clawq provides three layers of credential handling: direct values, environment variable references, and encrypted storage.

### Layer 1: Environment Variables

Reference secrets via `$` prefix in config values:

```jsonc
{
  "providers": [
    {
      "api_key": "$OPENAI_API_KEY",
      "kind": "openai"
    }
  ]
}
```

Key environment variables:

| Variable | Purpose |
|----------|---------|
| `CLAWQ_HOME` | Config directory (default `~/.clawq`) |
| `CLAWQ_ADMIN` | Set to `1` for admin operations |
| `CLAWQ_MASTER_KEY` | Master key for encrypted secret store |
| `CLAWQ_GITHUB_API_BASE` | Custom GitHub API base URL |
| `CLAWQ_RUNNER_TOKEN` | Runner authentication token |
| `CLAWQ_MCP_URL` | MCP server URL |
| `CLAWQ_TUNNEL_COMMAND` | Custom tunnel command |
| `CLAWQ_TUNNEL_URL_REGEX` | Regex to extract tunnel URL from output |

### Layer 2: Encrypted Secret Store

For secrets that must be stored in the config file at rest:

```bash
# Set a master key (one-time)
export CLAWQ_MASTER_KEY="your-strong-master-key"
```

Encrypted values use the `$ENC:` prefix + base64-encoded nonce and ciphertext:

```jsonc
{
  "channels": {
    "teams": {
      "app_secret": "$ENC:base64(nonce+ciphertext)..."
    }
  }
}
```

The encryption uses **AES-256-GCM** with a key derived from `CLAWQ_MASTER_KEY` via **PBKDF2-SHA256** (100,000 iterations, salt `"clawq-secret-store-v1"`).

### Layer 3: Credential Handles

Credential handles provide a type-safe abstraction for secrets. Define them in the top-level `credential_handles` array:

```jsonc
{
  "credential_handles": [
    {
      "id": "github-app:main",
      "provider": {
        "type": "env_var",
        "name": "GITHUB_APP_PRIVATE_KEY"
      },
      "description": "GitHub App PEM key",
      "status": "active"
    },
    {
      "id": "slack-token",
      "provider": {
        "type": "file",
        "path": "~/.clawq/secrets/slack-token.txt"
      },
      "description": "Slack bot token from file",
      "status": "active"
    },
    {
      "id": "encrypted-key",
      "provider": {
        "type": "encrypted",
        "cipher_text": "$ENC:..."
      },
      "description": "Encrypted API key",
      "status": "active"
    }
  ]
}
```

Provider types:

| Type | Description | Fields |
|------|-------------|--------|
| `env_var` | Read from environment variable | `name` |
| `file` | Read from file (supports `~` expansion) | `path` |
| `encrypted` | Decrypt via secret store | `cipher_text` |
| `prompt` | Interactive at startup | `description` |

### Credential Safety Guarantees

Credential values are **never**:
- Stored in the config record after parsing
- Serialized to JSON output
- Included in prompts or context
- Logged (always redacted)
- Exposed to sandboxed environments

To soft-delete a handle, set `"status": "deleted"`.

---

## 8. Egress Policy

Egress rules control which external hosts and paths Clawq is allowed to access.

### Rule Format

Rules are defined in access bundles:

```jsonc
{
  "access_bundles": [
    {
      "id": "engineering-bundle",
      "egress_rules": [
        {
          "host": "api.github.com",
          "path": "/repos/*",
          "method": "GET",
          "action": "allow",
          "log_policy": "log"
        },
        {
          "host": "api.openai.com",
          "action": "allow",
          "log_policy": "log"
        },
        {
          "host": "*.internal.corp.com",
          "action": "deny",
          "log_policy": "log"
        },
        {
          "host": "*",
          "action": "deny",
          "log_policy": "log"
        }
      ]
    }
  ]
}
```

### Rule Fields

| Field | Type | Description |
|-------|------|-------------|
| `host` | string (glob) | Hostname pattern. Supports `*` (any) and `*.example.com` (subdomains) |
| `path` | string (glob, optional) | URL path pattern. `"/api/*"` matches `/api/anything`. `null` = any path |
| `method` | string (optional) | HTTP method. Case-insensitive. `null` = any method |
| `action` | `allow` or `deny` | Whether to permit or block the request |
| `log_policy` | `log` or `no_log` | Whether to record this decision in the audit log |

### Evaluation Order

1. Rules from higher-priority scopes are evaluated first: **Room > Channel > Workspace > Default**
2. Within a scope, rules are evaluated **in order of definition**
3. **First match wins** — once a rule matches, its action is applied
4. **Default policy for unmatched requests: Deny with Log**

### Default Egress Rule

If no rules match a request, the default is:
```
{ "host": "*", "action": "deny", "log_policy": "log" }
```

This means **all outbound requests are denied by default**. You must explicitly allow hosts you need.

### Testing Egress Rules

Test your rules before deploying:

```bash
# Validate delivery with egress simulation
clawq rooms -- wizard validate-delivery --profile-id my-agent --connector teams --room "19:abc@thread.tacv2"
```

---

## 9. Budget Configuration

Budget limits prevent runaway costs. Limits are per-room-profile and reset periodically.

### Setting Budget Limits

Via the wizard:

```bash
clawq rooms -- wizard plan \
  --profile-id my-agent \
  --token-limit 1000000 \
  --cost-limit 100.00 \
  --reset-period monthly \
  --connector teams \
  --room "19:abc@thread.tacv2"
```

### Budget Fields

| Field | Description | Default |
|-------|-------------|---------|
| `token_limit` | Maximum tokens per period (0 = disabled) | `0` |
| `cost_limit_usd` | Maximum cost in USD per period (0 = disabled) | `0.0` |
| `reset_period` | Budget reset cycle | `monthly` |
| `soft_warn_threshold_pct` | Soft warning threshold | `0.8` (80%) |

Valid reset periods: `daily`, `weekly`, `monthly`, `yearly`.

### Soft vs Hard Limits

- **Soft limit** (80% by default): A warning is issued once per period when usage exceeds this threshold. The warning is debounced — it fires only once per period.
- **Hard limit** (100%): When usage reaches this limit, all further requests are denied. The denial message is **redacted** to avoid leaking budget details to users.

### Budget Denial Messages

When a budget limit is exceeded, the user sees a safe redacted message:

```
Budget exceeded for this profile.
```

This message intentionally **does not** include:
- Token limit values
- Cost limit values
- Currency identifiers (USD)
- The word "limits"

This prevents users from extracting budget configuration from denial messages.

### Budget Reservations

For concurrent requests, Clawq uses an atomic reservation system:

1. Before making a provider call, the system checks `(current_usage + reserved) < limit`
2. If within limits, a reservation slot is allocated
3. After the call completes, the reservation is converted to actual usage
4. This prevents race conditions when multiple requests are in flight

### Monitoring Budget State

Check current budget usage via readiness checks:

```bash
clawq rooms -- wizard rerun --profile-id my-agent --connector teams --room "19:abc@thread.tacv2"
```

The output includes budget state when a database is available:

```
[PASS] Budget State: tokens: 500/1000 (50.0%), cost: $2.50/$10.00 (25.0%), period: monthly, soft threshold: 80%
```

Or when limits are exceeded:

```
[FAIL] Budget State: tokens: 1100/1000 (110.0%), cost: $12.00/$10.00 (120.0%), period: monthly, soft threshold: 80% [HARD LIMIT EXCEEDED]
```

---

## 10. Monitoring

### Readiness Checks

The wizard runs comprehensive readiness checks before applying configuration:

| Check | Description |
|-------|-------------|
| Profile ID | Valid format (lowercase alphanumeric, hyphens, underscores, max 64 chars) |
| Model | Non-empty model identifier |
| Access Bundles | All referenced bundles exist in config |
| Connector Available | Connector has usable credentials configured |
| Connector Room | Room ID matches connector format requirements |
| Budget | Limits are non-negative |
| Max Tool Iterations | Between 1 and 1000 |
| Budget Reset Period | One of: daily, weekly, monthly, yearly |
| GitHub App | App ID, private key, and installations are valid |
| Repo Grants | Access bundles have valid `owner/repo` grants with capabilities |
| Webhook Config | Webhook secrets/paths are configured with a reachable endpoint |
| Room Backlink | Profile has access bundles with repo grants |
| Activity Ledger | SQLite schema is accessible |
| Egress Audit | SQLite schema is accessible |
| Budget State | Current usage vs limits (with DB) |
| Budget Denial Msg | Redaction is safe (no sensitive details leaked) |

### Activity Ledger

The room activity ledger records all room events to SQLite:

```bash
# Query via CLI (example structure)
clawq audit activity --room "19:abc@thread.tacv2" --limit 50
```

Fields recorded: `room_id`, `event_type`, `timestamp`, `actor`, `metadata` (JSON).

### Egress Audit

Every egress decision (allow/deny) is logged:

```bash
# Query via CLI
clawq audit egress --decision deny --limit 50
```

Filter options: `decision`, `session_key`, `tool_name`, `from_timestamp`, `to_timestamp`.

All sensitive fields are **redacted** before storage:
- Host, method, path are redacted
- Credential handle IDs are stored as opaque aliases only

### Access Snapshots

Immutable snapshots of resolved access policy are persisted for each work session:

- Records: config hash, bundle sources, all resolved grants/denials, instruction digests, egress rule count, room classification, room policy decision
- Work types tracked: `Room_turn`, `Background_task`, `Ambient_work`, `GitHub_trigger`, `Routine`

### Delivery Tracking

Message delivery follows a lifecycle:

```
Attempted -> Accepted -> Confirmed
          -> Failed
          -> Unconfirmed
```

Each state is recorded with: `room_id`, `thread_id`, `reply_to_id`, `service_url`, `connector`, `message_id`, `activity_id`.

### Audit Log Integrity

The audit log uses **HMAC-SHA256 signatures** with chained hashes for tamper detection. Each log entry's signature depends on the previous entry, making the log append-only and verifiable.

---

## 11. Troubleshooting

### "This command requires admin privileges"

Set the `CLAWQ_ADMIN` environment variable:

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

Each connector requires specific credentials:

| Connector | Required Credentials |
|-----------|---------------------|
| Teams | `app_id`, `app_secret` |
| Slack | `bot_token`, `signing_secret` |
| Discord | Discord config stanza present |
| Telegram | Telegram config stanza present |

### "Room ID format is invalid"

Each connector has specific room ID requirements:

- **Teams**: Must start with `19:` or end with `@thread.tacv2`. Example: `19:abc123@thread.tacv2`
- **Slack**: Must start with `C` (public), `G` (private), `D` (DM), or `#` (channel name). Examples: `C12345`, `#general`
- **Discord/Telegram**: Must be non-empty

### "Access bundle not found"

The specified access bundle ID doesn't exist in your config. Create it first, then re-run the wizard.

### "GitHub App private key invalid"

The PEM file at `private_key_path` cannot be parsed. Common causes:
- File doesn't exist or isn't readable
- File is not in PKCS#8 or PKCS#1 format
- File is not an RSA key

Verify:
```bash
# Check file exists and permissions
ls -la ~/.clawq/secrets/github-app.pem

# Verify it's a valid RSA key
openssl rsa -in ~/.clawq/secrets/github-app.pem -check -noout
```

### "Webhook configured but no reachable endpoint"

GitHub (or other) webhooks need a public endpoint. Either:
- Start the gateway: `clawq gateway start`
- Or start a tunnel: `clawq tunnel start`

### "Readiness checks failed"

Review the readiness check output for specific failures. The wizard shows `PASS` or `FAIL` for each check with a specific repair command when applicable.

### Rerun shows "Blocked" or "Manual repair" items

- **Blocked**: Missing dependencies (e.g., access bundles not created, connector not configured). Resolve the dependency first, then re-run.
- **Manual repair**: Needs human intervention (e.g., invalid room format, inconsistent budget). Fix the issue manually before re-running.

### Daemon not picking up config changes

The daemon polls every 10 seconds. For immediate reload:

```bash
# Send SIGHUP
kill -HUP $(pgrep -f "clawq daemon")

# Or restart fully
clawq daemon restart
```

### Dune build hangs or fails on lock

Dune locks the `_build` directory. Never run multiple dune commands in parallel:

```bash
# Check for stale locks
scripts/clean_stale_dune_locks.sh

# If lock persists, another dune process is running
ps aux | grep dune
```

### Budget shows "HARD LIMIT EXCEEDED" but requests should be allowed

Check the budget period. If the period rolled over, the budget resets automatically. Use the wizard rerun to see current state:

```bash
clawq rooms -- wizard rerun --profile-id my-agent --connector teams --room "19:abc@thread.tacv2"
```

### Minimal binary missing features

The `clawq-min` binary does not include:
- Room wizard
- Teams/Slack connectors
- Web UI
- Network integrations

Use the full `clawq` binary for these features.

---

## Quick Start Checklist

1. **Install**: `opam install . --deps-only --with-test && make build`
2. **Initialize config**: `~/.clawq/config.json` with at least a provider
3. **Set up a connector**: Configure Teams or Slack credentials
4. **Create an access bundle**: Define tools, repo grants, egress rules
5. **Run the room wizard**: `CLAWQ_ADMIN=1 clawq rooms wizard`
6. **Restart daemon**: `clawq daemon restart`
7. **Verify**: Send a test message to the configured room
8. **Set budgets**: Use wizard `--token-limit` and `--cost-limit` flags
9. **Monitor**: Use `clawq rooms wizard rerun` for health checks
