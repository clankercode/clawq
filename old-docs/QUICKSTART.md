# clawq Quickstart — Telegram Bot

Get a working AI assistant bot on Telegram in ~5 minutes.

## Prerequisites

- OCaml 5.1+ via opam (`opam switch clawq-5.1`)
- An LLM API key (OpenRouter, OpenAI, or any OpenAI-compatible provider)
- A Telegram bot token (from @BotFather)

## 1. Build

```bash
git clone <repo-url> clawq && cd clawq
make build
```

## 2. Create a Telegram Bot

1. Open Telegram and message [@BotFather](https://t.me/BotFather)
2. Send `/newbot` and follow the prompts
3. Copy the bot token (looks like `7123456789:AAF1k...`)

## 3. Get an LLM API Key

Sign up at one of these providers and grab an API key:

- [OpenRouter](https://openrouter.ai/) (recommended — access to many models)
- [OpenAI](https://platform.openai.com/)
- Any OpenAI-compatible API

## 4. Configure clawq

### Option A — Interactive wizard (recommended)

Run the onboard command to launch the interactive TUI wizard:

```bash
clawq onboard
```

The wizard walks you through every section: provider, model, security, channels, gateway, and memory. At the end it writes `~/.clawq/config.json`. You can re-run any time.

### Option B — Config set commands

Set individual values by dot-path:

```bash
clawq config set providers.0.api_key "sk-or-v1-YOUR_KEY_HERE"
clawq config set providers.0.base_url "https://openrouter.ai/api/v1"
clawq config set providers.0.model "openai/gpt-4o"
clawq config set channels.telegram.bot_token "7123456789:AAF1k_YOUR_TOKEN_HERE"
clawq config set channels.telegram.allow_from '["*"]'
```

Review the result with secrets redacted:

```bash
clawq config show
clawq config show channels    # show one section
clawq config get providers.0.model   # read a single value
```

### Option C — Manual edit

```bash
$EDITOR ~/.clawq/config.json
```

Minimal working config:

```json
{
  "providers": [
    {
      "name": "openrouter",
      "api_key": "sk-or-v1-YOUR_KEY_HERE",
      "base_url": "https://openrouter.ai/api/v1",
      "model": "openai/gpt-4o"
    }
  ],
  "channels": {
    "telegram": {
      "enabled": true,
      "bot_token": "7123456789:AAF1k_YOUR_TOKEN_HERE",
      "allow_from": ["*"]
    }
  }
}
```

### Configuration notes

- **allow_from**: `["*"]` allows all Telegram users. To restrict access, list specific chat IDs: `["123456789", "987654321"]`. Find yours by messaging [@userinfobot](https://t.me/userinfobot).
- **base_url**: Use `https://api.openai.com/v1` for OpenAI directly, or any OpenAI-compatible endpoint.
- **model**: Default model for this provider. Can be overridden per-request.

## 5. Validate

Check your config is valid:

```bash
clawq doctor
```

You should see `doctor: all checks passed`. If there are warnings, fix the noted issues.

Initialize workspace prompt files (`EGO.md`, `AGENTS.md`, etc.):

```bash
clawq workspace init
```

Check the full status:

```bash
clawq status
```

## 6. Start the Daemon

```bash
clawq agent
```

You should see:

```
clawq: [INFO] clawq daemon starting (pid=12345)
clawq: [INFO] Starting Telegram polling for account 'main'
clawq: [INFO] Daemon ready. Gateway on 127.0.0.1:3000
```

The daemon runs in the foreground. Use `Ctrl+C` to stop it.

To run in the background:

```bash
clawq agent &
```

## 7. Chat with Your Bot

Open Telegram, find your bot, and send a message. The bot will respond using the configured LLM.

**Built-in commands:**

| Command | Action |
|---------|--------|
| `/start` | Welcome message |
| `/help` | Show available commands |
| `/new` | Reset conversation (clear history) |

The bot maintains conversation history per chat, so follow-up questions work naturally.

## 8. Verify the Gateway

The daemon also runs an HTTP gateway:

```bash
curl http://127.0.0.1:3000/health
# {"status":"ok"}
```

## Advanced Features

### Docker Deployment

```bash
# Build the Docker image
make docker-build

# Run clawq in Docker
make docker-run

# Or run directly with docker
docker run -it --rm -p 3000:3000 \
  -e CLAWQ_MASTER_KEY="your-passphrase" \
  clawq:latest agent
```

### Secret Encryption

To encrypt API keys at rest:

1. Set `security.encrypt_secrets` to `true` in config
2. Set the `CLAWQ_MASTER_KEY` environment variable with a strong passphrase
3. Run `clawq auth encrypt` to encrypt all plaintext API keys in config
4. Keys are encrypted with AES-256-GCM and stored as `$ENC:...` values
5. At runtime, keys are automatically decrypted using the master key

### Vector Search (Hybrid Memory)

To enable semantic search alongside FTS keyword search:

1. Set `memory.search_enabled` to `true`
2. Configure an embedding provider:
   ```json
   "memory": {
     "search_enabled": true,
     "embedding_provider": "openai",
     "embedding_model": "text-embedding-3-small",
     "vector_weight": 50,
     "keyword_weight": 50
   }
   ```
3. Vector and keyword weights must sum to 100

### Cloudflare Tunnel

To expose your local clawq instance via a public URL:

```bash
# Requires cloudflared to be installed
clawq tunnel start
```

This spawns a `cloudflared` process and prints the assigned `*.trycloudflare.com` URL.

### Runtime Adapters

```bash
# Native runtime (default)
clawq runtime native start
clawq runtime native stop
clawq runtime native health

# Docker runtime
clawq runtime docker start
clawq runtime docker stop
clawq runtime docker health

# Check all runtime status
clawq runtime status
```

### MCP Server Configuration

The MCP server exposes tools over JSON-RPC via stdio. Configure it in config.json:

```json
"mcp": {
  "enabled": true,
  "exposed_tools": ["file_read", "file_write", "shell_exec"]
}
```

- Set `enabled` to `false` to disable the MCP server entirely
- Set `exposed_tools` to an array of tool names to restrict which tools are available
- Omit `exposed_tools` to expose all registered tools

### Resilience Configuration

Configure timeout, retry, and fallback behavior for LLM calls:

```json
"resilience": {
  "timeout_s": 120.0,
  "max_retries": 2,
  "base_delay_s": 1.0,
  "fallback_provider": "groq"
}
```

## Troubleshooting

**Bot doesn't respond:**
- Check the daemon is running and showing `Starting Telegram polling`
- Verify your bot token with `clawq doctor`
- Check that `allow_from` includes your chat ID (or is `["*"]`)
- Look at daemon logs for error messages

**"No providers configured" warning:**
- Make sure `providers` is set in `~/.clawq/config.json` with a valid API key

**LLM API errors:**
- Check your API key is valid and has credits
- Try a different model (e.g., `openai/gpt-3.5-turbo` for lower cost)
- Check the `base_url` matches your provider

**Permission denied / config not found:**
- Run `clawq onboard` to create the config directory
- Check `~/.clawq/config.json` exists and is readable

## Other Useful Commands

```bash
clawq models                       # list configured providers
clawq channel                      # show channel configuration
clawq auth                         # show API key status (secrets redacted)
clawq capabilities                 # list available features

# Config management
clawq config wizard                # re-run the interactive setup wizard
clawq config show                  # display full config (secrets redacted)
clawq config show security         # display one section
clawq config get providers.0.model # read a single value
clawq config set KEY VALUE         # set a value by dot-path
```
