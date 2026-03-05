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

Run the onboard command to generate a config template:

```bash
dune exec clawq -- onboard
```

This creates `~/.clawq/config.json`. Edit it with your keys:

```bash
$EDITOR ~/.clawq/config.json
```

Fill in these fields:

```json
{
  "default_temperature": 0.7,
  "providers": {
    "openrouter": {
      "api_key": "sk-or-v1-YOUR_KEY_HERE",
      "base_url": "https://openrouter.ai/api/v1"
    }
  },
  "agent_defaults": {
    "primary_model": "openai/gpt-4o"
  },
  "channels": {
    "cli": true,
    "telegram": {
      "accounts": {
        "main": {
          "bot_token": "7123456789:AAF1k_YOUR_TOKEN_HERE",
          "allow_from": ["*"]
        }
      }
    }
  },
  "gateway": {
    "host": "127.0.0.1",
    "port": 3000
  }
}
```

### Configuration notes

- **allow_from**: `["*"]` allows all Telegram users. To restrict access, list specific chat IDs (as strings): `["123456789", "987654321"]`. Find your chat ID by messaging [@userinfobot](https://t.me/userinfobot).
- **primary_model**: Any model your provider supports. For OpenRouter, see [openrouter.ai/models](https://openrouter.ai/models).
- **base_url**: Change this if using OpenAI directly (`https://api.openai.com/v1`) or a self-hosted endpoint.

## 5. Validate

Check your config is valid:

```bash
dune exec clawq -- doctor
```

You should see `doctor: all checks passed`. If there are warnings, fix the noted issues.

Check the full status:

```bash
dune exec clawq -- status
```

## 6. Start the Daemon

```bash
dune exec clawq -- agent
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
dune exec clawq -- agent &
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

## Troubleshooting

**Bot doesn't respond:**
- Check the daemon is running and showing `Starting Telegram polling`
- Verify your bot token with `dune exec clawq -- doctor`
- Check that `allow_from` includes your chat ID (or is `["*"]`)
- Look at daemon logs for error messages

**"No providers configured" warning:**
- Make sure `providers` is set in `~/.clawq/config.json` with a valid API key

**LLM API errors:**
- Check your API key is valid and has credits
- Try a different model (e.g., `openai/gpt-3.5-turbo` for lower cost)
- Check the `base_url` matches your provider

**Permission denied / config not found:**
- Run `dune exec clawq -- onboard` to create the config directory
- Check `~/.clawq/config.json` exists and is readable

## Other Useful Commands

```bash
dune exec clawq -- models        # List configured providers
dune exec clawq -- channel       # Show channel configuration
dune exec clawq -- auth          # Show API key status (redacted)
dune exec clawq -- capabilities  # List available features
```
