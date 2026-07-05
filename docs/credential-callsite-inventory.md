# Credential-Bearing Callsite Inventory

**Task:** P18.M1.E1.T001 — Inventory credential-bearing callsites  
**Date:** 2026-06-29  
**Last updated:** 2026-06-30  
**Author:** Automated codebase analysis

This document inventories every location in the Clawq codebase where
credentials (API keys, tokens, secrets, passwords) are used in outbound
requests, inbound verification, or stored in memory.

**Verification boundaries**: See [`verification-boundaries.md`](verification-boundaries.md)
for a cross-cutting view of all security-relevant subsystems.

## Classification Legend

- **Redaction behavior**: How the credential value is redacted when logged/displayed
- **Risk**: HIGH = credential sent over network; MEDIUM = credential verified in-memory; LOW = credential stored/loaded only
- **Enforceability**: EXISTING = redaction already works; PARTIAL = some paths redacted, some not; MISSING = no redaction

---

## 1. LLM Provider API Keys

### 1.1 OpenAI-Compatible Providers

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `provider.ml:180` | `Provider` | `provider.api_key` | `Authorization: Bearer <key>` | None in request path | HIGH | MISSING |
| `provider.ml:321` | `Provider` | `provider.api_key` | `Authorization: Bearer <key>` (streaming) | None in request path | HIGH | MISSING |
| `model_discovery.ml:386` | `Model_discovery` | `api_key` param | `Authorization: Bearer <key>` | None | HIGH | MISSING |

### 1.2 Anthropic Providers

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `provider_anthropic.ml:192-196` | `Provider_anthropic` | `provider.api_key` | `x-api-key: <key>` + `api-key: <key>` | None in request path | HIGH | MISSING |
| `provider_anthropic.ml:512-513` | `Provider_anthropic` | `provider.api_key` | `x-api-key: <key>` + `api-key: <key>` (streaming) | None in request path | HIGH | MISSING |

### 1.3 Gemini Providers

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `provider_gemini.ml:250` | `Provider_gemini` | `provider.api_key` | `x-goog-api-key: <key>` | None in request path | HIGH | MISSING |
| `provider_gemini.ml:277` | `Provider_gemini` | `provider.api_key` | `x-goog-api-key: <key>` (streaming) | None in request path | HIGH | MISSING |

### 1.4 Cohere Providers

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `provider_cohere.ml:144` | `Provider_cohere` | `provider.api_key` | `Authorization: Bearer <key>` | None in request path | HIGH | MISSING |
| `provider_cohere.ml:182` | `Provider_cohere` | `provider.api_key` | `Authorization: Bearer <key>` (streaming) | None in request path | HIGH | MISSING |

### 1.5 MiniMax Providers

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `provider_minimax.ml:239` | `Provider_minimax` | `provider.api_key` | `x-api-key: <key>` | None in request path | HIGH | MISSING |
| `provider_minimax.ml:541` | `Provider_minimax` | `provider.api_key` | `x-api-key: <key>` (streaming) | None in request path | HIGH | MISSING |

### 1.6 Google Vertex AI

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `provider_vertex.ml:160` | `Provider_vertex` | OAuth token (from gcloud) | `Authorization: Bearer <token>` | None in request path | HIGH | MISSING |
| `provider_vertex.ml:192` | `Provider_vertex` | OAuth token (streaming) | `Authorization: Bearer <token>` | None in request path | HIGH | MISSING |
| `provider_vertex.ml:~45` | `Provider_vertex` | `service_account_json` | Written to temp file, used via `gcloud` | File cleaned up after use | MEDIUM | PARTIAL |

### 1.7 OpenAI Codex OAuth

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `provider_openai_codex.ml:802` | `Provider_openai_codex` | `access_token` (from OAuth) | `Authorization: Bearer <token>` | None in request path | HIGH | MISSING |

---

## 2. Provider Quota Fetching

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `provider_quota.ml:576` | `Provider_quota` | Anthropic API key | `Authorization: Bearer <key>` | None | HIGH | MISSING |
| `provider_quota.ml:640` | `Provider_quota` | Codex access_token (from `~/.codex/auth.json`) | `Authorization: Bearer <token>` | None | HIGH | MISSING |
| `provider_quota.ml:692` | `Provider_quota` | Z.ai api_key | `Authorization: <key>` (no Bearer prefix) | None | HIGH | MISSING |
| `provider_quota.ml:881` | `Provider_quota` | Kimi api_key | `Authorization: Bearer <key>` | None | HIGH | MISSING |
| `provider_quota.ml:988` | `Provider_quota` | Cursor JWT (from state.vscdb) | `Cookie: WorkosCursorSessionToken=<token>` | None | HIGH | MISSING |

---

## 3. GitHub Integration

### 3.1 GitHub API Calls

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `github_api.ml:15` | `Github_api` | PAT token | `Authorization: Bearer <token>` | `redact_token` in debug logs | HIGH | EXISTING |
| `github_api.ml:28` | `Github_api` | PAT (via `auth_headers`) | `post_comment` | Via `auth_headers` | HIGH | EXISTING |
| `github_api.ml:43` | `Github_api` | PAT (via `auth_headers`) | `reply_to_review_comment` | Via `auth_headers` | HIGH | EXISTING |
| `github_api.ml:64` | `Github_api` | PAT (via `auth_headers`) | `add_reaction` | Via `auth_headers` | HIGH | EXISTING |
| `github_api.ml:79` | `Github_api` | PAT (via `auth_headers`) | `post_comment_returning_id` | Via `auth_headers` | HIGH | EXISTING |
| `github_api.ml:105` | `Github_api` | PAT (via `auth_headers`) | `edit_comment` | Via `auth_headers` | HIGH | EXISTING |
| `github_api.ml:122` | `Github_api` | PAT (via `auth_headers`) | `get_pr_files` | Via `auth_headers` | HIGH | EXISTING |

### 3.2 GitHub Webhook Verification

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `github_webhook.ml:46` | `Github_webhook` | `webhook_secret` | HMAC-SHA256 signature verification | Not logged | MEDIUM | EXISTING |

---

## 4. Discord Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `discord.ml:212` | `Discord` | `bot_token` | `Authorization: Bot <token>` (send_message) | None | HIGH | MISSING |
| `discord.ml:236` | `Discord` | `bot_token` | `Authorization: Bot <token>` (edit_message) | None | HIGH | MISSING |
| `discord.ml:252` | `Discord` | `bot_token` | `Authorization: Bot <token>` (delete_message) | None | HIGH | MISSING |
| `discord.ml:267` | `Discord` | `bot_token` | `Authorization: Bot <token>` (trigger_typing) | None | HIGH | MISSING |
| `discord.ml:301` | `Discord` | `bot_token` | `Authorization: Bot <token>` (add_reaction) | None | HIGH | MISSING |
| `discord.ml:317` | `Discord` | `bot_token` | `Authorization: Bot <token>` (delete_reaction) | None | HIGH | MISSING |
| `discord.ml:330` | `Discord` | `bot_token` | `Authorization: Bot <token>` (send_dm) | None | HIGH | MISSING |
| `discord_gateway.ml:184` | `Discord_gateway` | `bot_token` | `Authorization: Bot <token>` (gateway connect) | None | HIGH | MISSING |

---

## 5. Slack Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `slack.ml:116` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (send_message) | None | HIGH | MISSING |
| `slack.ml:124` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (send_message_with_id) | None | HIGH | MISSING |
| `slack.ml:141` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (send_message_reply) | None | HIGH | MISSING |
| `slack.ml:149` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (edit_message) | None | HIGH | MISSING |
| `slack.ml:165` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (delete_message) | None | HIGH | MISSING |
| `slack.ml:189` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (add_reaction) | None | HIGH | MISSING |
| `slack.ml:205` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (remove_reaction) | None | HIGH | MISSING |
| `daemon_util.ml:753` | `Daemon_util` | `slack_config.bot_token` | `Authorization: Bearer <token>` (deliver_room_progress) | None | HIGH | MISSING |
| `slack.ml:101` | `Slack` | `signing_secret` | HMAC-SHA256 request signature verification | Not logged | MEDIUM | EXISTING |

---

## 6. Telegram Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `telegram_api.ml:431` | `Telegram_api` | `bot_token` | URL path: `<base><token>/deleteWebhook` | `redact_token` in logs | HIGH | EXISTING |
| `telegram_api.ml:463` | `Telegram_api` | `bot_token` | URL path: `<base><token>/getUpdates` | `redact_token` in error logs | HIGH | EXISTING |
| `telegram_api.ml:654` | `Telegram_api` | `bot_token` | URL path: `<base><token>/getUpdates` (acknowledge) | None | HIGH | PARTIAL |
| `telegram_api.ml:711` | `Telegram_api` | `bot_token` | URL path: `<base><token>/sendChatAction` | None | HIGH | MISSING |
| Various `send_message` calls | `Telegram_api` | `bot_token` | URL path embedded in Telegram Bot API calls | None | HIGH | MISSING |

**Note:** Telegram embeds the bot token in the URL path (e.g., `https://api.telegram.org/bot<token>/sendMessage`), not in headers.

---

## 7. Matrix Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `matrix.ml:19` | `Matrix` | `access_token` | `Authorization: Bearer <token>` | None | HIGH | MISSING |
| `matrix.ml:37` | `Matrix` | `access_token` | `Authorization: Bearer <token>` (send_message) | None | HIGH | MISSING |
| `matrix.ml:77` | `Matrix` | `access_token` | `Authorization: Bearer <token>` (react) | None | HIGH | MISSING |
| `matrix.ml:89` | `Matrix` | `access_token` | `Authorization: Bearer <token>` (set_typing) | None | HIGH | MISSING |
| `matrix.ml:217` | `Matrix` | `access_token` | `Authorization: Bearer <token>` (sync loop) | None | HIGH | MISSING |

---

## 8. Mattermost Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `mattermost.ml:20` | `Mattermost` | `access_token` | `Authorization: Bearer <token>` (send_message) | None | HIGH | MISSING |
| `mattermost.ml:35` | `Mattermost` | `access_token` | `Authorization: Bearer <token>` (get_user_info) | None | HIGH | MISSING |
| `mattermost.ml:123` | `Mattermost` | `access_token` | WebSocket `authentication_challenge` token | None | HIGH | MISSING |

---

## 9. Microsoft Teams Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `teams_auth.ml:24-25` | `Teams_auth` | `app_id`, `app_secret` | OAuth2 client_credentials to Azure AD | None | HIGH | MISSING |
| `teams_auth.ml:~40` | `Teams_auth` | OAuth `access_token` (cached) | Used for outbound Bot Framework API calls | None | HIGH | MISSING |
| `teams_auth.ml:~145` | `Teams_auth` | Inbound JWT (from Authorization header) | Claims-only validation (no signature verify) | Not logged | MEDIUM | EXISTING |

**SECURITY NOTE:** `teams_auth.ml` has an explicit warning that JWT signature verification is NOT performed — only claims are checked.

---

## 10. Lark (Feishu) Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `lark.ml:61-62` | `Lark` | `app_id`, `app_secret` | POST body to `/auth/v3/tenant_access_token/internal` | None | HIGH | MISSING |
| `lark.ml:120` | `Lark` | Tenant access token (cached) | `Authorization: Bearer <token>` | None | HIGH | MISSING |
| `lark.ml:105` | `Lark` | `verification_token` | HMAC-SHA256 signature verification | Not logged | MEDIUM | EXISTING |

---

## 11. DingTalk Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `dingtalk.ml:23-24` | `Dingtalk` | `app_key`, `app_secret` | POST body to `/oauth2/accessToken` | None | HIGH | MISSING |
| `dingtalk.ml:69` | `Dingtalk` | Access token (cached) | `x-acs-dingtalk-access-token: <token>` | None | HIGH | MISSING |
| `dingtalk.ml:12-14` | `Dingtalk` | `app_secret` | HMAC-SHA256 signature computation | Not logged | MEDIUM | EXISTING |
| `dingtalk.ml:129-130` | `Dingtalk` | `app_key`, `app_secret` | WebSocket stream registration | None | HIGH | MISSING |

---

## 12. LINE Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `line_channel.ml:20` | `Line_channel` | `channel_access_token` | `Authorization: Bearer <token>` (reply) | None | HIGH | MISSING |
| `line_channel.ml:39` | `Line_channel` | `channel_access_token` | `Authorization: Bearer <token>` (push) | None | HIGH | MISSING |
| `line_channel.ml:11` | `Line_channel` | `channel_secret` | HMAC-SHA256 signature verification | Not logged | MEDIUM | EXISTING |

---

## 13. WhatsApp Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `whatsapp.ml:15` | `Whatsapp` | `access_token` | `Authorization: Bearer <token>` | None | HIGH | MISSING |
| `whatsapp.ml:136` | `Whatsapp` | `verify_token` | Webhook GET verification handshake | None | MEDIUM | MISSING |

---

## 14. Nostr Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `nostr.ml:31,63,96,126,151` | `Nostr` | `private_key` | Passed as `--sec` arg to `nak` CLI | None (CLI arg) | HIGH | MISSING |
| `nostr.ml:227` | `Nostr` | `private_key` | Decryption via `nak decrypt --sec` | None (CLI arg) | HIGH | MISSING |
| `nostr.ml:264` | `Nostr` | `private_key` | Relay auth via `nak event --auth --sec` | None (CLI arg) | HIGH | MISSING |

---

## 15. OneBot Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `onebot.ml:51` | `Onebot` | `access_token` (optional) | `Authorization: Bearer <token>` | None | HIGH | MISSING |
| `onebot.ml:77` | `Onebot` | `access_token` (optional) | `Authorization: Bearer <token>` | None | HIGH | MISSING |

---

## 16. Signal Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `signal.ml:~40` | `Signal` | None (local signal-cli API) | No auth headers | N/A | LOW | N/A |

**Note:** Signal uses a local signal-cli JSON-RPC or REST API with no authentication headers.

---

## 17. IRC Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `irc.ml:160` | `Irc` | `password` | `PASS <password>` command | None | HIGH | MISSING |
| `irc.ml:141,207` | `Irc` | `password` | SASL PLAIN auth (`\0nick\0password`) | None | HIGH | MISSING |

---

## 18. Email Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `email_channel.ml:268` | `Email_channel` | `password` | IMAP `LOGIN` command | None | HIGH | MISSING |
| `email_channel.ml:519` | `Email_channel` | `password` | SMTP `AUTH` (base64-encoded) | None | HIGH | MISSING |

---

## 19. Web Channel (UI/Chat)

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `ui_server.ml:61-64` | `Ui_server` | Pairing token (bearer) | `Authorization: Bearer <token>` extraction | Validated in-memory | MEDIUM | EXISTING |
| `ui_server.ml:82-88` | `Ui_server` | Generated session token | Returned to client after pairing | Stored in Hashtbl | MEDIUM | EXISTING |
| `chat_ui_assets.ml:~1148` | `Chat_ui_assets` | UI pairing token | `Authorization: Bearer <token>` in fetch calls | Stored in localStorage | MEDIUM | EXISTING |

---

## 20. Gateway Auth

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `command_bridge.ml:86` | `Command_bridge` | `gateway.auth_token` | `Authorization: Bearer <token>` | None | HIGH | MISSING |
| `command_bridge_gateway.ml:156-157` | `Command_bridge_gateway` | `auth_token` (from config or daemon_state) | `Authorization: Bearer <token>` | None | HIGH | MISSING |
| `http_server_0_util.ml:82-143` | `Http_server_0_util` | Bearer token / x-api-key | Inbound request validation | Validated via `Eqaf.equal` | MEDIUM | EXISTING |

---

## 21. Runner Relay Tokens

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `runner_relay.ml:14-30` | `Runner_relay` | Generated token (32 random bytes) | SHA256-hashed before storage | Token returned to caller once | MEDIUM | EXISTING |
| `runner_relay.ml:32-43` | `Runner_relay` | Token validation | Hash-and-lookup with TTL expiry | Hashed, not stored in plaintext | MEDIUM | EXISTING |

---

## 22. Web Search API

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `config_loader.ml:894-913` | `Config_loader` | `search_api_key` | Loaded from config for web search provider | None | HIGH | MISSING |
| `tools_builtin_net.ml:75-97` | `Tools_builtin_net` | User-supplied HTTP headers (`Authorization`, API keys, cookies, etc.) | Arbitrary headers passed through `http_request` to `Http_client.*` | None in request path | HIGH | MISSING |
| `tools_builtin_net.ml:365-416` | `Tools_builtin_net` | Brave `search_api_key` | `X-Subscription-Token: <key>` on Brave web search requests | None in request path | HIGH | MISSING |
| `tools_builtin_net.ml:937-960` | `Tools_builtin_net` | Brave `search_api_key` | `X-Subscription-Token: <key>` on Brave health-check probe | None in request path | HIGH | MISSING |

---

## 23. MCP and Z.ai MCP

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `config_loader.ml:923-956` | `Config_loader` | `zai_mcp.key` | Loaded from config for Z.ai MCP tools | None | HIGH | MISSING |
| `tools_builtin_zai.ml:38-55` | `Tools_builtin_zai` | `zai_mcp.key` / discovered Z.ai provider key | `Authorization: Bearer <key>` for Z.ai MCP `tools/call` requests | None in request path | HIGH | MISSING |
| `tools_builtin_zai.ml:151-154` | `Tools_builtin_zai` | `zai_mcp.key` / discovered Z.ai provider key | `Authorization: Bearer <key>` for Z.ai MCP initialize and discovery requests | None in request path | HIGH | MISSING |
| `mcp_client.ml:278-318` | `Mcp_client` | Configured HTTP MCP headers | Parsed from HTTP MCP server config into transport headers | None in transport setup | HIGH | MISSING |
| `mcp_client.ml:169` | `Mcp_client` | Configured HTTP MCP headers | `transport.headers` passed to HTTP MCP `transport.post` for JSON-RPC requests | None in request path | HIGH | MISSING |

---

## 24. Xiaomi MiMo Providers

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `xiaomi.ml:~155-180` | `Xiaomi` | API key (from env vars or `~/.mimo`) | Resolved and injected into provider config | None at resolution | HIGH | MISSING |

**Env vars:** `XIAOMI_API_KEY`, `XIAOMI_TOKEN_PLAN_CN_API_KEY`, `XIAOMI_TOKEN_PLAN_AMS_API_KEY`, `XIAOMI_TOKEN_PLAN_SGP_API_KEY`

---

## 25. Codex OAuth Config

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `config_loader.ml:104-108` | `Config_loader` | `access_token`, `refresh_token` | Loaded from config with `resolve_secret` | `resolve_secret` applied | LOW | EXISTING |

---

## 26. Audit Signing

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `audit.ml:135-146` | `Audit` | Signing key (derived from `CLAWQ_MASTER_KEY`) | HMAC-SHA256 for audit log integrity | Not logged | LOW | EXISTING |

---

## 27. Secret Store (Encryption at Rest)

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `secret_store.ml:~18` | `Secret_store` | `CLAWQ_MASTER_KEY` env var | PBKDF2 derivation for AES-256-GCM | Never logged | LOW | EXISTING |
| `secret_store.ml:~50` | `Secret_store` | Encrypted config values (`$ENC:` prefix) | Decrypted on config load | Decrypted values in memory | LOW | EXISTING |

---

## 28. Config Display Redaction

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `config_show.ml:6-9` | `Config_show` | All config keys containing `token`, `secret`, `password`, `api_key`, `private_key` | JSON display | Replaced with `***` | LOW | EXISTING |
| `config_show.ml:11` | `Config_show` | `tunnel_name` | JSON display | Replaced with `***` | LOW | EXISTING |

---

## 29. HTTP Debug Logging

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `http_debug.ml:75-76` | `Http_debug` | `authorization`, `x-api-key`, `api-key`, `cookie`, `set-cookie`, `proxy-authorization` | HAR file logging | `redact_token` applied | LOW | EXISTING |

---

## Summary by Risk Level

### HIGH Risk (credential sent over network, no redaction in request path)

- **LLM Providers** (OpenAI-compat, Anthropic, Gemini, Cohere, MiniMax, Vertex, Codex): ~20 callsites
- **Provider Quota**: 5 callsites
- **Discord**: 8 callsites
- **Slack**: 8 callsites
- **Telegram**: ~10 callsites (token in URL path)
- **Matrix**: 5 callsites
- **Mattermost**: 3 callsites
- **Teams**: 2 callsites (OAuth + outbound API)
- **Lark**: 2 callsites
- **DingTalk**: 3 callsites
- **LINE**: 2 callsites
- **WhatsApp**: 1 callsite
- **Nostr**: ~6 callsites (CLI args)
- **OneBot**: 2 callsites
- **IRC**: 2 callsites
- **Email**: 2 callsites
- **Gateway**: 3 callsites
- **Web Search/Z.ai**: 2 callsites
- **Xiaomi**: 1 callsite (env resolution)

### MEDIUM Risk (in-memory verification, not sent over network)

- **GitHub webhook verification**: 1 callsite
- **Slack signing secret**: 1 callsite
- **Lark verification token**: 1 callsite
- **LINE channel secret**: 1 callsite
- **Teams JWT claims**: 1 callsite
- **Web UI pairing**: 2 callsites
- **Runner relay tokens**: 2 callsites

### LOW Risk (storage/encryption only)

- **Secret store**: 2 callsites
- **Config redaction**: 2 callsites
- **HTTP debug redaction**: 1 callsite
- **Audit signing**: 1 callsite
- **Codex OAuth config load**: 1 callsite

---

## Enforceability Summary

| Class | Count | Description |
|-------|-------|-------------|
| EXISTING | ~25 | Redaction/logging already in place |
| PARTIAL | ~3 | Some code paths redacted, others not |
| MISSING | ~60 | No redaction in request/logging path |

---

## Key Observations

1. **Connector credentials are pervasive**: Discord, Slack, Telegram, Matrix, Mattermost, Teams, Lark, DingTalk, LINE, WhatsApp, Nostr, OneBot, IRC, and Email all pass credentials in HTTP headers or CLI arguments without redaction.

2. **LLM provider API keys lack redaction**: All provider implementations send API keys in headers without any logging redaction. The `Http_debug` module does redact these headers when HTTP debug logging is enabled, but normal log paths don't.

3. **Telegram embeds tokens in URLs**: Unlike other connectors that use headers, Telegram Bot API puts the token in the URL path, which may leak into access logs, referrer headers, or error messages.

4. **Nostr passes private keys as CLI arguments**: The `--sec` flag passes the private key to the `nak` CLI, which may be visible in process listings (`ps aux`).

5. **Teams JWT validation is claims-only**: The `teams_auth.ml` module explicitly documents that it does NOT verify JWT cryptographic signatures.

6. **Config redaction is comprehensive**: The `config_show.ml` module redacts all keys containing `token`, `secret`, `password`, `api_key`, or `private_key` substrings.

7. **HTTP debug logging is well-handled**: The `http_debug.ml` module redacts `authorization`, `x-api-key`, `api-key`, `cookie`, `set-cookie`, and `proxy-authorization` headers.

8. **Secret encryption at rest exists**: The `secret_store.ml` module provides AES-256-GCM encryption for config values using `CLAWQ_MASTER_KEY`, with the `$ENC:` prefix convention.

9. **Runner relay tokens are properly hashed**: Tokens are SHA256-hashed before storage and have TTL expiry.

10. **No centralized credential redaction middleware**: Each module handles (or doesn't handle) credential redaction independently. There is no shared middleware or interceptor that automatically redacts credentials from HTTP requests/responses before logging.
