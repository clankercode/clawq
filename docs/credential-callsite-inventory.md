# Credential-Bearing Callsite Inventory

**Task:** P18.M1.E1.T001 — Inventory credential-bearing callsites
**Date:** 2026-06-29
**Author:** Automated codebase analysis

This document inventories every location in the Clawq codebase where
credentials (API keys, tokens, secrets, passwords) are used in outbound
requests, inbound verification, or stored in memory.

## Classification Legend

- **Redaction behavior**: How the credential value is redacted when logged/displayed
- **Risk**: HIGH = credential sent over network; MEDIUM = credential verified in-memory; LOW = credential stored/loaded only
- **Enforceability**: EXISTING = redaction already works; PARTIAL = some paths redacted, some not; MISSING = no redaction

---

## 1. LLM Provider API Keys

### 1.1 OpenAI-Compatible Providers (Generic Path)

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
| `provider_vertex.ml:~45` | `Provider_vertex` | `service_account_json` | Written to temp file, used via `gcloud auth activate-service-account` | File cleaned up after use | MEDIUM | PARTIAL |

### 1.7 OpenAI Codex OAuth

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `provider_openai_codex.ml:802` | `Provider_openai_codex` | `access_token` (from OAuth) | `Authorization: Bearer <token>` | None in request path | HIGH | MISSING |

### 1.8 Provider Validation

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `config_validate.ml:8` | `Config_validate` | `api_key` | `Authorization: Bearer <key>` (test connection) | None | HIGH | MISSING |

### 1.9 STT/TTS Providers

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `tts.ml:45` | `Tts` | `provider.api_key` | `Authorization: Bearer <key>` | None | HIGH | MISSING |
| `stt.ml:31` | `Stt` | `provider.api_key` | `Authorization: Bearer <key>` | None | HIGH | MISSING |

### 1.10 Vector/Embedding Provider

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `vector.ml:92` | `Vector` | `provider.api_key` | `Authorization: Bearer <key>` | None | HIGH | MISSING |

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

## 3. Codex OAuth Token Exchange/Refresh

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `openai_codex_oauth.ml:497` | `Openai_codex_oauth` | `code` + `code_verifier` | POST form to `auth.openai.com/oauth/token` | None | HIGH | MISSING |
| `openai_codex_oauth.ml:519` | `Openai_codex_oauth` | `refresh_token` | POST form to `auth.openai.com/oauth/token` | None | HIGH | MISSING |
| `openai_codex_oauth.ml:9` | `Openai_codex_oauth` | `client_id` (hardcoded) | Sent in OAuth requests | Not sensitive (public client) | LOW | N/A |

---

## 4. Z.ai MCP Tools

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `tools_builtin_zai.ml:54` | `Tools_builtin_zai` | `api_key` | `Authorization: Bearer <key>` (MCP call) | None | HIGH | MISSING |
| `tools_builtin_zai.ml:153` | `Tools_builtin_zai` | `api_key` | `Authorization: Bearer <key>` (tool discovery) | None | HIGH | MISSING |
| `tools_builtin_zai.ml:115` | `Tools_builtin_zai` | Resolved from `zai_mcp.key` or provider keys | Key resolution | None at resolution | HIGH | MISSING |

---

## 5. MCP Client

### 5.1 HTTP Transport Headers

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `mcp_client.ml:278-284` | `Mcp_client` | Arbitrary headers from config `headers` field | Loaded from JSON config, passed to HTTP transport | None | HIGH | MISSING |
| `mcp_client.ml:111-120` | `Mcp_client` | Transport headers | Sent via `Cohttp_lwt_unix.Client.post` | None | HIGH | MISSING |
| `mcp_client.ml:169` | `Mcp_client` | Transport headers | Sent on every MCP message | None | HIGH | MISSING |

**Note:** MCP client allows arbitrary `headers` in config (e.g., `Authorization`, API keys). These are sent on every HTTP request without redaction.

### 5.2 Stdio Transport Environment

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `mcp_client.ml:291-297` | `Mcp_client` | `env` from config | Parsed as key-value pairs from JSON | None | HIGH | MISSING |
| `mcp_client.ml:321-326` | `Mcp_client` | `env` + parent environment | Appended to `Unix.environment()`, passed to `Lwt_process.open_process_full` | None | HIGH | MISSING |

**Note:** For stdio MCP servers, configured `env` entries (which may contain API keys) are merged with the full parent environment and forwarded to the child process.

---

## 6. GitHub Integration

### 6.1 GitHub API Calls

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `github_api.ml:10-18` | `Github_api` | PAT token | `Authorization: Bearer <token>` via `auth_headers` | `redact_token` in debug log | HIGH | EXISTING |
| `github_api.ml:28` | `Github_api` | PAT (via `auth_headers`) | `post_comment` | Via `auth_headers` | HIGH | EXISTING |
| `github_api.ml:43` | `Github_api` | PAT (via `auth_headers`) | `reply_to_review_comment` | Via `auth_headers` | HIGH | EXISTING |
| `github_api.ml:64` | `Github_api` | PAT (via `auth_headers`) | `add_reaction` | Via `auth_headers` | HIGH | EXISTING |
| `github_api.ml:79` | `Github_api` | PAT (via `auth_headers`) | `post_comment_returning_id` | Via `auth_headers` | HIGH | EXISTING |
| `github_api.ml:105` | `Github_api` | PAT (via `auth_headers`) | `edit_comment` | Via `auth_headers` | HIGH | EXISTING |
| `github_api.ml:122` | `Github_api` | PAT (via `auth_headers`) | `get_pr_files` (paginated) | Via `auth_headers` | HIGH | EXISTING |

### 6.2 GitHub Webhook Verification

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `github_webhook.ml:46` | `Github_webhook` | `webhook_secret` | HMAC-SHA256 signature verification | Not logged | MEDIUM | EXISTING |

---

## 7. Discord Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `discord.ml:212` | `Discord` | `bot_token` | `Authorization: Bot <token>` (send_message_with_id) | None | HIGH | MISSING |
| `discord.ml:236` | `Discord` | `bot_token` | `Authorization: Bot <token>` (edit_message) | None | HIGH | MISSING |
| `discord.ml:252` | `Discord` | `bot_token` | `Authorization: Bot <token>` (delete_message) | None | HIGH | MISSING |
| `discord.ml:267` | `Discord` | `bot_token` | `Authorization: Bot <token>` (trigger_typing) | None | HIGH | MISSING |
| `discord.ml:301` | `Discord` | `bot_token` | `Authorization: Bot <token>` (add_reaction) | None | HIGH | MISSING |
| `discord.ml:317` | `Discord` | `bot_token` | `Authorization: Bot <token>` (delete_own_reaction) | None | HIGH | MISSING |
| `discord.ml:330` | `Discord` | `bot_token` | `Authorization: Bot <token>` (send_dm) | None | HIGH | MISSING |
| `discord_gateway.ml:35` | `Discord_gateway` | `bot_token` | JSON `token` field in Identify payload | None | HIGH | MISSING |
| `discord_gateway.ml:60` | `Discord_gateway` | `bot_token` | JSON `token` field in Resume payload | None | HIGH | MISSING |
| `discord_gateway.ml:184` | `Discord_gateway` | `bot_token` | `Authorization: Bot <token>` (gateway fetch URL) | None | HIGH | MISSING |

---

## 8. Slack Integration

### 8.1 Slack REST API

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `slack.ml:116` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (send_message) | None | HIGH | MISSING |
| `slack.ml:124` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (send_message_with_id) | None | HIGH | MISSING |
| `slack.ml:141` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (send_message_reply) | None | HIGH | MISSING |
| `slack.ml:149` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (edit_message) | None | HIGH | MISSING |
| `slack.ml:165` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (delete_message) | None | HIGH | MISSING |
| `slack.ml:189` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (add_reaction) | None | HIGH | MISSING |
| `slack.ml:205` | `Slack` | `bot_token` | `Authorization: Bearer <token>` (remove_reaction) | None | HIGH | MISSING |
| `slack.ml:~953` | `Slack` | `bot_token` | Attachment/audio file fetch | None | HIGH | MISSING |

### 8.2 Slack Socket Mode

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `slack_socket.ml:4` | `Slack_socket` | `app_token` | `Authorization: Bearer <token>` (get WSS URL) | None | HIGH | MISSING |
| `slack_socket.ml:60` | `Slack_socket` | `app_token` | WebSocket connection | None | HIGH | MISSING |

### 8.3 Slack Webhook Verification

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `slack.ml:101` | `Slack` | `signing_secret` | HMAC-SHA256 request signature verification | Not logged | MEDIUM | EXISTING |

### 8.4 Slack Room Progress

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `daemon_util.ml:753` | `Daemon_util` | `slack_config.bot_token` | `Authorization: Bearer <token>` (deliver_room_progress) | None | HIGH | MISSING |

---

## 9. Telegram Integration

**Note:** Telegram embeds the bot token in the URL path (e.g., `https://api.telegram.org/bot<token>/sendMessage`), not in headers. This may leak into access logs, referrer headers, or error messages.

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `telegram_api.ml:431` | `Telegram_api` | `bot_token` | URL path: `<base><token>/deleteWebhook` | `redact_token` in logs | HIGH | EXISTING |
| `telegram_api.ml:463` | `Telegram_api` | `bot_token` | URL path: `<base><token>/getUpdates` | `redact_token` in error logs | HIGH | EXISTING |
| `telegram_api.ml:654` | `Telegram_api` | `bot_token` | URL path: `<base><token>/getUpdates` (acknowledge) | None | HIGH | PARTIAL |
| `telegram_api.ml:711` | `Telegram_api` | `bot_token` | URL path: `<base><token>/sendChatAction` | None | HIGH | MISSING |
| `telegram_api.ml:send_message` | `Telegram_api` | `bot_token` | URL path: `<base><token>/sendMessage` | None | HIGH | MISSING |
| `telegram_api.ml:send_document` | `Telegram_api` | `bot_token` | URL path: `<base><token>/sendDocument` | None | HIGH | MISSING |
| `telegram_api.ml:edit_message` | `Telegram_api` | `bot_token` | URL path: `<base><token>/editMessageText` | None | HIGH | MISSING |
| `telegram_api.ml:delete_message` | `Telegram_api` | `bot_token` | URL path: `<base><token>/deleteMessage` | None | HIGH | MISSING |
| `telegram_api.ml:answer_callback` | `Telegram_api` | `bot_token` | URL path: `<base><token>/answerCallbackQuery` | None | HIGH | MISSING |
| `telegram_api.ml:set_reaction` | `Telegram_api` | `bot_token` | URL path: `<base><token>/setMessageReaction` | None | HIGH | MISSING |
| `telegram_api.ml:send_poll` | `Telegram_api` | `bot_token` | URL path: `<base><token>/sendPoll` | None | HIGH | MISSING |
| `telegram_api.ml:get_file` | `Telegram_api` | `bot_token` | URL path: `<base><token>/getFile` | None | HIGH | MISSING |
| `telegram_api.ml:download_file` | `Telegram_api` | `bot_token` | URL path: `<base>/file/bot<token>/<path>` | None | HIGH | MISSING |
| `telegram_api.ml:1320` | `Telegram_api` | `bot_token` | URL path: `<base><token>/setMyCommands` | `redact_token` in error log | HIGH | EXISTING |

**Mitigation:** `daemon.ml:155` has `scrub_telegram_tokens` that scrubs Telegram tokens from stderr output.

---

## 10. Matrix Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `matrix.ml:19` | `Matrix` | `access_token` | `Authorization: Bearer <token>` | None | HIGH | MISSING |
| `matrix.ml:37` | `Matrix` | `access_token` | `Authorization: Bearer <token>` (send_message) | None | HIGH | MISSING |
| `matrix.ml:77` | `Matrix` | `access_token` | `Authorization: Bearer <token>` (react) | None | HIGH | MISSING |
| `matrix.ml:89` | `Matrix` | `access_token` | `Authorization: Bearer <token>` (set_typing) | None | HIGH | MISSING |
| `matrix.ml:217` | `Matrix` | `access_token` | `Authorization: Bearer <token>` (sync loop) | None | HIGH | MISSING |

---

## 11. Mattermost Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `mattermost.ml:20` | `Mattermost` | `access_token` | `Authorization: Bearer <token>` (send_message) | None | HIGH | MISSING |
| `mattermost.ml:35` | `Mattermost` | `access_token` | `Authorization: Bearer <token>` (get_user_info) | None | HIGH | MISSING |
| `mattermost.ml:123` | `Mattermost` | `access_token` | WebSocket `authentication_challenge` token in JSON body | None | HIGH | MISSING |

---

## 12. Microsoft Teams Integration

### 12.1 OAuth Token Fetch

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `teams_auth.ml:24-25` | `Teams_auth` | `app_id`, `app_secret` | OAuth2 client_credentials POST body to Azure AD | None | HIGH | MISSING |

### 12.2 Outbound Bot Framework API Calls

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `teams.ml:231` | `Teams` | OAuth `access_token` (cached) | `Authorization: Bearer <token>` (send_message) | None | HIGH | MISSING |
| `teams.ml:401` | `Teams` | OAuth `access_token` | `Authorization: Bearer <token>` (send_attachment) | None | HIGH | MISSING |
| `teams.ml:458` | `Teams` | OAuth `access_token` | `Authorization: Bearer <token>` (update_message) | None | HIGH | MISSING |
| `teams.ml:492` | `Teams` | OAuth `access_token` | `Authorization: Bearer <token>` (send_conversation) | None | HIGH | MISSING |
| `teams.ml:510` | `Teams` | OAuth `access_token` | `Authorization: Bearer <token>` (delete_message) | None | HIGH | MISSING |
| `teams.ml:567` | `Teams` | OAuth `access_token` | `Authorization: Bearer <token>` (upload_file) | None | HIGH | MISSING |
| `teams.ml:614` | `Teams` | OAuth `access_token` | `Authorization: Bearer <token>` (send_file_consent) | None | HIGH | MISSING |
| `teams.ml:1259` | `Teams` | OAuth `access_token` | `Authorization: Bearer <token>` (card actions) | None | HIGH | MISSING |
| `teams.ml:1343` | `Teams` | OAuth `access_token` | `Authorization: Bearer <token>` (file consent) | None | HIGH | MISSING |
| `teams_file_consent.ml:217` | `Teams_file_consent` | OAuth `access_token` | `Authorization: Bearer <token>` (upload to OneDrive) | None | HIGH | MISSING |
| `teams_file_consent.ml:246` | `Teams_file_consent` | OAuth `access_token` | `Authorization: Bearer <token>` (download from OneDrive) | None | HIGH | MISSING |

### 12.3 Inbound JWT Verification

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `teams_auth.ml:~145` | `Teams_auth` | Inbound JWT (from Authorization header) | Claims-only validation (no signature verify) | Not logged | MEDIUM | EXISTING |

**SECURITY NOTE:** `teams_auth.ml` has an explicit warning that JWT signature verification is NOT performed — only aud/iss/exp/nbf claims are checked.

---

## 13. Lark (Feishu) Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `lark.ml:61-62` | `Lark` | `app_id`, `app_secret` | POST body to `/auth/v3/tenant_access_token/internal` | None | HIGH | MISSING |
| `lark.ml:120` | `Lark` | Tenant access token (cached) | `Authorization: Bearer <token>` | None | HIGH | MISSING |
| `lark.ml:105` | `Lark` | `verification_token` | HMAC-SHA256 signature verification | Not logged | MEDIUM | EXISTING |

---

## 14. DingTalk Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `dingtalk.ml:23-24` | `Dingtalk` | `app_key`, `app_secret` | POST body to `/oauth2/accessToken` | None | HIGH | MISSING |
| `dingtalk.ml:69` | `Dingtalk` | Access token (cached) | `x-acs-dingtalk-access-token: <token>` | None | HIGH | MISSING |
| `dingtalk.ml:12-14` | `Dingtalk` | `app_secret` | HMAC-SHA256 signature computation | Not logged | MEDIUM | EXISTING |
| `dingtalk.ml:129-130` | `Dingtalk` | `app_key`, `app_secret` | WebSocket stream registration body | None | HIGH | MISSING |
| `dingtalk.ml:58` | `Dingtalk` | `webhook_url` (optional) | POST to webhook URL (contains secret path) | None | HIGH | MISSING |

---

## 15. LINE Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `line_channel.ml:20` | `Line_channel` | `channel_access_token` | `Authorization: Bearer <token>` (reply) | None | HIGH | MISSING |
| `line_channel.ml:39` | `Line_channel` | `channel_access_token` | `Authorization: Bearer <token>` (push message) | None | HIGH | MISSING |
| `line_channel.ml:11` | `Line_channel` | `channel_secret` | HMAC-SHA256 signature verification | Not logged | MEDIUM | EXISTING |

---

## 16. WhatsApp Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `whatsapp.ml:15` | `Whatsapp` | `access_token` | `Authorization: Bearer <token>` | None | HIGH | MISSING |
| `whatsapp.ml:136` | `Whatsapp` | `verify_token` | Webhook GET verification handshake (compared against query param) | None | MEDIUM | MISSING |

---

## 17. Nostr Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `nostr.ml:31` | `Nostr` | `private_key` | Passed as `--sec` arg to `nak` CLI (send NIP-17) | None (CLI arg, visible in `ps`) | HIGH | MISSING |
| `nostr.ml:63` | `Nostr` | `private_key` | Passed as `--sec` arg to `nak` CLI (send NIP-04) | None (CLI arg) | HIGH | MISSING |
| `nostr.ml:96` | `Nostr` | `private_key` | Passed as `--sec` arg to `nak encrypt` | None (CLI arg) | HIGH | MISSING |
| `nostr.ml:126` | `Nostr` | `private_key` | Passed as `--sec` arg to `nak` CLI (NIP-04 DM) | None (CLI arg) | HIGH | MISSING |
| `nostr.ml:151` | `Nostr` | `private_key` | Passed as `--sec` arg to `nak event --auth` | None (CLI arg) | HIGH | MISSING |
| `nostr.ml:227` | `Nostr` | `private_key` | Passed as `--sec` arg to `nak decrypt` | None (CLI arg) | HIGH | MISSING |
| `nostr.ml:264` | `Nostr` | `private_key` | Passed as `--sec` arg for relay auth | None (CLI arg) | HIGH | MISSING |

**SECURITY NOTE:** Private keys passed as CLI arguments are visible in process listings (`ps aux`).

---

## 18. OneBot Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `onebot.ml:51` | `Onebot` | `access_token` (optional) | `Authorization: Bearer <token>` (HTTP POST) | None | HIGH | MISSING |
| `onebot.ml:77` | `Onebot` | `access_token` (optional) | `Authorization: Bearer <token>` (HTTP POST) | None | HIGH | MISSING |
| `onebot.ml:181` | `Onebot` | `access_token` (optional) | JSON `access_token` in `meta::connect` WebSocket message | None | HIGH | MISSING |

---

## 19. Signal Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `signal.ml:~40` | `Signal` | None (local signal-cli API) | No auth headers — local API only | N/A | LOW | N/A |

**Note:** Signal uses a local signal-cli JSON-RPC or REST API with no authentication headers.

---

## 20. IRC Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `irc.ml:160` | `Irc` | `password` | `PASS <password>` command (non-SASL) | None | HIGH | MISSING |
| `irc.ml:141,207` | `Irc` | `password` | SASL PLAIN auth (`\0nick\0password` base64-encoded) | None | HIGH | MISSING |

---

## 21. Email Integration

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `email_channel.ml:268` | `Email_channel` | `password` | IMAP `LOGIN` command | None | HIGH | MISSING |
| `email_channel.ml:519` | `Email_channel` | `password` | SMTP `AUTH` (base64-encoded) | None | HIGH | MISSING |

---

## 22. Web Channel (UI/Chat)

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `web_channel.ml:61-64` | `Web_channel` | Pairing token (bearer) | `Authorization: Bearer <token>` extraction | Validated in-memory | MEDIUM | EXISTING |
| `web_channel.ml:82-88` | `Web_channel` | Generated session token | Returned to client after pairing | Stored in Hashtbl | MEDIUM | EXISTING |
| `web_channel.ml:112-116` | `Web_channel` | Session token | Validated on each request | TTL-based expiry | MEDIUM | EXISTING |
| `chat_ui_assets.ml:~1148` | `Chat_ui_assets` | UI pairing token | `Authorization: Bearer <token>` in JS fetch calls | Stored in localStorage | MEDIUM | EXISTING |

---

## 23. Gateway Auth

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `command_bridge.ml:86` | `Command_bridge` | `gateway.auth_token` | `Authorization: Bearer <token>` (runner token request) | None | HIGH | MISSING |
| `command_bridge_gateway.ml:156-157` | `Command_bridge_gateway` | `auth_token` (from config or daemon_state.json) | `Authorization: Bearer <token>` | None | HIGH | MISSING |
| `http_server_0_util.ml:82-143` | `Http_server_0_util` | Bearer token / x-api-key | Inbound request validation | Validated via `Eqaf.equal` (constant-time) | MEDIUM | EXISTING |

---

## 24. Runner Relay Tokens

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `runner_relay.ml:14-30` | `Runner_relay` | Generated token (32 random bytes) | SHA256-hashed before storage | Token returned to caller once, hash stored | MEDIUM | EXISTING |
| `runner_relay.ml:32-43` | `Runner_relay` | Token validation | Hash-and-lookup with TTL expiry | Hashed, not stored in plaintext | MEDIUM | EXISTING |
| `daemon.ml:1681-1685` | `Daemon` | `CLAWQ_RUNNER_TOKEN` | Injected into runner process environment | Environment variable visible to runner process | HIGH | MISSING |
| `daemon.ml:1682-1683` | `Daemon` | `CLAWQ_MCP_URL`, `CLAWQ_RUNNER_ASK_URL` | Injected into runner process env (localhost URLs with port) | Not sensitive (localhost) | LOW | N/A |

---

## 25. Shell/Git Process Execution

### 25.1 Core Process Spawner

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `process_group.ml:60-63` | `Process_group` | `env` array | Passed to `Unix.execve` | No filtering -- full env forwarded to child | MEDIUM | MISSING |

### 25.2 Git Operations

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `tools_builtin_net.ml:824-830` | `Tools_builtin_net` | `workspace_only_env()` + `GIT_TERMINAL_PROMPT=0` | Passed to `Process_group.start` for git commands | No credential filtering | MEDIUM | MISSING |
| `repo_manager.ml:134-135` | `Repo_manager` | `Unix.environment()` | Full parent env passed to git commands | No filtering | MEDIUM | MISSING |

### 25.3 Shell Commands

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `tools_builtin_proc.ml:524-533` | `Tools_builtin_proc` | `~env` parameter | Passed to `Process_group.start` for shell/exec commands | Inherits full parent environment | MEDIUM | MISSING |
| `slash_commands_bash.ml:15-16` | `Slash_commands_bash` | `Unix.environment()` | Full parent env passed to `/bin/bash -c` | No filtering | MEDIUM | MISSING |

### 25.4 Background Task Spawning

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `background_task_spawn.ml:288` | `Background_task_spawn` | `Unix.environment()` | Full parent env passed to runner process | No filtering | MEDIUM | MISSING |
| `background_task_spawn.ml:319` | `Background_task_spawn` | `Unix.environment()` | Full parent env passed to runner process | No filtering | MEDIUM | MISSING |
| `background_task_spawn.ml:499-511` | `Background_task_spawn` | `augment_env` + `Unix.environment()` | Env augmented with runner-specific vars, passed to `Process_group.start_to_file` | No filtering | MEDIUM | MISSING |

**Note:** `Process_group.start` receives an `env` array that is forwarded directly to `Unix.execve`. If the parent process has credentials in environment variables (e.g., `ANTHROPIC_API_KEY`, `OPENAI_API_KEY`), child processes inherit them.

---

## 26. HTTP Client Layer & Tool HTTP Requests

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `http_client.ml` (all calls) | `Http_client` | Any headers passed by callers | Forwarded to `Cohttp_lwt_unix.Client` | No redaction at HTTP client level | HIGH | MISSING |
| `tools_builtin_net.ml:92-97` | `Tools_builtin_net` | Arbitrary user-supplied `headers` | Forwarded to `Http_client` (GET/POST/PUT/PATCH/DELETE) | None | HIGH | MISSING |

**Note:** `Http_client` is a thin wrapper around Cohttp. It does not perform any credential redaction. The `http_request` tool allows agents to pass arbitrary headers (including `Authorization`, API keys) which are forwarded without redaction.

---

## 27. HTTP Debug Logging

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `http_debug.ml:70-81` | `Http_debug` | `authorization`, `x-api-key`, `api-key`, `cookie`, `set-cookie`, `proxy-authorization` | HAR file logging | `redact_token` applied | LOW | EXISTING |

**Gaps in HTTP debug redaction:**
- `x-goog-api-key` (Gemini) — NOT redacted
- `x-acs-dingtalk-access-token` (DingTalk) — NOT redacted
- `x-subscription-token` (Brave Search) — NOT redacted
- Credentials embedded in URLs (Telegram bot tokens) — NOT redacted at HTTP layer (mitigated by `daemon.ml:scrub_telegram_tokens` for stderr)
- Secrets in request bodies (Lark/DingTalk/Teams OAuth) — NOT redacted

---

## 28. Web Search API

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `tools_builtin_net.ml:415` | `Tools_builtin_net` | `search_api_key` | `X-Subscription-Token: <key>` (Brave Search) | None | HIGH | MISSING |
| `tools_builtin_net.ml:937` | `Tools_builtin_net` | `search_api_key` | Same key resolution | None | HIGH | MISSING |

---

## 29. Xiaomi MiMo Providers

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `xiaomi.ml:~155-180` | `Xiaomi` | API key (from env vars or `~/.mimo`) | Resolved and injected into provider config | None at resolution | HIGH | MISSING |

**Env vars:** `XIAOMI_API_KEY`, `XIAOMI_TOKEN_PLAN_CN_API_KEY`, `XIAOMI_TOKEN_PLAN_AMS_API_KEY`, `XIAOMI_TOKEN_PLAN_SGP_API_KEY`
**File:** `~/.mimo` (raw key or JSON with `api_key`/`token`/`key` field)

---

## 30. Codex OAuth Config Storage

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `config_loader.ml:115-118` | `Config_loader` | `access_token`, `refresh_token` | Loaded from config with `resolve_secret` | `resolve_secret` applied (may decrypt `$ENC:` values) | LOW | EXISTING |

---

## 31. Audit Signing

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `audit.ml:135-146` | `Audit` | Signing key (derived from `CLAWQ_MASTER_KEY` via PBKDF2) | HMAC-SHA256 for audit log integrity | Not logged | LOW | EXISTING |

---

## 32. Secret Store (Encryption at Rest)

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `secret_store.ml:~18` | `Secret_store` | `CLAWQ_MASTER_KEY` env var | PBKDF2 derivation for AES-256-GCM | Never logged | LOW | EXISTING |
| `secret_store.ml:~50` | `Secret_store` | Encrypted config values (`$ENC:` prefix) | Decrypted on config load | Decrypted values held in memory | LOW | EXISTING |

---

## 33. Config Display Redaction

| Callsite | Owner Module | Credential | Header/Usage | Redaction | Risk | Enforceability |
|----------|--------------|------------|--------------|-----------|------|----------------|
| `config_show.ml:6-9` | `Config_show` | All config keys containing `token`, `secret`, `password`, `api_key`, `private_key` | JSON display to user | Replaced with `***` | LOW | EXISTING |
| `config_show.ml:11` | `Config_show` | `tunnel_name` | JSON display | Replaced with `***` | LOW | EXISTING |

---

## Summary by Risk Level

### HIGH Risk (credential sent over network, no redaction in request path)

- **LLM Providers** (OpenAI-compat, Anthropic, Gemini, Cohere, MiniMax, Vertex, Codex): ~20 callsites
- **Provider Quota**: 5 callsites
- **Codex OAuth exchange/refresh**: 2 callsites
- **Z.ai MCP tools**: 2 callsites
- **MCP client headers**: 3 callsites
- **Discord**: 8 callsites
- **Slack** (REST + Socket Mode): 10 callsites
- **Telegram**: ~13 callsites (token in URL path)
- **Matrix**: 5 callsites
- **Mattermost**: 3 callsites
- **Teams** (OAuth + outbound): 12 callsites
- **Lark**: 2 callsites
- **DingTalk**: 4 callsites
- **LINE**: 2 callsites
- **WhatsApp**: 1 callsite
- **Nostr**: 7 callsites (CLI args)
- **OneBot**: 2 callsites
- **IRC**: 2 callsites
- **Email**: 2 callsites
- **Gateway**: 3 callsites
- **Runner env injection**: 1 callsite
- **Web Search**: 2 callsites
- **Xiaomi**: 1 callsite (env resolution)
- **STT/TTS/Vector**: 3 callsites

### MEDIUM Risk (in-memory verification, env inheritance, not sent over network)

- **GitHub webhook verification**: 1 callsite
- **Slack signing secret**: 1 callsite
- **Lark verification token**: 1 callsite
- **LINE channel secret**: 1 callsite
- **Teams JWT claims**: 1 callsite
- **Web UI pairing**: 3 callsites
- **Runner relay tokens**: 2 callsites
- **Process env inheritance**: 3 callsites

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
| EXISTING | ~30 | Redaction/logging already in place |
| PARTIAL | ~3 | Some code paths redacted, others not |
| MISSING | ~80 | No redaction in request/logging path |

---

## Key Observations

1. **Connector credentials are pervasive**: Discord, Slack, Telegram, Matrix, Mattermost, Teams, Lark, DingTalk, LINE, WhatsApp, Nostr, OneBot, IRC, and Email all pass credentials in HTTP headers, URL paths, or CLI arguments without redaction.

2. **LLM provider API keys lack redaction**: All provider implementations send API keys in headers without any logging redaction. The `Http_debug` module redacts `authorization`/`x-api-key`/`api-key` headers when HTTP debug logging is enabled, but misses `x-goog-api-key` and `x-acs-dingtalk-access-token`.

3. **Telegram embeds tokens in URLs**: Unlike other connectors that use headers, Telegram Bot API puts the token in the URL path, which may leak into access logs, referrer headers, or error messages. `daemon.ml:scrub_telegram_tokens` mitigates stderr leakage.

4. **Nostr passes private keys as CLI arguments**: The `--sec` flag passes the private key to the `nak` CLI, which is visible in process listings (`ps aux`).

5. **Teams JWT validation is claims-only**: The `teams_auth.ml` module explicitly documents that it does NOT verify JWT cryptographic signatures.

6. **MCP client allows arbitrary credential headers**: `mcp_client.ml` loads `headers` from config and sends them on every HTTP request without redaction.

7. **Process environment inheritance leaks credentials**: `Process_group.start` forwards the full parent environment to child processes via `Unix.execve`. Shell and git tool executions inherit all parent env vars, including any API keys.

8. **Config redaction is comprehensive**: The `config_show.ml` module redacts all keys containing `token`, `secret`, `password`, `api_key`, or `private_key` substrings.

9. **HTTP debug logging has gaps**: `http_debug.ml` redacts `authorization`, `x-api-key`, `api-key`, `cookie`, `set-cookie`, and `proxy-authorization`, but misses provider-specific headers like `x-goog-api-key` and `x-acs-dingtalk-access-token`, and does not redact credentials in URLs or request bodies.

10. **No centralized credential redaction middleware**: Each module handles (or doesn't handle) credential redaction independently. There is no shared middleware or interceptor that automatically redacts credentials from HTTP requests/responses before logging.
