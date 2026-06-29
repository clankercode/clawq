# Outbound Network Callsite Inventory

**Task:** P18.M2.E1.T003 — Inventory outbound network callsites
**Date:** 2026-06-29
**Prerequisites:** P18.M2.E1.T001 (egress rules), T002 (egress evaluator), T001 (credential inventory)

This document inventories every outbound network callsite in the Clawq
codebase, including transport mechanism, destination, credential usage,
redaction status, and enforceability classification for egress policy.

---

## Classification Legend

- **Transport**: HTTP (Http_client), HTTP-direct (Cohttp_lwt_unix.Client), WebSocket (Ws_client), TCP/TLS (raw socket), Subprocess (Lwt_process)
- **Credential**: What credential is used (if any)
- **Credential Exposure**: How the credential may be exposed — NONE (no credential), HEADER-REDACTED (Http_debug redacts this header in debug logs), HEADER-UNREDACTED (credential in header, not redacted), URL-PATH (token embedded in URL, may leak to access logs/referrers), CLI-ARG (visible in process listings via `ps aux`)
- **Enforceability**: EXISTING = egress evaluator can enforce (host is known/static); DYNAMIC = host is user/config-provided (evaluator can enforce if host is supplied at eval time); LOCAL = loopback/local only (egress policy not applicable)

---

## 1. LLM Provider API Calls

All LLM providers use `Http_client.post_json*` or `Http_client.post_stream_with`.

### 1.1 OpenAI-Compatible Providers

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `provider.ml:190` | `Provider` | `{base_url}/chat/completions` | `Authorization: Bearer {api_key}` | NONE | DYNAMIC |
| `provider.ml:329` | `Provider` | `{base_url}/chat/completions` (streaming) | `Authorization: Bearer {api_key}` | NONE | DYNAMIC |

`base_url` is user-configured per provider (e.g. `https://api.openai.com/v1`).

### 1.2 Anthropic Provider

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `provider_anthropic.ml:205` | `Provider_anthropic` | `{base_url}/messages` | `x-api-key: {api_key}` + `anthropic-version` header | NONE | DYNAMIC |
| `provider_anthropic.ml:521` | `Provider_anthropic` | `{base_url}/messages` (streaming) | `x-api-key: {api_key}` | NONE | DYNAMIC |

### 1.3 Gemini Provider

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `provider_gemini.ml:255` | `Provider_gemini` | `{base_url}/models/{model}:generateContent` | `x-goog-api-key: {api_key}` | NONE | DYNAMIC |
| `provider_gemini.ml:280` | `Provider_gemini` | `{base_url}/models/{model}:streamGenerateContent` | `x-goog-api-key: {api_key}` | NONE | DYNAMIC |

### 1.4 Cohere Provider

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `provider_cohere.ml:149` | `Provider_cohere` | `{base_url}/chat` | `Authorization: Bearer {api_key}` | NONE | DYNAMIC |
| `provider_cohere.ml:186` | `Provider_cohere` | `{base_url}/chat` (streaming) | `Authorization: Bearer {api_key}` | NONE | DYNAMIC |

### 1.5 MiniMax Provider

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `provider_minimax.ml:247` | `Provider_minimax` | `{base_url}/chat/completions` | `x-api-key: {api_key}` | NONE | DYNAMIC |
| `provider_minimax.ml:580` | `Provider_minimax` | `{base_url}/chat/completions` (streaming) | `x-api-key: {api_key}` | NONE | DYNAMIC |

### 1.6 Ollama Provider

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `provider_ollama.ml:138` | `Provider_ollama` | `{base_url}/api/chat` | None (local Ollama) | N/A | DYNAMIC |
| `provider_ollama.ml:227` | `Provider_ollama` | `{base_url}/api/chat` (streaming) | None | N/A | DYNAMIC |

### 1.7 Google Vertex AI Provider

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `provider_vertex.ml:167` | `Provider_vertex` | `https://{region}-aiplatform.googleapis.com/v1/projects/{project}/locations/{region}/publishers/google/models/{model}:generateContent` | `Authorization: Bearer {oauth_token}` (from `gcloud auth print-access-token`) | NONE | EXISTING (host is `*.googleapis.com`) |
| `provider_vertex.ml:197` | `Provider_vertex` | Same as above (streaming) | `Authorization: Bearer {oauth_token}` | NONE | EXISTING |

**Subprocess:** `provider_vertex.ml:34-35` runs `gcloud auth print-access-token` via `Lwt_process.open_process_in`. This spawns `gcloud` which makes its own outbound network calls to Google OAuth endpoints. The subprocess itself does not pass credentials through Clawq's HTTP client.

### 1.8 OpenAI Codex Provider

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `provider_openai_codex.ml:813` | `Provider_openai_codex` | `https://chatgpt.com/backend-api/codex/responses` (Responses API) | `Authorization: Bearer {access_token}` (OAuth) | HEADER-UNREDACTED | EXISTING (host is `chatgpt.com`)

---

## 2. Provider Model Discovery

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `model_discovery.ml:387` | `Model_discovery` | `{base_url}/models` | `Authorization: Bearer {api_key}` | NONE | DYNAMIC |
| `model_discovery.ml:407` | `Model_discovery` | `{base_url}/api/tags` (Ollama) | None | N/A | DYNAMIC |

---

## 3. Provider Quota Fetching

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `provider_quota.ml:582` | `Provider_quota` | `https://api.anthropic.com/api/oauth/usage` | `Authorization: Bearer {token}` (Anthropic OAuth) | NONE | EXISTING |
| `provider_quota.ml:648` | `Provider_quota` | `https://chatgpt.com/backend-api/wham/usage` | `Authorization: Bearer {token}` + `ChatGPT-Account-Id` header | NONE | EXISTING |
| `provider_quota.ml:695` | `Provider_quota` | `https://api.z.ai/api/monitor/usage/quota/limit` | `Authorization: {api_key}` (no Bearer prefix) | NONE | EXISTING |
| `provider_quota.ml:884` | `Provider_quota` | `https://api.kimi.com/coding/v1/usages` | `Authorization: Bearer {api_key}` | NONE | EXISTING |
| `provider_quota.ml:997` | `Provider_quota` | `https://www.cursor.com/api/usage` | `Cookie: WorkosCursorSessionToken={jwt}` | NONE | EXISTING |

---

## 4. OpenAI Codex OAuth

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `openai_codex_oauth.ml:471` | `Openai_codex_oauth` | `https://auth.openai.com/oauth/token` (via `form_post`) | `client_id` + `code`/`refresh_token` in POST body | NONE | EXISTING |
| `openai_codex_oauth.ml:516` | `Openai_codex_oauth` | `https://auth.openai.com/oauth/token` (refresh) | `client_id` + `refresh_token` in POST body | NONE | EXISTING |

**Note:** Uses `Cohttp_lwt_unix.Client.post` directly, bypassing `Http_client`.

---

## 5. GitHub Integration

### 5.1 GitHub API (via Http_client)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `github_api.ml:192` | `Github_api` | `https://api.github.com/repos/{owner}/{repo}/issues/{n}/comments` | `Authorization: Bearer {pat}` | HEADER (via `auth_headers`) | EXISTING |
| `github_api.ml:222` | `Github_api` | `https://api.github.com/repos/{owner}/{repo}/pulls/{n}/comments/{id}/replies` | `Authorization: Bearer {pat}` | HEADER | EXISTING |
| `github_api.ml:255` | `Github_api` | `https://api.github.com/repos/{owner}/{repo}/issues/comments/{id}/reactions` | `Authorization: Bearer {pat}` | HEADER | EXISTING |
| `github_api.ml:286` | `Github_api` | `https://api.github.com/repos/{owner}/{repo}/issues/{n}/comments` (returning ID) | `Authorization: Bearer {pat}` | HEADER | EXISTING |
| `github_api.ml:327` | `Github_api` | `https://api.github.com/repos/{owner}/{repo}/issues/comments/{id}` (edit) | `Authorization: Bearer {pat}` | HEADER | EXISTING |
| `github_api.ml:356` | `Github_api` | `https://api.github.com/repos/{owner}/{repo}/pulls/{n}/files` | `Authorization: Bearer {pat}` | HEADER | EXISTING |

### 5.2 GitHub App Token Exchange

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `github_app_token.ml:205` | `Github_app_token` | `https://api.github.com/app/installations/{id}/access_tokens` | `Authorization: Bearer {jwt}` (RS256 JWT signed with app private key) | NONE | EXISTING |

**Note:** The JWT is generated in-memory from the app's RSA private key (`github_app_token.ml:56-66`). The private key itself is never sent over the network.

---

## 6. Discord Integration

### 6.1 Discord REST API (HTTP)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `discord.ml:220` | `Discord` | `https://discord.com/api/v10/channels/{id}/messages` (send) | `Authorization: Bot {token}` | NONE | EXISTING |
| `discord.ml:240` | `Discord` | `https://discord.com/api/v10/channels/{id}/messages/{id}` (edit) | `Authorization: Bot {token}` | NONE | EXISTING |
| `discord.ml:255` | `Discord` | `https://discord.com/api/v10/channels/{id}/messages/{id}` (delete) | `Authorization: Bot {token}` | NONE | EXISTING |
| `discord.ml:270` | `Discord` | `https://discord.com/api/v10/channels/{id}/typing` | `Authorization: Bot {token}` | NONE | EXISTING |
| `discord.ml:303` | `Discord` | `https://discord.com/api/v10/channels/{id}/messages/{id}/reactions/{emoji}/@me` (add reaction) | `Authorization: Bot {token}` | NONE | EXISTING |
| `discord.ml:320` | `Discord` | `https://discord.com/api/v10/channels/{id}/messages/{id}/reactions/{emoji}/{user}` (delete reaction) | `Authorization: Bot {token}` | NONE | EXISTING |
| `discord.ml:340` | `Discord` | `https://discord.com/api/v10/users/@me/channels` (send DM) | `Authorization: Bot {token}` | NONE | EXISTING |
| `discord.ml:1243` | `Discord` | `https://cdn.discordapp.com/attachments/{id}/{id}/{filename}` (attachment download) | None | N/A | EXISTING |

### 6.2 Discord Gateway (WebSocket)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `discord_gateway.ml:185` | `Discord_gateway` | `https://discord.com/api/v10/gateway` (get gateway URL) | `Authorization: Bot {token}` | NONE | EXISTING |
| `discord_gateway.ml:223` | `Discord_gateway` | `wss://gateway.discord.gg/?v=10&encoding=json` | `Authorization: Bot {token}` (in IDENTIFY payload) | NONE | EXISTING |

---

## 7. Slack Integration

### 7.1 Slack Web API (HTTP)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `slack.ml:118` | `Slack` | `https://slack.com/api/chat.postMessage` | `Authorization: Bearer {token}` | NONE | EXISTING |
| `slack.ml:126` | `Slack` | `https://slack.com/api/chat.postMessage` (with ID return) | `Authorization: Bearer {token}` | NONE | EXISTING |
| `slack.ml:143` | `Slack` | `https://slack.com/api/chat.postMessage` (thread reply) | `Authorization: Bearer {token}` | NONE | EXISTING |
| `slack.ml:159` | `Slack` | `https://slack.com/api/chat.update` | `Authorization: Bearer {token}` | NONE | EXISTING |
| `slack.ml:170` | `Slack` | `https://slack.com/api/chat.delete` | `Authorization: Bearer {token}` | NONE | EXISTING |
| `slack.ml:199` | `Slack` | `https://slack.com/api/reactions.add` | `Authorization: Bearer {token}` | NONE | EXISTING |
| `slack.ml:215` | `Slack` | `https://slack.com/api/reactions.remove` | `Authorization: Bearer {token}` | NONE | EXISTING |
| `slack.ml:983` | `Slack` | `{file.url_private_download}` (Slack file download) | `Authorization: Bearer {token}` | HEADER-UNREDACTED | DYNAMIC |

### 7.2 Slack Socket Mode (WebSocket)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `slack_socket.ml:5` | `Slack_socket` | `https://slack.com/api/apps.connections.open` | `Authorization: Bearer {app_token}` | NONE | EXISTING |
| `slack_socket.ml:62` | `Slack_socket` | `wss://wss-primary.slack.com/link/...` (from `apps.connections.open` response) | `Authorization: Bearer {app_token}` (in WebSocket URL) | NONE | DYNAMIC |

---

## 8. Telegram Integration

All Telegram API calls embed the bot token in the URL path: `https://api.telegram.org/bot{token}/{method}`.

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `telegram_api.ml:433` | `Telegram_api` | `https://api.telegram.org/bot{token}/deleteWebhook` | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:467` | `Telegram_api` | `https://api.telegram.org/bot{token}/getUpdates` | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:659` | `Telegram_api` | `https://api.telegram.org/bot{token}/getUpdates` (acknowledge) | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:716` | `Telegram_api` | `https://api.telegram.org/bot{token}/sendChatAction` | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:852` | `Telegram_api` | `https://api.telegram.org/bot{token}/sendMessage` | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:881` | `Telegram_api` | `https://api.telegram.org/bot{token}/sendMessage` (plain text fallback) | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:946` | `Telegram_api` | `https://api.telegram.org/bot{token}/editMessageText` | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:986` | `Telegram_api` | `https://api.telegram.org/bot{token}/sendChatAction` (typing) | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:1010` | `Telegram_api` | `https://api.telegram.org/bot{token}/editMessageText` (inline keyboard) | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:1034` | `Telegram_api` | `https://api.telegram.org/bot{token}/editMessageText` (fallback) | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:1059` | `Telegram_api` | `https://api.telegram.org/bot{token}/deleteMessage` | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:1158` | `Telegram_api` | `https://api.telegram.org/bot{token}/setMessageReaction` | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:1179` | `Telegram_api` | `https://api.telegram.org/bot{token}/setMessageReaction` (clear) | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:1269` | `Telegram_api` | `https://api.telegram.org/bot{token}/sendPoll` | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:1323` | `Telegram_api` | `https://api.telegram.org/bot{token}/setMyCommands` | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:1414` | `Telegram_api` | `https://api.telegram.org/bot{token}/getFile` | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:1423` | `Telegram_api` | `https://api.telegram.org/file/bot{token}/{file_path}` | Token in URL path | URL-PATH | EXISTING |
| `telegram_api.ml:1483` | `Telegram_api` | `https://api.telegram.org/bot{token}/sendDocument` (multipart) | Token in URL path | URL-PATH | EXISTING |

---

## 9. Matrix Integration

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `matrix.ml:38` | `Matrix` | `{homeserver_url}/_matrix/client/v3/rooms/{room_id}/send/{txn_id}` | `Authorization: Bearer {access_token}` | NONE | DYNAMIC |
| `matrix.ml:78` | `Matrix` | `{homeserver_url}/_matrix/client/v3/rooms/{room_id}/send/m.reaction/{txn_id}` | `Authorization: Bearer {access_token}` | NONE | DYNAMIC |
| `matrix.ml:91` | `Matrix` | `{homeserver_url}/_matrix/client/v3/rooms/{room_id}/typing/{user_id}` | `Authorization: Bearer {access_token}` | NONE | DYNAMIC |
| `matrix.ml:216` | `Matrix` | `{homeserver_url}/_matrix/client/v3/sync` (long-poll) | `Authorization: Bearer {access_token}` | NONE | DYNAMIC |

---

## 10. Mattermost Integration

### 10.1 Mattermost REST API (HTTP)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `mattermost.ml:25` | `Mattermost` | `{base_url}/api/v4/posts` | `Authorization: Bearer {access_token}` | NONE | DYNAMIC |
| `mattermost.ml:36` | `Mattermost` | `{base_url}/api/v4/users/me` | `Authorization: Bearer {access_token}` | NONE | DYNAMIC |

### 10.2 Mattermost WebSocket

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `mattermost.ml:114` | `Mattermost` | `wss://{host}/api/v4/websocket` (or `ws://` for non-TLS) | `authentication_challenge` message with `access_token` | NONE | DYNAMIC |

---

## 11. Microsoft Teams Integration

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `teams_auth.ml:31` | `Teams_auth` | `https://login.microsoftonline.com/{tenant_id}/oauth2/v2.0/token` | `app_id` + `app_secret` in POST body (client_credentials grant) | NONE | EXISTING |

**Note:** Uses `Cohttp_lwt_unix.Client.post` directly, bypassing `Http_client`.

---

## 12. Lark (Feishu) Integration

### 12.1 Lark REST API (HTTP)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `lark.ml:66` | `Lark` | `https://open.feishu.cn/open-apis/auth/v3/tenant_access_token/internal` or `https://open.larksuite.com/open-apis/auth/v3/tenant_access_token/internal` | `app_id` + `app_secret` in POST body | NONE | EXISTING |
| `lark.ml:132` | `Lark` | `{base}/im/v1/messages?receive_id_type=chat_id` | `Authorization: Bearer {tenant_access_token}` | NONE | EXISTING |

### 12.2 Lark WebSocket

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `lark.ml:292` | `Lark` | `wss://{ws_host}{ws_path}` (from Lark API response) | Token embedded in WebSocket URL | NONE | DYNAMIC |

---

## 13. DingTalk Integration

### 13.1 DingTalk REST API (HTTP)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `dingtalk.ml:28` | `Dingtalk` | `https://api.dingtalk.com/v1.0/oauth2/accessToken` | `app_key` + `app_secret` in POST body | NONE | EXISTING |
| `dingtalk.ml:58` | `Dingtalk` | `{webhook_url}` (user-configured DingTalk webhook) | None (webhook URL is the secret) | NONE | DYNAMIC |
| `dingtalk.ml:83` | `Dingtalk` | `https://api.dingtalk.com/v1.0/robot/messages/sendByConversation` | `x-acs-dingtalk-access-token: {token}` | NONE | EXISTING |
| `dingtalk.ml:145` | `Dingtalk` | `https://api.dingtalk.com/v1.0/gateway/connections/open` (stream register) | `app_key` + `app_secret` in POST body | NONE | EXISTING |

### 13.2 DingTalk WebSocket

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `dingtalk.ml:158` | `Dingtalk` | `wss://{host}/v1.0/gateway/connections/open` (from API response) | Token in WebSocket URL | NONE | DYNAMIC |

---

## 14. LINE Integration

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `line_channel.ml:32` | `Line_channel` | `https://api.line.me/v2/bot/message/reply` | `Authorization: Bearer {channel_access_token}` | NONE | EXISTING |
| `line_channel.ml:51` | `Line_channel` | `https://api.line.me/v2/bot/message/push` | `Authorization: Bearer {channel_access_token}` | NONE | EXISTING |

---

## 15. WhatsApp Integration

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `whatsapp.ml:27` | `Whatsapp` | `https://graph.facebook.com/v18.0/{phone_number_id}/messages` | `Authorization: Bearer {access_token}` | NONE | EXISTING |

---

## 16. Nostr Integration (Subprocess)

Nostr uses the `nak` CLI tool via `Lwt_process.open_process_full`. Network calls happen inside the `nak` subprocess, not through Clawq's HTTP client.

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `nostr.ml:42` | `Nostr` | `{relay_urls}` (user-configured, e.g. `wss://relay.damus.io`) | `--sec {private_key}` CLI arg | CLI-ARG | DYNAMIC |
| `nostr.ml:70` | `Nostr` | `{relay_urls}` (NIP-04 encrypt) | `--sec {private_key}` CLI arg | CLI-ARG | DYNAMIC |
| `nostr.ml:102` | `Nostr` | `{relay_urls}` (NIP-44 encrypt) | `--sec {private_key}` CLI arg | CLI-ARG | DYNAMIC |
| `nostr.ml:134` | `Nostr` | `{relay_urls}` (event creation) | `--sec {private_key}` CLI arg | CLI-ARG | DYNAMIC |
| `nostr.ml:156` | `Nostr` | `{relay_urls}` (publish) | `--sec {private_key}` CLI arg | CLI-ARG | DYNAMIC |
| `nostr.ml:232` | `Nostr` | `{relay_urls}` (decrypt) | `--sec {private_key}` CLI arg | CLI-ARG | DYNAMIC |
| `nostr.ml:271` | `Nostr` | `{relay_urls}` (relay auth) | `--sec {private_key}` CLI arg | CLI-ARG | DYNAMIC |

**SECURITY NOTE:** The private key is passed as a CLI argument (`--sec`), which is visible in `ps aux` output.

---

## 17. OneBot Integration

### 17.1 OneBot REST API (HTTP)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `onebot.ml:68` | `Onebot` | `{http_url}/send_private_msg` | `Authorization: Bearer {access_token}` (optional) | NONE | DYNAMIC |
| `onebot.ml:94` | `Onebot` | `{http_url}/send_group_msg` | `Authorization: Bearer {access_token}` (optional) | NONE | DYNAMIC |

### 17.2 OneBot WebSocket

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `onebot.ml:169-170` | `Onebot` | `{ws_url}` (user-configured) | `access_token` in WebSocket auth message (optional) | NONE | DYNAMIC |

---

## 18. Signal Integration

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `signal.ml:36` | `Signal` | `http://localhost:{port}/v1/send` (local signal-cli REST API) | None | N/A | LOCAL |
| `signal.ml:58` | `Signal` | `http://localhost:{port}/v1/receive` (local signal-cli REST API) | None | N/A | LOCAL |
| `signal.ml:175` | `Signal` | `http://localhost:{port}/v1/events` (SSE stream) | None | N/A | LOCAL |
| `signal.ml:235` | `Signal` | `http://localhost:{port}/v1/about` (health check) | None | N/A | LOCAL |

---

## 19. IRC Integration (Raw TCP/TLS)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `irc.ml:76-88` | `Irc` | `{host}:{port}` (user-configured IRC server) | `PASS {password}` command | NONE | DYNAMIC |
| `irc.ml:102-114` | `Irc` | `{host}:{port}` (TLS variant) | SASL PLAIN (`\0{nick}\0{password}`) | NONE | DYNAMIC |
| `irc.ml:131-132` | `Irc` | `{host}:{port}` (TLS socket) | TLS handshake, then SASL PLAIN | NONE | DYNAMIC |

**Transport:** Raw TCP via `Lwt_unix.connect` + optional TLS via `Tls_lwt.Unix.client_of_fd`.

---

## 20. Email Integration (IMAP/SMTP)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `email_channel.ml:106-115` | `Email_channel` | `{imap_host}:{imap_port}` (IMAP) | IMAP `LOGIN {user} {password}` command | NONE | DYNAMIC |
| `email_channel.ml:139-148` | `Email_channel` | `{imap_host}:{imap_port}` (IMAP TLS) | IMAP `LOGIN` after TLS handshake | NONE | DYNAMIC |
| `email_channel.ml:437-454` | `Email_channel` | `{smtp_host}:{smtp_port}` (SMTP TLS) | SMTP `AUTH` (base64-encoded credentials) | NONE | DYNAMIC |

**Transport:** Raw TCP via `Lwt_unix.getaddrinfo` + `Lwt_unix.connect` + optional TLS via `Tls_lwt.Unix.client_of_fd`.

---

## 21. Web Search API

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `tools_builtin_net.ml:93-97` | `Tools_builtin_net` | `{url}` (user-supplied URL for `http_request` tool) | User-supplied headers (may include `Authorization`, cookies, etc.) | NONE | DYNAMIC |
| `tools_builtin_net.ml:277` | `Tools_builtin_net` | `{url}` (user-supplied URL for `fetch_url` tool) | None | N/A | DYNAMIC |
| `tools_builtin_net.ml:300` | `Tools_builtin_net` | `https://api.search.brave.com/res/v1/web/search` | `X-Subscription-Token: {search_api_key}` | NONE | EXISTING |
| `tools_builtin_net.ml:980` | `Tools_builtin_net` | `https://api.search.brave.com/res/v1/web/search` (health check) | `X-Subscription-Token: {search_api_key}` | NONE | EXISTING |
| `tools_builtin_net.ml:1016` | `Tools_builtin_net` | `https://api.search.brave.com/res/v1/web/search` (probe) | `X-Subscription-Token: {search_api_key}` | NONE | EXISTING |

---

## 22. Z.ai MCP Integration

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `tools_builtin_zai.ml:109` | `Tools_builtin_zai` | `https://api.z.ai/mcp` (or similar) | `Authorization: Bearer {zai_mcp.key}` | NONE | EXISTING |

---

## 23. MCP Client (Generic HTTP Transport)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `mcp_client.ml:124` | `Mcp_client` | `{url}` (user-configured MCP HTTP server URL) | Configured HTTP headers from MCP server config | NONE | DYNAMIC |

**Note:** Uses `Cohttp_lwt_unix.Client.post` directly, bypassing `Http_client`.

---

## 24. Gateway Auth

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `command_bridge.ml:91` | `Command_bridge` | `{gateway_url}/api/...` | `Authorization: Bearer {auth_token}` | NONE | DYNAMIC |
| `command_bridge_gateway.ml:139` | `Command_bridge_gateway` | `{gateway_url}/api/...` (GET) | `Authorization: Bearer {auth_token}` | NONE | DYNAMIC |
| `command_bridge_gateway.ml:175` | `Command_bridge_gateway` | `{gateway_url}/api/...` (POST) | `Authorization: Bearer {auth_token}` | NONE | DYNAMIC |

---

## 25. Attachment Downloads

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `attachment_download.ml:210` | `Attachment_download` | `{attachment_url}` (from message metadata, e.g. Discord CDN, Slack, etc.) | Varies by source (may include auth headers) | NONE | DYNAMIC |

---

## 26. Connector Room Progress

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `connector_room_progress.ml:65` | `Connector_room_progress` | Slack webhook or API endpoint | `Authorization: Bearer {token}` (via Slack module) | NONE | EXISTING |

---

## 27. Config Validation

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `config_validate.ml:12` | `Config_validate` | `{base_url}/models` (provider validation) | `Authorization: Bearer {api_key}` | NONE | DYNAMIC |

---

## 28. Health Checks (Local)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `runtime_docker.ml:100` | `Runtime_docker` | `http://localhost:{port}/health` | None | N/A | LOCAL |
| `runtime_native.ml:50` | `Runtime_native` | `http://{host}:{port}/health` | None | N/A | DYNAMIC |

---

## 29. Vector Embeddings

| Callsite | Module | Destination | Credential | Exposure | Enforceability |
|----------|--------|-------------|------------|----------|----------------|
| `vector.ml:97` | `Vector` | `{base_url}/v1/embeddings` (default: `https://api.openai.com/v1/embeddings`) | `Authorization: Bearer {api_key}` | HEADER-UNREDACTED | DYNAMIC |

---

## 30. Text-to-Speech (TTS)

| Callsite | Module | Destination | Credential | Exposure | Enforceability |
|----------|--------|-------------|------------|----------|----------------|
| `tts.ml:47` | `Tts` | `{base_url}/audio/speech` (default: `https://api.openai.com/v1/audio/speech`) | `Authorization: Bearer {api_key}` | HEADER-UNREDACTED | DYNAMIC |

---

## 31. Speech-to-Text (STT)

| Callsite | Module | Destination | Credential | Exposure | Enforceability |
|----------|--------|-------------|------------|----------|----------------|
| `stt.ml:51` | `Stt` | `{base_url}/audio/transcriptions` (default: `https://api.groq.com/openai/v1/audio/transcriptions`) | `Authorization: Bearer {api_key}` | HEADER-UNREDACTED | DYNAMIC |

---

## 32. Telemetry (OTLP)

| Callsite | Module | Destination | Credential | Exposure | Enforceability |
|----------|--------|-------------|------------|----------|----------------|
| `telemetry.ml:174` | `Telemetry` | `{endpoint}` (user-configured OTLP endpoint) | Configured OTLP headers | HEADER-UNREDACTED | DYNAMIC |

---

## 33. Tools Built-in IO (http_get)

| Callsite | Module | Destination | Credential | Exposure | Enforceability |
|----------|--------|-------------|------------|----------|----------------|
| `tools_builtin_io.ml:755` | `Tools_builtin_io` | `{url}` (user-supplied URL for `http_get` tool) | None | N/A | DYNAMIC |

---

## 34. Command Bridge Auth (Gateway Pairing)

| Callsite | Module | Destination | Credential | Exposure | Enforceability |
|----------|--------|-------------|------------|----------|----------------|
| `command_bridge_auth.ml:265` | `Command_bridge_auth` | `http://{host}:{port}/pair` (gateway pairing endpoint) | None (pairing token in POST body) | NONE | DYNAMIC |

---

## 35. Microsoft Teams Bot Framework API

All Teams Bot Framework calls use `Authorization: Bearer {oauth_token}` (from `teams_auth.ml` OAuth flow). Destination is `{service_url}/v3/conversations/...` where `service_url` is provided by the inbound activity.

| Callsite | Module | Destination | Credential | Exposure | Enforceability |
|----------|--------|-------------|------------|----------|----------------|
| `teams.ml:185` | `Teams` | `{service_url}/v3/conversations/{id}/activities` (POST) | `Authorization: Bearer {token}` | HEADER-UNREDACTED | DYNAMIC |
| `teams.ml:190` | `Teams` | `{service_url}/v3/conversations/{id}/activities/{id}` (PUT) | `Authorization: Bearer {token}` | HEADER-UNREDACTED | DYNAMIC |
| `teams.ml:195` | `Teams` | `{service_url}/v3/conversations/{id}/activities/{id}` (DELETE) | `Authorization: Bearer {token}` | HEADER-UNREDACTED | DYNAMIC |
| `teams.ml:217` | `Teams` | `{service_url}/v3/conversations/{id}/activities` (typing) | `Authorization: Bearer {token}` | HEADER-UNREDACTED | DYNAMIC |
| `teams.ml:1091` | `Teams` | `{content_url}` (attachment/audio download) | `Authorization: Bearer {token}` | HEADER-UNREDACTED | DYNAMIC |
| `teams_file_upload.ml:56` | `Teams_file_upload` | `{service_url}/v3/conversations/{id}/attachments` (upload) | `Authorization: Bearer {token}` | HEADER-UNREDACTED | DYNAMIC |
| `teams_file_upload.ml:101` | `Teams_file_upload` | `{service_url}/v3/conversations/{id}/activities` (send with attachment) | `Authorization: Bearer {token}` | HEADER-UNREDACTED | DYNAMIC |
| `teams_file_consent.ml:185` | `Teams_file_consent` | `{upload_url}` (OneDrive file upload) | `Authorization: Bearer {token}` | HEADER-UNREDACTED | DYNAMIC |
| `teams_adaptive_card.ml:26` | `Teams_adaptive_card` | `{service_url}/v3/conversations/{id}/activities/{id}` (update card) | `Authorization: Bearer {token}` | HEADER-UNREDACTED | DYNAMIC |

---

## 36. Tunnel Subprocesses

Tunnel managers spawn external processes that make their own outbound network connections. These are not interceptable by Clawq's egress evaluator.

| Callsite | Module | Destination | Credential | Exposure | Enforceability |
|----------|--------|-------------|------------|----------|----------------|
| `tunnel_cloudflare.ml:115` | `Tunnel_cloudflare` | Cloudflare tunnel endpoint (via `cloudflared` subprocess) | Cloudflare tunnel token (in env/config) | N/A (subprocess) | NOT-ENFORCEABLE |
| `tunnel_cloudflare.ml:193` | `Tunnel_cloudflare` | Cloudflare tunnel endpoint (via `cloudflared` subprocess) | Cloudflare tunnel token | N/A (subprocess) | NOT-ENFORCEABLE |
| `tunnel_ngrok.ml:41` | `Tunnel_ngrok` | `http://localhost:4040/api/tunnels` (ngrok local API check) | None | N/A | LOCAL |
| `tunnel_ngrok.ml:74` | `Tunnel_ngrok` | ngrok tunnel endpoint (via `ngrok` subprocess) | ngrok auth token (in config) | N/A (subprocess) | NOT-ENFORCEABLE |
| `tunnel_tailscale.ml:61` | `Tunnel_tailscale` | Tailscale funnel endpoint (via `tailscale` subprocess) | Tailscale auth (in system config) | N/A (subprocess) | NOT-ENFORCEABLE |
| `tunnel_tailscale.ml:93` | `Tunnel_tailscale` | Tailscale status (via `tailscale status --json`) | Tailscale auth | N/A (subprocess) | NOT-ENFORCEABLE |
| `tunnel_custom.ml:64` | `Tunnel_custom` | User-defined tunnel command (via `/bin/sh -c`) | Whatever the custom command uses | N/A (subprocess) | NOT-ENFORCEABLE |

---

## 37. Debug Server (Local)

| Callsite | Module | Destination | Credential | Redaction | Enforceability |
|----------|--------|-------------|------------|-----------|----------------|
| `command_bridge_debug.ml:137` | `Command_bridge_debug` | `localhost:{port}` (loopback only) | None | N/A | LOCAL |

**Note:** This is an inbound server, not an outbound call. Listed for completeness.

---

## Summary by Transport Mechanism

| Transport | Count | Description |
|-----------|-------|-------------|
| HTTP (Http_client) | ~115 | Most outbound calls go through `Http_client.*` |
| HTTP-direct (Cohttp) | 3 | `openai_codex_oauth.ml`, `teams_auth.ml`, `mcp_client.ml` |
| WebSocket (Ws_client) | 8 | Discord gateway, Slack socket, Lark, DingTalk, Mattermost, OneBot |
| TCP/TLS (raw) | 6 | IRC (3 paths), Email IMAP/SMTP (3 paths) |
| Subprocess | ~14 | Nostr via `nak` CLI, Vertex via `gcloud`, tunnels via `cloudflared`/`ngrok`/`tailscale` |

## Summary by Enforceability

| Class | Count | Description |
|-------|-------|-------------|
| EXISTING | ~70 | Host is known/static (e.g. `api.github.com`, `discord.com`, `api.telegram.org`) |
| DYNAMIC | ~55 | Host is user/config-provided; egress evaluator can enforce if host is supplied at eval time |
| LOCAL | ~5 | Loopback/local only; egress policy not applicable |
| NOT-ENFORCEABLE | ~7 | Subprocess-based network calls that cannot be intercepted by the OCaml egress evaluator |

## Summary by Redaction Status

| Class | Count | Description |
|-------|-------|-------------|
| HEADER-UNREDACTED | ~85 | Credential in HTTP header, not redacted in normal log paths |
| URL-PATH | ~18 | Token embedded in URL path (Telegram); may leak to access logs/referrers |
| CLI-ARG | ~7 | Credential visible in process listings via `ps aux` (Nostr) |
| HEADER-REDACTED | ~10 | `Http_debug` redacts auth headers in debug logs (GitHub) |
| NONE | ~20 | No credential used (local APIs, public endpoints) |

## Key Observations for Egress Policy Enforcement

1. **Static-host callsites are fully enforceable**: Discord (`discord.com`), Slack (`slack.com`), Telegram (`api.telegram.org`), GitHub (`api.github.com`), LINE (`api.line.me`), WhatsApp (`graph.facebook.com`), Microsoft (`login.microsoftonline.com`), Lark (`open.feishu.cn`/`open.larksuite.com`), DingTalk (`api.dingtalk.com`), Brave Search (`api.search.brave.com`), Anthropic (`api.anthropic.com`), Z.ai (`api.z.ai`), Kimi (`api.kimi.com`), Cursor (`www.cursor.com`), OpenAI Codex (`chatgpt.com`, `auth0.openai.com`).

2. **Dynamic-host callsites need host injection**: LLM providers, Matrix, Mattermost, OneBot, IRC, Email, MCP, gateway, and attachment downloads all use user-configured hosts. The egress evaluator can enforce these if the host is resolved before evaluation.

3. **Three callsites bypass Http_client**: `openai_codex_oauth.ml`, `teams_auth.ml`, and `mcp_client.ml` use `Cohttp_lwt_unix.Client` directly. These should be wrapped through `Http_client` or have their hosts added to egress evaluation separately.

4. **Nostr is subprocess-based**: Network calls happen inside `nak` CLI, which cannot be intercepted by Clawq's egress evaluator without OS-level controls (e.g. network namespaces, iptables).

5. **Telegram token-in-URL pattern**: All Telegram API calls embed the bot token in the URL path, which may leak into access logs, referrer headers, or error messages. This is a Telegram Bot API design constraint, not a Clawq design choice.

6. **Signal is local-only**: All Signal calls go to `localhost:{port}` (signal-cli REST API), which is not subject to egress policy.

7. **Tunnel subprocesses bypass egress control**: Cloudflare, ngrok, Tailscale, and custom tunnels spawn external processes that make their own network connections. These are not interceptable by Clawq's OCaml-level egress evaluator and require OS-level controls (network namespaces, iptables) for enforcement.

8. **No centralized outbound request interceptor**: Each module makes its own network calls independently. A centralized middleware that evaluates egress policy before any outbound request would need to intercept all three transport paths (Http_client, Cohttp direct, Ws_client) plus raw TCP/TLS connections.
