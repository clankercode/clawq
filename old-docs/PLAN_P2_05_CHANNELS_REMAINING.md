# P2-05: Channels — Lark/Feishu, Mattermost, LINE, DingTalk, iMessage, QQ/OneBot

## Context

This plan covers the remaining channel adapters needed for nullclaw parity: Lark/Feishu (enterprise Asia), Mattermost (self-hosted Slack-like), LINE (Japan/SE Asia), DingTalk (Alibaba enterprise), iMessage (macOS-only via AppleScript), and QQ/OneBot (China). These are lower priority than Signal, Matrix, IRC, Email but complete the channel surface.

---

## Part A: Mattermost

### Protocol
Mattermost uses a WebSocket for real-time events + REST API for sending. Straightforward given clawq already has `ws_client.ml`.

**Auth:** Personal access token or session token.
**Receive:** `WSS://{host}/api/v4/websocket` → events of `event:"posted"` with JSON post data.
**Send:** `POST /api/v4/posts` with `channel_id` and `message`.

### Config
```ocaml
mattermost : mattermost_config option
type mattermost_config = {
  enabled : bool;
  host : string;            (* e.g. "https://mattermost.example.com" *)
  token : string;           (* personal access token *)
  team_id : string;
  channel_ids : string list; (* channels to listen in; empty = all *)
  allow_from : string list;  (* user IDs; empty = allow all *)
}
```

### Implementation: `src/mattermost.ml`
- Connect via `Ws_client` (reuse existing TLS WebSocket infrastructure)
- Authenticate: send `{"seq":1,"action":"authentication_challenge","data":{"token":"..."}}`
- Parse `event="posted"` → extract `data.post` JSON → `user_id`, `message`, `channel_id`
- Send: `POST {host}/api/v4/posts {"channel_id":"...","message":"..."}`
- Session key: `mattermost:{channel_id}:{user_id}`
- Reconnect: same backoff pattern as Discord gateway

---

## Part B: Lark / Feishu

### Protocol
Lark (international) and Feishu (China) share the same Bytedance API with different base URLs.

**Two receive modes:**
1. **WebSocket** (preferred): persistent connection using Lark's event WSS gateway
2. **HTTP callback**: Lark POSTs events to a webhook URL (requires public endpoint)

**Auth:** App ID + App Secret → POST `/open-apis/auth/v3/tenant_access_token/internal` → tenant access token (2-hour TTL, cache + refresh).

**Send:** POST `/open-apis/im/v1/messages?receive_id_type=open_id` with JSON body.

### Config
```ocaml
lark : lark_config option
type lark_config = {
  enabled : bool;
  app_id : string;
  app_secret : string;
  verification_token : string;  (* for webhook mode *)
  mode : string;                (* "websocket" | "webhook" *)
  endpoint : string;            (* "lark" | "feishu" — selects base URL *)
  allow_from : string list;
}
```

Base URLs:
- Lark: `https://open.larksuite.com`
- Feishu: `https://open.feishu.cn`

### Implementation: `src/lark.ml`
- Token management: cache tenant_access_token with expiry, refresh 60s before expiry
- WebSocket mode: connect to Lark event gateway WSS URL, ack events with `{"code":0}`
- Webhook mode: add route to `http_server.ml` at configurable path; verify `X-Lark-Signature`
- Parse `message.content` (JSON-encoded, extract `text` field)
- Send via REST with tenant token in `Authorization: Bearer` header
- Session key: `lark:{chat_id}:{open_id}`

---

## Part C: LINE

### Protocol
LINE Messaging API is webhook-based (similar to WhatsApp Cloud API).

**Receive:** LINE POSTs events to your webhook URL. Verify `X-Line-Signature` (HMAC-SHA256 of body with channel secret).
**Send:** POST `https://api.line.me/v2/bot/message/reply` with `replyToken`.

### Config
```ocaml
line : line_config option
type line_config = {
  enabled : bool;
  channel_secret : string;
  channel_access_token : string;
  allow_from : string list;     (* LINE user IDs *)
  webhook_path : string;        (* default "/line/webhook" *)
}
```

### Implementation: `src/line_channel.ml` (avoid stdlib `Line` conflict)
- Add route to `http_server.ml`
- Verify `X-Line-Signature`: `base64(HMAC-SHA256(channel_secret, raw_body))`
- Parse `events[].type="message"` + `events[].message.type="text"` → extract `source.userId`, `message.text`, `replyToken`
- Reply via REST using `replyToken` (one-time use, must reply within 30s)
- For follow-up messages (after reply token used): use push API `/v2/bot/message/push`
- Session key: `line:{userId}`
- HMAC-SHA256 via `digestif.c` (already linked)

---

## Part D: DingTalk

### Protocol
DingTalk provides a streaming WebSocket API for bots (Stream Mode, preferred over HTTP webhook).

**Stream Mode:** Connect to `wss://api.dingtalk.com/v1.0/gateway/connections/open` with headers including app credentials. Receive events as JSON messages, ack with `{"code":"0","headers":{"messageId":"..."},"message":"success"}`.

**Send:** POST to DingTalk Open API with robot webhook URL or access token.

### Config
```ocaml
dingtalk : dingtalk_config option
type dingtalk_config = {
  enabled : bool;
  app_key : string;
  app_secret : string;
  robot_code : string;
  allow_from : string list;
}
```

### Implementation: `src/dingtalk.ml`
- Connect via `Ws_client` to stream endpoint
- Authenticate: compute HMAC-SHA256 signature from timestamp + app_secret for header auth
- Parse incoming `data.senderStaffId`, `data.text.content`
- Send via POST to DingTalk message API with access token
- Session key: `dingtalk:{conversationId}:{senderStaffId}`
- Reconnect on disconnect

---

## Part E: iMessage (macOS only)

### Protocol
iMessage has no official API. The only approach (as used by nullclaw) is **AppleScript** to interact with the Messages app, reading from the Messages SQLite database.

**Inbound:** Poll `~/Library/Messages/chat.db` SQLite for new messages (by timestamp), or use AppleScript to query Messages.app.
**Outbound:** AppleScript: `tell application "Messages" to send "text" to buddy "addr" of account "iMessage"`

**Platform restriction:** macOS only. Attempting to start this channel on Linux should return a clear error.

### Config
```ocaml
imessage : imessage_config option
type imessage_config = {
  enabled : bool;
  poll_interval_s : int;   (* default 5 *)
  allow_from : string list; (* phone numbers or emails *)
}
```

### Implementation: `src/imessage.ml`
- `start` function: check if running on macOS (`Sys.os_type = "Unix"` + check `/usr/bin/osascript` exists); if not, log warning and return immediately
- Poll loop: `Lwt_unix.sleep poll_interval_s` → run AppleScript to get recent messages → filter by `allow_from` → route to session
- Send: spawn `osascript -e 'tell application "Messages" to send "{text}" to buddy "{addr}" of service "iMessage"'`
- Session key: `imessage:{sender}`
- Track `last_seen_id` via SQLite `ROWID` to avoid reprocessing

---

## Part F: QQ / OneBot

### Protocol
QQ bots use the **OneBot v11** protocol, an unofficial standardized bot API layer. Common implementations: go-cqhttp, Lagrange, NapCat.

**Transport options:**
1. **HTTP callback** (simpler): OneBot implementation POSTs events to your URL
2. **WebSocket (forward)**: You connect to `ws://{host}:{port}` and receive events
3. **WebSocket (reverse)**: OneBot connects to your WebSocket server

For clawq: implement **WebSocket forward** mode (connect out to OneBot server).

**Receive:** Events as JSON: `{"post_type":"message","message_type":"private","sender":{"user_id":123},"message":"hello"}`
**Send:** POST to OneBot HTTP API: `/send_private_msg` or `/send_group_msg`

### Config
```ocaml
onebot : onebot_config option
type onebot_config = {
  enabled : bool;
  ws_url : string;          (* e.g. "ws://localhost:8080" *)
  http_api_url : string;    (* e.g. "http://localhost:8081" *)
  access_token : string option;
  allow_from : int list;    (* QQ user IDs; empty = allow all *)
}
```

### Implementation: `src/onebot.ml`
- Connect via `Ws_client` to `ws_url`
- Add `Authorization: Bearer {token}` header if access_token set
- Parse `post_type="message"` events → extract `sender.user_id`, `message` (string or array)
- Handle array message format: `[{"type":"text","data":{"text":"hello"}}]`
- Send: POST to `{http_api_url}/send_private_msg` with `{"user_id":123,"message":"reply"}`
- Session key: `onebot:{user_id}` or `onebot:group:{group_id}:{user_id}`

---

## File Summary

| Channel | New File | Modified Files |
|---------|----------|----------------|
| Mattermost | `src/mattermost.ml` | runtime_config, config_loader, daemon, dune |
| Lark/Feishu | `src/lark.ml` | runtime_config, config_loader, daemon, dune, http_server |
| LINE | `src/line_channel.ml` | runtime_config, config_loader, daemon, dune, http_server |
| DingTalk | `src/dingtalk.ml` | runtime_config, config_loader, daemon, dune |
| iMessage | `src/imessage.ml` | runtime_config, config_loader, daemon, dune |
| QQ/OneBot | `src/onebot.ml` | runtime_config, config_loader, daemon, dune |

All use `Ws_client` (already linked), `Http_client`, `digestif.c` (HMAC), and `Session`. No new opam deps.

Webhook-based channels (Lark webhook mode, LINE) add routes to `http_server.ml`.

## Test Strategy

- **Mattermost**: WS auth message format, `event="posted"` parser, channel filter
- **Lark**: tenant token refresh logic, webhook signature verify, message content JSON extraction
- **LINE**: X-Line-Signature HMAC verify, event parser, reply vs push decision
- **DingTalk**: stream mode ack format, signature header construction
- **iMessage**: macOS check (skip on Linux), AppleScript command construction, ROWID-based dedup
- **OneBot**: JSON event parser (string + array message formats), send endpoint selection

Run: `make test` after each module.

## Implementation Order

1. Mattermost (simplest — pure WebSocket, no new patterns)
2. LINE (webhook, similar to WhatsApp)
3. Lark/Feishu (more complex auth/token management)
4. DingTalk (WebSocket stream mode)
5. QQ/OneBot (WebSocket + HTTP hybrid)
6. iMessage (macOS-only, lowest priority)
