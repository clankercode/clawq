# Connector Reference Guide

Complete reference for all Clawq chat connectors: setup, configuration,
capabilities, progress rendering, delivery observability, and troubleshooting.

## 1. Connector Overview

Clawq connects to chat platforms through connectors that translate between
platform APIs and the Clawq session engine. Each connector has a capability
profile that determines which features are available in that platform.

### Supported Connectors

| Connector   | Connection Mode       | Status |
|-------------|-----------------------|--------|
| MS Teams    | Bot Framework webhook | Full   |
| Slack       | Socket Mode / Events  | Full   |
| Discord     | WebSocket Gateway     | Full   |
| Telegram    | Long polling          | Full   |
| GitHub      | Webhook               | Full   |
| Matrix      | REST API              | Basic  |
| Mattermost  | REST API              | Basic  |
| IRC         | IRC protocol          | Basic  |
| Email       | IMAP/SMTP             | Basic  |
| Signal      | REST API              | Basic  |
| WhatsApp    | Cloud API             | Basic  |
| DingTalk    | Webhook               | Basic  |
| Lark        | Webhook               | Basic  |
| Line        | Webhook               | Basic  |
| OneBot      | WebSocket             | Basic  |
| Nostr       | Relay                 | Basic  |
| iMessage    | Polling               | Basic  |
| Web         | HTTP                  | Basic  |
| Plain       | stdin/stdout          | Basic  |

### Quick Comparison

| Feature         | Teams | Slack | Discord | Telegram |
|-----------------|-------|-------|---------|----------|
| Edit in place   | Yes   | Yes   | Yes     | Yes      |
| Delete          | Yes   | Yes   | Yes     | Yes      |
| Reactions       | No    | Yes   | Yes     | No       |
| Typing indicator| Yes   | No    | No      | Yes      |
| Status messages | Yes   | Yes   | Yes     | Yes      |
| File sending    | Yes   | No    | No      | Yes      |
| Adaptive Cards  | Yes   | No    | No      | No       |
| Buttons         | Yes   | No    | No      | Yes      |
| Thread replies  | ~     | Native| Native  | ~        |
| History capture | Yes   | Yes   | Yes     | No       |
| Max msg length  | 28672 | 4000  | 2000    | 4096     |

Legend: `~` = thread-like (not native platform threads).

---

## 2. MS Teams

Teams connects via the Microsoft Bot Framework. It is the richest connector,
supporting Adaptive Cards, progress cards with action buttons, file consent
cards, and typing indicators.

### Setup

Run the interactive setup wizard:

```
clawq setup teams
```

Or configure manually in `config.json`:

```json
{
  "channels": {
    "teams": {
      "app_id": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
      "app_secret": "your-client-secret",
      "tenant_id": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
      "webhook_path": "/teams/webhook",
      "service_url": "https://smba.trafficmanager.net/amer/",
      "allow_teams": ["*"],
      "allow_users": ["*"],
      "mention_mode": "entity",
      "file_consent_cards": true
    }
  }
}
```

#### Config Fields

| Field                | Description                                              | Default    |
|----------------------|----------------------------------------------------------|------------|
| `app_id`             | Azure Bot App ID (UUID)                                  | required   |
| `app_secret`         | Azure Bot client secret                                  | required   |
| `tenant_id`          | Azure AD tenant ID                                       | required   |
| `webhook_path`       | HTTP path for incoming webhook                           | `/teams/webhook` |
| `service_url`        | Bot Framework service URL                                | `https://smba.trafficmanager.net/amer/` |
| `allow_teams`        | Team ID allowlist (`["*"]` for all)                      | `["*"]`    |
| `allow_users`        | User ID allowlist (`["*"]` for all)                      | `["*"]`    |
| `mention_mode`       | `entity`, `text`, or `none`                              | `entity`   |
| `file_consent_cards` | Use FileConsentCard for uploads in DMs                   | `true`     |

#### Azure Bot Registration Steps

1. Go to the [Azure Portal](https://portal.azure.com).
2. Create an **Azure Bot** resource.
3. Under Configuration, set the **Messaging endpoint** to:
   `https://your-domain.com/teams/webhook`
4. Copy the App ID and create a client secret under **Certificates & secrets**.
5. Under **Channels**, add Microsoft Teams.
6. In the Teams admin center, approve the bot for your organization.
7. Install the bot in Teams.

For local development, use a tunnel:

```
clawq tunnel start
```

### Webhook Handler

The webhook handler (`teams.ml:handle_webhook`) processes incoming activities:

1. **JWT verification** -- validates the Bot Framework auth header.
2. **Deduplication** -- activity IDs are checked against both an in-memory LRU
   and persistent SQLite dedup table.
3. **@mention stripping** -- `<at>` tags are removed from message text.
4. **Group chat filter** -- in group chats, the bot only responds when
   explicitly @mentioned.
5. **Access control** -- team and user allowlists are enforced.
6. **Session resolution** -- the session key is derived from team ID and
   conversation ID (see Threading below).

### Adaptive Cards

Teams renders progress updates and interactive menus as Adaptive Cards.
Cards are sent as Bot Framework attachments with content type
`application/vnd.microsoft.card.adaptive`.

> All Adaptive Card emitters use schema version **v1.4**.

Key card types:
- **Progress card** -- shows task checklist with status icons and action buttons
- **What-can-do card** -- shows room capabilities and degraded behaviors
- **Agent/model menu cards** -- interactive selection menus

Cards are edited in place via PUT to the Bot Framework activity endpoint.
If edit fails, a new card is sent as fallback.

### Progress Cards

Progress cards (`teams_progress_card.ml`) render a checklist of task steps as
an evolving Adaptive Card with:

- **Header** -- color-coded by overall state (blue=in progress, green=done,
  red=blocked, purple=complete)
- **Summary line** -- compact count by state (e.g. "3 done, 1 current, 1 blocked")
- **Checklist container** -- each item with icon, title, state label, and
  clickable links (transcript, session, record)
- **Action buttons** -- context-sensitive controls gated by room policy

#### Action Buttons

| Button     | Command                    | Shown when            |
|------------|----------------------------|-----------------------|
| Inspect    | `/background show N`       | Always                |
| Continue   | `/background resume N`     | Task can be resumed   |
| Cancel     | `/background cancel N`     | Task is running       |
| Retry      | `/background retry N`      | Task failed           |
| View Logs  | `/background logs N`       | Task has a log path   |
| Finalize   | `/background finalize N`   | Dirty worktree        |

Buttons are rendered as `Action.Submit` with Teams `imBack` data values.
Room policy is checked at render time -- buttons for denied tools are omitted.

### Delivery Lifecycle

Every outbound Teams message is tracked through granular lifecycle states
recorded to the room activity ledger:

```
Scheduled -> Generated -> Attempted -> Transport_accepted -> Message_id_recorded
                                                                |
                                              Edit_failed ------+-> Fallback_sent
                                                                |
                                              Failed ------------+
                                                                |
                                              User_visible_unconfirmed
```

Each state includes a correlatable `tracking_id` (format: `dlv_<ts>_<random>`).
Tracking IDs are queryable via the internal ledger API (`Teams_delivery_lifecycle.query_by_tracking_id`); there is no CLI command to query by tracking ID.

See [Delivery Observability](#7-delivery-observability) for details.

### What-Can-Do

The `/whatcando` command shows a real-time Adaptive Card with:

- **Connector Capabilities** -- edit, delete, reactions, typing, status, files,
  cards, buttons
- **Room & Session State** -- profile binding, history capture, persistence,
  delivery mode, max message length
- **Readiness** -- database availability, GitHub configuration
- **Degraded Behaviors** -- explanations for missing features

### Context Capture

When a room is bound to a profile, ambient message history is captured:

- Unaddressed group messages are recorded to the connector history.
- History can be persisted to the database (`connector_history.persist_to_db`)
  or kept in-memory only.
- Profile-bound rooms use scoped session keys: `teams:<conversation_id>`.
- Non-bound rooms use per-user keys: `teams:<team_id>:<conv_id>`.

### File Consent

Teams file uploads use a consent card flow:

1. A FileConsentCard is sent with filename, description, and size.
2. The user accepts or declines via the card action.
3. On accept, the file is uploaded to OneDrive.
4. On decline or timeout, a temporary download URL fallback is provided.

The `file_consent_cards` config field controls this behavior. In group chats
or when disabled, files are served via temporary download URLs instead.

### Thread Support

Teams uses thread-like replies (not native platform threads):

- **DMs and unbound rooms:** reply to the original activity ID creates a
  conversation thread.
- **Profile-bound rooms:** all messages share one session regardless of thread.
  The session key is `teams:<conversation_id>`.

### Message Splitting

Teams has a 28,672-character limit. Longer responses are split at whitespace
boundaries. If no whitespace is found before the limit, a forced mid-word
break occurs.

### Rate Limiting

- **Inbound:** per-conversation+user rate limiting with 60-second warning
  cooldowns.
- **Outbound:** per-conversation throttling (1 req/s minimum) with exponential
  backoff retry on 429, 412, 502, 504 (up to 3 attempts).

---

## 3. Slack

Slack connects via Socket Mode (WebSocket) or Events API (HTTP webhook).
It supports edit-in-place, reactions, native thread replies, and history
capture.

### Setup

Run the interactive setup wizard:

```
clawq setup slack
```

Or configure manually:

```json
{
  "channels": {
    "slack": {
      "bot_token": "xoxb-...",
      "signing_secret": "your-signing-secret",
      "app_token": "xapp-...",
      "socket_mode": true,
      "events_path": "/slack/events",
      "allow_channels": ["*"],
      "allow_users": ["*"]
    }
  }
}
```

#### Config Fields

| Field            | Description                                    | Default          |
|------------------|------------------------------------------------|------------------|
| `bot_token`      | Bot User OAuth Token (xoxb-...)                | required         |
| `signing_secret` | Signing secret for request verification        | required         |
| `app_token`      | App-Level Token for Socket Mode (xapp-...)     | required for SM  |
| `socket_mode`    | Use Socket Mode instead of HTTP Events API     | `true`           |
| `events_path`    | HTTP path for Events API webhooks              | `/slack/events`  |
| `allow_channels` | Channel ID allowlist                           | `["*"]`          |
| `allow_users`    | User ID allowlist                              | `["*"]`          |
| `allow_private_channels` | Explicit opt-in list for private channels (required under `deny` policy) | `[]` |
| `private_channel_policy` | `"deny"` (default) or `"allow_if_listed"` — controls whether private channels in `allow_channels` are permitted | `"deny"` |

#### Private Channel Policy (B735)

By default (`private_channel_policy: "deny"`), Clawq will **refuse to operate in Slack private channels**, even if they are listed in `allow_channels`. This is a defense-in-depth measure to prevent accidental exposure of sensitive private channel content.

To use a private channel, you must:
1. Set `private_channel_policy` to `"deny"` (or leave it at the default).
2. Add the channel ID to **both** `allow_channels` and `allow_private_channels`.

Example:
```json
{
  "channels": {
    "slack": {
      "allow_channels": ["C-public-123", "G-private-456"],
      "allow_private_channels": ["G-private-456"]
    }
  }
}
```

If you want the old behaviour where `allow_channels` alone controls access to private channels, set `private_channel_policy: "allow_if_listed"`.

Refusals are logged to the room activity ledger (`private_channel_refused` event) so misconfigurations are visible.

> **Scope note:** The `conversations:read` Bot Token Scope is required for channel metadata lookups. This is already part of the recommended setup.

#### Slack App Setup Steps

1. Go to [api.slack.com/apps](https://api.slack.com/apps).
2. Click **Create New App** > **From scratch**.
3. Under **OAuth & Permissions**, add Bot Token Scopes:
   - `app_mentions:read`
   - `chat:write`
   - `channels:history`
   - `groups:history`
   - `im:history`
   - `mpim:history`
4. Install the app to your workspace.
5. Copy the Bot User OAuth Token (xoxb-...).

**For Socket Mode (recommended):**
6. Navigate to **Settings** > **Socket Mode** and enable it.
7. Generate an App-Level Token with `connections:write` scope.
8. Copy the token (xapp-...).
9. Under **Event Subscriptions**, subscribe to bot events:
   `app_mention`, `message.channels`, `message.groups`, `message.im`.

**For Events API (HTTP):**
6. Under **Event Subscriptions**, set the Request URL to your webhook endpoint.
7. Copy the Signing Secret from **Basic Information**.

### Socket Mode

Socket Mode (`slack_socket.ml`) maintains a persistent WebSocket connection
to Slack's `apps.connections.open` endpoint. The flow:

1. POST to `apps.connections.open` with the app token to get a WSS URL.
2. Connect via WebSocket.
3. Receive envelopes; ack each with the `envelope_id`.
4. For `events_api` envelopes, extract the event body and pass to
   `Slack.handle_event`.
5. On `disconnect`, close and reconnect with exponential backoff.

Socket Mode is recommended because it does not require a public endpoint.

### Event Handling

The Slack event handler (`slack.ml:handle_event`) processes incoming events:

1. **URL verification** -- responds with the challenge token.
2. **Bot message filtering** -- ignores messages from bots.
3. **Access control** -- channel and user allowlists.
4. **Rate limiting** -- per-channel+user with 60s warning cooldowns.
5. **Session resolution** -- profile-bound rooms use `slack:<channel_id>`;
   others use `slack:<channel_id>:<user_id>`.
6. **Thread awareness** -- replies go into the originating thread when
   `thread_ts` is present.

### Progress Checklist

Slack renders progress as formatted mrkdwn text (`slack_progress_checklist.ml`):

```
:icon: *Task label*
:icon: 3/5 done | 1 current, 1 blocked

:white_check_mark: *Implement auth* — <https://...|transcript>
:arrows_counterclockwise: *Write tests* (working)
:no_entry_sign: *Deploy* (blocked)
```

Links use Slack's `<url|label>` syntax. Blocked items show only a generic
"(blocked)" indicator -- internal dependency details are never exposed.

### Thread Support

Slack supports native thread replies. When a message arrives with `thread_ts`,
responses are posted as replies in that thread. Profile-bound rooms share a
single session across the channel.

---

## 4. Discord

Discord connects via the Discord Gateway (WebSocket). It supports edit-in-place,
reactions, native thread replies, and ambient history capture.

### Setup

Run the interactive setup wizard:

```
clawq setup discord
```

Or configure manually:

```json
{
  "channels": {
    "discord": {
      "bot_token": "your-bot-token",
      "allow_guilds": ["*"],
      "allow_users": ["*"],
      "intents": 33281
    }
  }
}
```

#### Config Fields

| Field          | Description                                    | Default    |
|----------------|------------------------------------------------|------------|
| `bot_token`    | Discord bot token                              | required   |
| `allow_guilds` | Server (guild) ID allowlist                    | `["*"]`    |
| `allow_users`  | User ID allowlist                              | `["*"]`    |
| `intents`      | Gateway intents bitmask                        | `33281`    |

#### Default Intents

The default intents value `33281` includes:
- `GUILDS` (1)
- `GUILD_MESSAGES` (512)
- `MESSAGE_CONTENT` (32768)

That sum is exactly `33281` (1 + 512 + 32768). DIRECT_MESSAGES (4096)
is **not** included by default; enabling it yields `37377`, so DM support
must be toggled on explicitly via the setup wizard's intent toggler.

Use the setup wizard's intent toggler to enable/disable individual intents.

#### Discord Bot Setup Steps

1. Go to [discord.com/developers/applications](https://discord.com/developers/applications).
2. Click **New Application** and give it a name.
3. Go to **Bot** in the sidebar and copy the bot token.
4. Under **Privileged Gateway Intents**, enable:
   - MESSAGE CONTENT INTENT (required)
   - SERVER MEMBERS INTENT (optional)
5. Go to **OAuth2** > **URL Generator**.
6. Select scopes: `bot`.
7. Select permissions: Send Messages, Read Message History.
8. Open the generated URL to invite the bot to your server.

Invite URL pattern:
```
https://discord.com/oauth2/authorize?client_id=YOUR_APP_ID&permissions=68608&scope=bot
```

### Gateway Connection

Discord uses the Gateway WebSocket protocol (`discord_gateway.ml`):

1. Connect to `wss://gateway.discord.gg/?v=10&encoding=json`.
2. Send Identify (op 2) with bot token and intents.
3. Receive Hello (op 10) with heartbeat interval.
4. Start heartbeat loop (op 1) at the specified interval.
5. Receive Dispatch events (op 0) for MESSAGE_CREATE, READY, etc.
6. On disconnect, attempt resume (op 6) with stored session ID and sequence.

Resume state is persisted to SQLite so reconnections survive daemon restarts.
Fatal close codes (4004, 4010, 4011, 4012, 4013, 4014) prevent reconnection.

### Message Handling

The handler (`discord.ml:handle_message`) processes incoming messages:

1. **Bot filtering** -- ignores messages from bots.
2. **Access control** -- guild and user allowlists.
3. **Group chat filter** -- in guild channels, only responds when @mentioned.
4. **Rate limiting** -- per-channel+user with 60s warning cooldowns.
5. **Session resolution** -- `discord:<channel_id>:<author_id>`.
6. **Typing indicator** -- refreshed every 8s (Discord indicator lasts ~10s).

### REST API Rate Limiting

Discord REST calls use per-route rate limit tracking:

- Route buckets track `remaining` count and `reset_at` from response headers.
- Global rate limits (429) use `retry-after` header.
- Per-route mutexes serialize requests to the same route bucket.
- Up to 3 retries on 429 with exponential backoff.

### Thread Support

Discord supports native thread replies. When a message is in a thread,
responses are posted as thread replies. Channel-scoped sessions use the
channel ID as the room identifier.

---

## 5. Telegram

Telegram connects via long polling using the Bot API. It supports edit-in-place,
typing indicators, inline keyboards, polls, and file sending.

### Setup

Run the interactive setup wizard:

```
clawq setup telegram
```

Or configure manually:

```json
{
  "channels": {
    "telegram": {
      "accounts": {
        "main": {
          "bot_token": "123456:ABC-DEF...",
          "allow_from": ["*"]
        }
      },
      "text_coalesce_ms": 150
    }
  }
}
```

#### Config Fields

| Field                  | Description                              | Default   |
|------------------------|------------------------------------------|-----------|
| `accounts`             | Named map of Telegram bot accounts       | required  |
| `accounts.*.bot_token` | Bot token from @BotFather                | required  |
| `accounts.*.allow_from`| Chat ID allowlist                        | `["*"]`   |
| `accounts.*.totp`      | TOTP pairing config (optional)           | none      |
| `text_coalesce_ms`     | Delay to coalesce multi-part messages    | `150`     |

Multiple accounts can be configured for different bots.

#### Getting a Bot Token

1. Open Telegram and search for `@BotFather`.
2. Send `/newbot` and follow the prompts.
3. BotFather gives you a token like `123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11`.
4. Use that token in the config.

### Polling

Telegram uses long polling (`telegram_poll.ml`) to receive updates:

1. Call `getUpdates` with the last known `update_id` as offset.
2. Process each update (message, callback query, poll answer).
3. Advance the offset to acknowledge processed updates.
4. Repeat with configurable timeout.

The polling loop handles reconnection and backoff automatically.

### Message Handling

The handler (`telegram.ml:handle_update`) processes incoming updates:

1. **Pair command** -- `/pair <code>` for TOTP authentication.
2. **Auth check** -- verifies chat_id against allowlist or TOTP pairing.
3. **Rate limiting** -- per-chat with 60s warning cooldowns.
4. **Session resolution** -- `telegram:<chat_id>:<user_id>`.
5. **Typing indicator** -- sends `sendChatAction` every 3s while processing.
6. **Attachment handling** -- downloads photos, stickers, documents, voice.

### Inline Keyboards

Telegram supports inline keyboard buttons for rich interactions:

- Buttons are rendered as `InlineKeyboardMarkup` with `callback_data`.
- Callback queries are routed back to the session via `callback_routing`.
- Used for agent/model selection menus and interactive prompts.

### Polls

Telegram polls are supported via `sendPoll`:

- Single-choice and multiple-choice polls.
- Poll answers are routed back via `poll_routing`.
- Stale routing entries are cleaned up after 1 hour.

### File Sending

Telegram supports sending files via `sendDocument`:

- Files are sent with filename and optional description.
- Download URLs are provided as fallback when file sending fails.

### Thread Support

Telegram uses thread-like replies (not native message threads). Responses
are sent as replies to the original message in the same chat.

---

## 6. Capability Matrix

Detailed capability comparison across all connectors:

### Core Messaging

| Capability        | Teams | Slack | Discord | Telegram | GitHub | Matrix | Mattermost | IRC |
|-------------------|-------|-------|---------|----------|--------|--------|------------|-----|
| Edit in place     | Yes   | Yes   | Yes     | Yes      | Yes    | Yes    | Yes        | No  |
| Delete messages   | Yes   | Yes   | Yes     | Yes      | Yes    | Yes    | Yes        | No  |
| Reactions         | No    | Yes   | Yes     | No       | Yes    | No     | Yes        | No  |
| Typing indicator  | Yes   | No    | No      | Yes      | No     | No     | No         | No  |
| Status messages   | Yes   | Yes   | Yes     | Yes      | Yes    | Yes    | Yes        | No  |

### Rich Content

| Capability        | Teams | Slack | Discord | Telegram | GitHub | Matrix | Mattermost | IRC |
|-------------------|-------|-------|---------|----------|--------|--------|------------|-----|
| File sending      | Yes   | No    | No      | Yes      | No     | No     | No         | No  |
| Adaptive Cards    | Yes   | No    | No      | No       | No     | No     | No         | No  |
| Buttons           | Yes   | No    | No      | Yes      | No     | No     | No         | No  |

### Threading and History

| Capability        | Teams | Slack | Discord | Telegram | GitHub | Matrix | Mattermost | IRC |
|-------------------|-------|-------|---------|----------|--------|--------|------------|-----|
| Thread replies    | ~     | Native| Native  | ~        | ~      | Native | Native     | No  |
| History capture   | Yes   | Yes   | Yes     | No       | No     | No     | No         | No  |

### Message Limits

| Connector   | Max Length | Parse Mode |
|-------------|------------|------------|
| Teams       | 28672      | Markdown   |
| Slack       | 4000       | mrkdwn     |
| Discord     | 2000       | Markdown   |
| Telegram    | 4096       | MarkdownV2 |
| GitHub      | 65536      | Markdown   |
| Matrix      | 4000       | Markdown   |
| Mattermost  | 16383      | Markdown   |
| IRC         | 512        | Markdown   |

> **Telegram parse mode:** the Telegram connector uses both `MarkdownV2` and
> `HTML` depending on the path. The majority of send sites (chunked plain text,
> session notifier, rich notifier, tool results, error traces) use `MarkdownV2`
> via `Telegram_format.markdown_to_mdv2`. Rich-render paths
> (`send_chunked_html_with_fallback`) and explicit HTML-formatted slash-command
> output (model show/list/usage, status messages) use `HTML` with a
> plain-text fallback on parse-mode errors. The capability profile advertises
> `MarkdownV2` as the primary mode.

### Progress Delivery Strategy

| Connector   | Strategy              | Card Type     |
|-------------|-----------------------|---------------|
| Teams       | Edit in place         | Adaptive Card |
| Slack       | Edit in place         | mrkdwn text   |
| Discord     | Edit in place         | Markdown text |
| Telegram    | Edit in place         | Text only     |
| Others      | Buffered (no updates) | Text fallback |

### Card Strategy

| Connector   | Strategy      |
|-------------|---------------|
| Teams       | Cards         |
| Telegram    | Buttons       |
| Others      | Text fallback |

### Thread Reply Strategy

| Connector   | Strategy           |
|-------------|--------------------|
| Slack       | Native thread      |
| Discord     | Native thread      |
| Matrix      | Native thread      |
| Mattermost  | Native thread      |
| GitHub      | Thread-like reply  |
| Teams       | Thread-like reply  |
| Telegram    | Thread-like reply  |
| Email       | Thread-like reply  |
| Others      | Room fallback      |

---

## 7. Delivery Observability

### Lifecycle States

Every outbound message passes through tracked lifecycle states:

| State                     | Meaning                                    |
|---------------------------|--------------------------------------------|
| `scheduled`               | Content generated, queued for send         |
| `generated`               | HTTP body fully prepared                   |
| `attempted`               | HTTP request initiated                     |
| `transport_accepted`      | HTTP 2xx received from platform API        |
| `message_id_recorded`     | Platform message ID extracted              |
| `edit_failed`             | PUT to edit returned non-2xx               |
| `fallback_sent`           | New message sent after edit failure        |
| `failed`                  | Delivery definitively failed               |
| `user_visible_unconfirmed`| Sent but no message ID returned            |

Terminal states: `message_id_recorded`, `edit_failed`, `failed`,
`user_visible_unconfirmed`.

### Tracking IDs

Each delivery gets a correlatable tracking ID: `dlv_<timestamp>_<random>`.

All lifecycle events for the same delivery share the same tracking ID.
Query the room activity ledger by tracking ID to trace a message end-to-end.

### Ledger Events

Delivery events are recorded to the room activity ledger with event type
prefix `teams_delivery_`. Each event includes:

- `connector` -- connector name
- `room_id` -- conversation/channel ID
- `tracking_id` -- correlatable delivery ID
- `lifecycle_state` -- current state
- `task_id` -- associated background task
- Optional: `thread_id`, `activity_id`, `message_id`, `error`

Error messages are sanitized before storage.

### Querying the Ledger

The room activity ledger is queried via `clawq rooms ledger` (there is no
top-level `clawq ledger` command, and no `query` subcommand):

```
clawq rooms ledger <list|export|retention-cleanup> [filters]
```

Filters (each takes a value):

| Flag                | Aliases        | Field                    |
|---------------------|----------------|--------------------------|
| `--room-id`         | `--room`       | Room/conversation ID     |
| `--event-type`      | `--type`       | Event type               |
| `--from`            | `--since`      | From timestamp           |
| `--to`              | `--until`      | To timestamp             |
| `--actor`           |                | Actor                    |
| `--profile-id`      | `--profile`    | Profile ID (metadata)    |
| `--thread-id`       | `--thread`     | Thread ID (metadata)     |
| `--task-id`         | `--task`       | Task ID (metadata)       |
| `--background-id`   | `--background` | Background ID (metadata) |
| `--requester`       |                | Requester (metadata)     |
| `--status`          |                | Status (metadata)        |

Examples:

```bash
# Recent delivery failures
clawq rooms ledger list --event-type teams_delivery_failed

# Export all events for a room as JSON Lines
clawq rooms ledger export --room-id <conv_id> --jsonl

# Retention cleanup older than 30 days
clawq rooms ledger retention-cleanup --retention-days 30
```

Querying by `tracking_id` is not exposed at the CLI; use the internal
ledger API (`Teams_delivery_lifecycle.query_by_tracking_id`) for that.

### Failure Surfacing

- **Edit failures** are recorded as `edit_failed` with the error message.
- A fallback new message is attempted automatically.
- **Transport failures** (non-retryable HTTP errors) are recorded as `failed`.
- **Unconfirmed deliveries** (sent but no ID returned) are recorded as
  `user_visible_unconfirmed`.

---

## 8. Troubleshooting

### MS Teams

**Bot not responding to messages:**
- Verify the messaging endpoint is reachable (`clawq tunnel start` for local).
- Check that the bot is installed in the Teams channel.
- Verify `allow_teams` and `allow_users` include the relevant IDs.
- Check logs for JWT verification failures.

**OAuth token fetch fails:**
- Verify `app_id`, `app_secret`, and `tenant_id` are correct.
- Check that the client secret has not expired in Azure AD.
- Run `clawq status teams` to test the connection.

**Empty message errors (HTTP 400):**
- Teams rejects empty text payloads. Clawq guards against this, but
  if you see 400 errors, check that the response content is not empty.

**Adaptive Card edit fails:**
- Edit failures trigger automatic fallback to a new card message.
- Check the delivery ledger for `edit_failed` events.
- Card edits use `Teams_adaptive_card.edit_adaptive_card` (PUT) directly
  without the throttle/retry wrapper.

**Rate limiting (HTTP 429):**
- Outbound requests are throttled to 1 req/s per conversation.
- Retryable statuses (429, 412, 502, 504) are retried up to 3 times
  with exponential backoff.

### Slack

**Socket Mode connection fails:**
- Verify `app_token` starts with `xapp-`.
- Ensure Socket Mode is enabled in the Slack app settings.
- Check that the app token has `connections:write` scope.

**Events API not receiving events:**
- Verify the `events_path` matches the configured Request URL.
- For local development, use `clawq tunnel start`.
- Check that the signing secret is correct.

**Bot not responding:**
- Verify `bot_token` starts with `xoxb-`.
- Check that the bot has the required OAuth scopes.
- Verify `allow_channels` and `allow_users` include the relevant IDs.

**Thread replies not working:**
- Ensure the bot has `channels:history` and `groups:history` scopes.
- Thread replies require `thread_ts` from the original message.

### Discord

**Gateway connection fails:**
- Verify `bot_token` is correct.
- Check that the bot has been invited to the server with proper permissions.
- Ensure MESSAGE CONTENT INTENT is enabled in the Developer Portal.

**Bot not responding in guilds:**
- The bot only responds when @mentioned in guild channels.
- Verify `allow_guilds` includes the server ID (or `["*"]`).
- Check that the bot has Send Messages permission in the channel.

**Resume state issues:**
- Resume state is persisted to SQLite for reconnections.
- Fatal/unrecoverable close codes (4004, 4007, 4009, 4010-4014) auto-clear
  resume state (`should_clear_resume_state`) so the gateway reconnects fresh.
- There is no CLI subcommand to manually clear resume state; clearing it
  currently requires direct DB access.

**Rate limiting:**
- Discord rate limits are tracked per-route with response headers.
- Global rate limits use the `retry-after` header.
- Per-route mutexes prevent concurrent requests to the same bucket.

### Telegram

**Bot not receiving messages:**
- Verify `bot_token` is correct (from @BotFather).
- Check that the bot is not blocked by the user.
- Verify `allow_from` includes the chat ID (or `["*"]`).

**TOTP pairing required:**
- If TOTP is configured, users must pair first with `/pair <code>`.
- Get the code from `clawq otp-show`.

**Voice transcription fails:**
- Voice messages require the voice transcription service.
- Check that the voice file size and duration are within limits.

**Inline keyboard callbacks not working:**
- Callback routing entries are cleaned up after 1 hour.
- Ensure the callback_data matches the registered callback ID.

**Polling stops:**
- The polling loop handles reconnection automatically.
- Check logs for API errors or rate limiting.

---

## Key Source Files

| File                           | Purpose                                    |
|--------------------------------|--------------------------------------------|
| `src/teams.ml`                 | Teams webhook handler, auth, send/edit     |
| `src/teams_progress_card.ml`   | Adaptive Card builder for progress         |
| `src/teams_what_can_do.ml`     | Capability introspection card              |
| `src/teams_delivery_lifecycle.ml`| Delivery state tracking and ledger        |
| `src/teams_auth.ml`            | OAuth token management and JWT verification|
| `src/teams_file_consent.ml`    | File consent card flow (OneDrive)          |
| `src/slack.ml`                 | Slack event handler, send/edit/delete      |
| `src/slack_socket.ml`          | Slack Socket Mode WebSocket client         |
| `src/slack_progress_checklist.ml`| Slack mrkdwn progress renderer           |
| `src/discord.ml`               | Discord message handler, REST API          |
| `src/discord_gateway.ml`       | Discord Gateway WebSocket client           |
| `src/telegram.ml`              | Telegram update handler                    |
| `src/telegram_api.ml`          | Telegram Bot API client                    |
| `src/telegram_poll.ml`         | Telegram long polling loop                 |
| `src/connector_capabilities.ml`| Capability profiles per connector          |
| `src/room_progress_checklist.ml`| Durable checklist model (SQLite)          |
| `src/room_progress.ml`         | Connector-agnostic progress delivery       |
| `src/connector_room_progress.ml`| Per-connector callback dispatch           |
