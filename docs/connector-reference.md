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

The webhook handler (`Teams.handle_webhook`, implemented in
`src/teams_webhook.ml`) processes incoming activities:

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
Socket Mode requires a valid bot token plus app token (`connections:write`);
its signing secret is optional. HTTP Events and HTTP Interactivity require a
valid bot token plus signing secret. A Socket Mode connection never grants HTTP
trust: the HTTP endpoint rejects requests when no signing secret is configured.
Clearing the bot token disables native tasks, huddle tools and thread fallback
for either transport, including during an existing frozen huddle turn.
It supports edit-in-place, reactions, native thread replies, and history
capture.

### Native agent threads and task controls

Clawq implements the current **`agent_view`** experience. Declare
`features.agent_view.agent_description` in the Slack app manifest. Subscribe to
`app_mention`, `message.channels`, `message.groups`, `message.im`,
`app_home_opened`, `app_context_changed`, and `agent_session_stopped`. Enable
Interactivity: Socket Mode carries `interactive` envelopes; HTTP interactions
use the same signed endpoint as Events API callbacks. HTTP acknowledges before
running the model. Delivery retries are deduplicated in a bounded process-local
cache; delivery is not claimed to be exactly once across restarts. Slackbot
(`USLACKBOT`) messages never start tasks.

Native `agents.sessions.setStatus` and `agents.sessions.rename` require
`chat:write` and use **`channel_id` plus the root `thread_ts`**, in channels and
DMs. There is no Slack run/session ID in these responses. Clawq sets
`processing`, starts an owned `chat.startStream` progress marker where supported,
and explicitly restores `active` on success, failure or interruption using
`chat.stopStream` and `setStatus`. Posting a reply alone does not clear native
processing. Status writes are serialized per root until terminal cleanup
completes, so an old run cannot clear a newer run's processing state. The
aggregate native `status` may differ from this app's `agent_status` when other
agents are working. Custom tool progress and final messages remain ordinary
messages in the originating thread.

Slack streams have a ten-minute maximum lifetime; processing status expires
after an hour without a refresh. While its run still owns the root, Clawq
closes and replaces the marker every nine minutes and refreshes processing.
Terminal cleanup and connector/daemon shutdown stop maintenance and join every
run's in-flight UI cleanup, including queued requests that never activated,
before releasing ownership. Shutdown during preparation retains the request
with a failure reason and prevents model entry; restart interrupts are preserved.
Shutdown closes admission for that manager before cleanup is collected, so an
in-flight authorization/history reply cannot create a later run. Shutdown is
terminal for that manager; a restarted daemon creates a fresh manager. Ordinary
Socket reconnects and config reloads keep the manager open; they do not invoke
terminal shutdown. Terminal Socket teardown joins admitted control POST replies
and finishing control updates before cancelling delivery handlers, so their
returned timestamps remain available for cleanup. If streaming is unavailable,
processing refreshes continue and the public button remains the stop control.

A session key includes the workspace, app, channel and original root timestamp.
Root mentions and replies from authorized humans share that thread's history;
other roots remain separate. Existing room bindings retain their models,
instructions, tool denials and CWD inheritance; runtime profile reloads apply to
live threads. Exact-thread bindings and access scopes apply to the actual tool
snapshot and Slack egress, with room inheritance; the authorization room and
persisted `channel_id` remain the Slack channel ID. The reversible session key
survives eviction/restart and routes resumed, background and fallback completion
to the same root. The last
executing human is persisted for a freshly authorized restart run; old controls
are discarded on restart. Native restart execution rechecks that human's current
role and verified room policy after preparation and any pre-model compaction or
notification wait, then records a fresh room snapshot. The automatic resume
prompt supplies no Slack message authority for huddle tools. No caller authority
is restored from quoted history.

Automatic continuation, enabled keepalive, opted-in heartbeat and background
completion turns in native threads use the same current authorization and owned run lifecycle.
They recheck the persisted human (or the completion task's original human)
after Session preparation, refresh the verified room snapshot, and own their
processing stream and Stop control through cleanup. Synthetic completions queued
while busy retain that human separately from queued messages and reauthorize on
drain or restart replay. They supply no Slack message identity or huddle-tool
authority; missing human-request provenance remains a retained request.
Unmarked native input, including cron work without original human provenance,
is refused instead of borrowing the last thread owner.

Ad hoc leading `@template` syntax in a native turn stays on the owned Session
path; it does not launch a separate template agent. Existing room/thread agent
template bindings still apply. Direct CLI/session injections and unprofiled cron
jobs aimed at a native thread key have no verified Slack human/message/room
provenance and are refused with that reason. Submit a fresh Slack message for
new work. Profile-backed routine jobs use their separate worker session.

Deferred requests keep their original human, message, room and installation
route in the existing durable queue. Every drain owner and boot replay freshly
authorizes that requester before model/tools and activates and finishes that
request's native controls. Fresh native requests use the same final execution
boundary as deferred work: current admin membership and room policy are checked
after yielding preparation and before model entry. Verified shared, external,
group and DM classification stays distinct and supplies the persisted snapshot
and policy decision; the room ID remains the actual channel. Missing provenance
or revoked access retains the request with a failure reason; it never substitutes
the previous thread owner.
Mixed-author `/followup-append` inputs remain separate requests. Original-text
question answers, leading `!` interrupts and authorized admin `stop` are handled
before history is added to the model prompt or ordinary work is deferred.
An owned `!change direction` interrupts the active task and queues the normalized
`change direction` as a fresh authorized request with its original sender/message;
bare admin `stop` is consumed without a followup. Other humans cannot interrupt
the owned task through text controls.

Retained status send/edit/delete callbacks, persisted fallback and origin-bearing
room progress select current manager configuration at each write. Credential,
channel or exact-thread egress revocation refuses delivery; a refused live
notifier cannot recover through an old captured token. Valid legacy child-thread
progress keeps the room/root encoded in its typed key and must match the task
origin; arbitrary supplied thread locators are refused.

Before reading context, Clawq checks current configuration, workspace identity,
human identity/guest flags, bot and requesting-human channel membership,
private/shared-room policy and room egress. History uses `conversations.replies`
(up to five pages, 500 messages, 48 KB of source text and 6 KB per message),
including the root, sorted chronologically and deduplicated by timestamp.
Missing scopes, rate limits and incomplete/limited retention are reported as
unavailable or partial context; pagination cannot recover hidden history. Bot
and user history require the relevant `channels:history`, `groups:history`,
`im:history` or `mpim:history` scopes. Metadata checks additionally require the
relevant `channels:read`, `groups:read`, `im:read`, `mpim:read` and `users:read`.
Only this app's bot is labeled as assistant source; all other authors, bot
messages, attachments and browsing context remain quoted untrusted data.
Attached-file metadata in thread context, including files inside shared or
forwarded message attachments, is limited to id, name, title, mimetype,
filetype, size, user and created (no previews, lines or transcriptions).

Public/group channels require typed `is_private` and bot-membership metadata;
unknown privacy fails closed. A documented one-to-one DM instead uses
`is_im`, its exact human peer (`user`), fresh `users.info` human validation,
and the authenticated installation identity. The optional `is_user_deleted`
field may be absent; explicit deletion or malformed supplied values are denied.
It does not require channel-only `is_member`/`is_private` fields. DMs are private
for configuration policy: under the default `deny` policy, explicitly list the `D...` conversation in `allow_private_channels`
as well as `allow_channels`, or choose `allow_if_listed` with `allow_channels`.

Each run has a public **Stop this task** button with an opaque app-owned value.
Clawq verifies the clicking human is the authorized run owner, installation,
originating message and live generation. Queued controls remove only that
request from the existing durable Session queue; active stops use the existing
cooperative interrupt path. A provider/tool that has not yielded to the interrupt
check may finish its current operation before stopping. Native
`agent_session_stopped` is accepted only when `streaming_message_ts` names a
recorded live stream owned by the same run. **Empty-stream native stops are
ambiguous and are acknowledged without cancelling a run**; use its public
button. Delayed native events and stale buttons cannot cancel a successor.

`feature_disabled`, missing native scopes, unsupported tokens or unavailable
streaming leave ordinary thread replies/progress and public buttons available
when their own permissions allow. Errors are logged with the actual method and
capability failure; Clawq does not switch app experiences or credentials.
`app_home_opened` with `tab: messages` and standalone `app_context_changed` are
acknowledged without creating a task. The published standalone context event has
no reliable human/thread binding, so Clawq never fans it into execution threads.
Conditional `message.im.app_context` is quoted only with that message and grants
no channel access. Legacy `assistant_view` / `assistant_thread_*` compatibility
is deliberately not implemented.

References: [agent sessions](https://docs.slack.dev/ai/agent-sessions/),
[agent_view migration](https://docs.slack.dev/ai/migrating-to-agent-messaging/),
[setStatus](https://docs.slack.dev/reference/methods/agents.sessions.setStatus/),
[rename](https://docs.slack.dev/reference/methods/agents.sessions.rename/),
[native stop event](https://docs.slack.dev/reference/events/agent_session_stopped/),
[conversation metadata](https://docs.slack.dev/reference/methods/conversations.info/),
[stream lifetime](https://docs.slack.dev/reference/methods/chat.startStream/),
[thread context](https://docs.slack.dev/ai/agent-context-management/).

Clawq stays silent in its relay threads unless explicitly @-mentioned. It also
starts no turn for a top-level message mentioning a peer assistant, or a reply
whose root mentions a peer or whose fetched thread contains a peer post, unless
the message explicitly @-mentions Clawq. This also applies in authorized DMs.
Peer apps reuse
`channels.slack.standing_orders.allowed_targets`. Mentioned users are resolved
from thread evidence or `users.info` (cached for six hours for peers, ten minutes
for non-peers); lookup failures and unreadable threads fail open. An empty
allowlist adds no silence or peer lookup. Pause commands run before these gates.

### Huddles and conditional transcripts

The full daemon registers two read-only tools through its integration registry:

| Tool | Required arguments | Optional arguments |
| --- | --- | --- |
| `slack_huddles` | `channel_id`, `oldest`, `latest` | `cursor`, `limit` (1–15, default 15) |
| `slack_huddle_content` | `channel_id`, `huddle_ts` from discovery | `cursor`, `thread` (`huddle`/`canvas`, default `huddle`) |

Use an explicit C/G channel ID and Unix timestamp strings. Discovery accepts a
window of at most 31 days and reads one `conversations.history` page, retaining
only `huddle_thread` messages with room metadata. Repeat the identical query
with `next_cursor` when a cursor is returned. `pagination_exhausted` means Slack
returned no further page; `complete` additionally requires that Slack has not
reported `is_limited`. `history_coverage` identifies limited history or remaining
pages. When `is_limited` is true, the workspace history limit hides older
messages, and pagination cannot recover them. An empty limited page is not an
exhaustive negative search. The tools support channels, not direct messages.

Content retrieval revalidates the exact history source on every call, reads
one `conversations.replies` page (15 messages) for the selected huddle thread
or its documented `canvas_thread_ts` (`thread="canvas"`), and hydrates up to eight unique
file references through `files.info`, including returned
`huddle_transcript_file_id` references. Slack Connect `check_file_info` references
are hydrated by ID. A file ID supplied by the caller is never accepted as an
access grant. No returned download URL is fetched.

Results separate `thread_notes`, file previews/summary references, and
`transcripts`. A transcript contains only actual nonempty returned
`file.huddle_transcription` blocks/lines, with its file ID and source method.
Discussion, generic highlighted transcription, previews and summary IDs do not
establish a huddle transcript. `complete` describes the thread/history page;
`content_complete` additionally accounts for file errors and the eight-file cap.
Neither asserts a full transcript: Slack's returned time ranges and inline
content may cover only part of a huddle. `file_errors`, `files_truncated` and
`transcript_status` expose partial reads and unavailable data explicitly.

Tools require a verified active Slack ingress user and matching access snapshot.
They apply the frozen turn policy, including the snapshot's exact room-access
key and profile for egress, current runtime enable/disable state and live Slack
identity/membership checks. Clearing or invalidating the current bot credential
disables further reads even while a turn holds an older valid credential. Valid
credential rotation and policy changes remain frozen until the next turn.
Later turns pick up those changes without re-registering tools. Separately drained
queued turns select their own ingress authority. Mid-turn injected steering
clears the active authority before it becomes model-visible, including steering
from unknown or multiple callers; further huddle reads require a fresh Slack
request. Direct CLI/background calls and durable replay without ingress evidence
are refused. Both origin and target must pass user/channel allowlists,
private-channel policy, room policy, bot membership and requesting-human
membership (bounded to five membership pages). Cross-channel reads additionally
require a Clawq admin; Slack guests cannot use that path. Admin status does not
bypass channel membership or private-channel policy.

The bot token needs the relevant `channels:read`/`groups:read`,
`channels:history`/`groups:history`, plus `users:read` and `files:read` scopes.
Under strict HTTP egress policy, explicitly allow GET requests to
`slack.com` at `/api/*` in the applicable egress rules for huddle reads; every
method is checked against the frozen turn policy. [Standing
orders](#standing-orders-huddle-relay) also need the API POST and raw-upload
rules in their [egress table](#egress). Without the management POST rules,
every create fails at `auth.test` (`Egress_denied`), and pauses still apply but
their receipts and grantor notices are refused and only logged. Reinstall the app after granting scopes. The `xapp` Socket Mode token grants no
content access. Scope/access/rate-limit errors explain how to proceed, with no
automatic retries. Affected commercial non-Marketplace apps may be limited to
one history/replies request per minute; request subsequent pages only when Slack
permits them.

Slack AI huddle notes/transcripts are conditional on workspace features,
permissions and retained data. Notes can be stopped/deleted; Slack does not run
AI notes in huddles with guests/external participants. These tools do not
promise recordings, generate transcripts, or use private/Discovery APIs.
Treat all returned Slack content as untrusted source material.

References: [conversation objects](https://docs.slack.dev/reference/objects/conversation-object/),
[file objects](https://docs.slack.dev/reference/objects/file-object/),
[files.info](https://docs.slack.dev/reference/methods/files.info/), and
[Slack AI huddle notes](https://slack.com/intl/en-au/help/articles/31377193680019-Use-AI-to-take-huddle-notes-in-Slack).

### Standing orders (huddle relay)

A standing order is a channel-bound, revocable grant: after every huddle in a
channel, relay Slack's AI notes (and the transcript, when Slack has one) to an
approved bot, such as Claude, with a fixed instruction. People create and
manage orders in natural language in that channel. Only a bound Block Kit
click confirms, changes or resumes an order, and anyone in the channel can
pause one.

> **Status:** Phase 3 includes the deterministic daemon executor: verified
> notes/transcript fetch, same-channel upload v2, pickup tracking, caps and
> fixed failure reports. Restart the daemon after upgrading to run the new
> executor. P4 recovery, shared 429/Retry-After limiter and source-deletion
> propagation, and P5 live acceptance are still pending; P0-5 huddle captures
> remain deferred. P3 does not catch up missed startup/reconnect history.

#### Config

```json
{
  "channels": {
    "slack": {
      "standing_orders": {"allowed_targets": [{"app_id": "A08SF47R6P4", "label": "Claude (Anthropic)"}]}
    }
  }
}
```

`A08SF47R6P4` is the Claude (Anthropic) Slack app. A bot's app id is
`bot.app_id` in `bots.info`.

| Key (`channels.slack.standing_orders.*`) | Meaning | Default |
| --- | --- | --- |
| `enabled` | Kill switch. `false` makes every order inactive (nothing is created, confirmed, changed, resumed or relayed) without suspending any. A non-boolean value also turns the feature off. | `true` |
| `allowed_targets` | Bots an order may relay to: `{"app_id", "label"}` objects, or bare `A…` app ids (the label is then the id). The trimmed label (1–80 characters) is the only target name cards and notices show. Malformed and repeated entries are dropped with a warning. **An empty allowlist denies every order**, so the feature is off until you add a target. | `[]` |
| `creator_policy` | Who may create an order: `member` (any verified human channel member who is not deleted, not a guest and passes `allow_users`) or `admin` (clawq admins only). An unknown value falls back to `admin`. | `"member"` |

The daily relay cap defaults to 10 per order over the preceding 86400 s,
including re-posts; it is not a config key.
`security.tools_enabled` must also be true. Config changes apply live: the
next check reads the current config, and a confirm or change card made before
a config change answers "ask again to get a new card" when clicked.

#### Tools

The full daemon registers seven tools beside the huddle tools:

| Tool | Arguments | Risk |
| --- | --- | --- |
| `slack_standing_order_create` | `instruction` (≤ 1000 Unicode code points), `target` (an approved bot's name or mention), optional `when_no_transcript` (`relay_notes`, the default, or `skip`) | Medium |
| `slack_standing_order_list` | none | Low |
| `slack_standing_order_show` | `order` (`SO-n`) | Low |
| `slack_standing_order_pause` | `order` | Low |
| `slack_standing_order_resume` | `order` | Low |
| `slack_standing_order_cancel` | `order` | Low |
| `slack_standing_order_change` | `order`, `instruction`, optional `when_no_transcript` (kept when left out) | Medium |

No tool takes a grantor, team or channel argument. Each acts for the verified
sender of the current Slack message, only on orders of that message's channel,
and refuses steering, synthetic and queued-drain turns. A missing order and
another channel's order get the same "There is no standing order SO-n in this
channel." reply. Users see `SO-n`, a per-channel number; ids stay opaque.
`list` and `show` mark the instruction as untrusted data. The builtin `ops`
template allows only `list`, `show` and `pause`; no builtin template can create,
change, resume or cancel.

**Profile allowlists.** A room profile with an `allowed_tools` list must list
each management tool its members should use. Creating, confirming, changing
and resuming an order also need the three capability names `slack_huddles`,
`slack_huddle_content` and `slack_standing_order_fire`. Each is decided
separately, deny wins, on a `standing_order` access snapshot keyed
`slack:<channel_id>`, and all three must be allowed. A deny-list profile that
denies any of them blocks orders in that room. A capability deny makes no Slack
call for the check; on a click, the code-rendered refusal is the only Slack
call.

#### Egress

Every Slack call an order makes is a method from this closed list. Each call is
checked against the egress rules for `slack:<channel_id>` and audited under its
snapshot:

| Method | Verb |
| --- | --- |
| `auth.test` | POST |
| `bots.info` | GET |
| `users.info` | GET |
| `users.list` | GET |
| `conversations.info` | GET |
| `conversations.members` | GET |
| `chat.postMessage` | POST |
| `chat.update` | POST |
| `conversations.history` | GET |
| `conversations.replies` | GET |
| `files.info` | GET |
| `canvases.getContent` | POST |
| `reactions.get` | GET |
| `files.getUploadURLExternal` | GET |
| `files.completeUploadExternal` | POST |
| `files.delete` | POST |

Under strict egress, allow GET `slack.com` `/api/*` (as for huddle reads) plus
POST `slack.com` `/api/auth.test`, `/api/chat.postMessage`,
`/api/chat.update`, `/api/canvases.getContent`,
`/api/files.completeUploadExternal` and `/api/files.delete`.
`files.getUploadURLExternal` uses GET. Without the management POST rules every
create fails at `auth.test` (`Egress_denied`), and pauses still apply but their receipts and grantor
notices are refused and only logged. `clawq slack orders resume` makes these
calls from the CLI process too.

Upload egress: allow POST to `https://files.slack.com/` (all paths); no redirects are followed.

The raw upload is separate from the Slack API method table. It sends binary
content headers without an Authorization header. Policy checks the actual
upload capability URI, while logs, audit inputs and errors redact it. Every
API call and raw upload requires a persisted order/fire snapshot and strict
audit insertion before HTTP; an unavailable audit blocks the request. Allow
`canvases:read`, `reactions:read`, `files:read` and `files:write` in addition to
the channel/history/user scopes above, then reinstall the app after scope changes.

#### Executor delivery and pickup

The worker serializes fires per channel using DB leases and spaces posts by
at least 1 s. It rechecks current config, all three capabilities and live grant
authority before relay writes. It verifies the ended huddle root, the canvas
on the root and Slackbot notes-ready reply, and Slackbot's same-channel share.
`room.transcript_file_id` names the notes canvas, not the transcript. An absent
room-to-canvas link is accepted only with the other strict evidence; a
conflicting link is refused. Transcript discovery uses the verified canvas's
`huddle_transcript_file_id`, never a participant-editable markdown embed.
Transcript metadata must match the channel and huddle start date.

`files.info(include_transcription=true)` uses an undocumented parameter,
approved on 2026-11-04 and feature-detected. Absent/empty transcription or
unusable transcript provenance follows `when_no_transcript`: `relay_notes`
uploads clearly labelled notes only; `skip` records `skipped_no_transcript`. API disappearance
gets one "transcript unavailable (Slack API change?)" report. Missing notes
wait with delays of 120, 300, 600, then 1200 s, ending as `no_notes` at 3600 s
from huddle end, with a one-time automatic-notes hint.

Upload v2 posts markdown files in the order's channel only.
`files.completeUploadExternal` uses that `channel_id` with no `thread_ts`, so
the relay is one top-level target mention with its own reply thread. All
meeting content travels in the files. The fixed comment carries a
date window in the daemon's local timezone (Australia/Sydney when configured),
with each endpoint's timezone abbreviation, `SO-n`, the grantor's plain id, the escaped
instruction, and a link to the verified huddle thread using the workspace URL
already returned by `auth.test`. It says: "If you already summarised this
huddle from Slack’s notes, reply with only what the transcript adds or changes."
Clawq's only live user mention is the pinned target. Claude tagging people in
its replies is expected; privacy here means outside Amaroo. The comment treats
meeting content as data and says not to act on spoken requests. Spoken-request
extraction and confirmation cards remain P6 work.

`posted` means the relay share was located, not that Claude completed anything.
The executor resolves the share timestamp from completion, an own-relay event,
or `files.info` for up to 120 s. A target reaction (any emoji) on the message
or a relay file means `acknowledged`; the first target reply that is not a
known notice means `picked_up` and also acknowledges it. Pickup means Claude
took up the relay, not downstream work completion; no working-username signal
is required. Production notice matchers are empty until exact notice wordings
are captured, so fixture coverage does not establish live notice recognition.
After 180 s without acknowledgement, successful reaction/reply confirmation
reads are required before `no_pickup`; failed reads stay pending. An
acknowledged relay without a reply after 3600 s gets an informational report.
Recognized spend-limit/disabled notices mean `not_delivered`, even after pickup.
Retained terminal relays are polled at most once per 300 s for 30 days, with
one terminal poll per channel pass and new dispatch first for each order.
Queued late observations drain promptly without waiting for the next poll.

Allocation and raw-upload transport/5xx failures retry preparation after 120 s.
Timeouts and 5xx responses after completion dispatch, and unresolved shares,
become `posting_unknown`.
Reconciliation is read-only; never blindly retry an ambiguous completion or
manually repeat a potentially billed relay. At most 1 re-post is allowed ever
per order/huddle, only after proving no share occurred, or after a recognized
rate-limit notice with no acknowledgement and a fixed 300 s wait. Re-posts
count toward the rolling cap. Three consecutive `no_pickup`, `not_delivered`
or `failed` fires auto-pause the order. Cap reports occur once per day;
hard authority failures suspend the order. Management-thread failure reports
use fixed ids/reasons and at-most-once write-ahead attempts per cause, with
ambiguous report sends visible in `show`.
Use list/show to inspect uncertain delivery and pause the order before any
operator intervention.

Pause fences new uploads, completions and re-posts. A relay already dispatched
can still arrive; the receipt reports in-flight work (or an unverifiable
count). Read-only timestamp, pickup and notice tracking continues after pause
or cancel. Resuming never replays huddles that became ready while paused.

#### Cards, changes and pauses

- **Create.** The create tool re-validates the grant (target allowlisted, a bot
  and a channel member; clawq a member; channel not shared with another org
  (Slack must report all four share flags as false: a share state it leaves
  out is refused as temporary, so try again later) and without guests; creator
  policy; all three capability names; no other live
  order for this channel and target) and posts a code-rendered confirm card in
  the request's thread: when, do, the instruction in full as inert plain text,
  what happens with no transcript, who it runs as, spoken requests, privacy,
  the daily limit and an AI-notes check. Nothing is active yet. The order waits
  as `pending_confirmation` with its `SO-n`, and the card expires in 10
  minutes. Only the requester's **Confirm** click activates it. That click
  re-runs every check, and its Slack `action_ts` becomes `active_since`, so
  earlier huddles are never eligible. A click without a valid `action_ts` is
  refused. **Cancel** (the requester or a clawq admin) withdraws it. Double
  clicks confirm once. On confirm clawq posts a top-level activation notice with
  a **Pause** button. If that notice can't be posted, the order is paused and
  the failure is reported in the request's thread.
- **Change.** `slack_standing_order_change` posts a change card and changes
  nothing yet. Only the requester's **Confirm change** click applies it, and
  only while the requester is the order's grantor or a clawq admin. The click
  re-runs every check, then in one transaction cancels the old order and starts
  its successor as active with the next `SO-n`, the new instruction, the same
  channel and target, the clicker as grantor and a fresh daily cap. Changing a
  paused or suspended order starts relaying again, and the card says so.
- **Resume.** The resume tool only posts a resume card. Only the grantor's or a
  clawq admin's click resumes. The click re-runs every check, resets
  `active_since` to the click time and bumps the order's epoch. Huddles that
  became ready while the order was paused are never relayed.
  If the order has no bound activation notice, Slack resume repairs it and
  fail-safe pauses again if the notice fails; ordinary resumes reuse the
  order's existing notice across epochs.
- **Cancel.** The grantor or a clawq admin, who must also be a channel member.
- **Pause.** Pause doesn't depend on the LLM. It has three paths: an exact
  `pause SO-n` message (case-insensitive, optionally after one leading user
  mention and before a final `.` or `!`), the **Pause** button on the
  activation notice, and the pause tool. Anyone may pause who is a member of
  the channel and whom `users.info` reports as a human who is not deleted and
  not a guest, from an event verified for this team and app. `allow_users`,
  room policy, admin status and the capability names are not required. The
  message parser and the button run before chat authorization and before
  relay-thread silence, so a pause works when the LLM provider is failing. A
  human's `thread_broadcast` reply ("Also send to channel") reaches the parser
  too, and never starts a turn. Slackbot messages never pause. Pausing a
  pending order withdraws it. Pause paths normally return a code-rendered
  receipt or reason in the thread, with the silent cases below. The grantor is
  notified only when someone else pauses or withdraws the order. A grantor's
  own pause and outcomes that change nothing (already paused or not found)
  send no notice. A typed `pause SO-n` in a channel with no standing order at
  all is consumed and logged with no Slack post, so clawq never speaks in a
  channel no grant check admitted. A typed `pause SO-n` whose store read fails
  is consumed with no Slack post, and the failure is logged at warn with the
  cause. A button or tool pause that hits an unreadable store may get a
  "Pause not applied" reason or the generic internal-error reply. A button
  whose card can't be read is ignored and logged. In every store-read failure
  case the order is unchanged.
  Huddles that become ready while an order is paused are never relayed, even
  after a resume. Pause messages sent while the daemon is down or shutting down
  are not seen.

Management replies, receipts and notices are fixed templates with ids and
enumerated reasons. They never quote Slack error bodies, display names or file
titles (the operator's target label is the one exception), and never mention
the target bot.

#### Observation, privacy and rollback

The huddle observer records huddles only in channels with an active or paused
order for this team and app. It never calls Slack or runs an LLM. Private
channels follow `private_channel_policy` and `allow_private_channels`
unchanged (see [Private Channel Policy](#private-channel-policy-b735)): an
order can be created and checked only in a private channel that policy
admits. Binaries older than fail-closed access-snapshot decoding read a
`standing_order` snapshot as `room_turn`, so once orders exist, don't roll back
past that change.

#### CLI

`clawq slack orders list [--channel C] | show ID | fires ID [--limit N] |
pause ID | resume ID | cancel ID` gives operator parity. Reads are open;
mutations need `CLAWQ_ADMIN=1`, a refused one is audited, and each applied one
writes a room ledger row with actor `cli`. `ID` is the opaque id `list`
shows. CLI pause and cancel post nothing to Slack and don't notify the grantor.
CLI resume re-runs the resume checks with Slack. An order without a bound
activation notice must be resumed from Slack so the channel gets its notice.
Orders are created and changed
only in Slack. See the [CLI reference](cli-reference.md#clawq-slack).

### Socket Mode connection recovery

Socket Mode reconnects automatically after a transport failure or Slack's
`disconnect` controls (`warning`, `refresh_requested`, and `link_disabled`).
Each attempt obtains a fresh WebSocket URL. Opening the connection and receiving
Slack's `hello` each have a 30-second deadline. After `hello`, Clawq sends a
WebSocket ping every 15 seconds and requires the matching pong within 10 seconds,
so a silent or half-open connection cannot remain connected indefinitely.
Retries use exponential delays from 1 to 30 seconds and continue while the
connector runs. A connection that stays healthy for at least a minute resets
that delay; stopping the connector cancels retries and closes the transport.

`clawq status` includes the live Socket Mode state (`connecting`, `connected`,
`reconnecting`, or `stopped`), recovery reason, retry delay, and reconnect count.
The daemon writes this health snapshot to
`$CLAWQ_HOME/slack_socket_status.json` (default `~/.clawq/`); stale snapshots or
snapshots from an exited process report a disconnected state. Connection state
changes and retries also appear in the daemon logs.

Every envelope is acknowledged, including duplicates. Event delivery is
deduplicated by Slack's `event_id` (falling back to `envelope_id`) across
connections, with a bounded cache of 4096 accepted events. Accepted work
continues during a socket refresh; failures are logged. The cache is in memory,
so this does not guarantee deduplication across daemon restarts. Slack Socket
Mode does not provide a replay guarantee for messages sent during an outage;
automatic recovery restores delivery of newly received events without backfilling
channel history.

Protocol references: [Slack Socket Mode](https://docs.slack.dev/apis/events-api/using-socket-mode/)
and [WebSocket ping/pong](https://www.rfc-editor.org/rfc/rfc6455#section-5.5.2).

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
| `signing_secret` | Signing secret for HTTP request verification   | required for HTTP; optional for Socket Mode |
| `app_token`      | App-Level Token for Socket Mode (xapp-...)     | required for SM  |
| `socket_mode`    | Use Socket Mode instead of HTTP Events API     | `true`           |
| `events_path`    | HTTP path for Events API webhooks              | `/slack/events`  |
| `allow_channels` | Channel ID allowlist                           | `["*"]`          |
| `allow_users`    | User ID allowlist                              | `["*"]`          |
| `allow_private_channels` | Explicit opt-in list for private channels (required under `deny` policy) | `[]` |
| `private_channel_policy` | `"deny"` (default) or `"allow_if_listed"` — controls whether private channels in `allow_channels` are permitted | `"deny"` |
| `event_log` | Phase 0 diagnostic: metadata-only JSONL of every Slack event that reaches ingress (except `url_verification` handshakes, events received while the daemon is shutting down, and redeliveries already seen by event-id dedup) at `$CLAWQ_HOME/slack-events.jsonl` (0600, rotates at 16 MiB to `.1`); `capture_text_from` adds full text only for an exact `{channel, bot_id}` pair | `{"enabled": false, "capture_text_from": []}` |
| `standing_orders` | Huddle relay grants: `enabled`, `allowed_targets`, `creator_policy`; see [Standing orders](#standing-orders-huddle-relay). An empty `allowed_targets` denies every order | `{"enabled": true, "allowed_targets": [], "creator_policy": "member"}` |

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

The Slack event handler (`Slack.handle_event` in `src/slack.ml`) processes incoming events:

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

The polling loop handles reconnection and backoff automatically. A timed-out
`getUpdates` is retried once immediately before an error is surfaced, and
repeated identical timeout errors are logged as a single WARN line per
5-minute window (with an occurrence count) instead of one ERROR per poll
(B825). Auth, 409 conflict, and webhook errors are always surfaced.

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
- The polling loop handles reconnection automatically; a timed-out
  `getUpdates` is retried once, and repeated identical timeout errors are
  deduplicated to one WARN line per 5 minutes (B825).
- Check logs for API errors or rate limiting.

---

## Key Source Files

| File                           | Purpose                                    |
|--------------------------------|--------------------------------------------|
| `src/teams.ml`                 | Public Teams channel facade                |
| `src/teams_webhook.ml`         | Teams webhook handler and channel startup  |
| `src/teams_api.ml`             | Teams auth facade, send/edit/delete, cards |
| `src/teams_progress_card.ml`   | Adaptive Card builder for progress         |
| `src/teams_what_can_do.ml`     | Capability introspection card              |
| `src/teams_delivery_lifecycle.ml`| Delivery state tracking and ledger        |
| `src/teams_auth.ml`            | OAuth token management and JWT verification|
| `src/teams_file_consent.ml`    | File consent card flow (OneDrive)          |
| `src/slack_api.ml`             | Slack event parsing, auth, send/edit/delete |
| `src/slack.ml`                 | Slack event handler                        |
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
