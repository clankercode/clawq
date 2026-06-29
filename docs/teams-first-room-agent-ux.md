# Teams-First Room-Agent UX

This document covers the user experience for room-origin background tasks
delivered through chat connectors. Teams is the primary design target;
Slack provides a baseline text-only experience.

## Production Flow (Teams)

### Request Lifecycle

1. A user sends a message in a Teams channel (group chat or DM).
2. The webhook handler (`teams.ml`) verifies JWT auth, deduplicates the
   activity, strips `@mention` tags, and resolves a session key.
3. In group chats, the bot only responds when explicitly @mentioned.
4. The message is dispatched through `Session.turn`, which invokes the
   configured agent/runner.
5. Typing indicators are sent every 3 seconds while the session is active
   (up to 5 minutes idle).
6. The final response is delivered as a reply to the original message.

### Slash Commands

Teams supports the full slash-command surface: `/help`, `/reset`,
`/compact`, `/status`, `/model`, `/bg`, `/background`, `/cron`,
`/tasks`, `/whatcando`, and agent/model/config menus rendered as
Adaptive Cards. Commands typed as `clawq <subcommand>` are normalized
to `/<subcommand>`.

### Session Keys and Threading

- **DMs and unbound rooms:** Session key = `teams:<team_id>:<conversation_id>`.
  Thread replies get a thread-aware key: `teams:<team_id>:<conversation_id>:thread:<reply_to_id>`.
- **Profile-bound rooms:** Session key = `teams:<sanitized_conversation_id>`.
  All messages in the room share one session regardless of thread.

### Room Profile Binding

When a room is bound to a profile (via `/rooms_memory bind`), the session
scope changes to room-level, connector history capture activates (if
enabled), and room-scoped memory becomes available. The `/whatcando`
command shows the current binding state.

### Access Control

- **Admin/guest:** Determined per-user in the database. Admins see all
  commands; guests see a filtered set via `gate_admin`.
- **Room policy:** Per-profile tool allow/deny lists. Card action buttons
  (Inspect, Continue, Cancel) check room policy before executing.
- **Team/user allowlists:** `teams.allow_teams` and `teams.allow_users`
  in config restrict who can interact with the bot.

## Slack Baseline

Slack uses the same room-progress checklist model as Teams but renders
as plain mrkdwn text (no Adaptive Cards).

### Capabilities

| Feature           | Slack | Teams |
|-------------------|-------|-------|
| Edit messages     | Yes   | Yes   |
| Delete messages   | Yes   | Yes   |
| Reactions         | Yes   | No    |
| Typing indicator  | No    | Yes   |
| Status messages   | Yes   | Yes   |
| Adaptive Cards    | No    | Yes   |
| Buttons           | No    | Yes   |
| Thread replies    | Native | Thread-like |
| History capture   | Ambient | Ambient |
| Max message length| 4000  | 28672 |

### Progress Rendering

Slack renders checklists as formatted mrkdwn:

```
:icon: *Task label*
:icon: 3/5 done | 1 current, 1 blocked

:white_check_mark: *Implement auth* — <https://...|transcript>
:arrows_counterclockwise: *Write tests* (working)
:no_entry_sign: *Deploy* (blocked)
```

Links use Slack's `<url|label>` syntax. Blocked items show only a
generic "(blocked)" indicator -- the checklist model does not expose
internal dependency details.

## Progress Cards and Checklists

### Checklist Model (`room_progress_checklist.ml`)

The checklist is a durable, append-only model backed by SQLite. Each item
tracks:

| Field              | Description |
|--------------------|-------------|
| `task_id`          | Background task association |
| `title`            | Human-readable step description |
| `state`            | Lifecycle state (see below) |
| `transcript_url`   | Link to runner transcript |
| `session_url`      | Link to runner session |
| `session_record_id`| Reference to room session record |
| `delivery_state`   | Room notification status |

#### Item States

| State     | Icon | Meaning |
|-----------|------|---------|
| `Planned` | `[ ]` white square | Known but work not started |
| `Current` | `[~]` arrows | Actively being worked on |
| `Blocked` | `[!]` prohibited | Waiting on external dependency |
| `Done`    | `[x]` check | Complete, task continues |
| `Final`   | `[*]` flag | Terminal state for the task |

State transitions reset `delivery_state` to `pending` so the room gets
notified.

#### Delivery State

Each item tracks whether the room was notified:

| State       | Meaning |
|-------------|---------|
| `pending`   | Not yet sent |
| `sent`      | Sent but not confirmed |
| `confirmed` | Room received the update |
| `failed`    | Delivery failed (includes reason) |

`query_pending_delivery` returns items needing retry (pending, sent, or
failed).

### Teams Adaptive Card (`teams_progress_card.ml`)

On Teams, progress is rendered as an Adaptive Card (v1.3) with:

1. **Header** -- color-coded by overall state. The internal hex colors
   map to Adaptive Card semantic TextBlock colors: blue (`#3B82F6`) ->
   `Accent`, green (`#10B981`) -> `Good`, red (`#EF4444`) -> `Attention`,
   grey (`#6B7280`) and purple (`#8B5CF6`) -> `Default`.
2. **Summary line** -- compact count by state (e.g. "3 done, 1 current,
   1 blocked").
3. **Elapsed time** -- shown when available.
4. **Checklist container** -- each item as a TextBlock with icon, title,
   state label, and clickable links (transcript, session, record).
5. **Action buttons** -- context-sensitive controls (see below).

#### Action Buttons

Buttons appear on terminal cards and are gated by room policy:

| Button   | Command | Shown when |
|----------|---------|------------|
| Inspect  | `/background show N` | Always (if policy allows) |
| Continue | `/background resume N` | Task can be resumed |
| Cancel   | `/background cancel N` | Task is running/queued |
| Retry    | `/background retry N` | Task failed |
| View Logs| `/background logs N` | Task has a log path |
| Finalize | `/background finalize N` | Task has dirty worktree |

Progress card buttons are rendered as `Action.Submit` with Teams `imBack`
data values (e.g. `/background show N`, `/background resume N`). When the
user clicks a button, the value is sent as a regular message and routed
through the normal slash-command handler. Room policy is applied at card
render time -- buttons for denied tools are omitted entirely.

Separately, Teams invoke payloads (`task/inspect`, `task/continue`,
`task/cancel`) are handled in the invoke handler for card actions that
bypass the message flow (e.g. from Adaptive Card action handlers that use
`Action.Submit` with invoke type). These check room policy server-side
and return HTTP 403 for denied actions.

#### Card Edit vs. New Send

- **Edit in place:** When a previous progress message exists (tracked by
  task ID in an in-memory hashtable), the card is updated via PUT.
- **Fallback on edit failure:** If the edit raises any exception (any
  non-2xx response from `Teams_adaptive_card.edit_adaptive_card`, or any
  other error), a new card is sent and the message ID is updated.
- **First send:** No existing message; POST creates a new card.

### Fallback Text (`build_fallback_text`)

For connectors that do not support Adaptive Cards (Slack, Discord, etc.),
the same checklist is rendered as markdown text. The fallback includes the
same icon/summary/item structure but without interactive buttons.

## Fallback Behavior

### Message Splitting

Teams has a 28,672-character limit per message. Longer responses are
split at whitespace boundaries. If no whitespace is found before the
limit, a forced mid-word break occurs.

### Edit Failure Recovery

When editing an existing message fails (any exception from the edit
callback, not just retryable HTTP status codes):
1. The error is recorded in the delivery lifecycle ledger as `Edit_failed`.
2. A new message is sent as a fallback.
3. The new message ID replaces the old one in the progress hashtable.

For Teams Adaptive Cards, `Teams_adaptive_card.edit_adaptive_card` raises
on any non-2xx response. For text messages, the `teams.ml` throttled
PUT wrapper raises on failure as well.

### Rate Limiting

- **Inbound:** Per-conversation+user rate limiting with 60-second warning
  cooldowns. Rate-limited users receive "Please slow down" once per minute.
- **Outbound:** Functions using the `*_throttled` wrappers in `teams.ml`
  (`post_json_throttled`, `put_json_throttled`, `delete_throttled`) apply
  per-conversation throttling (1 request/second minimum interval) with
  exponential backoff retry (up to 3 attempts) on 429, 412, 502, 504.
  Note: Adaptive Card edits via `Teams_adaptive_card.edit_adaptive_card`
  call `Http_client.put_json` directly without this throttle/retry wrapper.

### Empty Message Guard

Empty or whitespace-only text is refused before sending. Teams rejects
empty payloads with HTTP 400 BadSyntax. The guard applies to both
`send_reply` and `edit_activity`.

### File Consent Flow

Teams file uploads use a consent card flow:
1. A file consent card is sent with filename, description, and size.
2. The user accepts or declines via the card action.
3. On accept, the file is uploaded; on decline or timeout, a temp download
   URL fallback is provided.

## Delivery Observability

### Lifecycle States (`teams_delivery_lifecycle.ml`)

Every Teams message delivery is tracked through granular lifecycle states
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

Each state transition includes:
- A correlatable `tracking_id` (format: `dlv_<timestamp>_<random>`)
- Task ID, room ID, connector name
- Optional thread ID, activity ID, message ID, error message

### Ledger Events

All delivery events are recorded to `Room_activity_ledger` with event
type prefix `teams_delivery_`. Query by tracking ID to trace a single
outbound message through its full lifecycle.

### Room Activity Ledger

Beyond delivery lifecycle, the ledger records:
- Delivery attempts (`record_delivery_attempt`)
- Delivery successes (`record_delivery_success`)
- Delivery failures (`record_delivery_failure`)

Error messages are sanitized before storage (see
`Room_activity_ledger.sanitize_error`).

### Capability Introspection (`/whatcando`)

The `/whatcando` command shows a real-time snapshot of room capabilities:
- **Connector capabilities:** edit, delete, reactions, typing, status,
  file sending, cards, buttons
- **Room state:** profile binding, history capture, history persistence,
  delivery mode, max message length
- **Readiness:** database availability, GitHub configuration
- **Degraded behaviors:** explanations for missing features

On Teams, this renders as an Adaptive Card with FactSets. On other
connectors, it renders as markdown text.

## Known Unsupported Connector Behavior

### Connectors Without Edit Support

Connectors where `can_edit = No_edit` (IRC, Lark, Line, DingTalk, OneBot,
Nostr, iMessage, Email, Signal, WhatsApp, Web, Plain):
- The capability profile classifies them as `Buffered_progress`, but
  `connector_room_progress.dispatch` only supports Slack, Teams, and
  Discord. For all other connectors, dispatch returns `None` and
  `deliver_room_progress` returns `false` -- no progress updates are sent
  to the room at all.
- No Adaptive Card support; text fallback only.
- No action buttons.

### Connectors Without Delete Support

Connectors where `can_delete = false` (IRC, Email, Web, Plain):
- Cannot retract or replace sent messages.
- Edit failures result in duplicate messages.

### Connectors Without Typing Indicators

Most connectors other than Teams and Telegram do not support typing
indicators. Users see no feedback while the agent is processing.

### Connectors Without Thread Replies

IRC, Lark, Line, DingTalk, OneBot, Nostr, iMessage, Signal, WhatsApp,
Web, Plain:
- No thread-aware delivery. All messages go to the room.
- `thread_reply_strategy` returns `Use_room_fallback`.

### Matrix

Matrix supports edit-in-place and thread replies but does not support
reactions, status messages, file sending, or cards. Despite the capability
profile, `connector_room_progress.dispatch` does not include Matrix, so
deliver_room_progress returns false and no progress updates are sent to
the room through this path.

### Mattermost

Mattermost supports edit, delete, reactions, status, and native threads
but does not support file sending, cards, or typing indicators.

### GitHub

GitHub supports edit, delete, reactions, status, and thread-like replies
but does not support file sending, cards, or typing indicators. Max
message length is 65536 characters.

## Architecture Overview

```
User message
    |
    v
teams.ml (webhook handler)
    |-- auth, dedup, mention filter
    |-- session key resolution
    |-- slash command routing
    |-- Session.turn (agent invocation)
    |
    v
Background task created
    |
    v
daemon_util.ml (task state watcher)
    |-- connector_room_progress.dispatch -> progress_callbacks
    |-- room_progress.deliver_progress_update_with_card
    |       |-- teams_progress_card.build_card (Adaptive Card)
    |       |-- teams_progress_card.build_fallback_text (markdown)
    |       |-- send_or_edit_card (Teams) / send_or_edit (Slack)
    |       |-- teams_delivery_lifecycle (ledger recording)
    |
    v
Room receives progress update
```

### Key Source Files

| File | Purpose |
|------|---------|
| `src/teams.ml` | Teams webhook handler, message send/edit/delete, auth |
| `src/teams_progress_card.ml` | Adaptive Card builder for progress checklists |
| `src/teams_what_can_do.ml` | Capability introspection card |
| `src/teams_delivery_lifecycle.ml` | Delivery state tracking and ledger events |
| `src/slack_progress_checklist.ml` | Slack mrkdwn progress renderer |
| `src/room_progress_checklist.ml` | Durable checklist model (SQLite) |
| `src/room_progress.ml` | Connector-agnostic progress delivery |
| `src/connector_room_progress.ml` | Per-connector callback dispatch |
| `src/connector_capabilities.ml` | Capability profiles per connector |
