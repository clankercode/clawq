# Teams Reply/Threading Support Matrix

Audit date: 2026-06-27 (P11.M3.E3.T001)

## Bot Framework Reply Model

Teams uses the **Microsoft Bot Framework** conversation model. Threading is
achieved via the REST API endpoint pattern, not via a separate `thread_ts`
or `reply_to_id` field in the activity body (unlike Slack).

### Key Identifiers

| Identifier | Scope | Description |
|---|---|---|
| `conversation_id` | Per-conversation | Stable ID for the conversation (DM, group chat, or team channel). Format: `19:xxx@thread.tacv2` for channels, `19:xxx@thread.v2` for group/DM. |
| `activity_id` | Per-message | Unique ID of a specific activity (message). Set by the Bot Framework on POST. |
| `reply_to_id` | Per-reply | The `activity_id` of the message being replied to. Not a field in the inbound activity — it is the `id` field of the activity the bot wants to reply to. |

### Reply URI Routing

The Bot Framework uses **URI path routing** to distinguish new messages from
threaded replies:

| `reply_to_id` | Endpoint | Effect |
|---|---|---|
| `""` (empty) | `POST /v3/conversations/{conv_id}/activities` | New top-level activity in the conversation |
| Non-empty | `POST /v3/conversations/{conv_id}/activities/{reply_to_id}` | Threaded reply to the specified activity |

All three outbound functions (`send_reply`, `send_adaptive_card`, `send_file`)
use this routing via the shared `build_reply_uri` helper.

## Room Type Behavior Matrix

### Personal (1:1 DM)

| Field | Value | Notes |
|---|---|---|
| `team_id` | `""` (absent) | No `channelData.team` in activity |
| `is_group` | `false` | |
| `conversation_id` | `19:xxx@thread.v2` | Unique per user |
| `session_key` | `teams:personal:{conv_id}` | Effectively per-user |
| Reply-to threading | **Supported** | Replies appear as threaded responses |
| @mention needed | **No** | Bot only receives direct messages |
| File consent cards | **Supported** | Primary file delivery path |

### Group Chat

| Field | Value | Notes |
|---|---|---|
| `team_id` | `""` (absent) | No `channelData.team` |
| `is_group` | `true` | |
| `conversation_id` | `19:xxx@thread.v2` | Shared across group |
| `session_key` | `teams:personal:{conv_id}` | Shared session for all group members |
| Reply-to threading | **Supported** | Replies appear as threaded responses |
| @mention needed | **Yes** | Bot only processes messages that @mention it |
| File consent cards | **Not supported** | Falls back to temp download URL |

### Team Channel

| Field | Value | Notes |
|---|---|---|
| `team_id` | `{team_guid}` | From `channelData.team.id` |
| `is_group` | `true` | |
| `conversation_id` | `19:xxx@thread.tacv2` | Shared per channel |
| `session_key` | `teams:{team_id}:{conv_id}` | Shared session per channel |
| Reply-to threading | **Supported** | Replies appear as threaded responses in channel |
| @mention needed | **Yes** | Bot only processes messages that @mention it |
| File consent cards | **Not supported** | Falls back to temp download URL |

## Reply Behavior by Function

### `send_reply`

- Builds text reply with optional @mention and notification control
- Routes via `reply_uri` based on `reply_to_id`
- Splits long messages (>28672 chars) into chunks; all chunks reply to the same `reply_to_id`
- Always called from `handle_webhook` with `reply_to_id = activity_id` of the inbound message
- Also used for standalone messages via `send_message` with `reply_to_id = ""`

### `send_adaptive_card`

- Sends Adaptive Card JSON as an attachment
- Routes via `reply_uri` based on `reply_to_id`
- Used for menus, polls, question presenters
- From `handle_webhook`: always `reply_to_id = activity_id`

### `send_file`

- Uploads attachment via Bot Framework attachment API, then sends a message referencing it
- Routes via `reply_uri` for the message part (not the upload)
- **NOTE:** Attachment upload endpoint returns HTTP 404 on Teams — file delivery uses temp download or file consent card instead

### `edit_activity`

- Uses `PUT /v3/conversations/{conv_id}/activities/{activity_id}` — targets a specific activity for in-place edit
- Not a reply operation — always targets a known `activity_id`

### `delete_activity`

- Uses `DELETE /v3/conversations/{conv_id}/activities/{activity_id}`
- Targets a specific activity for deletion

## Gaps for P13 Hardening

### G1: No explicit reply-to-thread support in webhook handler

`handle_webhook` always replies to `activity_id` (the triggering message). There
is no mechanism to reply to a *different* activity (e.g., reply to an older message
in the conversation). This is correct for conversational flow but limits
programmatic thread management.

**Severity:** Low — current behavior is correct for a chat bot.

### G2: `replyToId` not parsed from inbound activities

The Bot Framework includes `replyToId` in inbound activities when a user replies
to a specific message. `parse_activity` does not extract this field. If future
features need to know which message the user replied to (e.g., context-aware
responses), this field would need to be added to `teams_activity`.

**Severity:** Low — not needed for current functionality. Would be needed for
message-context-aware responses (P13 or later).

### G3: Multi-chunk reply atomicity

When `split_message` produces multiple chunks, each chunk is sent as a separate
POST to the same `reply_to_id`. In Teams, each chunk becomes a separate reply
in the thread. There is no way to batch them into a single threaded response.

**Severity:** Low — cosmetic issue for very long messages only.

### G4: No thread-aware connector history

`Connector_history` records unaddressed group messages but does not track which
messages are replies to which. Thread context is lost in the history buffer.

**Severity:** Low — thread context would be useful for richer context but is not
blocking any current functionality.

### G5: File delivery across room types

File consent cards only work in personal 1:1 chats. Group chats and team channels
fall back to temp download URLs. This is documented and handled by
`select_file_upload_delivery` but could benefit from a richer file delivery
mechanism (e.g., SharePoint integration) in P13.

**Severity:** Low — temp download works but requires `tunnel.url` configuration.

## Test Coverage

Reply routing is tested via direct unit tests of the `reply_uri` helper:

- Empty `reply_to_id` → activities collection endpoint
- Non-empty `reply_to_id` → specific activity endpoint
- `reply_to_id` with special characters (pct-encoded)
- Integration: `send_reply`, `send_adaptive_card`, `send_file` all use `reply_uri`
