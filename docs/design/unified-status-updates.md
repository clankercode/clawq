# Unified Status Update Abstraction Design

**B441** | 2026-03-14 | Design Document

## 1. Platform Research: Update/Edit Capabilities

### Telegram

- **API**: `editMessageText` via Bot API (`src/telegram_api.ml:590`)
- **Edit support**: Full in-place editing of bot's own messages; no time limit
- **Streaming**: Not native; simulated by repeated `editMessageText` calls
- **Parse modes**: `HTML`, `MarkdownV2`, `Markdown` (legacy)
- **Reactions**: `setMessageReaction` API for emoji reactions on messages
- **Message limits**: ~4096 characters per message
- **Delete**: `deleteMessage` API
- **Current impl**: `make_status_notifier` returns a `Status_message.notifier` with `send`/`edit`/`delete` (`src/telegram_api.ml:691-733`). Factory registered in `session_core.status_message_factories` per session key.

### Discord

- **API**: `PATCH /channels/{id}/messages/{id}` for editing own messages
- **Edit support**: Full in-place editing of bot's own messages; no time limit
- **Streaming**: Not native; simulated by repeated PATCH calls
- **Format**: Markdown (Discord flavor: `**bold**`, `*italic*`, `` `code` ``)
- **Reactions**: `PUT /channels/{id}/messages/{id}/reactions/{emoji}/@me`
- **Message limits**: 2000 characters per message
- **Delete**: `DELETE /channels/{id}/messages/{id}`
- **Rate limits**: Server-driven via response headers; separate bucket tracking in `src/discord.ml`
- **Current impl**: `make_status_notifier` at `src/discord.ml:232-247`. Also has its own duplicated consolidated/individual dispatch in the `on_chunk` handler (`src/discord.ml:888-960`).

### Slack

- **API**: `chat.update` using `ts` (timestamp) as message identifier
- **Edit support**: Full in-place editing of bot's own messages; no time limit
- **Streaming**: Not native; simulated by repeated `chat.update` calls
- **Format**: `mrkdwn` (Slack's Markdown variant: `*bold*`, `_italic_`, `` `code` ``)
- **Reactions**: `reactions.add` / `reactions.remove` APIs
- **Message limits**: ~4000 characters per message (blocks: 50 blocks, 3000 chars per text block)
- **Delete**: `chat.delete` API
- **Current impl**: `make_status_notifier` at `src/slack_api.ml:313-324`.
  `src/slack.ml:610-630` selects a `Status_update` strategy and keeps a thin
  reaction wrapper around the centralized handler's `on_chunk`.

### Teams (Microsoft Bot Framework)

- **API**: `PUT /v3/conversations/{conv}/activities/{id}` (Bot Framework REST)
- **Edit support**: Yes -- sending to the same activity ID replaces the message content. The `POST` to create returns an `id` field in the response JSON that can be used for subsequent `PUT` updates.
- **Streaming**: Not native; simulated by repeated PUT calls
- **Format**: Markdown (subset), Adaptive Cards (rich JSON-based card format)
- **Reactions**: Not available via Bot Framework REST (users can react, but bot cannot set reactions programmatically)
- **Message limits**: ~28672 characters (`max_message_chars` in `src/teams.ml:4`)
- **Delete**: `DELETE /v3/conversations/{conv}/activities/{id}`
- **Current impl**: `send_reply` at `src/teams.ml:302-336` creates new messages. `edit_activity` and `delete_activity` exist. `make_status_notifier` builds a `Status_message.notifier`. Factory and `Connector_capabilities.teams` registered per session key in the webhook handler. Has typing indicator support (`src/teams.ml:206`).

### Matrix

- **API**: `PUT /_matrix/client/v3/rooms/{room}/send/m.room.message/{txn_id}` for sending; editing via `m.relates_to` with `rel_type: "m.replace"` and `m.new_content` in the event body
- **Edit support**: Yes -- send a new event with `m.relates_to.event_id` pointing to the original event and `rel_type: "m.replace"`. The `m.new_content` field contains the replacement body.
- **Streaming**: Not native; simulated by repeated edit events
- **Format**: Plain text (`m.text` msgtype); optionally `org.matrix.custom.html` for formatted body
- **Reactions**: `m.reaction` event type with `m.relates_to.rel_type: "m.annotation"`
- **Message limits**: ~65536 bytes per event (server-dependent)
- **Delete**: Redaction via `POST /_matrix/client/v3/rooms/{room}/redact/{event_id}/{txn_id}`
- **Current impl**: `send_message` at `src/matrix.ml:18-35` sends plain text only. No edit, delete, or status notifier support. Uses `Session.turn` (not `turn_stream`), so no streaming.

### Other Platforms (Brief Notes)

| Platform | Edit | Delete | Reactions | Format | Notes |
|----------|------|--------|-----------|--------|-------|
| Mattermost | Yes (PUT) | Yes | Yes | Markdown | Very similar to Slack API |
| IRC | No | No | No | Plain text | Send-only; no edit possible |
| Signal | No | Yes | Yes (limited) | Plain text | Cannot edit after send |
| WhatsApp Business | No | Yes | Yes (limited) | Simple Markdown | Cannot edit messages |
| Zulip | Yes | Yes | Yes | Markdown | Full edit support like Slack |

## 2. Content DSL Design

The content DSL provides semantic types that `Format_adapter` renders to platform-specific markup. This builds on the existing `Format_adapter.connector` type (`src/format_adapter.ml:3-10`) and its rendering functions.

### Element Types

```ocaml
(** Inline elements *)
type inline =
  | Text of string
  | Bold of string
  | Italic of string
  | Code of string
  | Link of { text : string; url : string }
  | Emoji of string       (** UTF-8 emoji or platform-specific name *)

(** Block-level elements *)
type block =
  | Paragraph of inline list
  | CodeBlock of { language : string option; content : string }
  | ToolEntry of {
      emoji : string;
      name : string;
      state : Status_message.tool_state;
      summary : string option;
      result_preview : string option;
      error_detail : string option;
      timing : string option;
    }
  | ProgressBar of { done_count : int; total : int; bar_width : int }
  | CollapsedTools of { count : int }
  | ToolSummary of {
      total : int;
      emoji_breakdown : string;
      total_time : string;
      parallel : bool;
    }
  | Separator
  | ThinkingPreview of string

(** A document is a list of blocks *)
type document = block list
```

### Rendering

Each block renders through `Format_adapter` functions:

- `ToolEntry` -> uses `bold`, `code`, `italic`, `escape` (matching current `Status_message.render` at `src/status_message.ml:94-277`)
- `ProgressBar` -> uses Unicode block characters (matching current render)
- `CodeBlock` -> uses `Format_adapter.code_block`
- `Paragraph` -> maps inline elements through `Format_adapter.bold`, `italic`, etc.
- `CollapsedTools` -> simple text line
- `ToolSummary` -> formatted summary footer

The renderer function signature:

```ocaml
val render_document : Format_adapter.connector -> document -> string
```

This replaces the monolithic `Status_message.render` with a composable pipeline: build a `document`, then render it for the target connector.

## 3. Connector Capabilities Type

```ocaml
(** How a connector handles message updates *)
type edit_support =
  | Edit_in_place     (** Can edit own messages (Telegram, Discord, Slack, Teams, Matrix) *)
  | Delete_and_resend (** Cannot edit; delete old + send new *)
  | No_edit           (** Send-only, no update possible (IRC, Signal) *)

type thread_reply_support =
  | Native_thread_replies
  | Thread_like_replies
  | No_thread_replies

type progress_delivery =
  | Edit_progress_in_place
  | Delete_and_resend_progress
  | Buffered_progress

type card_strategy = Use_cards | Use_buttons | Use_text_fallback
type history_capture_support = Ambient_history_capture | No_history_capture

(** Connector capability profile *)
type t = {
  can_edit : edit_support;
  can_delete : bool;
  can_react : bool;
  can_type : bool;              (** Typing indicator support *)
  can_show_status : bool;       (** Status/progress update support *)
  can_send_files : bool;
  can_send_cards : bool;
  can_send_buttons : bool;
  thread_replies : thread_reply_support;
  history_capture : history_capture_support;
  max_message_length : int;     (** Platform's message size limit *)
  connector : Format_adapter.connector;
  parse_mode : string;          (** Parse mode string for Status_message *)
  debounce_interval : float;    (** Minimum interval between edits (seconds) *)
}
```

### Predefined Profiles

Profiles are centralized in `src/connector_capabilities.ml` and built with a
`make` helper so unsupported capabilities default to `false`, `No_edit`,
`No_thread_replies`, or `No_history_capture`. Current highlights:

- Telegram: edit/delete/type/status/files/buttons, thread-like replies, HTML.
- Discord: edit/delete/react/status, native threads, ambient history capture.
- Slack: edit/delete/react/status, native threads, no ambient history capture
  until a Slack capture implementation is added.
- Teams: edit/delete/type/status/files/cards/buttons, thread-like replies,
  ambient history capture.
- Matrix: edit/delete/status, native threads.
- Plain/IRC-style connectors: send-only defaults unless explicitly overridden.

Strategy helpers (`thread_reply_strategy`, `progress_delivery`,
`card_strategy`, `history_capture_strategy`, and `should_capture_history`) map
raw capabilities to runtime behavior. Connector history capture must pass both
configuration (`connector_history.enabled`) and the connector capability matrix.

### Registration

Capabilities are registered per session key in `Session_core`, alongside the existing `status_message_factories` table (`src/session_core.ml:50`):

```ocaml
(* In session_core.ml *)
connector_capabilities : (string, Connector_capabilities.t) Hashtbl.t;
```

Each connector registers its capabilities when setting up a session, before registering the status notifier factory.

## 4. Unified Dispatch: `Status_update` Module

### Problem

Historically, consolidated vs. individual dispatch logic existed in three
places:

1. `session_turn.ml:26-112` (`stream_turn_with_visibility`)
2. `discord.ml:888-960` (inline `on_chunk` handler)
3. `slack.ml` (inline `on_chunk` handler, now centralized through
   `Status_update.make_handler` with a reaction wrapper)

These paths checked `agent_defaults.show_tool_calls && tool_status_mode =
"consolidated"`, then branched between `Status_message` (edit-in-place) and
`Stream_visibility` (individual notifications).

### Design

A new `Status_update` module centralizes this dispatch into a single function:

```ocaml
(** Unified status update handler *)

type handler = {
  on_chunk : Provider.chunk -> unit Lwt.t;
  finalize : unit -> unit Lwt.t;
  get_thinking : unit -> string;
}

type strategy =
  | Consolidated  (** Edit-in-place via Status_message *)
  | Individual    (** Per-event notifications via Stream_visibility *)
  | Buffered      (** Accumulate, send summary at end *)

val select_strategy :
  agent_defaults:Runtime_config.agent_defaults ->
  capabilities:Connector_capabilities.t option ->
  strategy

val make_handler :
  strategy:strategy ->
  notifier_factory:(unit -> Status_message.t) option ->
  notify:(string -> unit Lwt.t) ->
  agent_defaults:Runtime_config.agent_defaults ->
  parse_mode:string ->
  handler
```

### Strategy Selection

```ocaml
let select_strategy ~agent_defaults ~capabilities =
  if not agent_defaults.show_tool_calls then Individual
  else if agent_defaults.tool_status_mode <> "consolidated" then Individual
  else
    match capabilities with
    | None -> Individual
    | Some caps ->
      match caps.can_edit with
      | Edit_in_place -> Consolidated
      | Delete_and_resend -> Consolidated  (* Status_message handles delete+resend via notifier.edit returning Some new_id *)
      | No_edit -> Buffered
```

### Handler Implementations

**Consolidated** (wraps `Status_message`):
- `on_chunk`: Routes `ToolStart`/`ToolResult` to `Status_message.tool_start`/`tool_result`. Routes `ThinkingDelta` to `Status_message.update_thinking` + internal buffer.
- `finalize`: Calls `Status_message.finalize`.
- `get_thinking`: Returns buffered thinking text.
- This is the current `consolidated_status_on_chunk` logic from `session_turn.ml:1-19`.

**Individual** (wraps `Stream_visibility`):
- `on_chunk`: Delegates to `Stream_visibility.on_chunk` with settings derived from `agent_defaults`.
- `finalize`: No-op.
- `get_thinking`: Returns `Stream_visibility.thinking_text`.
- This is the current fallback path from `session_turn.ml:73-86`.

**Buffered** (new, for no-edit connectors):
- `on_chunk`: Accumulates `ToolStart`/`ToolResult` events in an internal list. Buffers `ThinkingDelta`.
- `finalize`: Renders a single summary message of all tool calls and sends it via `notify`. If no tools were used, sends nothing.
- `get_thinking`: Returns buffered thinking text.
- For connectors that cannot edit messages, this avoids flooding with individual notifications and instead sends one final summary.

### Integration Point

`session_turn.ml:stream_turn_with_visibility` simplifies to:

```ocaml
let stream_turn_with_visibility mgr ~notify agent ~key ... =
  let capabilities = Hashtbl.find_opt mgr.connector_capabilities key in
  let factory = Hashtbl.find_opt mgr.status_message_factories key in
  let strategy = Status_update.select_strategy ~agent_defaults ~capabilities in
  let handler = Status_update.make_handler ~strategy
    ~notifier_factory:factory ~notify ~agent_defaults
    ~parse_mode:(match capabilities with
      | Some c -> c.parse_mode
      | None -> "Markdown") in
  let* response = Agent.turn_stream agent ... ~on_chunk:handler.on_chunk () in
  let* () = handler.finalize () in
  let thinking = handler.get_thinking () in
  ...
```

Discord and Slack `on_chunk` handlers become:

```ocaml
(* In discord.ml / slack.ml message handler *)
let capabilities = Connector_capabilities.discord (* or .slack *) in
let factory = Some (fun () -> Status_message.create ~notifier:... ~parse_mode:... ()) in
let strategy = Status_update.select_strategy ~agent_defaults ~capabilities:(Some capabilities) in
let handler = Status_update.make_handler ~strategy ~notifier_factory:factory
  ~notify:send_fn ~agent_defaults ~parse_mode:capabilities.parse_mode in
(* ... use handler.on_chunk in turn_stream call ... *)
```

This eliminates the ~80-line and ~65-line duplicated dispatch blocks in Discord and Slack.

## 5. Migration Path

### Phase 1: Add Foundation Modules (no behavioral change)

1. **Create `src/connector_capabilities.ml`**: Define the `edit_support` and `t` types, predefined profiles, and a `connector_capabilities` hashtable in `Session_core`.

2. **Create `src/content_dsl.ml`**: Define the `inline`, `block`, `document` types and `render_document` function. Initially, `render_document` can produce the same output as `Status_message.render` to ensure parity.

3. **Create `src/status_update.ml`**: Define `handler`, `strategy`, `select_strategy`, and `make_handler`. The `Consolidated` and `Individual` paths wrap existing `Status_message` and `Stream_visibility` logic.

### Phase 2: Centralize Dispatch

4. **Refactor `session_turn.ml`**: Replace `stream_turn_with_visibility`'s inline dispatch with `Status_update.make_handler`. The `consolidated_status_on_chunk` helper moves into `Status_update`.

5. **Refactor `discord.ml`**: Remove the ~80-line inline `on_chunk` dispatch (lines 888-960). Instead, use `Status_update.make_handler` and pass `handler.on_chunk` to `Session.turn_stream`. Keep reaction tracking as a wrapper around the handler's `on_chunk`.

6. **~~Refactor `slack.ml`~~**: Done. Slack now uses
   `Status_update.make_handler` and keeps connector-specific reaction tracking
   as a wrapper around `handler.on_chunk` (`src/slack.ml:610-630`).

### Phase 3: Onboard Teams (DONE)

7. **~~Add `edit_activity` to `src/teams.ml`~~**: Done. `edit_activity` uses `PUT /v3/conversations/{conv}/activities/{id}`.

8. **~~Add `make_status_notifier` to `src/teams.ml`~~**: Done. Builds a `Status_message.notifier` using `send_reply` (send), `edit_activity` (edit), and `delete_activity` (delete).

9. **~~Register capabilities and factory~~**: Done. Capabilities registered once per session key; factory re-registered unconditionally per message (captures current `reply_to_id`). B495 fixed a bug where `with_registered_notifier` cleanup was wiping these registrations. B499 fixed a related issue where background task completion turns ran without any notifier, causing tool call visibility notifications to be silently dropped; `inject_background_task_completion` now wraps `Session.turn` in `with_registered_notifier` using `dispatch_resumed_message` as the notifier.

### Phase 4: Onboard Matrix

10. **Add `edit_message` to `src/matrix.ml`**: Send a new `m.room.message` event with:
    ```json
    {
      "msgtype": "m.text",
      "body": "* new text",
      "m.new_content": { "msgtype": "m.text", "body": "new text" },
      "m.relates_to": {
        "rel_type": "m.replace",
        "event_id": "$original_event_id"
      }
    }
    ```

11. **Add `delete_message` to `src/matrix.ml`**: Implement redaction via `POST /_matrix/client/v3/rooms/{room}/redact/{event_id}/{txn_id}`.

12. **Add `make_status_notifier` to `src/matrix.ml`**: Build a `Status_message.notifier` using the new send/edit/delete functions. Note: `send_message` needs to return the `event_id` from the PUT response.

13. **Switch Matrix to `turn_stream`**: Change from `Session.turn` to `Session.turn_stream` (or use `stream_turn_with_visibility` via the registered factory path). Register capabilities and factory per session key.

### Phase 5: Migrate Status_message to Content DSL (Optional)

14. **Replace `Status_message.render`** with `Content_dsl.render_document`. Build the document from `Status_message.t` state, then render via `Format_adapter`. This decouples content semantics from rendering.

## 6. Thinking/Streaming Support

### Current Behavior

- **Connectors with edit support** (Telegram, Discord, Slack): Thinking deltas are buffered during the turn. After finalization, if `show_thinking` is enabled, the full thinking text is sent as a separate message via `notify` (`session_turn.ml:49-52`). During the turn, thinking text is also streamed into the consolidated status message via `Status_message.update_thinking` (`src/status_message.ml:463-466`).

- **Connectors without status support** (Matrix, HTTP gateway): Thinking is buffered in `Stream_visibility.thinking_buf` and sent after the turn completes.

### Unified Approach

The `Status_update.handler` abstracts thinking handling per strategy:

| Strategy | During Turn | After Turn |
|----------|-------------|------------|
| **Consolidated** | `update_thinking` streams into the edit-in-place status message, showing a live preview (truncated to 200 chars). Thinking buffer accumulates full text. | Full thinking sent as separate message if `show_thinking` enabled. |
| **Individual** | Thinking buffered silently (no per-delta messages to avoid noise). | Full thinking sent as separate message if `show_thinking` enabled. |
| **Buffered** | Thinking buffered silently. | Full thinking included in the summary message, or sent as a separate message. |

### Configuration

Controlled by existing `agent_defaults` fields:

- `show_thinking : bool` -- whether to show thinking at all
- `tool_status_mode : string` -- `"consolidated"` (edit-in-place) vs `"individual"` (per-event)

No new configuration needed. The strategy is fully determined by `agent_defaults` + `Connector_capabilities.t`.

### Streaming Support Matrix

| Connector | Edit-in-place Thinking | Post-turn Thinking | Tool Status |
|-----------|----------------------|-------------------|-------------|
| Telegram | Yes (live preview in status msg) | Yes (separate msg) | Consolidated |
| Discord | Yes (live preview in status msg) | Yes (separate msg) | Consolidated |
| Slack | Yes (live preview in status msg) | Yes (separate msg) | Consolidated |
| Teams | Yes (once edit_activity added) | Yes (separate msg) | Consolidated |
| Matrix | Yes (once edit_message added) | Yes (separate msg) | Consolidated |
| HTTP/WS Gateway | N/A (streams via SSE/WS) | Via stream | Via stream |
| IRC/Signal | No | Yes (separate msg) | Buffered |

## Appendix: File Reference

| File | Lines | Role |
|------|-------|------|
| `src/status_message.ml` | 505 | Consolidated status message (edit-in-place) |
| `src/stream_visibility.ml` | 606 | Individual notification fallback + token estimation |
| `src/format_adapter.ml` | 119 | Per-connector text formatting (bold, italic, code, etc.) |
| `src/typing_indicator.ml` | 87 | Generic typing indicator loop |
| `src/session_turn.ml` | 841 | Turn dispatch: factory -> consolidated, else -> individual |
| `src/connector_status.ml` | 43 | Status and interrupt-ack emoji mapping per connector |
| `src/status_phase.ml` | 3 | Status phase ADT (Received, Processing, Completed, Failed) |
| `src/session_core.ml` | ~700 | Session state, factory registration (line 50, 320) |
| `src/telegram_api.ml` | ~1086 | Telegram API + `make_status_notifier` (line 733) |
| `src/discord.ml` | ~1160 | Discord channel + duplicated dispatch (line 888) |
| `src/slack.ml` | ~940 | Slack channel + `Status_update` reaction wrapper (around line 620) |
| `src/teams.ml` | ~930 | Teams channel + `make_status_notifier` + factory registration |
| `src/matrix.ml` | 223 | Matrix channel, plain text only, no status support |

## Appendix: New Modules Summary

| New File | ~Lines | Purpose |
|----------|--------|---------|
| `src/connector_capabilities.ml` | ~80 | Capability profiles per platform |
| `src/content_dsl.ml` | ~190 | Semantic content types + renderer (includes QuestionBlock for ask_user_question) |
| `src/question_presenter.ml` | ~310 | Rich question rendering: strategy selection, buttons/polls/cards per connector |
| `src/status_update.ml` | ~150 | Unified handler: strategy selection + dispatch |
