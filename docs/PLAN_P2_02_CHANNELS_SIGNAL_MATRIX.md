# P2-02: Channels — Signal & Matrix

## Context

clawq currently has 5 channels (CLI, HTTP, Telegram, Discord, Slack). nullclaw has 18. Signal and Matrix are high-priority additions: Signal is privacy-focused and widely used for secure communication; Matrix (Element, etc.) is the dominant open federated chat protocol.

Both use long-running connection patterns already established in clawq: Signal via signal-cli JSON-RPC + SSE, Matrix via long-poll sync.

## Architecture

Both channels implement `Channel.S`:
```ocaml
module type S = sig
  val name : string
  val start : config:Runtime_config.t -> session_manager:Session.t -> unit Lwt.t
end
```

They integrate into `daemon.ml` alongside Discord/Slack as `Lwt.async` spawned tasks.

---

## Part A: Signal Channel

### How Signal Works

Signal does not expose a native API. The standard approach is **signal-cli**, a Java CLI daemon that bridges Signal's sealed-sender protocol to a local HTTP/JSON-RPC interface.

Two modes (matching nullclaw's implementation):
1. **JSON-RPC mode** (default): JSON-RPC at `http://localhost:8080/api/v1/rpc`, SSE events at `http://localhost:8080/api/v1/events`
2. **REST mode**: `signal-cli-rest-api` Docker image at `http://localhost:8080/v2/send` + `/v1/receive/`

### Config

Add to `Runtime_config.channels`:
```ocaml
signal : signal_config option

type signal_config = {
  enabled : bool;
  daemon_url : string;        (* default: "http://localhost:8080" *)
  mode : string;              (* "jsonrpc" | "rest" *)
  phone_number : string;      (* E.164 format, e.g., "+15551234567" *)
  allow_from : string list;   (* allowed sender numbers/UUIDs; empty = allow all *)
  max_chunk_bytes : int;      (* default 4096 *)
}
```

### Implementation: `src/signal.ml`

**Receive (SSE events in JSON-RPC mode):**
```
GET http://{daemon_url}/api/v1/events
Accept: text/event-stream

data: {"method":"receive","params":{"envelope":{"source":"+1...","dataMessage":{"message":"Hello"}}}}
```

Parse `envelope.source` as sender, `dataMessage.message` as text. Filter by `allow_from` if set.

**Send (JSON-RPC):**
```
POST http://{daemon_url}/api/v1/rpc
Content-Type: application/json

{"jsonrpc":"2.0","method":"send","params":{"recipient":["+1..."],"message":"reply"},"id":1}
```

**Receive (REST mode):**
```
GET http://{daemon_url}/v1/receive/{phone_number}
```
Returns JSON array of messages. Poll on interval (1-2s).

**Send (REST mode):**
```
POST http://{daemon_url}/v2/send
{"message":"...", "number":"+1...", "recipients":["+1..."]}
```

**Message chunking:** Split responses exceeding `max_chunk_bytes` at UTF-8 character boundaries.

**SSE client:** Reuse `ws_client`-style persistent HTTP connection; parse `data:` lines from streaming response body via `cohttp-lwt-unix` streaming body reader.

### Files

- **Create**: `src/signal.ml` — Signal channel implementation
- **Modify**: `src/runtime_config.ml` — add `signal_config` type and field
- **Modify**: `src/config_loader.ml` — parse `channels.signal` JSON object
- **Modify**: `src/daemon.ml` — start Signal channel if enabled
- **Modify**: `src/command_bridge.ml` — include Signal in `cmd_status` channel listing
- **Modify**: `src/dune` — add `signal` to `clawq_runtime_integrations` modules

### External Requirement

signal-cli must be installed and running as a daemon. Document this in QUICKSTART.md. `cmd_doctor` should check for signal-cli reachability if signal config is present.

---

## Part B: Matrix Channel

### How Matrix Works

Matrix is an open federated protocol. clawq connects to a homeserver (e.g., `matrix.org`, self-hosted Synapse/Dendrite) as a bot account using a user access token.

**Inbound:** Long-poll `/_matrix/client/v3/sync?timeout=30000&since={batch_token}`
**Outbound:** PUT to `/_matrix/client/v3/rooms/{roomId}/send/m.room.message/{txnId}`

### Config

Add to `Runtime_config.channels`:
```ocaml
matrix : matrix_config option

type matrix_config = {
  enabled : bool;
  homeserver_url : string;    (* e.g., "https://matrix.org" *)
  access_token : string;      (* bot account access token *)
  user_id : string;           (* @bot:matrix.org *)
  allow_rooms : string list;  (* allowed room IDs; empty = allow all *)
  allow_users : string list;  (* allowed sender user IDs; empty = allow all *)
  max_chunk_bytes : int;      (* default 4000 *)
}
```

### Implementation: `src/matrix.ml`

**Sync loop:**
```
GET {homeserver_url}/_matrix/client/v3/sync?timeout=30000&since={batch_token}
Authorization: Bearer {access_token}
```

Parse response: `rooms.join.{roomId}.timeline.events[]` where `type = "m.room.message"` and `content.msgtype = "m.text"`. Extract `sender` and `content.body`.

Filter: skip own messages (`sender = config.user_id`). Apply `allow_users` / `allow_rooms` filters. Persist `next_batch` token to file (`~/.clawq/matrix_sync_token`) between restarts.

**Send:**
```
PUT {homeserver_url}/_matrix/client/v3/rooms/{roomId}/send/m.room.message/{txnId}
Authorization: Bearer {access_token}
Content-Type: application/json

{"msgtype":"m.text","body":"..."}
```

`txnId` is a monotonic counter (UUID or timestamp-based) stored in process state to ensure idempotency.

**Typing notification (optional):**
```
PUT {homeserver_url}/_matrix/client/v3/rooms/{roomId}/typing/{userId}
{"typing":true,"timeout":30000}
```

**Chunking:** Split at 4000-byte boundary, send as separate messages.

**Error handling:** 401 → log token invalid, stop channel. 429 → respect `Retry-After` header. Network error → exponential backoff with max 60s delay.

### Session Key Format

`matrix:{roomId}:{senderId}` — consistent with Discord/Slack session key pattern.

### Files

- **Create**: `src/matrix.ml` — Matrix channel implementation
- **Modify**: `src/runtime_config.ml` — add `matrix_config`
- **Modify**: `src/config_loader.ml` — parse `channels.matrix`
- **Modify**: `src/daemon.ml` — start Matrix if enabled
- **Modify**: `src/dune` — add `matrix` to `clawq_runtime_integrations`

---

## Test Strategy

### Signal Tests
- JSON-RPC message parsing (mock data → `(sender, text)`)
- REST response parsing
- `allow_from` filter: blocked sender returns no message
- Chunking: 5000-byte message → two chunks
- SSE event line parser: `data: {...}\n\n` → JSON decode

### Matrix Tests
- Sync response parser: extract room events, filter own messages
- Typing notification format
- `next_batch` token read/write
- `allow_rooms` / `allow_users` filters
- Transaction ID generation (monotonic, unique per message)
- 4000-byte chunk boundary (UTF-8 safe)

Run: `make test` after each module.

## Dependencies

- No new opam dependencies. HTTP via `cohttp-lwt-unix`. Streaming response bodies already supported.
- Signal requires external `signal-cli` daemon (not a code dependency).
