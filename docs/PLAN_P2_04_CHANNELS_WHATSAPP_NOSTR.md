# P2-04: Channels — WhatsApp & Nostr

## Context

WhatsApp (via the Meta Cloud API) and Nostr (a decentralized relay-based protocol) each use distinct paradigms. WhatsApp is webhook-push from Meta's servers; Nostr uses WebSocket relays with cryptographic identity. Both expand clawq to important ecosystems.

---

## Part A: WhatsApp Channel (Meta Cloud API)

### How It Works

The **Meta WhatsApp Cloud API** (v18.0+) is webhook-based:
- **Inbound:** Meta POSTs events to your configured webhook URL. The HTTP gateway must expose this endpoint.
- **Outbound:** POST to `https://graph.facebook.com/v18.0/{phone_number_id}/messages`
- **Verification:** GET to your webhook URL with `hub.mode=subscribe&hub.challenge=...&hub.verify_token={token}` — return the challenge value.

### Config

```ocaml
whatsapp : whatsapp_config option

type whatsapp_config = {
  enabled : bool;
  phone_number_id : string;    (* From Meta Developer Console *)
  access_token : string;       (* Permanent system user token *)
  verify_token : string;       (* Webhook verification token you choose *)
  api_version : string;        (* default "v18.0" *)
  allow_from : string list;    (* E.164 phone numbers; empty = allow all *)
}
```

### Implementation

**Webhook registration in `http_server.ml`:**

Add routes to the existing Cohttp gateway:
- `GET /whatsapp/webhook` — verify token handshake:
  ```ocaml
  (* hub.mode=subscribe, hub.verify_token matches config, return hub.challenge *)
  ```
- `POST /whatsapp/webhook` — inbound message handler:
  ```json
  {"object":"whatsapp_business_account","entry":[{"changes":[{"value":{"messages":[{"from":"15551234567","text":{"body":"Hello"}}]}}]}]}
  ```

Parse nested JSON structure, extract `from` (sender phone E.164), `text.body` (message text), `id` (message ID for dedup). Apply `allow_from` filter.

**Outbound (new `src/whatsapp.ml`):**
```
POST https://graph.facebook.com/{api_version}/{phone_number_id}/messages
Authorization: Bearer {access_token}
Content-Type: application/json

{
  "messaging_product": "whatsapp",
  "to": "+15551234567",
  "type": "text",
  "text": {"body": "reply text"}
}
```

**Session key:** `whatsapp:{sender_phone}`

**Message dedup:** Track recent message IDs in bounded in-memory set (max 500, LRU).

### Files

- **Create**: `src/whatsapp.ml` — outbound sender + config types
- **Modify**: `src/http_server.ml` — add WhatsApp webhook routes
- **Modify**: `src/runtime_config.ml` — add `whatsapp_config`
- **Modify**: `src/config_loader.ml` — parse `channels.whatsapp`
- **Modify**: `src/daemon.ml` — pass whatsapp config to http_server (it's webhook-based, no polling loop needed)
- **Modify**: `src/dune` — add `whatsapp` to `clawq_runtime_integrations`

### Setup Requirements

User must configure a webhook URL (via tunnel or public server) in Meta Developer Console. The existing `tunnel_cloudflare.ml` can provide the URL. Document in QUICKSTART.

---

## Part B: Nostr Channel

### How It Works

[Nostr](https://nostr.com) is a decentralized protocol. Clients connect to one or more **relays** (WebSocket servers). Messages are signed JSON events. Private messages use **NIP-17** (Gift Wraps, preferred) or **NIP-04** (legacy encrypted DMs).

**Identity:** An `nsec` private key (32 bytes, Bech32-encoded) used to sign all events.

**Receive:** Subscribe to relay with `["REQ", "sub-id", {"#p": [pubkey], "kinds": [1059]}]` (NIP-17 gift wraps directed at our pubkey), or `["REQ", ..., {"#p": [pubkey], "kinds": [4]}]` for NIP-04.

**Send:** Publish a signed event: `["EVENT", {...}]`

### Approach: Use `nak` CLI

The `nak` tool (Go binary, `github.com/fiatjaf/nak`) handles relay interaction, NIP-17 gift-wrap encoding/decoding, and key signing. This avoids implementing Schnorr signatures and Bech32 decoding in OCaml from scratch.

```bash
# Receive NIP-17 DMs
nak req --sec {nsec} -k 1059 -p {pubkey} wss://relay.damus.io

# Send NIP-17 DM
nak direct-message --sec {nsec} -p {recipient_pubkey} "Hello" wss://relay.damus.io
```

This matches nullclaw's approach (it also shells out to `nak`).

### Config

```ocaml
nostr : nostr_config option

type nostr_config = {
  enabled : bool;
  nsec : string;                 (* private key in nsec1... format *)
  relays : string list;          (* WebSocket relay URLs *)
  allow_from : string list;      (* npub or hex pubkeys; empty = allow all *)
  nip : int;                     (* 17 (default) or 4 *)
  nak_path : string;             (* path to nak binary, default "nak" *)
}
```

### Implementation: `src/nostr.ml`

**Receive loop:**
Spawn `nak req` subprocess with `Lwt_process`, read lines from stdout, parse JSON events:
```json
{"kind":1059,"pubkey":"...","content":"<encrypted>","tags":[...]}
```

For NIP-17: pipe through `nak decrypt --sec {nsec}` to get the inner rumor.
Parse inner event for sender pubkey and `content` field (plaintext message).

Track processed event IDs in bounded set (max 1000) to avoid re-processing on relay resync.

**Send:**
```ocaml
let send_nostr_dm ~config ~recipient_pubkey ~text =
  let args = ["direct-message"; "--sec"; config.nsec; "-p"; recipient_pubkey; text]
             @ config.relays in
  run_process "nak" args
```

**Session key:** `nostr:{sender_pubkey_hex}`

**Protocol mirroring:** Track which NIP each sender used; reply using the same NIP.

**Relay rotation:** Connect to all configured relays simultaneously; deduplicate events by ID.

### Files

- **Create**: `src/nostr.ml`
- **Modify**: `src/runtime_config.ml` — add `nostr_config`
- **Modify**: `src/config_loader.ml` — parse `channels.nostr`
- **Modify**: `src/daemon.ml` — start Nostr if enabled
- **Modify**: `src/dune` — add `nostr` to `clawq_runtime_integrations`

### External Requirement

`nak` binary must be installed. `cmd_doctor` should check for `nak` in PATH if Nostr config is present.

---

## Test Strategy

### WhatsApp Tests
- Webhook verification: GET with valid `hub.verify_token` → return `hub.challenge`; invalid token → 403
- Inbound JSON parser: nested entry/changes/messages structure → `(sender, text)`
- `allow_from` filter: number not in list → message discarded
- Outbound request format: verify JSON body structure and Authorization header
- Dedup: same message ID processed twice → only one session turn

### Nostr Tests
- Config parse: `nsec`, `relays`, `nip` fields
- Event ID dedup set behavior: LRU eviction at 1000 entries
- Process spawn command construction: `nak req --sec {nsec} -k 1059 ...`
- `allow_from` filter: unknown pubkey → discarded
- Protocol mirroring: NIP-04 sender → NIP-04 reply

Run: `make test` after each module.

## Dependencies

- WhatsApp: no new deps (HTTP via `cohttp-lwt-unix`)
- Nostr: `nak` external binary; no new opam packages (process via `Lwt_process` already available)
