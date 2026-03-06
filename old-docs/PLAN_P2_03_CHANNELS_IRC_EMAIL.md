# P2-03: Channels — IRC & Email

## Context

IRC and Email are low-frequency but high-compatibility channels that expand clawq's reach to legacy and enterprise environments. IRC uses a raw TLS socket with line-based protocol; Email uses IMAP polling for inbound and SMTP for outbound.

Both are self-contained and have no WebSocket or special runtime requirements beyond TCP.

---

## Part A: IRC Channel

### Protocol

IRC uses a plain-text line-based protocol over TCP (optionally TLS). Standard flow:

```
→ NICK {nickname}
→ USER {username} 0 * :{realname}
← :server 001 {nick} :Welcome...
→ JOIN #{channel}
← :sender!user@host PRIVMSG #channel :message text
→ PRIVMSG #channel :reply text
```

Heartbeat: respond to `PING :server` with `PONG :server` to stay connected.

### Config

```ocaml
irc : irc_config option

type irc_config = {
  enabled : bool;
  host : string;
  port : int;                  (* default 6697 for TLS, 6667 for plain *)
  tls : bool;                  (* default true *)
  nick : string;
  username : string;
  realname : string;
  password : string option;    (* NickServ or SASL PLAIN *)
  channels : string list;      (* list of channels to JOIN, e.g. ["#clawq"] *)
  allow_from : string list;    (* allowed nick!user@host patterns; empty = all *)
  command_prefix : string;     (* prefix triggering bot, e.g. "!clawq " or direct mention *)
}
```

### Implementation: `src/irc.ml`

**Connection:**
Use `Lwt_unix` for TCP. TLS via `tls-lwt` + `ca-certs` (already a project dependency via `ws_client`).

```ocaml
(* Establish TLS connection *)
let connect ~host ~port ~tls =
  Lwt_unix.getaddrinfo host (string_of_int port) [] >>= fun addrs ->
  let fd = Lwt_unix.socket ... in
  Lwt_unix.connect fd addr >>= fun () ->
  if tls then Tls_lwt.Unix.client_of_fd ... (* reuse ws_client pattern *)
  else Lwt.return (plain_io fd)
```

**Read loop:**
Read lines, parse IRC messages:
```
":nick!user@host COMMAND params :trailing"
```

Handle:
- `PING :x` → `PONG :x`
- `PRIVMSG #chan :text` → extract sender nick, check `allow_from`, route to session
- `001` (welcome) → JOIN configured channels
- `433` (nick in use) → retry with `{nick}_`

**Send:**
```
PRIVMSG {target} :{message}\r\n
```
Chunk at 450 bytes (IRC line limit ~512 bytes; leave room for prefix overhead). Send multiple PRIVMSG lines.

**Authentication (SASL PLAIN, if password set):**
```
→ CAP REQ :sasl
← :server CAP * ACK :sasl
→ AUTHENTICATE PLAIN
← AUTHENTICATE +
→ AUTHENTICATE {base64("\0{nick}\0{password}")}
← 903 :SASL authentication successful
→ CAP END
```

**Session key:** `irc:{channel}:{sender_nick}`

**Reconnect:** On connection drop, exponential backoff (5s, 10s, 20s, up to 120s), then reconnect and re-JOIN channels.

### Files

- **Create**: `src/irc.ml`
- **Modify**: `src/runtime_config.ml` — add `irc_config`
- **Modify**: `src/config_loader.ml` — parse `channels.irc`
- **Modify**: `src/daemon.ml` — start IRC if enabled
- **Modify**: `src/dune` — add `irc` to `clawq_runtime_integrations`

### Dependencies

`tls-lwt`, `ca-certs` already linked via `ws_client`. No new dependencies.

---

## Part B: Email Channel

### Protocol

**Inbound:** IMAP (RFC 3501) polling — connect, SELECT INBOX, SEARCH UNSEEN, FETCH message headers and body.
**Outbound:** SMTP (RFC 5321) — connect, EHLO, STARTTLS or SMTPS, AUTH LOGIN or PLAIN, MAIL FROM / RCPT TO / DATA.

### Config

```ocaml
email : email_config option

type email_config = {
  enabled : bool;
  (* Inbound IMAP *)
  imap_host : string;
  imap_port : int;             (* default 993 for TLS *)
  imap_tls : bool;             (* default true *)
  imap_username : string;
  imap_password : string;
  imap_poll_interval_s : int;  (* default 60 *)
  (* Outbound SMTP *)
  smtp_host : string;
  smtp_port : int;             (* default 587 STARTTLS or 465 SMTPS *)
  smtp_tls : bool;
  smtp_username : string;
  smtp_password : string;
  from_address : string;
  (* Filtering *)
  allow_from : string list;    (* exact, @domain, or bare domain; empty = all *)
  subject_prefix : string;     (* optional subject filter, e.g. "[clawq]" *)
}
```

### Implementation: `src/email.ml`

#### IMAP client (inbound)

Use raw TLS socket (same pattern as IRC above, reusing `tls-lwt`). IMAP is line-based with tagged commands:

```
→ A001 LOGIN {user} {password}
← A001 OK LOGIN completed
→ A002 SELECT INBOX
← * 10 EXISTS
← A002 OK [READ-WRITE] SELECT completed
→ A003 SEARCH UNSEEN
← * SEARCH 8 9 10
← A003 OK SEARCH completed
→ A004 FETCH 8 (BODY[HEADER.FIELDS (FROM SUBJECT MESSAGE-ID IN-REPLY-TO)] BODY[TEXT])
← * 8 FETCH (...)
← A004 OK FETCH completed
→ A005 STORE 8 +FLAGS (\Seen)
```

Parse `FROM:`, `SUBJECT:`, `MESSAGE-ID:`, `IN-REPLY-TO:` headers. Decode RFC 2047 encoded words (`=?utf-8?B?...?=` base64, `=?utf-8?Q?...?=` quoted-printable). Strip basic HTML tags. Apply `allow_from` filter (match exact address, `@domain`, or bare domain).

**Seen tracking:** Mark as `\Seen` after processing. Store seen Message-IDs in a bounded in-memory set (LRU, max 1000) for dedup across sessions.

**Threading:** Track sender's last `Message-ID` to set `In-Reply-To` and `References` on reply.

#### SMTP client (outbound)

```
→ EHLO clawq
← 250-STARTTLS
→ STARTTLS
(upgrade to TLS)
→ AUTH LOGIN
→ {base64(username)}
→ {base64(password)}
→ MAIL FROM:<{from_address}>
→ RCPT TO:<{recipient}>
→ DATA
→ From: {from_address}\r\n
→ To: {recipient}\r\n
→ Subject: Re: {original_subject}\r\n
→ In-Reply-To: {message_id}\r\n
→ References: {message_id}\r\n
→ Content-Type: text/plain; charset=utf-8\r\n
→ \r\n
→ {body}\r\n
→ .\r\n
← 250 OK
→ QUIT
```

**Session key:** `email:{sender_address}`

**Poll loop:** Every `imap_poll_interval_s` seconds, connect → fetch unseen → process → disconnect. Short-lived connections (no persistent IMAP IDLE for simplicity; can be added later).

### Files

- **Create**: `src/email_channel.ml` (named `email_channel` to avoid stdlib conflict)
- **Modify**: `src/runtime_config.ml` — add `email_config`
- **Modify**: `src/config_loader.ml` — parse `channels.email`
- **Modify**: `src/daemon.ml` — start Email if enabled
- **Modify**: `src/dune` — add `email_channel` to `clawq_runtime_integrations`

### Dependencies

No new opam packages. All socket/TLS via `lwt_unix` + `tls-lwt` + `ca-certs`.

---

## Test Strategy

### IRC Tests
- IRC message parser: `":nick!u@h PRIVMSG #chan :hello"` → `(sender="nick", channel="#chan", text="hello")`
- PING handler: `"PING :server"` → `"PONG :server\r\n"`
- 450-byte chunking (UTF-8 safe)
- SASL PLAIN encoding: `base64("\0nick\0pass")`
- `allow_from` pattern matching: exact nick, wildcard `*!*@*`
- Reconnect backoff timing calculation

### Email Tests
- IMAP SEARCH response parser: `"* SEARCH 8 9 10"` → `[8; 9; 10]`
- RFC 2047 decoder: `"=?utf-8?B?SGVsbG8=?="` → `"Hello"` (base64), `"=?utf-8?Q?Hello?="` → `"Hello"` (QP)
- HTML stripper: `"<b>bold</b> text"` → `"bold text"`
- `allow_from` filter: `"@example.com"` matches `"user@example.com"`
- Threading header construction: `In-Reply-To` set from stored Message-ID
- SMTP DATA format: newline-escaped body (`"."` on its own line escaped as `".."`)

Run: `make test` after each module.
