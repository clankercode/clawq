# P2-06: OTP Pairing

## Context

clawq's current pairing implementation is a config flag stub — `gateway.pairing_required` exists in the config but the actual 6-digit OTP challenge/response flow is not implemented. The gateway only checks a static `auth_token` in `Authorization: Bearer` headers.

nullclaw has a full pairing system: a one-time 6-digit code displayed on startup, a brute-force lockout (5 attempts, 5-minute lockout), and SHA-256-hashed bearer tokens issued after successful pairing. This plan implements the equivalent.

## Design

### Pairing Flow

1. **Startup**: If `gateway.pairing_required = true` and no paired tokens exist, generate a random 6-digit code. Print it prominently to stdout/daemon log.
2. **Client calls** `POST /pair` with `{"code": "123456"}`.
3. **Server**:
   - If locked out: return 423 with retry-after.
   - If code matches: generate random 32-byte bearer token, store its SHA-256 hash in `~/.clawq/paired_tokens.json`, return plaintext token to client (only time it's shown).
   - If code doesn't match: increment fail counter; if ≥ 5, set lockout timestamp.
4. **Subsequent requests**: `Authorization: Bearer {token}` validated by hashing token and checking against stored hashes.

### Data Structures

```ocaml
(* src/pairing.ml *)
type t = {
  mutable code : string option;         (* current 6-digit code, None if disabled *)
  mutable fail_count : int;
  mutable lockout_until : float option; (* Unix timestamp, None = not locked *)
  mutable token_hashes : string list;   (* SHA-256 hex hashes of accepted tokens *)
  tokens_path : string;                 (* ~/.clawq/paired_tokens.json *)
}

type pair_result =
  | Paired of string          (* new bearer token, plaintext *)
  | AlreadyPaired             (* tokens exist, pairing closed *)
  | InvalidCode
  | LockedOut of int          (* seconds remaining *)
  | Disabled
```

### Token Storage

`~/.clawq/paired_tokens.json`:
```json
{"tokens": ["sha256hex...", "sha256hex..."]}
```

Load on startup, persist after each pairing. Tokens are SHA-256 hashes; plaintext is never stored.

### Security Properties

- **Brute-force protection**: max 5 attempts; 5-minute lockout (300 seconds). Lock state is in-memory only (resets on daemon restart — acceptable; the code also resets).
- **Constant-time comparison**: use `Eqaf.equal` (already a dependency) for token hash comparison.
- **Single-use code**: once a client pairs successfully, `code` is set to `None`. Can regenerate via `POST /pair/regenerate` (admin-only, requires existing bearer token).
- **One-time display**: token printed once at pairing time, never stored in plaintext.
- **Code generation**: `Mirage_crypto_rng.generate 4` → 4 bytes → `Int32` mod 1_000_000, zero-padded to 6 digits.

### HTTP Endpoints (modify `http_server.ml`)

**`POST /pair`**:
```json
Request:  {"code": "123456"}
Response 200: {"token": "base64-or-hex-bearer-token"}
Response 423: {"error": "locked_out", "retry_after_seconds": 287}
Response 403: {"error": "invalid_code"}
Response 409: {"error": "already_paired"}
Response 404: {"error": "pairing_disabled"}
```

**`POST /pair/regenerate`** (requires valid bearer token):
```json
Response 200: {"code": "654321"}
```

**`GET /pair/status`** (no auth required):
```json
{"pairing_enabled": true, "paired": false, "locked_out": false}
```

### Auth Middleware (modify `http_server.ml`)

Current auth: compare `Authorization: Bearer {token}` directly against `config.gateway.auth_token`.

New auth:
1. If `pairing_required = false` and `auth_token` is set: legacy direct comparison (backward-compatible).
2. If `pairing_required = true`: hash the presented bearer token (SHA-256), look up in `token_hashes`. Use `Eqaf.equal` for each comparison to prevent timing attacks.
3. If `pairing_required = true` and no paired tokens: allow only `/pair*` endpoints and `/health`.

### CLI Integration

`cmd_auth` in `command_bridge.ml` already exists but may just stub. Ensure it:
1. Calls `POST /pair` with user-provided code
2. Saves received token to `~/.clawq/gateway_token` (mode 0600)
3. Prints success with note that token is saved

Add `auth token` subcommand: display the stored token path and creation time.

### Config Changes (`runtime_config.ml`)

Add to `gateway_config`:
```ocaml
pairing_required : bool;          (* default false *)
max_pair_attempts : int;          (* default 5 *)
pair_lockout_seconds : int;       (* default 300 *)
```

## Files to Create/Modify

- **Create**: `src/pairing.ml` — pairing state machine, code gen, brute-force logic
- **Modify**: `src/http_server.ml` — add `/pair`, `/pair/regenerate`, `/pair/status` routes; update auth middleware
- **Modify**: `src/daemon.ml` — initialize `Pairing.t`, print code on startup, pass to http_server
- **Modify**: `src/command_bridge.ml` — enhance `cmd_auth` to call `/pair`, save token
- **Modify**: `src/runtime_config.ml` — add pairing fields to `gateway_config`
- **Modify**: `src/config_loader.ml` — parse new fields
- **Modify**: `src/dune` — add `pairing` to `clawq_runtime_core` (state machine logic) or `clawq_runtime_integrations` (HTTP integration)

## Test Strategy

1. **Code generation**: output is exactly 6 digits, in range 000000–999999
2. **Brute-force**: 5th invalid attempt triggers lockout; 6th attempt during lockout returns `LockedOut`
3. **Lockout expiry**: `lockout_until` in past → attempts reset; lockout cleared
4. **Constant-time**: pairing uses `Eqaf.equal` (verify in code review, not easily unit-tested)
5. **Token hashing**: SHA-256 of known token matches stored hash
6. **Single-use code**: after successful pair, code is `None`; second pair attempt → `AlreadyPaired`
7. **Regenerate**: requires valid token; produces new 6-digit code; old code invalidated
8. **Auth middleware**: valid token hash → 200; invalid → 401; `/pair` accessible without token

Run: `make test`. Add suite `pairing` to `test/test_main.ml`.

## Dependencies

- `Mirage_crypto_rng` (already linked) — secure random for code + token generation
- `Digestif.c` (already linked) — SHA-256 for token hashing
- `Eqaf` (already linked) — constant-time comparison
- No new opam packages
