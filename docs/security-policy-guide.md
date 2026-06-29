# Clawq Security & Policy Guide

**Status**: Authoritative  
**Last updated**: 2026-06-30  
**Scope**: Credential management, egress policy, room policy, invocation restrictions, audit, and verification status

This guide is the single reference for Clawq's security model. It covers how credentials are stored and resolved, how outbound network requests are controlled, how rooms are classified for policy enforcement, and how role-based access restrictions work.

---

## Table of Contents

1. [Credential Management](#1-credential-management)
2. [Egress Policy](#2-egress-policy)
3. [Egress Audit](#3-egress-audit)
4. [Room Policy](#4-room-policy)
5. [Invocation Restrictions](#5-invocation-restrictions)
6. [Credential Security](#6-credential-security)
7. [Network Security](#7-network-security)
8. [Verification Status](#8-verification-status)

---

## 1. Credential Management

Clawq uses a **credential handle** abstraction to decouple credential storage from credential usage. Handles are opaque identifiers referenced by access bundles; the actual credential values are resolved only at the call boundary.

### 1.1 Credential Handles

A credential handle binds an ID to a provider:

```yaml
# In config (access bundle)
credential_handles:
  - github-app:main
  - slack-bot:prod
```

Each handle is defined in the credential handle registry:

```yaml
credential_handles:
  - id: "github-app:main"
    provider:
      type: env_var
      name: "GITHUB_TOKEN"
    description: "GitHub PAT for main org"
    status: "active"
```

**Key invariant**: The credential value itself is NEVER stored in the config record, serialized to JSON, included in prompts, snapshots, logs, the ledger, or worker sandboxes. Only the handle ID is referenced.

### 1.2 Providers

Four provider types are supported:

| Provider | Resolution | Config Example |
|----------|-----------|----------------|
| `env_var` | Reads from named environment variable | `type: env_var`, `name: "GITHUB_TOKEN"` |
| `file` | Reads from file path (supports `~` expansion) | `type: file`, `path: "~/.config/my-token"` |
| `encrypted` | Decrypts `$ENC:...` cipher text via `Secret_store` (AES-256-GCM, PBKDF2-derived key from `CLAWQ_MASTER_KEY`) | `type: encrypted`, `cipher_text: "$ENC:a1b2c3..."` |
| `prompt` | Interactive supply at startup (not supported for automatic resolution) | `type: prompt`, `description: "Enter API key"` |

### 1.3 Lease API

The credential lease API resolves handles into request-ready decorations at the call boundary. This is the core security boundary for credential access.

```ocaml
(* Resolve a credential handle into a lease *)
val resolve_lease :
  config:Runtime_config.t ->
  handle_id:string ->
  header_name:string ->
  (lease, resolution_error) result

(* Apply lease decorations to side effects only *)
val apply_headers : lease -> ((string * string) list -> unit) -> unit
val apply_env_vars : lease -> ((string * string) list -> unit) -> unit
val apply_url_segment : lease -> (string -> unit) -> unit
```

**Security model**: The `lease` type has two parts:
- `identity` -- contains only **redacted** values (safe for logging, prompts, tool arguments)
- `decorations` -- an abstract type that internally holds raw values; callers cannot construct or inspect it directly

The `apply_*` functions are the **only** way to access raw values. They take `unit`-returning closures intended for side effects (HTTP requests, subprocess invocation) at the call boundary.

```ocaml
(* Example: resolving and applying a credential *)
let lease = Credential_lease.resolve_lease ~config ~handle_id:"github-app:main"
              ~header_name:"Authorization" in
match lease with
| Error e -> log_error (Credential_lease.resolution_error_to_string e)
| Ok lease ->
    (* Safe: identity is redacted *)
    Printf.printf "Using credential: %s (%s)"
      lease.identity.handle_id lease.identity.redacted_value;
    (* Only way to access raw value: apply at call boundary *)
    Credential_lease.apply_headers lease (fun headers ->
        Http_client.post_json ~uri ~headers ~body)
```

### 1.4 Scoped Access

Credential handles can be scoped to specific access bundles. The `resolve_scoped_*` functions enforce that a handle is in the allowed list before resolving:

```ocaml
val resolve_scoped_lease :
  config:Runtime_config.t ->
  allowed_handle_ids:string list ->
  handle_id:string ->
  header_name:string ->
  (lease, resolution_error) result
```

If the handle ID is not in `allowed_handle_ids`, resolution fails with `Handle_not_allowed`.

### 1.5 Redaction

Redaction is applied by `redact_secret`: first 3 characters visible, rest replaced with asterisks.

```
Input:  "ghp_abc123xyz789"
Output: "ghp************"
```

The `redacted_identity` type carries:
- `handle_id` -- the opaque identifier (not a secret)
- `provider_type` -- "env_var", "file", "encrypted", or "prompt"
- `description` -- human-readable description
- `redacted_value` -- first 3 chars + asterisks

---

## 2. Egress Policy

Egress policy controls which outbound HTTP requests Clawq is allowed to make. Every outbound request is evaluated against an ordered set of rules; the first matching rule wins.

### 2.1 Rule Format

```yaml
# In an access bundle
egress_rules:
  - host: "api.github.com"
    path: None          # optional: match any path
    method_: None       # optional: match any method
    action: allow
    log_policy: log

  - host: "*.example.com"
    path: "/api/*"
    method_: "POST"
    action: allow
    log_policy: no_log

  - host: "malicious.example.com"
    action: deny
    log_policy: log
```

Each rule has:

| Field | Type | Description |
|-------|------|-------------|
| `host` | `string` | Host pattern. Supports glob wildcards: `*` (any host), `*.example.com` (any subdomain), exact match |
| `path` | `string option` | Optional path pattern. Supports `*` suffix: `/api/*` matches `/api/anything`. `None` matches any path |
| `method_` | `string option` | Optional HTTP method (case-insensitive). `None` matches any method |
| `action` | `Allow \| Deny` | Whether to permit or block the request |
| `log_policy` | `Log \| No_log` | Whether to log matching requests |

### 2.2 Host Matching

Host matching uses a recursive backtracking glob matcher:

| Pattern | Matches | Does Not Match |
|---------|---------|----------------|
| `*` | Any host | -- |
| `*.example.com` | `sub.example.com`, `deep.sub.example.com` | `example.com` |
| `api.example.com` | `api.example.com` | `api2.example.com` |
| `*.api.example.com` | `v1.api.example.com` | `api.example.com` |

Matching is case-insensitive for hostnames.

### 2.3 Path Matching

| Pattern | Matches | Does Not Match |
|---------|---------|----------------|
| `*` | Any path | -- |
| `/api/*` | `/api/users`, `/api/v1/items` | `/api`, `/v1/api/users` |
| `/v1/users` | `/v1/users` | `/v1/users/123` |
| `None` (in rule) | Any path | -- |

### 2.4 First-Match-Wins

Rules are evaluated in order. The first rule whose host, path, and method all match determines the action. This means rule ordering matters:

```yaml
egress_rules:
  # Rule 0: Allow GitHub API
  - host: "api.github.com"
    action: allow

  # Rule 1: Deny everything else under github.com
  - host: "*.github.com"
    action: deny

  # Rule 2: Allow any host (will never match if rules 0-1 are above)
  - host: "*"
    action: allow
```

### 2.5 Default Deny

If no rule matches, the request is **denied** with `matched_rule_index: -1` and logged. An empty `egress_rules` list means all outbound requests are denied.

```ocaml
(* Egress evaluator returns Deny when no rule matches *)
let evaluate ~rules ~host ?path ?method_ () =
  match find_first_match 0 rules with
  | Some (rule, idx) -> { action = rule.action; matched_rule_index = idx; ... }
  | None -> { action = Deny; matched_rule_index = -1; ... }
```

### 2.6 Policy-Aware HTTP Client

The `Policy_http_client` module wraps every outbound HTTP request with policy evaluation:

```ocaml
(* These functions check egress policy before making the request *)
val Policy_http_client.get :
  rules:egress_rule list -> uri:string -> headers:(string * string) list ->
  ?audit:audit_context -> unit -> (string, policy_error) result Lwt.t

val Policy_http_client.post_json :
  rules:egress_rule list -> uri:string -> headers:(string * string) list ->
  body:string -> ?audit:audit_context -> unit ->
  (string, policy_error) result Lwt.t
```

When a request is denied, the caller receives a `policy_error` with:
- `host`, `path`, `method_` -- what was requested
- `matched_rule_index` -- which rule denied it (-1 for default deny)
- `message` -- human-readable explanation

### 2.7 CLI Examples

```bash
# Show current egress rules (resolved from all bundles)
clawq config get security.egress_rules

# Egress rules are part of access bundles
clawq access show <bundle-id>

# View egress audit log (see section 3)
clawq egress audit --limit 50
clawq egress audit --decision denied
clawq egress audit --session-key "slack:C12345"
```

---

## 3. Egress Audit

Every egress policy decision (allowed or denied) is recorded into a dedicated SQLite table for compliance and debugging.

### 3.1 Audit Events

Each event captures:

| Field | Description | Redaction |
|-------|-------------|-----------|
| `id` | Auto-incrementing ID | -- |
| `timestamp` | ISO 8601 with microseconds | -- |
| `decision` | `allowed` or `denied` | -- |
| `host_redacted` | Redacted hostname | `api.github.com` -> `a**.g******.com` |
| `method_redacted` | Redacted HTTP method | `POST` -> `P**T` |
| `path_redacted` | Redacted URL path | `/api/v1/users/123` -> `/api/**` |
| `matched_rule_index` | Index of matching rule (-1 for default deny) | -- |
| `session_key` | Session that made the request | Stored as-is |
| `snapshot_id` | Access snapshot ID | Stored as-is |
| `tool_name` | Tool that triggered the request | Stored as-is |
| `profile_id` | Room profile ID | Stored as-is |
| `credential_handle_ids` | Credential handle IDs (opaque aliases, never actual values) | Stored as-is |

### 3.2 Redaction Rules

**Host redaction**: Keeps TLD and first label visible, obscures intermediate labels.
```
"api.github.com"     -> "a**.g******.com"
"example.com"        -> "e******.com"
"localhost"          -> "l********"
"sub.api.example.com" -> "s**.a**.e******.com"
```

**Method redaction**: Shows first and last character when length >= 3.
```
"GET"   -> "G*T"
"POST"  -> "P**T"
"PUT"   -> "P*T"
"PATCH" -> "P***H"
```

**Path redaction**: Keeps first segment, obscures the rest.
```
"/api/v1/users/123" -> "/api/**"
"/health"           -> "/health"
"/"                 -> "/"
```

### 3.3 Query Interface

```ocaml
val Egress_audit.query :
  db:Sqlite3.db ->
  ?decision:decision ->
  ?session_key:string ->
  ?tool_name:string ->
  ?from_timestamp:string ->
  ?to_timestamp:string ->
  ?limit:int ->
  unit ->
  event list
```

Filter by decision, session, tool, or time range. Default limit: 100 events.

### 3.4 Retention

Old audit events can be purged:

```ocaml
val Egress_audit.delete_before :
  db:Sqlite3.db -> before_timestamp:string -> int
```

Returns the number of deleted rows.

### 3.5 JSON Export

Each event serializes to JSON via `event_to_json` for integration with external SIEM systems.

---

## 4. Room Policy

Room policy classifies conversations by their external/guest dimensions and applies per-connector policy actions.

### 4.1 Room Classification

Rooms are classified into five scopes:

| Scope | Meaning | Example |
|-------|---------|---------|
| `Rm_dm` | Direct message between two internal users | Slack DM, Teams 1:1 chat |
| `Rm_group` | Internal group conversation | Slack channel, Teams group chat |
| `Rm_external` | Room with external participants | Slack Connect channel with external org |
| `Rm_shared` | Shared room/channel with another organization | Teams shared channel |
| `Rm_unknown` | Connector does not expose classification metadata | IRC, custom connectors |

A `room_classification` record contains:
- `connector` -- lowercase connector name ("teams", "slack", etc.)
- `room_id` -- room/channel identifier
- `scope` -- one of the five scopes above
- `has_external_users` -- true when the connector detects outside users
- `tenant_id` -- tenant/organization identifier when available

### 4.2 Policy Actions

Three policy actions control what happens in each room type:

| Action | Behavior |
|--------|----------|
| `Policy_allow` | Proceed without restriction |
| `Policy_warn msg` | Proceed but log and surface the warning to the requester |
| `Policy_deny (reason, allow_admin_override)` | Deny work; optionally allow admin callers to override |

### 4.3 Policy Configuration

```yaml
external_room_policy:
  default_action:
    type: warn
    message: "This room has external participants. Be careful with sensitive data."
  per_connector:
    teams:
      type: deny
      reason: "External Teams rooms are not permitted."
      allow_admin_override: true
    slack:
      type: warn
      message: "Slack Connect channel detected."
```

### 4.4 Evaluation Logic

1. **Internal DMs and groups** (`Rm_dm`, `Rm_group`) are always allowed -- policy only gates rooms with external/sharing dimensions.
2. **External/shared rooms** (`Rm_external`, `Rm_shared`) use the per-connector override if present, otherwise the default action.
3. **Unknown rooms** (`Rm_unknown`) use the per-connector override if present, otherwise the default action.

```ocaml
(* Combined room policy + role check (primary entry point) *)
val Invocation_restrict.check_room_policy_and_role :
  config:Runtime_config.t ->
  key:string ->
  channel:string option ->
  channel_id:string option ->
  user_group:string option ->
  ?has_external_users:bool ->
  work_kind:work_kind ->
  unit ->
  (room_classification * string, string) result
```

### 4.5 Admin Override

When a `Policy_deny` has `allow_admin_override: true`, admin callers can proceed after acknowledging the risk. Non-admin callers see a message suggesting they ask an admin to approve.

### 4.6 Connector Support

Connectors that expose guest/external metadata feed it into the classification. Unsupported connectors return `Rm_unknown` and the policy's default action applies. The `room_status_message` function provides a human-readable explanation:

```
"This room includes external users (connector: teams). External participants may have different access levels."
```

---

## 5. Invocation Restrictions

Invocation restrictions enforce role-based access for different kinds of work.

### 5.1 Work Kinds

| Work Kind | Description | Minimum Role |
|-----------|-------------|--------------|
| `Room_work` | Room turn / session work | Any (room policy handles external restrictions) |
| `Routine` | Scheduled routine execution | Member |
| `Memory_mutation` | Memory save/correct/forget | Member |
| `GitHub_trigger` | GitHub webhook-triggered work | Member |
| `Background_task` | Background task spawning | Member |

### 5.2 Caller Roles

| Role | Description |
|------|-------------|
| `Admin` | Admin user (full access) |
| `Member` | Regular member |
| `Guest` | Guest/external user |
| `Unknown` | Unknown role (treated as guest) |

### 5.3 Role Resolution

The caller's role is resolved from the `user_group` field. If absent, the caller is treated as `guest`.

```ocaml
let role = caller_role_of_string (Option.value user_group ~default:"guest")
```

### 5.4 Denial Messages

Denials are explainable and safe to show to users:

```
Access denied: guest role is not permitted to perform routine. Required: admin or member.
```

### 5.5 Combined Check

The `check_room_policy_and_role` function combines role-based and room-based restrictions in a single call:

1. First checks role-based restrictions (e.g., guest cannot run routines)
2. If role check passes, evaluates room policy (e.g., external room restrictions)
3. Returns `Ok (classification, decision)` or `Error msg`

---

## 6. Credential Security

### 6.1 What Gets Redacted

| Surface | Redaction |
|---------|-----------|
| **Config display** (`clawq config show`) | All keys containing `token`, `secret`, `password`, `api_key`, `private_key`, `tunnel_name` are replaced with `***` |
| **HTTP debug logs** (`http_debug.ml`) | Headers `authorization`, `x-api-key`, `api-key`, `cookie`, `set-cookie`, `proxy-authorization` are redacted via `redact_token` |
| **Credential lease identity** | First 3 characters + asterisks |
| **Egress audit events** | Host, method, path all redacted (see section 3.2) |
| **Runner relay tokens** | SHA256-hashed before storage; token returned to caller once |
| **Audit signing** | HMAC-SHA256 key derived from `CLAWQ_MASTER_KEY`, never logged |
| **Secret store** | `CLAWQ_MASTER_KEY` env var never logged; encrypted values use AES-256-GCM |

### 6.2 Where Credentials Appear

Credentials appear in three contexts:

1. **HTTP headers** -- Most connectors pass tokens via `Authorization: Bearer <token>` or custom headers
2. **URL paths** -- Telegram embeds bot tokens in the URL path (`/bot<token>/sendMessage`)
3. **CLI arguments** -- Nostr passes private keys via `--sec` flag to the `nak` CLI (visible in `ps aux`)

### 6.3 Known Inventory

The full credential callsite inventory is documented in [`credential-callsite-inventory.md`](credential-callsite-inventory.md). Summary:

| Category | Callsites | Redaction Status |
|----------|----------|-----------------|
| LLM Providers | ~20 | `Http_debug` redacts in debug logs; normal log paths unredacted |
| Connectors (Discord, Slack, etc.) | ~60 | Most unredacted in request path |
| GitHub | ~10 | Existing (via `auth_headers`) |
| Telegram | ~18 | Token in URL path (Telegram API design constraint) |
| Config display | ~5 | Existing (config_show) |
| HTTP debug | ~5 | Existing (redact_token) |
| Secret store | ~2 | Existing (AES-256-GCM) |

### 6.4 Security Notes

- **Teams JWT validation is claims-only**: `teams_auth.ml` does NOT verify JWT cryptographic signatures -- only claims are checked.
- **Nostr CLI exposure**: Private keys passed as `--sec` arguments are visible in `ps aux` output.
- **No centralized credential redaction middleware**: Each module handles (or doesn't handle) redaction independently.
- **Encryption at rest**: Use `$ENC:` prefix with `CLAWQ_MASTER_KEY` for config values:
  ```yaml
  api_key: "$ENC:a1b2c3d4e5f6..."
  ```

---

## 7. Network Security

### 7.1 Outbound Callsite Inventory

The full network callsite inventory is documented in [`network-callsite-inventory.md`](network-callsite-inventory.md).

**Transport breakdown**:

| Transport | Count | Description |
|-----------|-------|-------------|
| HTTP (`Http_client`) | ~115 | Most outbound calls |
| HTTP-direct (Cohttp) | 3 | `openai_codex_oauth.ml`, `teams_auth.ml`, `mcp_client.ml` |
| WebSocket | 8 | Discord gateway, Slack socket, Lark, DingTalk, Mattermost, OneBot |
| TCP/TLS (raw) | 6 | IRC, email IMAP/SMTP |
| Subprocess | ~20 | Nostr via `nak`, Vertex via `gcloud`, tunnels, ACP, MCP stdio |

### 7.2 Enforceability Classes

| Class | Count | Description |
|-------|-------|-------------|
| **EXISTING** | ~70 | Host is known/static (e.g., `api.github.com`, `discord.com`). Fully enforceable by egress evaluator. |
| **DYNAMIC** | ~55 | Host is user/config-provided. Egress evaluator can enforce if host is resolved before evaluation. |
| **LOCAL** | ~5 | Loopback/local only. Egress policy not applicable. |
| **NOT-ENFORCEABLE** | ~7 | Subprocess-based calls that cannot be intercepted by OCaml-level egress evaluator. |

### 7.3 Static-Host Callsites (Fully Enforceable)

These services use fixed hosts and are fully covered by egress rules:

| Service | Host |
|---------|------|
| GitHub | `api.github.com` |
| Discord | `discord.com`, `gateway.discord.gg` |
| Slack | `slack.com`, `wss-primary.slack.com` |
| Telegram | `api.telegram.org` |
| LINE | `api.line.me` |
| WhatsApp | `graph.facebook.com` |
| Microsoft | `login.microsoftonline.com` |
| Lark | `open.feishu.cn`, `open.larksuite.com` |
| DingTalk | `api.dingtalk.com` |
| Brave Search | `api.search.brave.com` |
| Anthropic | `api.anthropic.com` |
| Z.ai | `api.z.ai` |
| Kimi | `api.kimi.com` |
| Cursor | `www.cursor.com` |
| OpenAI Codex | `chatgpt.com`, `auth0.openai.com` |

### 7.4 Dynamic-Host Callsites

These use user-configured hosts and require host injection at evaluation time:

- LLM providers (OpenAI-compatible, Anthropic, Gemini, Cohere, MiniMax, Vertex, Ollama)
- Matrix, Mattermost, OneBot
- IRC, Email
- MCP HTTP servers
- Gateway, attachment downloads
- Vector embeddings, TTS, STT

### 7.5 Non-Interceptable Callsites

These bypass Clawq's OCaml-level egress evaluator:

| Module | Mechanism | Risk |
|--------|-----------|------|
| Nostr (`nak` CLI) | Subprocess with CLI args | Private key in `ps aux` |
| Vertex (`gcloud` CLI) | Subprocess | OAuth token managed by gcloud |
| Tunnel subprocesses | `cloudflared`, `ngrok`, `tailscale`, custom | Auth managed by tunnel tool |
| Shell execution | User-supplied commands | Full network access |

**Mitigation**: Use OS-level controls (network namespaces, iptables, seccomp) to restrict subprocess network access.

### 7.6 Bypassed Callsites

Three callsites use `Cohttp_lwt_unix.Client` directly, bypassing `Http_client` and its policy checks:

1. `openai_codex_oauth.ml` -- OAuth token exchange with `auth.openai.com`
2. `teams_auth.ml` -- OAuth client_credentials to Azure AD
3. `mcp_client.ml` -- HTTP MCP server transport

These should be migrated to `Http_client` or have hosts added to egress evaluation separately.

---

## 8. Verification Status

The verification boundaries document ([`verification-boundaries.md`](verification-boundaries.md)) provides a cross-cutting view of all security-relevant subsystems.

### 8.1 Status Legend

| Tag | Meaning |
|-----|---------|
| **[RUNTIME]** | Enforced by runtime code logic |
| **[TEST]** | Covered by executable conformance tests (Alcotest) |
| **[PROOF-CANDIDATE]** | Candidate for formal proof or stronger verification |
| **[GAP]** | Known gap: neither runtime code nor test enforces this invariant |

### 8.2 Scope Resolution

All scope-resolution invariants are enforced at runtime AND covered by tests. All are proof candidates.

| Category | Invariants | Runtime | Tests | Proof Candidates | Gaps |
|----------|-----------|---------|-------|-----------------|------|
| Determinism | INV-DET-1..2 | Yes | 4 tests | 2 | 0 |
| Precedence | INV-PREC-1..3 | Yes | 3 tests | 3 | 0 |
| Conflict Resolution | INV-CONF-1..5 | Yes | 7 tests | 5 | 0 |
| Global Security | INV-SEC-1..3 | Yes | 3 tests | 3 | 0 |
| Snapshots | INV-SNAP-1..4 | Yes | 7 tests | 4 | 0 |
| Egress Ordering | INV-EGR-1 | Yes | 1 test | 1 | 0 |
| Reload | INV-RLD-1..5 | Yes | 9 tests | 5 | 0 |

### 8.3 Memory Policy Isolation

Most memory-policy invariants are enforced at runtime and tested. Known gaps:

| Category | Invariants | Runtime | Tests | Gaps |
|----------|-----------|---------|-------|------|
| Scope Isolation | INV-MEM-ISO-1..4 | Yes | 8 tests | 0 |
| Visibility | INV-VIS-1..5 | Yes | 7 tests | 0 |
| Grant Resolution | INV-GRANT-1..4 | Yes | 7 tests | 0 |
| Credential | INV-CRED-1..3 | Yes | 3 tests | 0 |
| Egress Default-Deny | INV-EGR-1..3 | Yes | 2 of 3 | 0 |
| MCP Filter | INV-FILTER-1..3 | Yes | 1 of 3 | 0 |
| Budget | INV-BUDG-1..5 | Yes | 0 tests | 0 |
| Session Lifecycle | INV-SESS-1..4 | Yes | 0 tests | 0 |
| **Redaction** | **INV-REDACT-3b** | **No** | **No** | **1 (FTS redaction)** |
| Ledger | INV-LEDGER-1..3 | Yes | 5 tests | 0 |

### 8.4 Known Gaps

#### Critical

| ID | Subsystem | Description |
|----|-----------|-------------|
| **INV-REDACT-3b** | Memory Redaction | `Memory.search` FTS path does not filter `sm.redacted_at IS NULL` |

#### Moderate

| ID | Subsystem | Description |
|----|-----------|-------------|
| INV-BUDG-* | Budget | No dedicated invariant tests (enforced by code) |
| INV-SESS-* | Session Lifecycle | No dedicated invariant tests (enforced by code) |
| INV-FILTER-2 | MCP/Skills | No dedicated skills filter test (enforced by code) |
| INV-EGR-2 | Egress Evaluator | Unmatched-destinations-remain-denied not directly tested |

#### Low

| ID | Subsystem | Description |
|----|-----------|-------------|
| Credential redaction | Connectors | Most connector credentials unredacted in normal log paths |
| Subprocess egress | Network | Nostr, tunnels bypass OCaml egress evaluator |

### 8.5 Proof Candidates

All scope-resolution and memory-policy invariants are candidates for formal verification. Priority targets:

1. **INV-DET-1..2** (Determinism) -- Same inputs produce same outputs
2. **INV-SEC-1..3** (Global Security) -- No scope can escalate beyond global policy
3. **INV-MEM-ISO-1..4** (Scope Isolation) -- Memory scopes are isolated
4. **INV-EGR-1** (Egress Ordering) -- First-match-wins is deterministic
5. **INV-REDACT-1..4** (Redaction) -- Credential values never leak to logs/prompts

---

## Quick Reference

### Config File Locations

| File | Purpose |
|------|---------|
| `~/.clawq/config.yaml` | Main config (providers, channels, security) |
| `~/.clawq/access.yaml` | Access bundles, scopes, credential handles |
| `~/.clawq/secrets/` | Encrypted credential store |

### Key CLI Commands

```bash
# Show config (redacted)
clawq config show

# Show access bundles and scopes
clawq access list
clawq access show <bundle-id>

# Show credential handles
clawq credentials list

# Show egress rules for current scope
clawq egress rules

# Query egress audit log
clawq egress audit --limit 50
clawq egress audit --decision denied
clawq egress audit --tool-name "http_request"

# Show room classification
clawq rooms status

# Show invocation restrictions
clawq permissions check --work-kind routine --user-group guest
```

### Key Source Files

| File | Purpose |
|------|---------|
| `src/credential_lease.ml` | Credential resolution and lease API |
| `src/egress_evaluator.ml` | Egress rule matching (first-match-wins) |
| `src/egress_audit.ml` | Egress audit event recording |
| `src/policy_http_client.ml` | Policy-aware HTTP client wrapper |
| `src/room_policy.ml` | Room classification and policy evaluation |
| `src/invocation_restrict.ml` | Role-based invocation restrictions |
| `src/runtime_config_types.ml` | Type definitions for all security types |
| `src/secret_store.ml` | AES-256-GCM encryption at rest |
| `src/http_debug.ml` | HTTP debug logging with credential redaction |
| `src/config_show.ml` | Config display with credential redaction |

### Related Documentation

| Document | Purpose |
|----------|---------|
| [`credential-callsite-inventory.md`](credential-callsite-inventory.md) | Every location where credentials are used |
| [`network-callsite-inventory.md`](network-callsite-inventory.md) | Every outbound network callsite |
| [`verification-boundaries.md`](verification-boundaries.md) | Cross-cutting verification status |
| [`scope-resolution-invariants.md`](scope-resolution-invariants.md) | Scope resolution invariant specifications |
| [`memory-policy-isolation-invariants.md`](memory-policy-isolation-invariants.md) | Memory policy invariant specifications |
