# Backlog Task Decomposition

## Task Hierarchy

```
I001 (idea, in-progress)
├── [Epic] E-GH: GitHub Webhook Channel
│   ├── T-GH1: Config types + loader + defaults
│   ├── T-GH2: github_api.ml — GitHub REST client
│   ├── T-GH3: github_webhook.ml — HMAC + parse + /clawq extract
│   ├── T-GH4: github.ml — orchestration
│   ├── T-GH5: http_server.ml — webhook routing
│   ├── T-GH6: daemon.ml — wire github channel
│   └── T-GH7: tests + integration
│
├── [Epic] E-CF: Cloudflare Tunnel
│   ├── T-CF1: cf_tunnel.ml — static URL resolution
│   └── T-CF2: cf_tunnel.ml — managed named tunnel subprocess
│
└── [Task] T-DOCS: Setup guide
```

---

## Epic: GitHub Webhook Channel

### T-GH1: Config types + loader + defaults

**What**: Add all new types to `runtime_config.ml`, parse them in `config_loader.ml`, extend `to_json` and `merge_with_coq`, add defaults.

**Files**: `src/runtime_config.ml`, `src/config_loader.ml`

**Acceptance criteria**:
- `github_auth`, `github_repo_config`, `github_config` types defined
- `tunnel_config` extended with `url`, `managed`, `tunnel_name`, `config_dir`
- `channel_config` has `github : github_config option`
- Config JSON with `channels.github` parses correctly
- Config without `channels.github` → `channels.github = None` (no regression)
- `tunnel.url`, `tunnel.managed`, `tunnel.tunnel_name` parse from JSON
- `to_json` round-trip preserves all new fields
- No new opam dependencies
- `make test` passes
- `make fmt-check` passes

**Test notes**: Add roundtrip cases in `test_config.ml` or similar for new fields.

**Dependencies**: None — first task.

---

### T-GH2: github_api.ml — GitHub REST client

**What**: Create `src/github_api.ml` with `auth_headers`, `post_comment`, `reply_to_review_comment`, `get_pr_files`.

**Files**: `src/github_api.ml`, `src/dune`

**Acceptance criteria**:
- `auth_headers (GithubPat token)` returns correct Authorization header
- `post_comment` sends `POST /repos/{o}/{r}/issues/{n}/comments` with correct body
- `reply_to_review_comment` sends `POST /repos/{o}/{r}/pulls/{n}/comments/{id}/replies`
- `get_pr_files` parses `[{filename, status, additions, deletions, ...}]` response array, handles pagination (follow `Link: rel="next"` header up to 300 files)
- Token redacted in all logs
- On non-2xx response: log warning with status code, return `unit` (don't crash)
- Module lives in `clawq_runtime_integrations`
- `make test` passes
- `make fmt-check` passes

**Test notes**: Mock HTTP responses using test fixtures from `api-examples/github/`.

**Dependencies**: T-GH1 (needs `Runtime_config.github_auth` type)

---

### T-GH3: github_webhook.ml — HMAC + parse + /clawq extract

**What**: Create `src/github_webhook.ml`. Pure logic, no I/O, no Lwt. Highly testable.

**Files**: `src/github_webhook.ml`, `src/dune`, `test/test_github.ml`

**Acceptance criteria**:
- `verify_signature` uses `Digestif.SHA256.hmac_string` + `Eqaf.equal` (constant time)
- Signature header format: `sha256=<hex>` — parse and verify correctly
- `parse_event` handles: `pull_request` (opened/edited/reopened only), `issue_comment` (created), `pull_request_review_comment` (created)
- `parse_event` returns `Ignored` for: unknown event types, unsupported actions (closed, merged, deleted, etc.)
- `extract_clawq` correctly identifies `/clawq` trigger (case-insensitive, after leading whitespace)
- `extract_clawq` extracts from `/clawq` line to next blank line or EOF
- `extract_clawq` strips `/clawq` prefix + trims user_message
- `extract_clawq` returns `None` if no `/clawq` found
- `extract_clawq` uses FIRST `/clawq` only (subsequent ignored)
- `full_context_preamble` includes: repo, PR#, title, body, author, event type, diff hunk (review), files list, `gh pr diff` instruction, PR URL
- `session_key` returns correct namespaced key for each event type
- All test suites in `test/test_github.ml` pass (see component-design.md for suite list)
- Uses example payloads from `api-examples/github/*.json` as test input
- `make test` passes, `make fmt-check` passes

**Dependencies**: T-GH1 (config types), research agents must complete (need payload examples)

---

### T-GH4: github.ml — orchestration

**What**: Create `src/github.ml`. Ties together webhook parsing, session routing, and API reply.

**Files**: `src/github.ml`, `src/dune`

**Acceptance criteria**:
- `handle_webhook` follows the exact flow in component-design.md
- Signature failure → return `{|{"error":"invalid signature"}|}` (403 set by http_server)
- `react_to` filter: empty list = all supported; non-empty = only listed types
- `allow_users` filter: `["*"]` = all; otherwise exact username match
- No `/clawq` in event → return `"ok"` (200), no session turn called
- Agent error → post error comment, log error, return `"ok"` (don't propagate exception)
- PR review comment → `reply_to_review_comment` (in-thread)
- Issue comment / PR body → `post_comment` (top-level)
- `format_reply` wraps command in blockquote + appends agent response
- PR files fetch failure → log debug, continue without files (non-fatal)
- `agent_name` field is read from config, logged at Debug, then ignored
- `make test` passes, `make fmt-check` passes

**Logging** (all at Info unless noted):
- Each handled event: `GitHub: {owner}/{repo} {event_type} #{n} by @{user}`
- Ignored events: Debug level
- Signature failures: Warn
- Agent errors: Error
- Reply posted: Info `GitHub: replied to {owner}/{repo} #{n}`
- agent_name set: Debug `GitHub: agent_name={name} configured but routing not yet active`

**Dependencies**: T-GH1, T-GH2, T-GH3

---

### T-GH5: http_server.ml — webhook routing

**What**: Add GitHub webhook routes to existing HTTP server.

**Files**: `src/http_server.ml`

**Acceptance criteria**:
- New optional params `?github_config` and `?github_api_limiter` added to `handler` and `start`
- `POST /github/webhook/{anything}` matched when github_config is Some and path matches a configured repo
- Unknown GitHub webhook paths → 404
- Missing X-Hub-Signature-256 → delegate to `Github.handle_webhook` which returns 403-equivalent body (http_server returns 403)
- Body is fully read before any early return
- No regression on existing routes (`/health`, `/chat`, `/chat/stream`, slack path)
- `make test` passes, `make fmt-check` passes

**Note**: The 403 for bad signature is returned from `github.ml` returning a special sentinel, OR github.ml raises an exception caught here, OR handle_webhook returns `(status * body)` pair. Simplest: `Github.handle_webhook` returns `string` and takes `~reply_status` ref or returns `[ `OK | `Forbidden ] * string`. Choose cleanest approach consistent with existing slack pattern (which just returns string body, always 200).

**Recommendation**: match the slack pattern — `handle_webhook` always returns string body with `200 OK`, BUT for signature failure return the body `{"error":"invalid signature"}` and the http_server sets 403. Implement this by having `handle_webhook` return a `result`:
```ocaml
type webhook_result =
  | Ok of string        (* 200 *)
  | BadSignature        (* 403 *)
```

**Dependencies**: T-GH1, T-GH4

---

### T-GH6: daemon.ml — wire github channel

**What**: Integrate tunnel startup and GitHub config into daemon.

**Files**: `src/daemon.ml`

**Acceptance criteria**:
- `Cf_tunnel.start` called after session_manager creation if `config.tunnel.enabled`
- Tunnel URL logged at Info level when known
- Warning logged if `config.channels.github <> None && tunnel_url = None`
- `Http_server.start` called with `?github_config:config.channels.github`
- `github_api_limiter` created and passed to http_server
- `write_state` includes `github_enabled` and `tunnel_url` fields
- Channels log line includes `github=`
- `daemon_state.json` updated accordingly
- Tunnel supervisor run via `Lwt.async` (does not block gateway `Lwt.pick`)
- `make test` passes, `make fmt-check` passes

**Dependencies**: T-CF1, T-CF2, T-GH5

---

### T-GH7: Tests + integration validation

**What**: Complete test coverage and end-to-end validation.

**Files**: `test/test_github.ml` (complete), `test/dune`

**Acceptance criteria**:
- All 5 test suites from component-design.md present and passing
- Test fixtures loaded from `api-examples/github/*.json`
- HTTP handler suite uses mock session manager (in-process, no daemon needed)
- `dune exec test/test_main.exe -- list` shows `github_*` suites
- `make test` passes (all tests including new)
- `make fmt-check` passes

**Dependencies**: T-GH3, T-GH4, T-GH5, research agents (for payload fixtures)

---

## Epic: Cloudflare Tunnel

### T-CF1: cf_tunnel.ml — static URL resolution

**What**: Create `src/cf_tunnel.ml` with `resolve_static` and the `start` entry point (static path only — managed returns `Lwt.return_unit` stub for now).

**Files**: `src/cf_tunnel.ml`, `src/dune`

**Acceptance criteria**:
- `resolve_static` returns `Some url` if `config.tunnel.url` is non-empty
- `resolve_static` returns `Some url` if `CLAWQ_TUNNEL_URL` env var is set (config.url takes priority)
- `resolve_static` returns `None` if neither is set
- `start` calls `resolve_static`, calls `on_url` immediately if Some, returns `(initial_url, Lwt.return_unit)` for non-managed
- Module lives in `clawq_runtime_integrations`
- `make test` passes, `make fmt-check` passes

**Dependencies**: T-GH1 (needs extended `tunnel_config`)

---

### T-CF2: cf_tunnel.ml — managed named tunnel subprocess

**What**: Implement managed cloudflared subprocess spawning, URL parsing, and restart loop.

**Files**: `src/cf_tunnel.ml`

**Acceptance criteria**:
- When `config.tunnel.managed = true` and `config.tunnel.tunnel_name` is non-empty:
  - Spawn `cloudflared tunnel run {tunnel_name}` via `Lwt_process.open_process`
  - (Optionally) pass `--config {config_dir}/config.yml` if `config_dir` non-empty
  - Read process output line by line (Lwt_io)
  - Extract tunnel URL using regex pattern from `api-examples/cloudflare/output-format.md`
  - Call `on_url` callback when URL first detected
  - Log all cloudflared output at Debug level, URL at Info
  - On process exit: log warn, restart with exponential backoff (1s → 2s → 4s → 8s → max 60s)
  - Reset backoff after 5 minutes of stable uptime
  - `start` returns `(None, supervisor_promise)` for managed case (URL not known until subprocess starts)
- When `config.tunnel.managed = true` but `tunnel_name` is empty: log error, return `(None, Lwt.return_unit)` (don't crash daemon)
- Static URL takes priority: if `config.tunnel.url` non-empty, managed subprocess is NOT started
- `make test` passes, `make fmt-check` passes

**Test notes**: Managed subprocess is hard to unit test. Test that static URL priority works, that empty tunnel_name is handled gracefully, and that URL extraction regex works on fixture output strings from `api-examples/cloudflare/output-format.md`.

**Dependencies**: T-CF1, cloudflare research agent (for exact output format + regex)

---

## Task: T-DOCS: Setup guide

**What**: Create `docs/github-webhook-setup.md` — step by step guide for users.

**Contents**:
1. Prerequisites: cloudflared installed, GitHub PAT with `repo` scope
2. Cloudflare tunnel setup (named tunnel): `cloudflared login`, `cloudflared tunnel create`, DNS route
3. clawq config: tunnel + github sections with annotated example
4. GitHub webhook registration: URL, content type, secret, which events to select
5. Testing: post a PR comment with `/clawq hello`, verify response
6. Troubleshooting: common errors (signature mismatch, tunnel not running, wrong event type)

**Dependencies**: All implementation tasks complete

---

## Future Tasks (not in v1 scope)

### T-GH8: Per-repo agent routing (after model routing feature lands)
Wire `agent_name` from `github_repo_config` to actual model/agent selection in Session.turn.

### T-GH9: GitHub App auth support
Implement `GithubApp` variant: JWT generation (RS256), installation token fetch + caching (1h expiry), config parsing.

### T-CF3: Quick tunnel support in managed mode
Add quick tunnel option: `cloudflared tunnel --url http://localhost:{port}`. Useful for dev/testing despite unstable URL.

### T-GH10: Auto-register GitHub webhooks via API
When tunnel URL becomes known (or changes), call GitHub API to create/update webhook registration automatically. Requires `admin:repo_hook` scope.

### T-GH11: Fetch PR diff hunks via API
Optionally fetch `patch` field from PR files API for richer context. Needs truncation logic for large PRs. Currently the agent uses `gh pr diff` instead.

---

## Dependency Graph

```
T-GH1 ─┬─► T-GH2 ─┬─► T-GH4 ─┬─► T-GH5 ─► T-GH6 ─► T-GH7
        └─► T-GH3 ─┘           └─► T-GH7
                                                    ▲
T-CF1 ──────────────────────────────────► T-GH6 ───┘
T-CF2 (extends T-CF1)──────────────────► T-GH6
```

T-GH3 and T-GH2 can be developed in parallel after T-GH1.
T-CF1/CF2 can be developed in parallel with T-GH2/GH3.
T-GH7 and T-GH5 require T-GH3+T-GH4.
T-GH6 requires everything except T-DOCS.

---

## Estimated effort

| Task | Estimate |
|---|---|
| T-GH1 | 2h |
| T-GH2 | 2h |
| T-GH3 | 3h |
| T-GH4 | 2h |
| T-GH5 | 1h |
| T-GH6 | 1h |
| T-GH7 | 2h |
| T-CF1 | 1h |
| T-CF2 | 2h |
| T-DOCS | 1h |
| **Total** | **~17h** |

Original estimate was 10h. The addition of managed cloudflared (v1), in-thread review replies, PR files fetching, auth abstraction, and comprehensive tests accounts for the increase.
