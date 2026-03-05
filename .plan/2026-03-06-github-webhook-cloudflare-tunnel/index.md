# I001: Cloudflare Tunnel + GitHub Webhook Integration

**Plan directory**: `.plan/2026-03-06-amber-delta/`
**Date**: 2026-03-06
**Status**: In planning

## Goal

Enable a conversational workflow directly in GitHub PRs and issues: engineers comment `/clawq <request>` and clawq replies as a GitHub comment. Subsequent `/clawq` comments on the same PR continue the session — the agent remembers the thread. This is the primary user-facing value.

Supporting docs:
- [Config Schema](config-schema.md)
- [Component Design](component-design.md)
- [Backlog Tasks](backlog-tasks.md)
- [Auth Abstraction](auth-abstraction.md)

---

## Design Decisions (locked)

| Decision | Choice | Rationale |
|---|---|---|
| Tunnel lifecycle | External (config/env) + managed subprocess (named tunnel) both in v1 | Explicit URL is zero-config; managed spawns cloudflared for UX |
| Tunnel type (managed) | Named tunnel only | Stable URL required for webhook registration to persist across restarts |
| GitHub auth | PAT now, GitHub App later | Auth abstraction layer designed to make App support a clean add-on |
| PR diff context | Filenames + stats; agent uses `gh` for hunks | Keeps context bounded; `gh` is available in the tool environment |
| Multi-`/clawq` per comment | First occurrence only | Predictable, no multi-reply spam |
| Session key | Per-PR/issue: `github:{owner}/{repo}:pr:{n}` | Sustains threads across all interaction types on the same PR |
| Review comment replies | In-thread (`in_reply_to_id`) | Keeps code review conversation in the diff view |
| Other replies | Top-level PR/issue comment | Consistent, easy to find |
| Model routing | `agent_name` field parsed + stored, not yet routed | Clean placeholder; wired when model routing lands |

---

## Architecture Overview

```
GitHub PR/issue comment ("/clawq do X")
  → POST {tunnel_url}/github/webhook/myrepo
  → HTTP server: verify X-Hub-Signature-256 (HMAC-SHA256)
  → Github_webhook.parse_event (pull_request | issue_comment | pr_review_comment)
  → check /clawq present → extract user_message + build full_context
  → session key: "github:owner/repo:pr:42"  (per-PR, sustained)
  → Session.turn ~key ~message:user_message  (context injected as preamble)
  → Github_api.post_comment / reply_to_review_comment
  → GitHub shows clawq's reply
  → next /clawq on same PR → same session → agent remembers
```

### Tunnel URL Resolution (priority order)

1. `tunnel.url` in config (explicit, production-stable)
2. `CLAWQ_TUNNEL_URL` environment variable
3. If `tunnel.managed = true`: spawn cloudflared named tunnel subprocess, parse URL from output
4. None available: log warning, GitHub webhooks inoperable (daemon still starts)

### Thread Sustaining

Session key = `github:{owner}/{repo}:pr:{n}` for PRs, `github:{owner}/{repo}:issue:{n}` for issues.

Uses existing `Session` / `Memory` infrastructure unchanged. All `/clawq` interactions on a PR — whether in the PR body, issue comments, or PR review comments — share one session. The agent accumulates context across the thread naturally.

### Context Injection

Each agent turn receives:

**Context preamble** (injected before user message):
```
## GitHub Context
Repository: owner/repo
PR #42: "Fix authentication flow"
Author: @pr-author
State: open | base: main → head: fix/auth

PR Description:
  <full PR body>

Event: PR review comment by @commenter
File: src/auth.ml  line 47
Diff hunk:
  <hunk from webhook payload — free, no API call needed>
Full comment:
  I think there's an issue here.

  /clawq Can you check if the token expiry logic handles clock skew?

Changed files (3):
  - src/auth.ml (+45 -12)
  - src/session.ml (+8 -3)
  - test/test_auth.ml (+30 -0)

To inspect the diff: `gh pr diff 42 --repo owner/repo`
To view PR:  https://github.com/owner/repo/pull/42
```

**User message** (extracted `/clawq` paragraph, prefix stripped):
```
Can you check if the token expiry logic handles clock skew?
```

Note: for `pull_request_review_comment` events the diff hunk is included in the webhook payload for free (no extra API call). For `issue_comment` and `pull_request` events, only metadata + PR files are available.

### Reply Format

PR review comments (in-thread):
```markdown
> `/clawq` Can you check if the token expiry logic handles clock skew?

Yes — line 47 in `auth.ml` computes expiry as `now + ttl` using the server clock...
```

Issue comments and PR-level (top-level):
```markdown
> `/clawq` Can you summarize what this PR does?

This PR refactors the authentication flow to use short-lived tokens...
```

---

## New Files

| File | Purpose |
|---|---|
| `src/github_api.ml` | GitHub REST client (post comment, reply to review, get PR files) |
| `src/github_webhook.ml` | HMAC verify, event parse, `/clawq` extraction |
| `src/github.ml` | Orchestration: parse → session → reply |
| `src/cf_tunnel.ml` | Tunnel URL resolution + cloudflared subprocess management |
| `api-examples/github/` | Example payloads and API docs (from research agents) |
| `api-examples/cloudflare/` | Cloudflared output format docs |

## Modified Files

| File | Change |
|---|---|
| `src/runtime_config.ml` | Add `github_config`, `github_repo_config`; extend `tunnel_config` |
| `src/config_loader.ml` | Parse github and extended tunnel config |
| `src/http_server.ml` | Add GitHub webhook route |
| `src/daemon.ml` | Wire tunnel + github channel |
| `test/test_github.ml` | New test suite |

---

## Key Constraints

- **No new opam deps for v1**: HMAC via existing `digestif.c`, HTTP via existing `cohttp-lwt-unix` + `http_client.ml`, JSON via `yojson`. Subprocess management via `Lwt_process`.
- **Runtime split**: `github.ml`, `github_api.ml`, `github_webhook.ml`, `cf_tunnel.ml` all go in `clawq_runtime_integrations` (not core).
- **Audit trail**: GitHub webhook events are logged via existing `Audit` module when audit is enabled.
- **Rate limiting**: outgoing GitHub API calls gated at 1/s per repo using existing `Rate_limiter`.
