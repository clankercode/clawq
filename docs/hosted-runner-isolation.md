# Hosted Runner Isolation (B775)

How Clawq isolates hosted subscription runners (Codex/Claude background
tasks) on a worker machine, and what trust assumptions remain for the
operator.

## Threat model

Hosted runners execute with their provider permission-bypass flags
(`--dangerously-bypass-approvals-and-sandbox`, `--dangerously-skip-permissions`),
so the model can run arbitrary commands *inside* its boundary. Issue text and
prompts are untrusted input. The boundary must therefore come from the host
system, enforced outside the model:

- **Minimal environment (always applied when isolation is enabled).** The
  runner environment is built from an allowlist (`PATH`, `HOME`, locale,
  terminal, XDG dirs, `CLAWQ_*` session vars). `GITHUB_TOKEN`/`GH_TOKEN`,
  `SSH_AUTH_SOCK`, cloud credentials (`AWS_*`, `GOOGLE_*`), GitHub App key
  paths, and provider API keys are absent *by construction* — the agent can
  never fall back from subscription auth to pay-as-you-go API keys, and it
  never holds publisher authority.
- **Filesystem sandbox (argv-level).** The runner argv is wrapped in
  bubblewrap (preferred) or firejail: read-only system directories, read-write
  access only to the task worktree, the task log directory, and explicitly
  granted paths. PID/IPC/UTS namespaces are unshared; network stays available
  for provider APIs. The wrapper composes with every session host (direct,
  Herdr, tmux) because it rewrites argv before the host sees it. Untrusted
  text remains a single argv element end to end — never a shell string.
- **Publisher separation.** GitHub replies and draft PRs are performed by the
  trusted publisher in the Clawq daemon using short-lived repository-scoped
  credentials. The hosted agent has no GitHub credentials at all: env
  stripping removes token variables and the sandbox denies the App private
  key file.

## Configuration

```json
{
  "security": {
    "hosted_runner_isolation": "off | prefer | require",
    "sandbox_backend": "auto | bubblewrap | firejail | none",
    "extra_allowed_paths": ["~/.cargo"]
  }
}
```

- `off` (default): legacy behavior, no isolation. Only acceptable on a
  dedicated single-user machine that runs nothing else of value.
- `prefer`: sandbox when a backend is available; log a warning and continue
  without one otherwise.
- `require`: **fail closed** — task startup fails with an actionable error
  when no isolation backend is available. Remote/subscriber worker deployment
  (B774) must run with `require`.

Unknown values are treated as `require` (fail closed), never as `off`.

Provider CLI state (`~/.codex`, `~/.claude`, `~/.claude.json`,
`~/.config/claude`, `~/.cache`) is bind-mounted read-write so the worker
identity's subscription login keeps working. Tokens are never copied into
work items, control-plane state, or logs.

## What is enforced outside the model

- Repository and actor allowlists (`channels.github.repos[].allow_users`,
  webhook HMAC verification) run before any work item exists.
- Work-item dedup (`github_work_items.dedup_key`) prevents duplicate launches.
- Concurrency limits (`max_running_tasks`, `max_concurrent_native_agents`),
  cancellation (`background cancel`), and lease/heartbeat controls (B774) are
  control-plane state.
- Audit: task lifecycle events land in the room activity ledger and task
  records; session-host identity (`host_kind`/`host_session_id`) makes
  restart recovery verifiable, and a stale host session (e.g. PID reuse or a
  Herdr terminal that now hosts something else) is refused, not adopted.

## Remaining trust assumptions

- **Dedicated user account (minimum for real deployments).** bubblewrap
  denies filesystem access but the runner still executes as your uid. A
  dedicated `clawq-worker` user keeps provider login state separate from
  your personal credentials and limits blast radius if isolation is
  misconfigured.
- **VMs (strongest).** A per-worker VM bounds kernel-level escapes and is
  recommended when running work from repositories you do not control.
- **Herdr.** The Herdr server runs as the same user; anyone who can reach its
  socket can attach to or start terminals. Treat the Herdr socket as
  user-private (it is by default) and do not expose it across users. The
  sandbox wraps the runner *inside* the pane, so attach/inspect stays
  available without widening the agent's boundary.
- **tmux (B770).** Same property as Herdr: the tmux server socket is
  user-private; the sandbox travels with the runner argv, not the pane.
- **Network.** The sandbox intentionally shares the network namespace so the
  provider CLI can reach its API. Outbound restrictions, if needed, must come
  from the host firewall or VM policy for now.
- **Provider CLIs.** The official Codex/Claude CLIs handle their own auth
  and may prompt for reauthentication; a worker whose login expires reports
  readiness errors rather than borrowing other credentials.
