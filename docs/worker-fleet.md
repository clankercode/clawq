# Clawq Worker Fleet

How to run the GitHub agent work queue across a lightweight control plane and
one or more trusted subscriber PCs, and how to diagnose it.

## The four layers (keep them distinct)

A hosted work item flows through four independent concerns. Confusing them is
the most common operational mistake:

1. **Queue transport** — the durable work-item store and lease protocol on the
   control plane (`github_work_items` + `/worker/*` endpoints). It assigns
   work; it performs no inference or build.
2. **Session host** — how a runner process is hosted on a worker: `herdr`
   (preferred, attachable), `tmux` (widely-available fallback), or `direct`
   (process group). Selecting a host never changes intake, leasing, or
   publication.
3. **Runner** — the provider agent that does the work: the official Codex or
   Claude CLI, authenticated by that worker's local subscription login.
4. **Publication policy** — the repository-owned `.clawq/publication-policy.json`
   that decides reply vs draft PR, base branch, and history rules. Publication
   is performed by the trusted control-plane publisher, never the model.

## Setup

### Control plane (lightweight coordinator, e.g. a small always-on host)

1. Run the Clawq daemon with a GitHub channel configured
   (`channels.github`) and a `gateway.auth_token` set — workers authenticate
   with this token.
2. The daemon stores queued work durably, reclaims expired leases, and
   publishes results. It runs **no** inference, repository build, or test
   workload.
3. Optionally set `security.hosted_runner_isolation` here too if the control
   plane also runs local work.

Verify: `clawq worker readiness --role control-plane`.

### Each subscriber PC (trusted worker)

No provider subscription credentials are ever copied to the control plane.

1. Install the official Codex and/or Claude CLI and **sign in locally**.
   Subscription credentials stay on this machine.
2. Install a session host: `herdr` (preferred) or `tmux` (fallback).
3. Set `security.hosted_runner_isolation=require` (needs `bwrap` or
   `firejail`) so runner environments are sandboxed and credential-bearing
   variables are stripped.
4. Connect outbound to the control plane:

   ```bash
   clawq worker run \
     --server https://<control-plane-host> \
     --id <stable-worker-id> \
     --repos owner/repo[,owner2/repo2] \
     --token <gateway-auth-token> \
     [--runners claude,codex] [--hosts herdr,tmux]
   ```

   The worker connects outbound only — subscriber PCs need no inbound port
   forwarding.
5. Verify readiness: `clawq worker readiness --id <id> --repos <repos>`.

`clawq worker setup` prints this flow.

## Readiness

`clawq worker readiness` classifies every boundary as **pass / warn / fail**
with an actionable repair step, and never prints secret values (tokens are
reported as present/absent, not shown):

- `queue` — control-plane database reachable (or `--server` returns 200).
- `queue-auth` — gateway auth token present.
- `worker` — stable worker identity supplied.
- `runner:<name>` — official CLI present (subscription login is verified by
  the CLI itself).
- `host:<name>` — session host available and ready.
- `repos` — at least one repository grant.
- `sandbox` — isolation policy has a working backend (`require` fails closed).
- `publisher` — GitHub publication configured on the control plane.
- `version` — clawq build version.

## Status and inspection

`clawq worker status --server <url> [--token <t>]` reports queue depth
(queued/running/blocked), registered workers with last-seen time (stale
workers show an old timestamp), and active leases with owner and expiry.

To inspect a running hosted session on a worker:

- Herdr: `herdr agent attach <terminal_id>` (the id is in `background show`).
- tmux: `tmux attach -t <session-name>`.

Task logs live under `~/.clawq/background-logs/` on the worker.

## Operational procedures

- **Upgrade.** Update the control plane first, then workers. A worker whose
  version is incompatible with the queue reports it in readiness; drain and
  upgrade it.
- **Credential expiry.** Provider CLIs handle their own auth. An expired
  login surfaces as a runner failure, not a fallback to other credentials —
  re-run the CLI's login on that worker.
- **Drain a worker.** Stop starting new work: stop the `clawq worker run`
  loop. In-flight items finish under their existing lease; if the process is
  killed mid-item, the lease expires and the control plane requeues it (up to
  the attempt limit).
- **Cancellation.** Cancel a work item on the control plane; the worker's next
  heartbeat or completion is rejected (`lease_stale` / `item_terminal`) and it
  stops.
- **Retry.** Failed items can be re-triggered from GitHub; lease expiry
  requeues automatically up to the attempt limit, after which the item fails
  with an actionable reason.
- **Lease loss.** If a worker loses its lease (expiry, or another worker
  claimed after expiry), its heartbeat and completion are rejected and it
  abandons the item quietly. No duplicate result is published — completion is
  token-gated and idempotent.
- **Emergency disable.** Rotate or remove `gateway.auth_token` on the control
  plane (all workers immediately fail auth), or stop the daemon (queue is
  durable and resumes on restart).

## Trust boundaries

See [Hosted Runner Isolation](hosted-runner-isolation.md) for the sandbox and
credential-separation model. In short: the hosted agent gets only its
worktree plus explicitly granted caches/tools/provider-login; GitHub publisher
credentials stay with the control-plane daemon and never reach a worker or the
model.

## Two-worker smoke test

Reproducible check that two workers claim exclusively and the control plane
only coordinates:

1. On the control plane, trigger two `/clawq runner=auto host=direct <request>`
   work items on an allowed repo.
2. Start two workers with distinct `--id` values against the same `--server`,
   both advertising the repo.
3. Observe with `clawq worker status`: each item is leased by exactly one
   worker (no item has two owners); queue `running` never exceeds the number
   of items.
4. Kill one worker mid-item. Within the lease TTL the control plane requeues
   its item and the surviving worker (or a restarted one) claims it. The
   final GitHub reply appears exactly once.
5. Confirm the control plane performed no inference: its CPU stays idle while
   the workers run the provider CLIs.

The lease invariants exercised here (single valid lease per item, expiry
reclaim, idempotent completion) are also covered by the automated
`work_item_lease` test suite.
