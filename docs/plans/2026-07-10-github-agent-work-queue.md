# GitHub Agent Work Queue

Status: planned for implementation in Clawq backlog B768-B776. Amaroo adoption
is tracked separately in AmarooHQ/amaroo issues #889-#892 and must start only
after the corresponding Clawq capabilities land.

## Goal

Let an authorized GitHub user invoke one Clawq interface from an issue or pull
request and have a locally subscription-authenticated Codex or Claude agent
answer, plan, or implement the request. Agent execution belongs on trusted
subscriber PCs, not on the lightweight control-plane host.

The same work-item path must support:

- `/clawq` requests, configured leading at-mentions, and assignment when the
  configured GitHub identity is actually assignable;
- no-code answers and plans posted to the originating thread;
- code-changing results published as reviewable draft pull requests under a
  repository-owned policy; and
- multiple workers claiming queued work exclusively while using their own
  local provider subscriptions.

## Locked decisions

1. **Clawq owns the interface.** GitHub, queue transports, session hosts, and
   model providers are adapters around one normalized work-item contract.
2. **Provider authentication stays local.** Workers invoke the official Codex
   or Claude CLI using that machine's subscription login. OAuth tokens are not
   copied into work items, control-plane state, or Clawq logs.
3. **Queue, host, and runner are separate concepts.** The queue assigns work;
   Herdr or tmux hosts an inspectable terminal session; Codex or Claude performs
   the work.
4. **Herdr is the preferred host.** tmux is the widely available fallback. The
   current direct process-group launcher remains a compatibility adapter.
5. **Workers connect outbound.** Subscriber PCs do not expose public inbound
   services. A lightweight control plane may run on a small host such as cachy,
   but it performs no inference, repository build, or test workload.
6. **The model never owns publisher credentials.** A trusted publisher decides
   whether a validated result is a thread reply or a draft PR and performs the
   GitHub mutation with a short-lived, repository-scoped credential.
7. **Repository policy is authoritative.** Base selection, history rules,
   validation, publication, automerge, labels, and prohibited commands are
   policy inputs rather than assumptions embedded in a generic prompt.
8. **Delivery is at least once; effects are idempotent.** Duplicate webhooks,
   lease expiry, reconnects, and repeated completion delivery cannot execute or
   publish the same logical work twice.

## Subscription assumptions

Workers invoke the providers' official CLIs rather than extracting OAuth tokens
or impersonating a first-party client. Each worker operator signs in locally and
accepts that provider quotas, reauthentication, and subscription policy may
change independently of Clawq.

- Codex supports ChatGPT sign-in for local subscription access. Business and
  Enterprise workspaces can additionally issue Codex access tokens intended for
  trusted non-interactive local automation. Personal cached login may require
  interactive reauthentication and must not be copied between workers.
- Claude Code supports subscription login and the official non-interactive
  `claude -p` path. Clawq keeps the existing explicit Anthropic OAuth-inference
  opt-in and must not silently switch to pay-as-you-go credentials.
- A worker advertises current authenticated capability and quota/readiness; the
  queue does not assume Codex and Claude are always equally available.

These assumptions must be rechecked against current provider documentation
before production rollout. A provider policy change should disable that worker
capability with an actionable readiness error rather than fall back to a
different billing source without operator consent.

## Alternatives considered

- Provider-specific GitHub agents and Claude/Codex actions split configuration,
  credentials, lifecycle, and result handling by provider, so they do not meet
  the one-interface requirement.
- `gh-aw` supplies useful GitHub workflow and safe-output patterns but its engine
  configuration is API-key-oriented and does not replace Clawq's local
  subscription-authenticated runner/session model.
- GitHub Actions self-hosted runners can remain a future queue transport adapter,
  but the normalized work-item and worker contracts must not depend on Actions.
- Herdr and tmux are execution/session hosts, not queues. Selecting either must
  not change GitHub intake, leasing, runner selection, or publication semantics.

## Architecture

```text
GitHub comment / mention / assignment
                 |
                 v
      verified Clawq GitHub intake
        authorize, dedupe, acknowledge
                 |
                 v
        durable work-item queue
          claim, lease, heartbeat
                 |
        +--------+--------+
        |                 |
        v                 v
  trusted worker A  trusted worker B
  Herdr or tmux     Herdr or tmux
  Codex or Claude   Codex or Claude
        |                 |
        +--------+--------+
                 v
          validated result
                 |
                 v
       trusted GitHub publisher
         reply or draft PR
```

## Work-item contract

A durable work item needs, at minimum:

- a stable idempotency/deduplication key;
- repository plus issue, PR, or review-thread identity;
- requester, trigger kind, authorization result, and source delivery identity;
- bounded prompt context and a repository-policy reference;
- requested runner and host preferences, without provider credentials;
- required repository/capability constraints;
- queued, leased, running, blocked, succeeded, failed, expired, and cancelled
  lifecycle state;
- lease owner, token, heartbeat, attempt count, and expiry;
- hosted provider-session and Herdr/tmux identity;
- result kind: reply, change, blocked, or failed; and
- publication identity/status so retries are idempotent.

## Security boundary

Issue text is untrusted input. It must be passed as structured data or literal
stdin/file content and never interpolated into a shell command. The outer host
sandbox must remain effective even when provider CLIs are invoked with their
own non-interactive permission modes.

The hosted agent receives only its selected worktree and explicitly granted
caches, tools, sockets, network destinations, and local provider login. It must
not inherit GitHub publisher tokens, GitHub App private keys, a general SSH
agent, unrelated cloud credentials, other users' provider credentials, or
control-plane authority.

The control plane enforces actor/repository allowlists, concurrency, leases,
cancellation, retry limits, and audit records outside the model. Remote worker
deployment is blocked until these controls fail closed when isolation is
unavailable.

## Implementation plan

| Backlog | Deliverable | Blocked by |
|---|---|---|
| B768 | Provider-neutral durable session-host interface, preserving the direct process adapter | None |
| B769 | Herdr host adapter with inspect, message, wait, cancel, attach, and restart recovery | B768 |
| B770 | tmux host adapter with the same lifecycle as a fallback | B768 |
| B771 | Complete `/clawq` issue request to hosted Codex/Claude to GitHub reply path | B768, B769 |
| B775 | Sandbox and credential separation for hosted subscription runners | B768, B769 |
| B772 | Reply-or-draft-PR publication under repository-owned execution policy | B771, B775 |
| B773 | Leading at-mention and assignment intake using the same work-item path | B771 |
| B774 | Remote subscriber-worker capabilities, atomic leases, heartbeats, expiry, and idempotent completion | B771, B769, B775 |
| B776 | One setup, readiness, status, drain, recovery, and troubleshooting surface | B772, B773, B774, B775 |

Recommended build order:

1. B768, then B769.
2. B771 to prove one complete hosted answer/plan path.
3. B775 before any remote-worker or draft-PR production use.
4. B772, B773, and B774 as independently verifiable extensions.
5. B776 and a two-worker soak.
6. B770 can proceed after B768 and is not required for the Herdr-first MVP.

## Verification gates

- A duplicate GitHub delivery creates one logical work item and one result.
- Two workers racing for one item cannot both hold a valid lease.
- Worker loss permits bounded recovery without accepting stale completion.
- No-change work posts a reply without creating commits, branches, or PRs.
- Code-changing work publishes exactly one draft PR under repository policy.
- Provider subscription credentials remain on the worker and publisher
  credentials remain outside the agent process.
- Unauthorized actors, repositories, host takeover, path escape, environment
  leakage, quoted mentions, bot self-replies, and shell injection fail closed.
- Queue/control-plane restart, worker reconnect, cancellation, drain, credential
  expiry, and version mismatch have actionable observable outcomes.
- A two-worker test demonstrates exclusive claims and measures that the control
  plane performs coordination rather than inference/build workload.

## Amaroo adoption after Clawq

Amaroo-specific behavior is intentionally not embedded in Clawq:

- [Amaroo #889](https://github.com/AmarooHQ/amaroo/issues/889) proves one
  trusted Herdr worker can answer Amaroo issue requests while cachy remains a
  lightweight coordinator.
- [Amaroo #890](https://github.com/AmarooHQ/amaroo/issues/890) adds Amaroo's
  draft-PR policy and preserves human `af:ready` consent plus the single wf2
  merge actor.
- [Amaroo #891](https://github.com/AmarooHQ/amaroo/issues/891) deploys and
  soaks at least two subscriber PCs against the shared queue.
- [Amaroo #892](https://github.com/AmarooHQ/amaroo/issues/892) later offloads
  only expensive wf2 autofix agent execution; wf2 selection, gates, audit,
  halt controls, head verification, and merging stay on the single supervisor.

None of those Amaroo issues should receive `ready-for-agent` until its listed
Clawq dependencies have landed and the relevant readiness checks pass.
