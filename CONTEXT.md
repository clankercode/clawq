# Clawq

Clawq is an AI assistant runtime: an agent loop that talks to LLM backends, is reachable from chat surfaces and a CLI, and can spawn long-running work. This glossary pins the domain language so the same concept is named the same way everywhere.

## The agent loop

**Turn**:
One exchange driven by the agent loop — build history, call a provider, run any tool calls, produce a response. The unit the loop iterates over.
_Avoid_: request, round, exchange (for the loop iteration).

**Session**:
A persistent conversation: its history plus the agent state derived from it. Owns the lock under which a turn runs.
_Avoid_: conversation, thread (thread means something else on a Channel).

**Compaction**:
Summarising older history into a shorter form when it grows too large for the context window, preserving tool-group integrity and scoped memory references.
_Avoid_: summarisation (too generic), truncation (compaction summarises, it doesn't drop).

## Providers

**Provider**:
An LLM backend the agent calls to obtain a response (buffered or streamed). Distinct from a Runner.
_Avoid_: model (a model is the thing a provider serves), backend, vendor.

**Wire format**:
The request/response protocol shape a provider speaks — e.g. Anthropic Messages, OpenAI Chat, OpenAI Responses, Gemini. The axis on which providers genuinely differ; several vendors share one wire format and vary only by quirk (base path, name map, retry policy).
_Avoid_: protocol, schema, API shape.

## Chat surfaces

**Channel**:
A running transport and its lifecycle — connect, receive, reconnect — for one chat surface (Telegram long-poll, Discord gateway, Slack socket). What the daemon starts and supervises.
_Avoid_: connector (that is a distinct concept — see below), integration, platform.

**Connector**:
The delivery identity of a chat surface used to pick formatting and capabilities (can it edit, react, send cards, capture ambient history). What `format_adapter` and `connector_capabilities` key on.
_Avoid_: channel (a Channel is the live transport; a Connector is the rendering identity). Older `Channel.S` terminology conflated the two; the concrete `Channel.t` startup specification now models only the live transport lifecycle.

**Room**:
A durable shared conversation identity on a Connector, bound to one shared Room Session and, when configured, a room-agent profile. A Teams Channel, Slack channel, Discord channel, Matrix room, or Telegram group can be a Room.
_Avoid_: channel (the Channel is the live transport), thread (a thread is nested interaction context), conversation (too generic).

**Connector actor**:
Verified ingress evidence for one sender: Connector plus tenant/workspace/account scope plus the Connector's immutable user ID. Display names are mutable metadata and never identity.
_Avoid_: Actor (a GitHub event has a separate actor), user name, requester string, Session identity.

**Principal**:
A durable Clawq human identity that may own several explicitly linked Connector actors and external-account bindings. A Principal can participate in many Rooms; a Room or Session never owns the Principal's credentials.
_Avoid_: Room user, Session owner, Connector account.

**Identity link**:
The revisioned association between a verified Connector actor and a Principal. Cross-Connector links require private two-sided proof or audited admin repair.
_Avoid_: account match, email match, display-name match.

**Actor snapshot**:
Immutable attribution evidence captured from a Connector actor when an intent or delayed job is created. It never grants authority; execution re-resolves the live Principal, identity link, account binding, and policy.
_Avoid_: credential snapshot, authorization cache, Session owner.

**GitHub account binding**:
A verified association between one Principal, a GitHub App, and a GitHub numeric user ID, with mutable login/avatar metadata and an opaque encrypted-credential reference.
_Avoid_: PAT, GitHub login (logins can change), Room credential.

**Attribution mode**:
The actor requirement declared by a GitHub mutation: `App`, `User_required`, or `User_preferred`. It controls authorization selection and fallback, independently of action confirmation.
_Avoid_: auth type, OAuth scope, impersonation mode.

**Authorization transaction**:
A private, expiring, one-time web or device flow bound to an initiating Principal and source context. Completing it links an account but does not confirm a GitHub mutation.
_Avoid_: login Session, setup confirmation, action approval.

**Token generation**:
A monotonically changing version of one encrypted GitHub user-token record. Leases, refresh, unlink, and revocation use it to prevent stale credentials from regaining authority.
_Avoid_: token ID, Session revision.

**GitHub item**:
A pull request or issue identified by repository, kind, and number. The stable subject used by event routes, journal entries, projections, and GitHub tools.
_Avoid_: ticket (not all items are issues), PR (excludes issues), notification (an event produces a notification).

**GitHub event route**:
A durable rule mapping GitHub item events at Item, Repo, or Org scope to a Room or Session, with a versioned filter and delivery policy.
_Avoid_: subscription (reserved as the compatibility name for the older per-PR model), webhook (the webhook is ingress, not the routing rule).

**Lifecycle event**:
A normalized GitHub event that changes the major lifecycle of an item and therefore creates a new visible card, such as opened, ready, reopened, closed, transferred, or merged.
_Avoid_: update (updates edit the current projection), webhook event (too transport-specific).

**Event journal**:
The durable ordered record of normalized events accepted for a Room or Session. It preserves structured history beyond Session compaction and is the source for projection replay and agent retrieval.
_Avoid_: audit ledger (the audit ledger records security and operational decisions), chat history (journal entries may be hidden).

**Item projection**:
The durable per-destination view of one GitHub item's current visible state, including the current card/message identity and render revision. Derived from the Event journal.
_Avoid_: card (a card is one Connector rendering), session (the Room Session remains authoritative).

## Commands

**Command**:
A named verb Clawq exposes — the same verb reachable from the CLI and, where allowed, from a chat surface. Has a name, an argument grammar, an admin scope, and a handler.
_Avoid_: action, subcommand.

**Slash command**:
A Command issued inside a chat surface as `/name …`, parsed from text rather than CLI argv.
_Avoid_: chat command, bot command.

## Work

**Runner**:
An external coding-agent CLI that Clawq drives as a subprocess (Codex, Claude, Kimi, Gemini, Opencode, Cursor). Distinct from a Provider — a Runner is a whole agent, a Provider is an LLM endpoint.
_Avoid_: agent (overloaded), tool, backend.

**Background task**:
A long-running job Clawq spawns and supervises to a terminal outcome, with its own log and worktree.
_Avoid_: job (a Cron job is a distinct scheduled concept), process.

**Task tree**:
A hierarchy of tasks mutated as a batch of typed operations under one transaction.
_Avoid_: todo list, plan (a Plan pipeline is a distinct concept).

**Cron job**:
A schedule plus a target the Scheduler runs on a recurring basis, recording each run's delivery outcome.
_Avoid_: routine (used interchangeably in some cloud-agent contexts; prefer Cron job here), timer.

## Extensions

**Tool**:
A capability the agent can invoke within a turn, declaring its parameters and an invocation. Built-in or user-defined.
_Avoid_: function, plugin.

**Skill**:
A user-defined Tool backed by a shell command, discovered from a skill definition.
_Avoid_: command (a Command is a Clawq verb), script.
