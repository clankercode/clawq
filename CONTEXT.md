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
_Avoid_: channel (a Channel is the live transport; a Connector is the rendering identity). Note: the code currently conflates the two — `channel.ml` calls its type "module type for connectors" — pinning them apart is deliberate.

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
