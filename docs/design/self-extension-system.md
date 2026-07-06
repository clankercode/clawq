# Design Memo: Self-Extension / Plugin System for Clawq

**Status**: Research deliverable (B737)  
**Date**: 2026-06-30  
**Author**: Research subagent  
**Scope**: Design recommendation only — no implementation

---

## 1. Problem Statement

Clawq already has the substrate for an extension system — MCP client/server,
structured pipelines, agent templates, a tool registry, skills, background
tasks, and safety primitives (landlock, egress_policy, credential_lease) — but
these surfaces are disconnected. There is no unified mechanism for clawq to
**extend itself** on demand: a user says "add a feature that does X," a coding
model authors it against a documented contract, a review model hardens it, and
the result registers safely.

The goal is **not** to integrate a catalogue of third-party tools (explicitly
rejected by the user). The goal is a self-extension mechanism that lets clawq
grow its own feature surface ad-hoc, using GLM-5.2 for authoring and GPT-5.5
for review-and-fix.

### What Claude Tag tells us

Anthropic's Claude Tag (2026-06-23) is a gated, curated product whose value
derives from: (a) @Claude as a multiplayer teammate in Slack, (b) ambient
awareness across channels, (c) a curated skill catalogue for high-leverage
workflows (metrics-chase, support-ticket-triage, root-cause-bug-hunt), and
(d) tight admin controls over tool/data access per channel. Clawq already has
(a) room agents, (b) room memory + egress policy, and (d) access_bundles. What
it lacks is (c) — a mechanism to grow the catalogue organically rather than
hand-authoring every pipeline.

---

## 2. Existing Substrate Inventory

Before evaluating options, here is what already exists in clawq that is
relevant to extension:

### 2.1 Structured Pipelines (`structured_pipeline*.ml`)

- **Format**: YAML/JSON definitions with typed inputs, multi-step execution
  (prompt steps, pipeline composition, agent steps).
- **Storage**: `~/.clawq/pipelines/` (user-created) + builtin registrations.
- **Execution**: `structured_pipeline_run.ml` — full engine with JSON schema
  validation, retry logic, model selection.
- **Builtins**: `research-report`, `build-review-carm`, `plan-build-review-carm`
  (thin catalogue — GAP-5 in audit).
- **Strengths**: Already YAML/JSON (agent-authorable), structured inputs/outputs,
  composable (pipeline steps can invoke other pipelines), version field + DB
  persistence of `pipeline_version`.
- **Weaknesses**: No versioned registry/update policy, no enable/disable, no
  manifest, no tests co-located with definition.

### 2.2 Agent Templates (`agent_template*.ml`)

- **Format**: Markdown files with YAML frontmatter (name, role, goal, backstory,
  system_prompt, allowed_tools, disallowed_tools).
- **Storage**: `~/.clawq/agents/` (user-created) + 11 builtins.
- **Discovery**: Filesystem scan + caching.
- **Strengths**: Rich persona definition, tool allow/disallow lists, model
  override, metadata.
- **Weaknesses**: No packaging, no tests, no versioning.

### 2.3 MCP Client + Server (`mcp_client.ml`, `mcp_server.ml`)

- **Client**: Full MCP 2024-11-05 protocol (stdio + HTTP transports),
  credential-lease-aware.
- **Server**: Can expose clawq's own `Tool_registry` as an MCP server
  (tools/list, tools/call).
- **Strengths**: Standard protocol, immediate interoperability with other
  MCP-aware agents.
- **Weaknesses**: External process management, no native packaging/registry,
  protocol version drift risk.

### 2.4 Tool Registry (`tool_registry.ml`, `tool.ml`)

- **Format**: OCaml `Tool.t` records (name, description, parameters_schema,
  invoke, risk_level).
- **Registry**: Mutable list with register/find/replace/remove.
- **Strengths**: Simple, already used by MCP server.
- **Weaknesses**: Requires OCaml compilation — not agent-authorable at runtime.

### 2.5 Skills (`skills.ml`, `builtin_skills.ml`)

- **Format**: `SKILL.md` files (markdown with frontmatter) in named
  subdirectories under `~/.clawq/skills/`. Legacy JSON skill files also
  supported but deprecated with warnings.
- **Storage**: `~/.clawq/skills/<name>/SKILL.md` (user-created) + builtin skills.
- **Execution**: Skills are injected as tool descriptions/instructions; they
  guide the model's behavior rather than executing code directly.
- **Strengths**: Agent-discoverable, composable with tool use, rich markdown
  instructions.
- **Weaknesses**: No structured output, no versioning, instructions-only (no
  executable contract).

### 2.6 Background Tasks + Subagent (`background_task*.ml`, `subagent_tool.ml`)

- **Task queue**: SQLite-backed, worktree-isolated, multi-runner (codex,
  claude, kimi, gemini, opencode, cursor, local).
- **Subagent tool** (B712): Clean LLM-facing spawn/poll API.
- **Delegate tool**: Simpler spawn interface.
- **Strengths**: Already the "clawq runs coding agents" infrastructure.
- **Weaknesses**: No extension registration — tasks are fire-and-forget.

### 2.7 Safety Primitives

- **Landlock** (`landlock.ml`): Linux Landlock LSM filesystem sandboxing.
  Already used to sandbox workspace + config dir.
- **Egress policy** (`egress_evaluator.ml`): Glob-based URL/hostname network
  access control. Per-room, per-snapshot.
- **Credential lease** (`credential_lease.ml`): Scoped credential resolution
  with type-safe redaction. Prevents accidental leakage into logs/prompts.
- **Room budget**: Token/cost limits per room (referenced in room-agent
  architecture).

---

## 3. Extension Mechanism Options

### Option A: Pipeline-First (Structured Pipelines as Primary Extension)

**Concept**: Make structured pipelines the primary extension contract.
Every extension is a YAML/JSON pipeline definition in `~/.clawq/pipelines/`.
Agent templates and skills are secondary surfaces that pipelines can reference.

**Pros**:
- Minimal new infrastructure — pipelines already exist and are agent-authorable.
- YAML/JSON is trivially generated by LLMs.
- Composable — pipeline steps can invoke other pipelines.
- Structured inputs/outputs enable validation and chaining.

**Cons**:
- Pipelines are execution-focused — they don't express hooks, event handlers,
  or non-pipeline tools.
- No packaging — a pipeline is a single file with no co-located tests, schema,
  or manifest.
- Doesn't cover the "add a new tool" use-case (pipelines produce text/JSON,
  not callable tools).

**Verdict**: Good primitive, insufficient as the sole extension mechanism.

### Option B: Plugin Directory (clawq-Native Plugin Format)

**Concept**: Define a plugin directory format inspired by Claude Code's
`.claude-plugin/` model but tailored to clawq's surfaces. Each plugin is a
directory under `~/.clawq/plugins/` containing a manifest, pipeline defs,
agent templates, skill files, and tests. The manifest declares metadata,
version, dependencies, sandbox grants, and enable/disable state.

```
~/.clawq/plugins/<name>/
  plugin.yaml          # manifest: name, version, description, author,
                       #   sandbox grants, dependencies
  pipelines/           # structured pipeline YAML files
  agents/              # agent template markdown files
  skills/              # SKILL.md skill definitions (markdown)
  tests/               # expected-input/output test fixtures
```

**Pros**:
- Unified packaging: everything about an extension lives together.
- Versioning and enable/disable are manifest-level.
- Co-located tests enable automated validation.
- Agent-authorable: YAML manifest + YAML pipelines + markdown agents.
- Sandbox grants are explicit in the manifest (landlock, egress, credentials).
- Per-room enable/disable via room access_bundles.

**Cons**:
- New infrastructure: manifest parser, plugin loader, registry state machine.
- Slightly more ceremony than bare pipeline files.
- Need to define the manifest schema carefully.

**Verdict**: Strongest fit for the "clawq extends clawq" goal. The manifest
adds modest overhead but provides the packaging, versioning, and sandbox
contract that bare pipelines lack.

### Option C: MCP-Only (All Extensions as MCP Servers)

**Concept**: Every extension is an MCP server. Clawq connects to it via
its existing MCP client. The extension exposes tools, resources, and prompts
via the MCP protocol.

**Pros**:
- Standard protocol — extensions are interoperable with any MCP-aware agent.
- No new packaging format — the MCP server IS the extension.
- Clawq's MCP client already works.

**Cons**:
- MCP servers are external processes — requires process management, health
  monitoring, restart logic.
- MCP servers are typically written in TypeScript/Python/other languages —
  not trivially authorable by an LLM against clawq's OCaml substrate.
- No native integration with clawq's room/scope model — MCP servers don't
  know about rooms, egress policy, or credential leases.
- Heavy for simple extensions (a single pipeline step doesn't need a whole
  server process).
- MCP protocol version drift (2024-11-05 vs 2025-03-26 vs 2025-06-18)
  creates maintenance burden.

**Verdict**: MCP is the right external interface (clawq should continue to
expose its tools via MCP server mode), but it's too heavy and disconnected
from clawq's room model to be the primary internal extension mechanism.

### Option D: Hybrid — Plugin Directory + MCP Exposure

**Concept**: Combine Option B (plugin directory as the native format) with
automatic MCP server exposure. A registered plugin's tools are added to
clawq's `Tool_registry`, which is already exposed via `mcp_server.ml`. This
means self-authored extensions are immediately available to external MCP
clients without any additional work.

**Pros**:
- Best of both worlds: native packaging + standard protocol exposure.
- Zero additional work for MCP exposure — it's automatic via the existing
  registry.
- Plugin tools participate in room scope, egress policy, credential leases.

**Cons**:
- Same new infrastructure as Option B for the plugin directory layer.
- Plugin tools are OCaml-registered, so they need a dynamic dispatch
  mechanism (shell invocation or interpreted pipeline step).

**Verdict**: **Recommended.** This is the natural architecture: plugins are
the packaging, pipelines/templates/skills are the content, and MCP exposure
is free.

---

## 4. Recommended Architecture

### 4.1 Primary Extension Contract: Plugin Directory

The primary extension unit is a **plugin directory** under `~/.clawq/plugins/<name>/`.
The content types inside are:

| Surface | Format | Agent-Authorable | Notes |
|---------|--------|-----------------|-------|
| Pipelines | YAML | Yes — trivially | Primary content type for most extensions |
| Agent templates | Markdown + YAML frontmatter | Yes | For extensions that need a persona |
| Skills | SKILL.md (markdown) | Yes | Instruction-based extensions |
| Tools | JSON schema + shell command | Partially | For simple I/O tools; complex tools need OCaml |

**Why pipelines are the dominant content type**: Structured pipelines are
YAML, they compose (pipeline steps invoke other pipelines), they have typed
inputs/outputs, and they're trivially generated by LLMs. Most self-authored
extensions will be pipeline-only plugins.

### 4.2 Plugin Tool Dispatch Model

For extensions that need to register a callable **tool** (not just a pipeline),
the execution model is: **treat pipeline steps as the tool implementation, with
the pipeline's final output as the tool result.**

Specifically:
- A plugin declares a pipeline in its manifest.
- At registration time, clawq wraps the pipeline as a `Tool.t` where:
  - `name` = pipeline name,
  - `parameters_schema` = derived from the pipeline's `inputs` definitions,
  - `invoke` = runs the pipeline with the provided arguments and returns the
    final step's output.
- This means every pipeline is automatically a tool. No separate tool dispatch
  mechanism is needed.
- For extensions that need direct shell access or HTTP calls, the pipeline
  should use `agent` steps (which have tool access) rather than `prompt` steps.

This model avoids the complexity of a separate tool dispatch layer while
ensuring every extension is immediately callable as a tool by the room agent,
other pipelines, or external MCP clients.

### 4.3 Plugin Manifest Schema

```yaml
# ~/.clawq/plugins/metrics-chase/plugin.yaml
name: metrics-chase
version: 1.0.0
description: >
  Chase product metrics: query data sources, compute trends,
  flag anomalies, and produce a structured metrics report.
author: clawq (self-authored)
license: MIT

# Sandbox declarations — clawq enforces these
sandbox:
  filesystem:
    read: []           # additional read paths beyond plugin dir
    write: []          # additional write paths beyond plugin dir
  network:
    allowed_hosts:
      - "*.google.com"
      - "api.mixpanel.com"
  credentials:
    handles: []        # credential lease handles this plugin may use

# Enable/disable state (managed by registry)
enabled: true

# Room grants (managed per-room via access_bundles)
# rooms: [engineering, ops]  # if set, only available in these rooms

# Dependencies on other plugins
# depends_on: []

# Content manifest
pipelines:
  - metrics-chase.yaml
agents: []
skills: []
```

### 4.4 Hello World: Agent-Authored Extension

Here is a concrete example of a self-authored extension, as it would be
generated by GLM-5.2 and reviewed by GPT-5.5.

**User request**: "Add a feature that summarizes the last 24h of GitHub
activity for a repo."

**GLM-5.2 generates**:

```yaml
# ~/.clawq/plugins/github-daily-summary/plugin.yaml
name: github-daily-summary
version: 1.0.0
description: >
  Summarize the last 24 hours of GitHub activity for a repository:
  commits, PRs, issues, and discussion activity.
author: clawq (self-authored)
sandbox:
  network:
    allowed_hosts:
      - "api.github.com"
  credentials:
    handles: []
pipelines:
  - github-daily-summary.yaml
```

```yaml
# ~/.clawq/plugins/github-daily-summary/pipelines/github-daily-summary.yaml
name: github-daily-summary
version: "1.0.0"
description: >
  Summarize the last 24h of GitHub activity for a repo.
inputs:
  repo:
    type: string
    description: "Repository in owner/repo format (e.g. octocat/Hello-World)"
    required: true
  token:
    type: string
    description: "GitHub personal access token (optional, for private repos)"
    required: false
steps:
  - name: fetch_activity
    kind: agent
    model: "anthropic:claude-sonnet-4-6"
    max_turns: 5
    task: >
      Analyze the GitHub repository {{repo}} activity from the last 24 hours.

      Steps:
      1. Use the `shell` tool to run: gh api repos/{{repo}}/events?per_page=100
         (requires `gh` CLI authenticated -- if unavailable, use `http_request`
         tool against https://api.github.com/repos/{{repo}}/events with the
         {{token}} input as Bearer token if provided).
      2. Filter events from the last 24 hours.
      3. Categorize by type: PushEvent, PullRequestEvent, IssuesEvent,
         IssueCommentEvent, CreateEvent, DeleteEvent.
      4. Produce a structured JSON summary with fields: summary (string),
         stats (object with commits, prs_opened, prs_merged, issues_opened,
         issues_closed), notable_items (array of {type, title, url,
         significance}).
      5. Output ONLY the JSON object, no other text.
```

**GPT-5.5 reviews** against the extension contract and returns:
- PASS: manifest schema valid, pipeline uses `agent` step (has tool access),
  sandbox declarations present.
- FIX: add `max_turns` to prevent runaway agent loops.
- FIX: add fallback path for environments without `gh` CLI.
- MINOR: `http_request` tool may not be available -- check tool registry or
  use `shell` with `curl`.

**GLM-5.2 applies fixes**, GPT-5.5 re-reviews, PASS.

**Registry**: plugin registered as `draft` -> user confirms -> `enabled`.

---

## 5. Authoring Loop Spec

### 5.1 Trigger

A self-extension request originates from a room conversation:

```
User: "Add a feature that chases down product metrics from our Mixpanel
       dashboard and flags anomalies."
```

The room agent recognizes this as an extension request and dispatches the
authoring loop.

### 5.2 Model Roles

| Role | Model | Responsibility |
|------|-------|---------------|
| **Author** | GLM-5.2 (or equivalent strong coding model) | Generate plugin manifest, pipeline YAML, agent templates, skill files. Must produce artifacts that conform to the extension contract (manifest schema, pipeline schema, sandbox declarations). |
| **Reviewer** | GPT-5.5 (via `review-and-fix` loop) | Validate against acceptance criteria: schema conformance, sandbox completeness, error handling, edge cases, security. Iterates until PASS or declares blockers. |
| **User** | Human | Confirms registration of reviewed extension. Optional — can be bypassed for low-risk extensions. |

### 5.3 Review-and-Fix Invocation

The review loop uses the existing `delegate` / `subagent` infrastructure:

1. **Spawn review subagent** with model `openai-codex:gpt-5.5` and a prompt
   that includes:
   - The extension contract (manifest schema, pipeline schema, sandbox rules).
   - The generated plugin directory contents.
   - Explicit acceptance criteria to check against.

2. **GPT-5.5 produces a review report** structured as:
   ```
   REVIEW: <plugin-name> v<version>
   STATUS: PASS | FAIL
   FINDINGS:
     [BLOCKER] <finding description + location>
     [MAJOR]   <finding description + location>
     [MINOR]   <finding description + location>
   FIXES REQUIRED:
     - <concrete fix instructions for the author>
   ```

3. **If FAIL**: dispatch GLM-5.2 again with the fix instructions. Re-review.

4. **If PASS**: proceed to registry.

5. **Exit conditions**: 
   - PASS with no BLOCKER/MAJOR findings.
   - Max 3 review rounds (prevent infinite loops). After 3 rounds, surface
     remaining findings to the user for manual resolution.

### 5.4 Registry State Machine

```
draft → reviewed → enabled → room-granted
  │        │          │          │
  │        │          │          └─ Extension is available in a specific room
  │        │          │             (via room access_bundle grants).
  │        │          │
  │        │          └─ Extension is globally enabled. Available in all
  │        │             rooms unless room-scoped restrictions apply.
  │        │
  │        └─ Review-and-fix loop completed (PASS). Awaiting user
  │           confirmation for registration. Extension is not yet
  │           executable.
  │
  └─ Plugin directory created, manifest parsed, initial validation passed.
     Not yet reviewed. Not executable.
```

**State transitions**:

| From | To | Trigger | Guard |
|------|----|---------|-------|
| (new) | `draft` | Plugin directory created in `~/.clawq/plugins/` | Manifest parses, name unique, version valid |
| `draft` | `reviewed` | GPT-5.5 review returns PASS | No BLOCKER/MAJOR findings |
| `draft` | (rejected) | GPT-5.5 review returns FAIL after 3 rounds | Findings surfaced to user |
| `reviewed` | `enabled` | User confirms (or auto-confirm for low-risk) | Risk level assessed from sandbox declarations |
| `enabled` | `disabled` | User disables | — |
| `disabled` | `enabled` | User re-enables | — |
| `enabled` | `room-granted` | Room access_bundle includes plugin | Room admin grants |
| `room-granted` | `enabled` | Room access_bundle removes plugin | — |

**Persistence**: Plugin state is stored in a registry file
(`~/.clawq/plugin-registry.json`) that maps plugin names to their current
state, version, and room grants.

### 5.5 Risk Assessment

Risk level is determined by the plugin's sandbox declarations:

| Sandbox Profile | Risk Level | Auto-Confirm | Review Rounds |
|----------------|------------|-------------|---------------|
| No network, no credentials, pipelines only | Low | Yes | 1 |
| Network to allowed hosts, no credentials | Medium | No | 2 |
| Credentials or broad network | High | No | 3 |

---

## 6. Seed Pipelines / Tools (<=8)

The user's explicit steer: **do NOT integrate a bunch of stuff**. The seed
catalogue is a small set of high-leverage pipelines grown via the self-extension
loop, mapped to Claude Tag use-cases. Each is a pipeline-only plugin.

| # | Plugin Name | Claude Tag Use-Case | Description |
|---|------------|--------------------|----|
| 1 | `metrics-chase` | Chase product metrics | Query data sources (Mixpanel, PostHog, GA), compute trends, flag anomalies, produce structured metrics report. |
| 2 | `support-ticket-triage` | Triage support tickets | Ingest tickets from a source (email, GitHub issues, Zendesk-style), classify severity, suggest owners, draft responses. |
| 3 | `root-cause-bug-hunt` | Root-cause bug investigation | Given a bug report, systematically investigate: reproduce, bisect, trace, hypothesis-test, produce root-cause analysis. |
| 4 | `code-review-round` | Code review | Structured code review pipeline: diff analysis, style check, security scan, test coverage assessment, review summary. |
| 5 | `incident-postmortem` | Incident response | Given incident timeline and logs, produce structured postmortem: timeline, root cause, impact, action items. |
| 6 | `pr-description-gen` | PR documentation | Generate comprehensive PR descriptions from diffs: summary, testing notes, breaking changes, migration guide. |
| 7 | `onboarding-guide` | Team onboarding | Generate onboarding guides for new team members: codebase overview, architecture, key files, getting started. |
| 8 | `security-audit-lite` | Security review | Lightweight security audit: dependency scan, secrets detection, OWASP checklist, produce findings report. |

**Explicitly rejected**: Broad third-party integration catalogue. These 8
pipelines are the *seed* — the self-extension mechanism lets clawq grow more
on demand. The point is that clawq doesn't ship 50 integrations; it ships a
mechanism and 8 examples.

---

## 7. Risk / Safety Section

### 7.1 Sandbox Model

Self-authored extensions are sandboxed using **existing primitives**:

#### Filesystem: Landlock

- Plugins run within the Landlock sandbox that already constrains clawq's
  workspace + config dir access.
- The plugin manifest declares additional read/write paths; clawq validates
  these against the Landlock ruleset before granting.
- Default: plugin runs within the existing process-level Landlock sandbox,
  which grants read-write to the workspace directory, `~/.clawq/`, and `/tmp`,
  plus read-only to system paths. No additional per-plugin narrowing exists
  today — that is a follow-up (B740).

#### Network: Egress Policy

- Plugins declare `sandbox.network.allowed_hosts` in their manifest.
- Clawq evaluates these against the existing `egress_evaluator.ml` glob
  matcher.
- Default: no network access unless explicitly declared.
- Per-room egress rules (already implemented) further restrict at runtime.

**Note**: Plugin-specific sandbox grants (declaring per-plugin filesystem and
network restrictions in the manifest) require new enforcement work. The existing
Landlock sandbox is process/workspace-scoped -- it constrains the entire clawq
process, not individual plugins. Per-plugin enforcement is a follow-up
implementation bug (see B740).

#### Credentials: Credential Lease

- Plugins declare `sandbox.credentials.handles` — a list of credential
  lease handle IDs.
- Clawq resolves these through the existing `credential_lease.ml` API,
  which provides type-safe redaction and scoped access.
- Default: no credential access unless explicitly declared and granted.

### 7.2 Containment of Malicious/Broken Extensions

| Threat | Mitigation |
|--------|-----------|
| Extension reads files outside workspace | Landlock filesystem sandbox — cannot be bypassed by pipeline YAML |
| Extension exfiltrates data to attacker's server | Egress policy — host globs must be declared and approved |
| Extension leaks credentials into output | Credential lease — redacted values only; `apply_*` functions are the sole raw-value boundary |
| Extension runs forever / burns tokens | Room budget (token/cost limits per room) + pipeline step timeout |
| Extension produces harmful output | Review-and-fix gate (GPT-5.5) catches obvious issues; room agent's system prompt applies safety rules |
| Extension is buggy and crashes | Pipeline execution is isolated per-run; a crash doesn't affect other pipelines or the main session |
| Extension conflicts with builtins | Name collision detection at registration; builtin tools take precedence |
| Plugin sandbox grants are too broad | Manifest declares grants; user must confirm medium/high-risk plugins; per-room grants provide additional scoping |

### 7.3 What We Do NOT Protect Against

- A sophisticated prompt injection in a pipeline's system prompt that
  convinces the model to ignore safety rules. This is an inherent LLM
  risk, not specific to extensions.
- A pipeline that uses `shell` tool to run arbitrary commands within the
  existing sandbox. The sandbox already constrains this; we don't add
  further restrictions beyond what landlock provides.

---

## 8. MCP-Only vs Native Plugin Dir: Decision

### Decision: **Native Plugin Directory** with automatic MCP exposure

**Rationale**:

| Criterion | MCP-Only | Native Plugin Dir | Winner |
|-----------|---------|-------------------|--------|
| Agent-authorable | No (requires external process in TS/Python) | Yes (YAML + markdown) | Native |
| Room/scope integration | No (MCP servers don't know about rooms) | Yes (plugins participate in access_bundles) | Native |
| Credential integration | No (separate credential management) | Yes (credential lease handles) | Native |
| Egress policy | No (MCP servers manage their own networking) | Yes (egress evaluator) | Native |
| Standard protocol | Yes (MCP is the standard) | No (clawq-specific) | MCP |
| External interoperability | Yes (any MCP client can use) | Only via MCP server mode | MCP |
| Packaging | Process management | Directory + manifest | Native |
| Lightweight extensions | Heavy (whole server process) | Light (YAML file) | Native |

**The hybrid**: Native plugin directory is the internal format. Clawq's
existing MCP server mode (`mcp_server.ml`) automatically exposes registered
plugin tools via the MCP protocol. This gives us:
- Agent-authorable extensions (YAML/markdown, not external processes).
- Full integration with room scope, egress policy, credential leases.
- Free MCP exposure for external interoperability.

**Trade-off accepted**: The native format is clawq-specific. Extensions
aren't portable to other MCP-aware agents without adaptation. This is
acceptable because the user's goal is "clawq extends clawq," not "clawq
ships a generic plugin ecosystem."

---

## 9. Implementation Roadmap (Follow-up Bugs)

These bugs are **suggested** — not created. The coordinator should create them
as appropriate.

### Phase 1: Plugin Loader + Registry

| Bug | Title | Description |
|-----|-------|-------------|
| B738 | Plugin manifest parser + registry state machine | Parse `plugin.yaml`, validate schema, manage state transitions (draft/reviewed/enabled/room-granted), persist to `~/.clawq/plugin-registry.json`. |
| B739 | Plugin loader: integrate with existing pipeline/template/skill discovery | Scan `~/.clawq/plugins/*/` and register contained pipelines, agent templates, and skills into their respective discovery mechanisms. |
| B740 | Plugin sandbox enforcement | Validate plugin sandbox declarations against landlock ruleset, egress policy, and credential lease availability. Reject plugins that request unavailable grants. |

### Phase 2: Authoring Loop

| Bug | Title | Description |
|-----|-------|-------------|
| B741 | Authoring loop: GLM-5.2 plugin generation | Implement the "dispatch GLM-5.2 to author a plugin" path. Takes a user request, generates plugin directory with manifest + pipeline YAML, validates against contract. |
| B742 | Authoring loop: GPT-5.5 review-and-fix integration | Implement the review subagent dispatch. GPT-5.5 reviews generated plugin against acceptance criteria, returns structured findings, iterates until PASS or max rounds. |
| B743 | Authoring loop: user confirmation + registration | Implement the confirmation step (for medium/high risk) and the `draft → reviewed → enabled` transition. |

### Phase 3: Seed Catalogue

| Bug | Title | Description |
|-----|-------|-------------|
| B744 | Seed pipeline: metrics-chase | Author the metrics-chase pipeline via the self-extension loop (dogfooding the authoring loop). |
| B745 | Seed pipeline: support-ticket-triage | Author the support-ticket-triage pipeline. |
| B746 | Seed pipeline: root-cause-bug-hunt | Author the root-cause-bug-hunt pipeline. |

### Phase 4: Integration

| Bug | Title | Description |
|-----|-------|-------------|
| B747 | Room access_bundle integration | Wire plugins into room access_bundles so rooms can grant/revoke access to specific plugins. |
| B748 | Per-room plugin skills (I057 follow-up) | Load skills from plugin directories into the per-room skill discovery path. |
| B749 | MCP server mode: auto-expose plugin tools | Ensure registered plugin tools appear in `mcp_server.ml` tools/list output. |

---

## 10. Relationship to Existing Ideas

| Idea | Connection |
|------|-----------|
| **I055** (MCP/plugins for remote runners) | Plugin system provides the packaging; MCP exposure handles the protocol. Remote runners can discover plugin tools via MCP. |
| **I057** (Per-room/thread skills) | Plugin loader registers skills into the same discovery path that I057 will thread through room sessions. |
| **I034** (Daily Briefing Pipeline) | The daily briefing is a natural candidate for a pipeline-only plugin — exactly the pattern this memo recommends. |
| **I028** (Agents into background tasks / slash commands) | Plugins can expose their pipelines as slash commands via the existing `command_bridge_pipeline.ml` integration. |

---

## 11. Open Questions

1. **Plugin dependency graph**: If plugin A's pipeline step invokes plugin B's
   pipeline, how do we detect cycles? Recommendation: shallow dependency
   resolution (one level) with a cycle check at registration time.

2. **Marketplace / sharing**: The memo explicitly rejects broad integration,
   but should there be a `clawq plugins export/import` mechanism for sharing
   self-authored plugins between clawq instances? Recommendation: defer to a
   future bug. The immediate need is local self-extension.

3. **Plugin update mechanism**: When a plugin is updated (version bump), how
   do we handle running pipelines that reference the old version? Recommendation:
   pipelines always run against the currently-enabled version; version pinning
   is a future enhancement.

---

## 12. Sources

### In-Repo (verified)

- `src/mcp_client.ml` — MCP client implementation (stdio + HTTP, credential-lease-aware)
- `src/mcp_server.ml` — MCP server exposing `Tool_registry`
- `src/structured_pipeline.ml` — Pipeline types, YAML parsing, discovery, builtins (`research-report`, `build-review-carm`, `plan-build-review-carm`)
- `src/structured_pipeline_run.ml` — Pipeline execution engine
- `src/structured_pipeline_schema.ml` — JSON Schema validator
- `src/agent_template.ml` — Agent template types, parsing, discovery
- `src/agent_template_builtins.ml` — builtin archetype registry
- `src/agent_template_builtins_*.ml` — generated builtin archetype groups
- `src/tool_registry.ml` — Tool registry (register/find/replace/remove)
- `src/tool.ml` — Tool type definition (name, schema, invoke, risk_level)
- `src/skills.ml` — Skills loading (SKILL.md format, deprecated JSON legacy)
- `src/builtin_skills.ml` — Builtin skill definitions
- `src/background_task*.ml` — Background task infrastructure
- `src/subagent_tool.ml` — B712 subagent spawn/poll
- `src/landlock.ml` — Landlock filesystem sandboxing
- `src/egress_evaluator.ml` — Egress policy evaluation
- `src/credential_lease.ml` — Credential lease resolution
- `.backlog/ideas/I055*.todo` — MCP/plugins for remote runners
- `.backlog/ideas/I057*.todo` — Per-room/thread skills
- `.backlog/ideas/I034*.todo` — Daily Briefing Pipeline
- `.backlog/ideas/I028*.todo` — Agents into background tasks / slash commands
- `src/access_snapshot.ml` — Access snapshots and access_bundles terminology

### External Research (read and cited)

- `~/.llm-general/ai-coding/mcp/protocol-fundamentals.md` — MCP 2025-06-18 spec reference
- `~/.llm-general/ai-coding/opencode/plugins.md` — OpenCode plugin architecture (JS/TS modules, hooks, custom tools)
- `~/.llm-general/ai-coding/opencode/publishing-oc-plugins.md` — OpenCode plugin publishing (npm, local, ecosystem)
- `~/.llm-general/ai-coding/claude-code/plugin-submission.md` — Claude Code plugin distribution (marketplace model, `plugin.json` manifest, community aggregators)
- https://www.anthropic.com/news/introducing-claude-tag — Claude Tag announcement (curated skill catalogue, metrics-chase, support-ticket-triage, root-cause-bug-hunt)
