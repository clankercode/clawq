---
name: feature
description: Automate the creation of a new feature for clawq. Gathers requirements, discovers existing capabilities, decomposes into implementation layers, gates risky work behind approval, and delegates implementation. Prefers skills/rigs/cron/templates over OCaml code changes.
argument-hint: [feature description in natural language]
---

# /feature — Automated Feature Creation

Create a new clawq feature end-to-end: gather requirements, discover existing capabilities, decompose into implementation layers (preferring no-recompilation approaches), gate risky changes behind admin approval, and delegate implementation.

## Mandatory Progress Protocol

**You MUST emit a progress message at the start of every phase below before doing anything else in that phase.** Use `send_message(text=...)` — it delivers immediately to the active session. Failing to emit progress leaves the user staring at a silent terminal; treat this as a hard requirement, not a suggestion.

The six messages are exactly:

1. `Add Feature, step 1/6: Gathering requirements...`
2. `Add Feature, step 2/6: Discovering capabilities...`
3. `Add Feature, step 3/6: Decomposing feature...`
4. `Add Feature, step 4/6: Checking approvals...`
5. `Add Feature, step 5/6: Implementing...`
6. `Add Feature, step 6/6: Verifying and reporting...`

Additionally, emit a `send_message` whenever you actually load a skill, spawn a background task, or are about to wait on a long operation (>5 s). Do not emit a loading message when `use_skill` is a no-op because that no-argument skill is already present in current agent context. Examples:

- `Loading skill: <name>`
- `Spawning background task <runner> on branch <branch>...`
- `Waiting on background task <id> (this may take a few minutes)...`

If `send_message` is not registered (no active session), fall back to printing a plain assistant message that begins with the same text — but only after attempting send_message first.

## Phase 1: Requirement Gathering

**Step 1.0 (MANDATORY):** call `send_message(text="Add Feature, step 1/6: Gathering requirements...")`.

If `$ARGUMENTS` provides a description, parse it for context and pre-fill answers. Confirm with the user.

Collect via `ask_user_question` (or conversationally if unavailable):

1. **Feature name** (text) — short identifier (e.g. "daily-digest")
2. **Description** (text) — what the feature should do
3. **Access method preference** (single_select: "slash command", "scheduled task", "new tool", "CLI command", "let the skill decide")
4. **Acceptance criteria** (text) — how to verify it works
5. **Priority** (single_select: "low", "medium", "high", "critical")

## Phase 2: Capability Discovery

**Step 2.0 (MANDATORY):** call `send_message(text="Add Feature, step 2/6: Discovering capabilities...")`.

Read existing capabilities:

1. `file_read("docs/public/llms-full.txt")` — full self-knowledge reference
2. `skill_list()` — existing skills
3. `shell_exec("clawq rig list")` — existing rigs
4. `shell_exec("clawq cron list")` — existing cron jobs
5. `shell_exec("clawq agents list")` — existing agent templates
6. `tool_search("<keywords from feature description>")` — deferred tool discovery

After each `shell_exec` call above, emit `send_message(text="Checked: <thing>")` so the user sees discovery progress.

Summarize: "Here's what clawq can already do related to your request: ..."

## Phase 3: Feature Decomposition

**Step 3.0 (MANDATORY):** call `send_message(text="Add Feature, step 3/6: Decomposing feature...")`.

Classify each requirement into implementation layers (prefer lower layers):

| Layer | What | Recompilation? | Approval |
|-------|------|----------------|----------|
| 0 | Already exists — use existing tool/skill/command | No | None |
| 1 | SKILL.md — agent instructions + existing tools | No | None |
| 2 | Rig — setup workflow + external tools | No | Confirm |
| 3 | Skill + Cron — periodic execution | No | Confirm |
| 4 | Agent Template — specialized persona | No | None |
| 5 | MCP Server — new tool via external process | No | Confirm |
| 6 | OCaml changes — core runtime modification | **Yes** | **Admin required** |

Present the decomposition table and ask for confirmation via `ask_user_question` (type: confirm):
"Here's how I'd implement this feature. Proceed?"

## Phase 4: Approval Gating

**Step 4.0 (MANDATORY):** call `send_message(text="Add Feature, step 4/6: Checking approvals...")`.

**Layers 0-4** (no recompilation, low risk): Proceed after user confirmation.

**Layer 5** (MCP server, medium risk): Confirm with user, explain what will be created.

**Layer 6** (OCaml changes, high risk):

1. Check if the user has admin privileges (from the session context).
2. If **guest**: save the full feature plan for admin review:
   - Write the plan JSON to a temp file
   - Run: `shell_exec("clawq held-items save --name '<name>' --desc '<description>' --plan-file /tmp/held_plan_<timestamp>.json --layer 6 --requestor '<sender_id>' --channel '<channel>'")`
   - Respond: "This feature requires OCaml code changes. Your plan has been saved as a held item for admin review. An admin can view and approve it via `/held-items`."
   - Stop here for guest users.
3. If **admin**: present full impact summary (files to change, tests needed, estimated scope), then confirm via `ask_user_question`.
4. Only after admin confirmation: proceed to Phase 5.

## Phase 5: Implementation

**Step 5.0 (MANDATORY):** call `send_message(text="Add Feature, step 5/6: Implementing...")`.

Execute in dependency order based on decomposition. Before each Layer block below, emit `send_message(text="Implementing layer <N>: <short description>")`.

### Layer 0 (already exists)
Show usage instructions to the user. Done.

### Layer 1 (SKILL.md)
- Create the skill file: `file_write("~/.clawq/skills/<name>/SKILL.md", "<skill content>")`
  - Include frontmatter with name, description, argument-hint
  - Write clear step-by-step instructions the agent can follow
- Test immediately: `use_skill("<name>", arguments="test")`
- Report: "Created skill `/skill <name>`. Use it in chat or it auto-triggers from matching keywords."

### Layer 2 (Rig)
- Write a rig definition markdown describing setup steps and configuration
- Report: "Created rig. Install via `clawq rig install <name>` or `/rig install <name>`."

### Layer 3 (Skill + Cron)
- Create the skill first (Layer 1 flow)
- Add cron job: `shell_exec("clawq cron add <name> <session> '<schedule>' '<message>' --ttl <duration>")`
- Report: "Created skill + cron job. Runs on schedule: `<schedule>`. Manual trigger: `/skill <name>`."

### Layer 4 (Agent Template)
- Write template to `~/.clawq/agents/<name>.md`
- Report: "Created agent template `@<name>`. Use via `/delegate @<name> <prompt>`."

### Layer 5 (MCP Server)
- Write a server script (Python or Node) implementing the MCP protocol
- Register in `~/.clawq/mcp_servers.json`
- Report: "Created MCP server. New tools will be available in agent context after restart."

### Layer 6 (OCaml changes)
1. Create backlog items: `shell_exec("bl idea '<title>' --body '<detailed requirements and acceptance criteria>'")`
2. Ask user to choose execution strategy via `ask_user_question` (single_select):

   **Small tasks** (single-component, straightforward):
   - "Quick (Kimi K2)" — fast iteration
   - "Standard (Codex GPT-5.4)" — reliable general-purpose
   - "Thorough (Claude Opus)" — complex logic

   **Large tasks** (multi-component, cross-cutting):
   - "Single coder" — one agent, coherent single-pass
   - "Small team (orchestrator + coder + reviewer)" — multi-file with review
   - "Full team (lead + N coders + reviewer)" — cross-subsystem changes

3. Before spawning, emit `send_message(text="Spawning background task <runner> on branch <branch>...")`.
4. Spawn background task:
   ```
   background_task_enqueue(
     runner: "<selected, or local for a native subagent>",
     repo_path: "<repo>",
     prompt: "<constructed prompt with requirements, AC, file targets>",
     branch: "clawq-feature-<name>",
     use_worktree: true
   )
   ```
5. Monitor: `background_task_logs(id)`, `background_task_wait(id)`, and `background_task_transcript(id, regex: "...")` for bounded conversation history. Local/native subagents persist history under `__bg_task:<id>`.
6. Steer: `background_task_send_message(id, message: "...")`
7. On failure: `background_task_recover(id)`

## Phase 6: Verification and Reporting

**Step 6.0 (MANDATORY):** call `send_message(text="Add Feature, step 6/6: Verifying and reporting...")`.

1. **Skills**: invoke via `use_skill` to verify loading
2. **Cron**: verify via `shell_exec("clawq cron list")`
3. **Rigs**: verify via `shell_exec("clawq rig list")`
4. **OCaml**: check `background_task_list` for completion, inspect `background_task_transcript` when context is needed, verify `make test` passed
5. **Store**: `memory_store(key="feature:<name>", value="<summary of what was created>")`

Present final report:
- What was created (with specific file paths)
- How to access/use it (specific commands the user can run)
- What's pending (if OCaml implementation in progress)
- Link to backlog item (if created)

## Access Method Decision Matrix

| Feature Type | Recommended Access |
|---|---|
| Workflow/automation | `/skill <name> <args>` in chat |
| Periodic task | Automatic on schedule; manual via `/skill <name>` |
| Setup/config | `/rig install <name>` |
| Specialized agent | `/delegate @<name> <prompt>` |
| External tool | Available as tool in agent context (via MCP) |
| New CLI command | `clawq <name>` (after OCaml changes) |
| New built-in tool | Available in agent tool list (after recompilation) |
