(* Builtin skills shipped with the clawq binary.
   These are discoverable alongside user-defined SKILL.md skills. *)

let is_test_skill_name name =
  String.length name >= 5 && String.sub name 0 5 = "test-"

(* Each entry: (name, description, instructions) *)
let all_skills =
  [
    ( "test-browser-automation-tool-all",
      "Comprehensive end-to-end test of the browser automation tool. Exercises \
       all 20 actions, batch mode, workflows, tab management, and error \
       handling.",
      {|# Browser Automation Tool — Comprehensive Test Suite

You are running a comprehensive test of the `browser` tool. Execute every section below in order. For each test, report PASS or FAIL with a brief explanation. Stop and report if a critical failure prevents further testing.

## Prerequisites

Before starting, verify the browser tool is available:
1. Check that the `browser` tool is in your tool list.
2. If not available, report "SKIP: browser tool not registered" and stop.

## Phase 1: Basic Navigation and Content

### Test 1.1: Navigate to a page
```json
{"action": "navigate", "url": "data:text/html,<html><head><title>Test Page</title></head><body><h1>Hello Browser</h1><p id='content'>Test content here</p></body></html>"}
```
**Expected**: Success, page loads.

### Test 1.2: Get page content
```json
{"action": "content"}
```
**Expected**: HTML containing "Hello Browser" and "Test content here".

### Test 1.3: Take screenshot
```json
{"action": "screenshot"}
```
**Expected**: Base64-encoded PNG data returned.

### Test 1.4: Take full-page screenshot
```json
{"action": "screenshot", "full_page": true}
```
**Expected**: Base64-encoded PNG data returned.

## Phase 2: JavaScript Evaluation

### Test 2.1: Simple evaluation
```json
{"action": "evaluate", "javascript": "document.title"}
```
**Expected**: Returns "Test Page".

### Test 2.2: DOM manipulation
```json
{"action": "evaluate", "javascript": "document.getElementById('content').textContent = 'Modified'; 'done'"}
```
**Expected**: Returns "done".

### Test 2.3: Verify modification
```json
{"action": "content"}
```
**Expected**: HTML now contains "Modified" instead of "Test content here".

### Test 2.4: Evaluate with return value
```json
{"action": "evaluate", "javascript": "JSON.stringify({a: 1, b: [2,3]})"}
```
**Expected**: Returns JSON string.

## Phase 3: Interaction (Click, Type, Wait)

### Test 3.1: Navigate to interactive page
```json
{"action": "navigate", "url": "data:text/html,<html><body><input id='input1' type='text' placeholder='Type here'><button id='btn1' onclick=\"document.getElementById('result').textContent='clicked'\">Click Me</button><div id='result'></div></body></html>"}
```

### Test 3.2: Type into input
```json
{"action": "type", "selector": "#input1", "text": "Hello World"}
```
**Expected**: Success.

### Test 3.3: Click button
```json
{"action": "click", "selector": "#btn1"}
```
**Expected**: Success.

### Test 3.4: Verify click result
```json
{"action": "evaluate", "javascript": "document.getElementById('result').textContent"}
```
**Expected**: Returns "clicked".

### Test 3.5: Wait for selector
```json
{"action": "wait", "selector": "#result", "timeout": 5}
```
**Expected**: Success (element already exists).

### Test 3.6: Wait timeout (negative test)
```json
{"action": "wait", "selector": "#nonexistent", "timeout": 2}
```
**Expected**: Timeout error after ~2 seconds.

## Phase 4: Tab Management

### Test 4.1: List initial tabs
```json
{"action": "list_tabs"}
```
**Expected**: At least one tab listed.

### Test 4.2: Open new tab
```json
{"action": "new_tab", "url": "data:text/html,<html><body><h1>Tab 2</h1></body></html>"}
```
**Expected**: New tab created, returns tab info.

### Test 4.3: List tabs (should show 2)
```json
{"action": "list_tabs"}
```
**Expected**: Two tabs listed.

### Test 4.4: Switch to first tab
```json
{"action": "switch_tab", "tab": 0}
```
**Expected**: Success, switched to original tab.

### Test 4.5: Verify first tab content
```json
{"action": "evaluate", "javascript": "document.getElementById('result') ? 'original' : 'wrong tab'"}
```
**Expected**: Returns "original".

### Test 4.6: Close second tab
```json
{"action": "close_tab", "tab": 1}
```
**Expected**: Success.

### Test 4.7: Verify single tab remains
```json
{"action": "list_tabs"}
```
**Expected**: One tab listed.

## Phase 5: Script Injection

### Test 5.1: Load autorun script
```json
{"action": "load_script", "javascript": "window.__injected = true;", "name": "test-marker"}
```
**Expected**: Script registered.

### Test 5.2: List scripts
```json
{"action": "list_scripts"}
```
**Expected**: Shows "test-marker" script.

### Test 5.3: Navigate to verify autorun
```json
{"action": "navigate", "url": "data:text/html,<html><body>Script test</body></html>"}
```
Then:
```json
{"action": "evaluate", "javascript": "window.__injected === true ? 'injected' : 'not injected'"}
```
**Expected**: Returns "injected".

### Test 5.4: Unload script
```json
{"action": "unload_script", "name": "test-marker"}
```
**Expected**: Script removed.

### Test 5.5: Verify unload
```json
{"action": "list_scripts"}
```
**Expected**: No scripts (or "test-marker" not listed).

## Phase 6: Compound Workflows

### Test 6.1: navigate_and_extract
```json
{"action": "navigate_and_extract", "url": "data:text/html,<html><body><article>Important article content</article></body></html>", "selector": "article"}
```
**Expected**: Returns text content from the article element.

### Test 6.2: fill_form
Navigate first:
```json
{"action": "navigate", "url": "data:text/html,<html><body><form><input name='user' id='f1'><input name='pass' id='f2' type='password'><button type='submit' id='sub'>Submit</button></form></body></html>"}
```
Then:
```json
{"action": "fill_form", "fields": [{"selector": "#f1", "value": "testuser"}, {"selector": "#f2", "value": "testpass"}]}
```
**Expected**: Both fields filled successfully.

### Test 6.3: snapshot_all
```json
{"action": "snapshot_all"}
```
**Expected**: Returns combined content + accessibility tree + screenshot.

### Test 6.4: run_script
```json
{"action": "run_script", "javascript": "document.querySelectorAll('input').length"}
```
**Expected**: Returns 2 (two input fields).

## Phase 7: Batch Mode

### Test 7.1: Batch of actions
```json
{
  "batch": [
    {"action": "navigate", "url": "data:text/html,<html><body><div id='counter'>0</div></body></html>"},
    {"action": "evaluate", "javascript": "document.getElementById('counter').textContent = '1'; '1'"},
    {"action": "evaluate", "javascript": "document.getElementById('counter').textContent"},
    {"action": "screenshot"}
  ]
}
```
**Expected**: All 4 actions succeed, third returns "1".

### Test 7.2: Batch with error (stop-on-first-error)
```json
{
  "batch": [
    {"action": "evaluate", "javascript": "'step1-ok'"},
    {"action": "click", "selector": "#nonexistent-element-xyz"},
    {"action": "evaluate", "javascript": "'step3-should-not-run'"}
  ]
}
```
**Expected**: First action succeeds, second fails (element not found), third does NOT execute.

## Phase 8: Error Handling

### Test 8.1: Missing action parameter
```json
{}
```
**Expected**: Error message about missing "action" parameter.

### Test 8.2: Invalid action
```json
{"action": "nonexistent_action"}
```
**Expected**: Error message about unknown action.

### Test 8.3: Navigate without URL
```json
{"action": "navigate"}
```
**Expected**: Error about missing "url" parameter.

### Test 8.4: Click without selector
```json
{"action": "click"}
```
**Expected**: Error about missing "selector" parameter.

## Phase 9: Browser Agent (perform)

### Test 9.1: Simple instruction
```json
{"action": "perform", "instructions": "Navigate to data:text/html,<html><body><h1>Agent Test</h1></body></html> and tell me what the page title says"}
```
**Expected**: Agent navigates and returns content mentioning the page.

## Phase 10: Cleanup

### Test 10.1: Close browser
```json
{"action": "close"}
```
**Expected**: Browser session closed.

### Test 10.2: Verify close (double close should be safe)
```json
{"action": "close"}
```
**Expected**: No error (idempotent close).

## Summary

After all tests, produce a summary table:

| Phase | Tests | Passed | Failed | Skipped |
|-------|-------|--------|--------|---------|
| 1. Basic Navigation | 4 | | | |
| 2. JS Evaluation | 4 | | | |
| 3. Interaction | 6 | | | |
| 4. Tab Management | 7 | | | |
| 5. Script Injection | 5 | | | |
| 6. Workflows | 4 | | | |
| 7. Batch Mode | 2 | | | |
| 8. Error Handling | 4 | | | |
| 9. Browser Agent | 1 | | | |
| 10. Cleanup | 2 | | | |
| **Total** | **39** | | | |

Report the overall result: ALL PASS / PARTIAL PASS (N failures) / FAIL|}
    );
    ( "idea",
      "Log a planning idea to the backlog. Use when the user has a feature \
       idea, enhancement suggestion, or brainstorm they want tracked.",
      {|# idea

File `$ARGUMENTS` as a backlog idea using `bl idea`.

## Setup

Get the live CLI interface:
!`bl idea --help`

## Instructions

1. Interpret the idea description in `$ARGUMENTS`.
2. Craft a concise, searchable title (reword if the raw input is vague or verbose).
3. Quick duplicate check: `bl list --search "keywords"` — if an existing idea clearly covers it, mention it and stop. Keep this brief (one search, move on).
4. Choose the right invocation:
   - **Simple ideas** (clear one-liner): `bl idea --simple "Title here"`
   - **Detailed ideas** (enough context for a body): `bl idea --title "Title" --body "Description"`
5. Add `--priority`, `--complexity`, or `--estimate` only when clearly inferable from the description.
6. Report the created idea ID and title.

## Constraints

- Use `--simple` for straightforward ideas; only expand `--body` when the description warrants it.
- Keep duplicate searching to one quick search — move on promptly.
- Use only flags from the live `--help` output above.
- Only ask follow-up if the description is truly unintelligible.|}
    );
    ( "briefing-hourly",
      "Hourly breaking-news check across the user's monitored topics. \
       Deterministic orchestration with pre-flight config validation; \
       guarantees web_search is never called with empty args; delivers results \
       to the user's session via send_to_session. Invoked by the \
       briefing-hourly cron job.",
      {|# briefing-hourly — Hourly breaking-news check

You are running the hourly briefing on the `cron:briefing` worker session. The user's DM session is recorded as `delivery_session` in the rig config; you will deliver your output to that session via `send_to_session` at the end. Follow every step exactly in order. Do not call any tool before completing Step 1's pre-flight validation.

## Step 1: Load and validate config

Call `memory_recall` with `query="rig:briefing:config"` to retrieve the briefing configuration.

**Pre-flight validation (MUST complete before any other tool call):**

1. If memory_recall returns no result or an empty value, respond with EXACTLY: `Briefing not configured. Run /rig install briefing first.` and stop.
2. The returned value is a JSON string. Parse it. If parsing fails, respond with `Briefing config malformed: <one-line reason>.` and stop.
3. Locate the `topics` field. If it is missing, empty, or not an array, respond with `Briefing has no topics configured. Run /rig adjust briefing.` and stop.
4. Locate the `delivery_session` field. If it is missing or empty, respond with `Briefing has no delivery_session. Run /rig adjust briefing to migrate.` and stop. The skill refuses to silently log into the worker session — without delivery_session the user would never see the output.

## Step 2: Emit planned queries (audit trail)

Before any web_search call, emit a single assistant message listing your planned queries, one per line, in this exact format:

```
Planned hourly queries:
- <topic 1> breaking
- <topic 2> breaking
```

This makes silent failures observable in session logs.

## Step 3: Sequential web_search

For each topic in the topics list (one at a time, never parallel):

- Build a non-empty query string: `<topic> breaking` (or `<topic> just announced`).
- Call `web_search(query="<built_query>")`. The `query` parameter MUST be a non-empty string — never call web_search with `args={}` or any empty query.
- If the result indicates `rate-limited` or `no results`, record the topic as skipped and continue to the next topic. Do not retry.
- Search APIs throttle at ~1 req/sec; making these calls sequentially (never parallel) honors that limit.

## Step 4: Compose briefing text

- If nothing genuinely notable was found across all topics, the briefing text is EXACTLY: `Nothing notable.` (still delivered in Step 5 so the user knows the check ran).
- Otherwise, write 2-3 sentences per notable item with a link. Only report truly significant events (breaking news, just-announced milestones), not routine updates.

## Step 5: Deliver to user (MANDATORY — runtime-enforced)

Call `send_to_session` exactly once with:

- `session_id` = the `delivery_session` value from the config (Step 1).
- `message` = the briefing text from Step 4 (including the literal `Nothing notable.` case).
- `wake_agent` = false (silent notification — the user reads it when they next open the channel).
- `store_in_history` = true (default).

**CRITICAL DELIVERY CONTRACT:** The runtime tracks whether `send_to_session` was called during this turn. If you complete Steps 1–4 but do NOT call `send_to_session`, the cron run will be marked `incomplete_delivery` (not `ok`) and the user will see a failure in `clawq cron history`. There is no way to satisfy this contract without the actual tool call — emitting a status line like "Delivered briefing..." as plain text does NOT count.

After send_to_session succeeds, your final assistant message should be a short status line like `Delivered briefing to <delivery_session>: <N> notable items.` so the cron run log shows what happened. Do not duplicate the full briefing in this status line.|}
    );
    ( "briefing-daily",
      "Daily briefing across RSS feeds, monitored topics, and weather. \
       Deterministic orchestration with pre-flight config validation; \
       guarantees web_search is never called with empty args; delivers results \
       to the user's session via send_to_session. Invoked by the \
       briefing-daily cron job.",
      {|# briefing-daily — Daily briefing

You are generating the user's daily briefing on the `cron:briefing` worker session. The user's DM session is recorded as `delivery_session` in the rig config; you will deliver your output to that session via `send_to_session` at the end. Follow every step exactly in order. Do not call any tool before completing Step 1's pre-flight validation.

## Step 1: Load and validate config

Call `memory_recall` with `query="rig:briefing:config"` to retrieve the briefing configuration.

**Pre-flight validation (MUST complete before any other tool call):**

1. If memory_recall returns no result or an empty value, respond with EXACTLY: `Briefing not configured. Run /rig install briefing first.` and stop.
2. The returned value is a JSON string. Parse it. If parsing fails, respond with `Briefing config malformed: <one-line reason>.` and stop.
3. Locate the `feeds_file` path (default `~/.clawq/briefing_feeds.txt`), the `topics` array, the `weather_location` (optional), and the `rss_tool` (default `sfeed`).
4. Locate the `delivery_session` field. If it is missing or empty, respond with `Briefing has no delivery_session. Run /rig adjust briefing to migrate.` and stop. The skill refuses to silently log into the worker session.

## Step 2: Emit plan (audit trail)

Before any tool call after Step 1, emit a single assistant message in this exact format:

```
Daily briefing plan:
- Feeds: <feeds_file>
- RSS tool: <rss_tool>
- Topics: <topic1>, <topic2>, ...
- Weather: <location_or_none>
- Delivery: <delivery_session>
```

## Step 3: Fetch RSS headlines

Use shell_exec to run the configured RSS tool against `<feeds_file>`. Extract the top 5 most notable headlines. If the command fails, record the failure and continue (the briefing still has value with topics + weather).

## Step 4: Sequential web_search for topics

For each topic in the topics list (one at a time, never parallel):

- Build a non-empty query string: `<topic> today`.
- Call `web_search(query="<built_query>")`. The `query` parameter MUST be a non-empty string — never call web_search with `args={}` or any empty query.
- If the result indicates `rate-limited` or `no results`, record the topic as skipped and continue.

## Step 5: Weather (if configured)

If `weather_location` is set, call `web_search(query="<location> weather today")` and extract today's forecast.

## Step 6: Compose

Write a 400-800 word briefing with sections (omit empty sections):

- **Top Headlines** — from Step 3
- **Topic Updates** — from Step 4
- **Weather** — from Step 5
- **Worth Reading** — anything notable from headlines or topics

Use bullet points and links. Keep it scannable.

## Step 7: Deliver to user (MANDATORY — runtime-enforced)

Call `send_to_session` exactly once with:

- `session_id` = the `delivery_session` value from the config (Step 1).
- `message` = the composed briefing from Step 6. If all sections came up empty, send EXACTLY: `Daily briefing: no notable items today.` so the user still knows the run completed.
- `wake_agent` = false (silent notification — the user reads it when they next open the channel).
- `store_in_history` = true (default).

**CRITICAL DELIVERY CONTRACT:** The runtime tracks whether `send_to_session` was called during this turn. If you complete Steps 1–6 but do NOT call `send_to_session`, the cron run will be marked `incomplete_delivery` (not `ok`) and the user will see a failure in `clawq cron history`. There is no way to satisfy this contract without the actual tool call — emitting a status line like "Delivered daily briefing..." as plain text does NOT count.

After send_to_session succeeds, your final assistant message should be a short status line like `Delivered daily briefing to <delivery_session>: <N> headlines, <M> topic updates.` Do not duplicate the full briefing in this status line.|}
    );
    ( "feature",
      "Automate the creation of a new feature for clawq. Gathers requirements, \
       discovers existing capabilities, decomposes into implementation layers, \
       gates risky work behind approval, and delegates implementation.",
      {|# /feature — Automated Feature Creation

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
| New built-in tool | Available in agent tool list (after recompilation) |}
    );
    ( "pipeline-designer",
      "Design and create structured output pipelines. Gathers requirements, \
       designs step sequences with output schemas, generates valid YAML \
       pipeline definitions, and validates them.",
      {|# /pipeline-designer — Structured Output Pipeline Designer

Design and create multi-step structured output pipelines for clawq. Each pipeline is a YAML file defining a sequence of LLM prompt steps with validated JSON Schema outputs. Steps can reference previous step outputs and compose other pipelines.

**Note:** This skill provides in-conversation pipeline authoring guided by the agent. For an interactive CLI setup wizard, use `clawq pipeline wizard` instead. Both approaches produce the same pipeline YAML files — use whichever fits your workflow.

## Progress Reporting

At the start of each phase, call send_message to report progress:
- send_message(text="Pipeline Designer, step 1/5: Gathering requirements...")
- send_message(text="Pipeline Designer, step 2/5: Designing step sequence...")
- send_message(text="Pipeline Designer, step 3/5: Defining output schemas...")
- send_message(text="Pipeline Designer, step 4/5: Generating YAML definition...")
- send_message(text="Pipeline Designer, step 5/5: Validating pipeline...")

Always send the progress message before starting each phase.

## Phase 1: Gather Requirements

> First, call send_message(text="Pipeline Designer, step 1/5: Gathering requirements...").

If `$ARGUMENTS` provides a description, parse it for context and pre-fill answers. Confirm with the user.

Collect via `ask_user_question` (or conversationally if unavailable):

1. **Pipeline name** (text) — short identifier using alphanumeric chars and hyphens (e.g. "research-report")
2. **Description** (text) — what the pipeline should accomplish
3. **Inputs** — what parameters the pipeline needs (name, type, description, required?, default value?)
4. **Desired outputs** — what the final output should look like
5. **Number of steps** (optional) — rough estimate of how many steps

## Phase 2: Design Step Sequence

> First, call send_message(text="Pipeline Designer, step 2/5: Designing step sequence...").

Based on requirements, design the step sequence:

1. Identify logical stages (e.g. outline -> draft -> review)
2. Determine data flow between steps (which step outputs feed into which step prompts)
3. Decide if any steps should reference existing pipelines (composability)
4. Choose appropriate models for each step if different from default (optional)
5. Set retry counts for steps that may need multiple attempts (default: 2)

Present the proposed step sequence to the user for review before proceeding.

## Phase 3: Define Output Schemas

> First, call send_message(text="Pipeline Designer, step 3/5: Defining output schemas...").

For each prompt step, define a JSON Schema for the expected output. Follow these rules:

### Supported JSON Schema Keywords

The pipeline validator supports this subset of JSON Schema:

**Type keywords:** `type` (one of: `object`, `array`, `string`, `integer`, `number`, `boolean`, `null`)

**Object keywords:**
- `properties` — map of property name to sub-schema
- `required` — array of required property names
- `additionalProperties` — boolean (default true; set false to reject extra keys)

**Array keywords:**
- `items` — schema for array elements
- `minItems`, `maxItems` — integer bounds on array length

**String keywords:**
- `minLength`, `maxLength` — integer bounds on string length
- `enum` — array of allowed values

**Numeric keywords:**
- `minimum`, `maximum` — numeric bounds

### Schema Design Guidelines

- Keep schemas focused — only require what subsequent steps actually need
- Use `required` for fields that downstream steps depend on
- Prefer `string` for free-text content, `integer` for counts, `array` for lists
- Use `enum` to constrain categorical values (e.g. `["low", "medium", "high"]`)
- Nest objects for structured sub-components
- Set `additionalProperties: false` only when strict shape control matters

## Phase 4: Generate YAML Definition

> First, call send_message(text="Pipeline Designer, step 4/5: Generating YAML definition...").

Generate the complete pipeline YAML file. Use `file_write` to save it to `~/.clawq/pipelines/<name>.yaml`.

### Pipeline YAML Format Reference

```yaml
name: pipeline-name
version: "1.0"
description: What this pipeline does

inputs:
  input_name:
    type: string
    description: What this input is for
    required: true
    default: optional-default-value

steps:
  # Prompt step — calls an LLM and validates the output
  - name: step-name
    prompt: |
      Your prompt text here.
      Use {{input.input_name}} for input variables.
      Use {{previous_step_name}} for full JSON output of a previous step.
      Use {{previous_step_name.field}} for a specific field from a previous step.
    system_prompt: Optional system prompt override
    model: Optional model override (e.g. "openai:gpt-5.4")
    output_schema:
      type: object
      properties:
        field_name:
          type: string
        another_field:
          type: integer
      required: [field_name]
    max_retries: 2

  # Pipeline step — runs another pipeline as a sub-step
  - name: sub-step-name
    pipeline: other-pipeline-name
    input_map:
      other_input: "{{input.my_input}}"
      derived_input: "{{previous_step_name.field}}"
```

### Template Variable Syntax

- `{{input.X}}` — replaced with the value of input parameter `X`
- `{{step_name}}` — replaced with the full JSON output of the named step
- `{{step_name.field}}` — replaced with a specific field extracted from a step's JSON output (top-level string/number fields only; objects/arrays use the full JSON representation)

### Key Constraints

- Step names must be unique within a pipeline
- Step names must be valid identifiers (alphanumeric + hyphens)
- Pipeline steps can reference other pipelines by name (max nesting depth: 3)
- `max_retries` defaults to 1 if omitted
- `version` should be a string (e.g. "1.0")

## Phase 5: Validate Pipeline

> First, call send_message(text="Pipeline Designer, step 5/5: Validating pipeline...").

After writing the YAML file, validate it:

```
shell_exec("clawq pipeline validate <name>")
```

If validation fails, fix the issues and re-validate. Common issues:
- Duplicate step names
- Invalid JSON Schema (unknown type, malformed properties)
- Missing required fields in pipeline definition
- YAML syntax errors

Optionally, show the user how to run the pipeline:

```
clawq pipeline run <name> --input key1=value1 --input key2=value2
```

## Example Pipelines

### Research Report Pipeline
A 3-step pipeline: outline the report structure, write the draft, then review it.
- Inputs: `topic` (required), `depth` (default: "medium")
- Steps: outline -> draft -> review
- Each step's schema validates the expected structure

### Code Review Pipeline
A 2-step pipeline: analyze code, then generate review feedback.
- Inputs: `code` (required), `language` (default: "ocaml")
- Steps: analyze -> review
- Analysis step outputs: issues array, complexity score
- Review step outputs: summary, recommendations array, rating

### Data Extraction Pipeline
A 2-step pipeline: extract entities, then classify them.
- Inputs: `text` (required), `categories` (required)
- Steps: extract -> classify
- Extract outputs array of entities with name and context
- Classify maps each entity to a category from the input list |}
    );
    ( "run-task",
      "Runs a backlog item (bug, feature, task, epic, milestone, phase) \
       end-to-end by gathering context, constructing a startup prompt, \
       spawning a worktree-backed subagent, and managing the claim/done/review \
       cycle.",
      {|# run-task

Execute backlog target `$ARGUMENTS` end-to-end: gather context, build a startup prompt, spawn a native/local or worktree-backed subagent, and manage the claim/done/review cycle.

## Setup

Get backlog usage reference:
!`bl howto`

## Step 1: Classify the target

Interpret `$ARGUMENTS` as a backlog target. If none provided, ask for one.

| Class | Examples |
|-------|----------|
| Leaf item | `B123`, `F123`, `I123`, `P1.M2.E3.T4` |
| Scope item | epic `P1.M2.E3`, milestone `P1.M2`, phase `P1` |

## Step 2: Gather backlog context

### Leaf items
Run `bl claim TARGET` — capture the full output. If claim fails due to existing ownership, surface that explicitly and decide whether to continue, hand off, or reuse an existing worker.

### Scope items
Run all three:
- `bl show TARGET`
- `bl tree TARGET --unfinished`
- `bl list TARGET --available --json`

Summarize runnable epics/tasks from the available list.

## Step 3: Construct the startup prompt

Build the prompt from the command outputs gathered above. If the user gave extra steering, append it after a final `---` separator.

### Leaf prompt

```text
$ bl howto
<OUTPUT OF bl howto>

---

$ bl claim TARGET
<OUTPUT OF bl claim TARGET>

---

<TAIL INSTRUCTION>
```

**Leaf tail (normal):**
> Complete TARGET, then mark it done, then load and use your `review-and-fix` skill.

**Leaf tail (plan mode):**
> Plan and complete TARGET. After planning, run `git rebase` against your parent branch to ensure you're up to date. Make `bl done TARGET` the second-to-last step of the plan, and make loading and using the `review-and-fix` skill the final step of the plan.

Use plain Git for parent-branch checks and rebases. Do not use Graphite (`gt`)
or other stack-management tools to infer or mutate branch relationships.

### Scope prompt (epic, milestone, phase)

```text
$ bl howto
<OUTPUT OF bl howto>

---

$ bl show TARGET
<OUTPUT OF bl show TARGET>

---

$ bl tree TARGET --unfinished
<OUTPUT OF bl tree TARGET --unfinished>

---

$ bl list TARGET --available --json
<OUTPUT OF bl list TARGET --available --json>

---

Runnable epics now:
<RUNNABLE SUMMARY>

---

<TAIL INSTRUCTION>
```

**Epic tail:**
> Complete epic TARGET. Use `bl list TARGET --available --json` to identify runnable tasks in this epic. Claim the relevant runnable tasks for TARGET, complete them, mark them done, and refresh epic availability until the epic is complete or blocked. When the epic is complete, load and use `review-and-fix`, then commit. If all remaining work is blocked, summarize blockers and stop.

**Milestone/Phase tail:**
> Work through TARGET epic-by-epic. Use the scoped tree and runnable epic summary to choose an epic in this scope that currently has runnable tasks. Complete one epic before moving to the next. Within each epic, claim the runnable tasks, complete them, mark them done, and refresh epic availability until that epic is complete or blocked. After each completed epic, load and use `review-and-fix`, then commit. Refresh the scope state and continue until no runnable epics remain. If blocked, summarize the blockers and stop.

## Step 4: Choose runner and agent role

Prefer the local runner for most tasks. Escalate to external only when genuinely needed.

| Sizing | Runner | When |
|--------|--------|------|
| Small/medium | `local` + agent role | Default. Most leaf items and single-epic scope. |
| Large (>8h, cross-cutting) | External (`opencode`, `claude`) | Multi-file refactors spanning the whole codebase. |
| Entire project | `local` team-lead + local coders | Orchestrator spawns per-epic/task agents. |

### Agent roles (local runner)

| Task type | agent_name | use_worktree |
|-----------|------------|--------------|
| Implement feature/fix | `coder` | true |
| Write/run tests | `tester` | true |
| Refactor/cleanup | `refactorer` | true |
| Debug/root-cause | `debugger` | true |
| Review code | `reviewer` | false |
| Explore/research | `researcher` | false |
| Plan architecture | `planner` | false |
| Orchestrate subtasks | `team-lead` | false |
| Run entire project | `ceo` | false |

## Step 5: Launch the subagent

```
background_task_enqueue(
  runner: "local",
  agent_name: "<role>",
  repo_path: "<repo root>",
  prompt: "<constructed prompt>",
  branch: "clawq-bg-<TARGET>",
  use_worktree: true/false
)
```

Automerge is enabled by default. To disable it for a specific task, pass `automerge: false`.

## Step 6: Monitor and steer

- `background_task_list` — check status
- `background_task_logs` / `background_task_wait` — follow progress
- `background_task_transcript` — inspect bounded conversation history, with regex filters and JSONL export for large results
- `background_task_send_message` — send clarifications
- `background_task_resume` / `background_task_recover` — handle stalls

Verify the worker is making progress; don't assume success from a clean launch.

## Step 7: Close out

- When a worktree-backed task finishes, the system automatically sends a **completion pass** message to the agent. The agent resumes with its session context, commits remaining changes, rebases against master, reviews all changes for correctness/quality/completeness/safety (review-and-fix guard), runs checks, and outputs the sentinel `OK_TASK_DONE_CHECKED_REBASED_COMMITED`.
- Since automerge is enabled by default, the system then attempts a fast-forward merge. If automerge was disabled, the user is notified normally.
- Inspect changes and verify tests/review status.
- Use `background_finalize(id=...)` only when manual rebase and fast-forward merge is desired after the completion pass.
- If review reveals follow-up issues, defer done state until after review/fix is clean.

## Constraints

- Reconstruct prompts from backlog outputs.
- Surface claim conflicts explicitly.
- Use clawq-native worktree/background tools over ad-hoc shell workflows.
- For interactive task completion, use plain Git for branch ancestry and rebase
  steps; do not call Graphite (`gt`) unless the user explicitly asks for it.
- Branch names should visibly contain the target ID (e.g. `clawq-bg-B467`). |}
    );
  ]

let builtin_metas () =
  List.map
    (fun (name, description, _instructions) -> (name, description, "(builtin)"))
    all_skills

let find_builtin name =
  let name_lower = String.lowercase_ascii name in
  List.find_opt
    (fun (n, _, _) -> String.lowercase_ascii n = name_lower)
    all_skills
