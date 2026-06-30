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
