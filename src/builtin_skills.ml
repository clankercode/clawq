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
