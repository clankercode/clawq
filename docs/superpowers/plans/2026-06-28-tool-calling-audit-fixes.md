# Tool Calling Audit Fixes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix confirmed tool-calling deficiencies found by the parallel audits across Anthropic-compatible, OpenAI/Responses, and executor/validation paths.

**Architecture:** Keep provider parsers responsible for preserving raw model tool-call payloads and producing well-formed normalized `Provider.tool_call` values. Keep `Agent_2_tools` responsible for validation, execution, failure classification, and raw-failure logging. Keep replay/redaction utilities provider-shape-aware so diagnostic raw payloads do not break future requests or leak tool arguments in session display.

**Tech Stack:** OCaml 5.1, dune, Alcotest, Lwt, Yojson.Safe, Cohttp test servers where existing tests already use them.

## Global Constraints

- Work in `.worktrees/tool-calling-audit` on branch `tool-calling-audit`.
- Do not touch the unrelated dirty main-checkout file `test/test_agent_scoped_memory.ml`.
- Never run concurrent dune commands; use `-j1`.
- Unit tests must remain Quick-safe: no live network and no real sleeps except allowed local fake servers.
- Use TDD for each production behavior change: add/adjust failing test, verify red, implement, verify green.
- Preserve existing runtime behavior unless a confirmed audit finding requires a semantic fix.
- Prefer minimal local changes over broad provider refactors.

---

## Audit Findings Folded Into Scope

### Confirmed P1/P2 Fixes

- OpenAI/Codex replay must preserve assistant `phase` in sanitized Responses message items.
- Codex streaming must accept official final argument events (`response.function_call_arguments.done` / tool variant), repair partial buffers from completed response output, and surface `response.failed` / `response.incomplete` as errors.
- Generic OpenAI Chat tool-call responses must not silently return `ToolCalls []` after dropping malformed calls, and should preserve raw tool-call payloads for failure logs.
- `tool_search` must execute when advertised, without requiring an actual registered `Tool.t` named `tool_search`.
- Tool failure classification must use the raw invoke result before postprocessing so long `Error:` outputs are still failures and raw model tool-call data is logged.
- Stuck detection should honor structured `Provider.is_error`, not only `content` prefix.
- Malformed tool argument JSON should become an explicit tool-call error, not an empty `{}` coercion that can hide bad model output.
- Anthropic streaming empty `content_block.input = {}` with no deltas should normalize to `arguments = "{}"`, matching MiniMax.
- Anthropic raw diagnostic payloads must not keep otherwise-invalid empty assistant messages alive after tool-call stripping.
- Session-display redaction must recursively redact Anthropic raw payload shapes (`content[].tool_use.input`, streaming `data_raw`) as well as OpenAI function-call shapes.
- MiniMax should preserve raw tool-use payloads in `provider_response_items_json` for both non-streaming and streaming ToolCalls.

### Deferred / Explicitly Not In This Patch Unless Small

- Adding assistant visible text to `Provider.ToolCalls` is real but invasive because the variant lacks a `content` field and many call sites pattern-match it. Do not mix that larger schema migration into this patch unless all smaller fixes land cleanly and tests show the change is local.
- Broad P3 provider parser coverage for Gemini/Cohere/Ollama/Vertex is useful but lower priority than the current production Telegram/Xiaomi and OpenAI/Codex failure modes. Add raw preservation opportunistically only if a touched file makes it trivial.
- Docs are not blocking; optionally update `src/ANTHROPIC_API_REQUIREMENTS.md` after code if time permits.

---

### Task 1: Codex Responses Replay and Streaming Robustness

**Files:**
- Modify: `src/provider_openai_codex.ml`
- Modify: `test/test_provider_openai_codex.ml`
- Modify: `test/test_streaming.ml`

**Interfaces:**
- Consumes: `Provider_openai_codex.sanitize_input_item : Yojson.Safe.t -> Yojson.Safe.t option`
- Produces: Codex stream parsing that emits valid `Provider.ToolCalls` when final args arrive via `.done`, repairs partial args from completed `response.output`, and fails on upstream failed/incomplete events.

- [ ] **Step 1: Preserve `phase` red test**
  - Change `test_sanitize_message_strips_metadata` so the sanitized key list includes `"phase"` and the value remains `"final_answer"`.
  - Run: `opam exec --switch=clawq-5.1 -- dune exec --root . -j1 test/test_main.exe -- test provider_openai_codex`
  - Expected before implementation: FAIL because `phase` is stripped.

- [ ] **Step 2: Codex final args `.done` red tests**
  - Add tests in `test/test_streaming.ml` that feed SSE events to the existing Codex stream helper:
    - only `response.function_call_arguments.done` carries final arguments and should produce `arguments = "{...}"`;
    - partial delta has invalid/truncated JSON but completed `response.output[].arguments` is complete, and final call uses completed arguments.
  - Run the focused streaming suite/cases.
  - Expected before implementation: FAIL or keep partial args.

- [ ] **Step 3: Codex failed/incomplete red test**
  - Add a stream test where the SSE has `type = "response.failed"` or `"response.incomplete"` with error/details.
  - Expected behavior: `process_stream` fails with an actionable upstream error string.

- [ ] **Step 4: Implement Codex changes**
  - In `sanitize_input_item`, keep `phase` for `type = "message"` if present.
  - In stream handling, support `.done` argument events as authoritative for the buffer.
  - On completed response, replace buffer args with `response.output[].arguments` when non-empty and different from the accumulated buffer; completed output is authoritative.
  - On failed/incomplete events, raise/fail the stream with raw event context.

- [ ] **Step 5: Verify Task 1**
  - Run focused provider/streaming tests.
  - Commit: `git commit -m "fix: harden codex tool-call streaming"`.

### Task 2: Generic OpenAI Chat Raw Tool Payloads and Malformed Tool Calls

**Files:**
- Modify: `src/provider.ml`
- Add/modify: `test/test_provider.ml` or existing provider test file discovered in the repo.

**Interfaces:**
- Produces: `Provider.ToolCalls` from generic OpenAI Chat includes `provider_response_items_json = Some raw_tool_calls_json` when tool calls are present.
- Produces: malformed non-empty raw tool calls return an error instead of `ToolCalls []`.

- [ ] **Step 1: Raw payload red test**
  - Add a local fake OpenAI-compatible response containing one valid `choices[0].message.tool_calls[]`.
  - Assert returned `Provider.ToolCalls.provider_response_items_json` contains the raw tool call id/name/arguments.

- [ ] **Step 2: All-malformed red test**
  - Fake response has non-empty `tool_calls` but every item is missing function/name/arguments.
  - Expected: provider call fails with `malformed tool_call` or equivalent; it must not return `ToolCalls { calls = [] }`.

- [ ] **Step 3: Implement provider changes**
  - Serialize raw `tool_calls_json` into `provider_response_items_json` for successful tool-call responses.
  - If raw list is non-empty and parsed list is empty, fail with an actionable error including a raw preview.

- [ ] **Step 4: Verify Task 2**
  - Run focused provider tests.
  - Commit: `git commit -m "fix: preserve openai chat raw tool calls"`.

### Task 3: Executor Validation, `tool_search`, and Failure Classification

**Files:**
- Modify: `src/agent_2_tools.ml`
- Modify: `src/stuck_detector.ml`
- Modify: `test/test_phase3.ml`
- Modify: stuck detector tests if present, otherwise add focused cases to an existing suitable test file.

**Interfaces:**
- Produces: `tool_search` special-case works before registry lookup.
- Produces: malformed JSON arguments return a tool error and do not invoke the target tool.
- Produces: success/failure classification uses raw result before postprocessing.
- Produces: stuck detector treats `Provider.is_error = true` as an error.

- [ ] **Step 1: `tool_search` execution red test**
  - Add an Agent-level test with a registry containing deferred tools and a `Provider.tool_call` named `tool_search`.
  - Assert execution returns a `tool_search` result, not `unknown tool`.

- [ ] **Step 2: Malformed args red test**
  - Add non-streaming execution test with a tool that would set `invoked := true`; call it with `arguments = ""` or invalid JSON.
  - Assert `invoked = false`, result is an error, and `Provider.is_error = true`.

- [ ] **Step 3: Long error classification red test**
  - Use a tool returning a long string beginning with `Error:` and configure summarization/postprocess path if needed.
  - Assert audit/logical result is classified failure even if postprocessed content no longer starts with `Error:`.

- [ ] **Step 4: Stuck detector red test**
  - Build history with repeated tool messages where `is_error = true` and content does not start `Error:`.
  - Assert stuck detector flags consecutive/same errors.

- [ ] **Step 5: Implement executor changes**
  - Special-case `tool_search` before `Tool_registry.find`.
  - Replace malformed JSON `{}` fallback with `Error: failed to parse tool arguments as JSON ...` pre-validation.
  - Use raw invoke result to compute `success`, then stamp `Provider.is_error` on the postprocessed result.
  - Make `Stuck_detector.is_error` check both `Provider.is_error` and legacy `Error:` prefix.

- [ ] **Step 6: Verify Task 3**
  - Run focused phase3/stuck tests.
  - Commit: `git commit -m "fix: validate and classify tool-call failures"`.

### Task 4: Anthropic-Compatible Streaming and History Integrity

**Files:**
- Modify: `src/provider_anthropic.ml`
- Modify: `src/message_history.ml`
- Modify: `src/provider_types.ml` only if malformed replay behavior can be improved locally.
- Modify: `test/test_provider_anthropic.ml`
- Modify: `test/test_memory_retention.ml` or `test/test_memory.ml` for history integrity.

**Interfaces:**
- Produces: streamed empty object tool input finalizes as `"{}"`.
- Produces: history cleanup strips or drops Anthropic raw diagnostic payloads when corresponding tool calls are removed, so empty assistant messages are not preserved solely by raw diagnostics.

- [ ] **Step 1: Anthropic parse coverage red tests**
  - Add tests for empty input, nested input, multiple tool-use blocks in `test_provider_anthropic.ml`.
  - Empty input should assert `arguments = "{}"`.

- [ ] **Step 2: Anthropic streaming red test**
  - Add a Quick-safe local stream test or expose a pure helper mirroring MiniMax if necessary.
  - Feed `content_block_start` with `input = {}` and no deltas.
  - Assert final `arguments = "{}"` and raw event payload is preserved.

- [ ] **Step 3: History integrity red test**
  - Create an assistant message with no visible content/tool_calls but Anthropic raw `provider_response_items_json` containing `content[].tool_use`.
  - Run existing history integrity cleanup path.
  - Assert message is dropped or raw payload cleared so Anthropic serialization cannot emit empty assistant text.

- [ ] **Step 4: Implement Anthropic changes**
  - In streaming finalize, if a tool buffer is empty because `content_block.input = {}` and no deltas arrived, set `arguments = "{}"`.
  - In history cleanup, recognize Anthropic raw body/event shapes and do not let diagnostic raw payload alone keep an empty assistant alive after tool-call stripping.

- [ ] **Step 5: Verify Task 4**
  - Run focused Anthropic and memory/history tests.
  - Commit: `git commit -m "fix: normalize anthropic tool-call replay"`.

### Task 5: Raw Payload Preservation and Redaction for Anthropic/MiniMax

**Files:**
- Modify: `src/provider_minimax.ml`
- Modify: `src/command_bridge_session_fmt.ml`
- Modify: `test/test_provider_minimax.ml`
- Modify: `test/test_command_bridge.ml`

**Interfaces:**
- Produces: MiniMax ToolCalls preserve raw response/SSE tool payloads in `provider_response_items_json`.
- Produces: session display redacts nested tool arguments recursively across OpenAI, Anthropic, and streaming raw-event shapes.

- [ ] **Step 1: MiniMax raw preservation red tests**
  - Add non-streaming parse test asserting raw provider body is preserved when tool calls are returned.
  - Add streaming test asserting raw tool-use events are preserved.

- [ ] **Step 2: Redaction red tests**
  - Extend `test_command_bridge.ml` with:
    - Anthropic raw body containing `content[].tool_use.input.command` / `path` / `content`;
    - streaming raw event list containing `data_raw` JSON with nested tool input.
  - Assert displayed session output redacts sensitive/raw tool args.

- [ ] **Step 3: Implement MiniMax raw preservation**
  - Store full response body for non-streaming ToolCalls.
  - Store raw stream tool-use events for streaming ToolCalls.

- [ ] **Step 4: Implement recursive redaction**
  - Parse `provider_response_items_json`; recursively redact object fields named `arguments`, `input`, `data_raw` JSON payloads that contain tool input, and common tool arg keys such as `command`, `path`, `content`.
  - Preserve malformed raw strings safely with conservative redaction.

- [ ] **Step 5: Verify Task 5**
  - Run focused MiniMax and command bridge tests.
  - Commit: `git commit -m "fix: preserve and redact raw tool payloads"`.

### Task 6: Final Verification and Review

**Files:**
- Potential docs: `src/ANTHROPIC_API_REQUIREMENTS.md` if created/updated.

- [ ] **Step 1: Format and build**
  - Run: `opam exec --switch=clawq-5.1 -- dune build --root . -j1 @fmt --auto-promote`
  - Run: `opam exec --switch=clawq-5.1 -- dune build --root . -j1 src/main.exe`

- [ ] **Step 2: Focused tests**
  - Run provider/tool suites touched by tasks 1-5.

- [ ] **Step 3: Full quick suite**
  - Run: `make test` from the worktree, or equivalent non-concurrent dune command if Makefile changes cwd correctly.

- [ ] **Step 4: Code review**
  - Request a review subagent with base `ad14af1d` and current HEAD.
  - Fix Critical/Important findings or document why they are invalid.

- [ ] **Step 5: Merge/cleanup handoff**
  - Confirm main checkout dirty file remains untouched.
  - Report commits, tests, known deferrals, and whether worktree should be merged/removed.

--- SUMMARY ---

- Fix the highest-confidence production bugs first: Codex streaming/replay, generic OpenAI raw payloads, executor validation/classification, Anthropic empty args/history, MiniMax raw preservation, and recursive redaction.
- Use TDD per task: write failing tests, verify red, implement, verify green, commit.
- Defer larger schema migration for preserving assistant text alongside tool calls unless a small safe path appears.
- Final verification requires formatting, build, focused tests, quick suite, and code review before merge.