# B383: task_tree Output Verbosity — Design Note

## Problem

The task_tree tool's output on mutation operations (add, update, remove, clear,
archive, restore, seed) is unnecessarily verbose. A batch of 7-10 operations
easily produces 1.5-2k+ tokens of tool result, consuming agent context budget
without adding proportional information value.

The output has two concatenated sections:
1. **Per-operation confirmation lines** — e.g. `Added 3: Implement auth [pending] (child of 1)\n`
2. **Compact summary** — the full `render_compact` output showing counts, active tasks, blocked tasks, next tasks, and archive nudge

These sections overlap significantly: the compact summary already communicates
the final state, making per-op confirmations largely redundant for ongoing work.
The existing LLM-based summarizer (`Summarizer.maybe_summarize`) fires when
output exceeds `threshold_chars` (default 1500), but this adds latency and cost
for every large task_tree mutation, and may not preserve task IDs reliably.

### Output anatomy (current)

For a seed of 7 tasks:
```
Added 1: Design API [pending]
Added 2: Implement endpoints [pending] (child of 1)
Added 3: Write tests [pending] (child of 1)
Added 4: Set up CI [pending]
Added 5: Deploy staging [pending] (child of 4)
Added 6: QA review [pending]
Added 7: Ship [pending]

Tasks: 7 total (7 pending)
Next:
  [ ] #1 — Design API
  [ ] #4 — Set up CI
  [ ] #6 — QA review
```

That's ~500 chars for 7 tasks, but with notes and deeper nesting it grows fast.
A realistic 15-task seed with notes: ~1200-1800 chars. After updating a few
statuses in the same batch, easily 2000+.

### System prompt injection (every turn)

The task tree is also injected into the system prompt every turn via
`render_focus` (session_core.ml:828). This is already compact and well-budgeted.
No change needed there.

## Root Causes

1. **Per-op confirmations are always emitted** — even for large batches where
   the compact summary contains all the information the agent needs.
2. **No per-output token budget** — `render_compact` and per-op lines are
   concatenated without any character/token cap on the combined result.
3. **The LLM summarizer is overkill** — task_tree output is structured data
   that can be mechanically budgeted without an LLM call.

## Proposed Solution: Built-in Output Budgeting

### Strategy

Add a deterministic output budget to `process_operations` that caps the tool
result at a configurable character limit (default: ~800 chars / ~200 tokens).
No LLM summarizer needed — the budgeting is mechanical and preserves all IDs.

### Concrete Changes

#### 1. Collapse per-op confirmations for large batches

**File:** `src/task_tree.ml`, `process_operations` (line 398+)

When the batch contains >3 operations, replace per-op confirmation lines with a
single summary line:

```
Applied 7 operations (5 add, 2 update). 0 errors.
```

Keep per-op lines for batches of 1-3 operations (small batches benefit from
explicit confirmation).

**Implementation sketch:**
```ocaml
(* After the operation loop, before appending compact summary *)
let op_summary =
  if n <= 3 then Buffer.contents results
  else
    let counts = (* count by op type from the ops list *) in
    Printf.sprintf "Applied %d operations (%s). 0 errors.\n"
      n (format_op_counts counts)
in
```

#### 2. Cap total output with a hard character budget

**File:** `src/task_tree.ml`, end of `process_operations`

After assembling `op_summary + compact_summary + warning`, truncate the compact
summary section if total output exceeds `max_output_chars` (default 800).

Truncation strategy for the compact summary:
- Always keep the `Tasks: N total (...)` counts line
- Always keep `Active:` section (these are the most important)
- Always keep `Blocked:` section
- Truncate `Next:` to 2 items (from 3) if over budget
- Drop the archive nudge line if still over budget

**Implementation sketch:**
```ocaml
let max_output_chars = 800 in
let output = op_summary ^ "\n" ^ summary ^ warning in
if String.length output <= max_output_chars then Ok output
else
  (* Use render_compact_budgeted with reduced limits *)
  let summary = render_compact_budgeted ~db ~session_key ~max_next:2
    ~include_archive_nudge:false in
  Ok (op_summary ^ "\n" ^ summary ^ warning)
```

#### 3. Add `render_compact_budgeted` variant

**File:** `src/task_tree_core.ml`

A variant of `render_compact` that accepts optional parameters:
- `~max_next` (default 3, can be reduced to 1-2)
- `~include_archive_nudge` (default true, can be disabled)
- `~max_note_chars` (default unlimited, can be capped to 40 chars)

This avoids duplicating the render logic while giving `process_operations` a
knob to stay within budget.

#### 4. Truncate notes in per-op confirmations

**File:** `src/task_tree.ml`, the update/add branches in `process_operations`

Notes in per-op confirmations are unbounded. Cap displayed notes to 60 chars
in the confirmation output (the full note is stored in the DB regardless).

### What NOT to Change

- **System prompt injection** (`render_focus` in session_core.ml) — already
  compact and well-structured. No change needed.
- **Notification output** (`format_notification`) — already event-driven and
  concise. No change needed.
- **The summarizer** — task_tree output should be mechanically budgeted before
  reaching the summarizer threshold. The summarizer remains as a fallback for
  edge cases.

## Acceptance Criteria

1. A seed/batch of 10 tasks produces a tool result under 800 characters.
2. A batch of 1-3 operations still shows per-operation confirmation lines.
3. Active and blocked tasks are always shown in the output (never truncated).
4. Task IDs are always preserved in the output.
5. Notes displayed in tool output are capped at 60 chars.
6. The `render_focus` system prompt output is unchanged.
7. Existing tests pass; new tests cover the batched-summary and budget behavior.
8. No LLM summarizer call is needed for typical task_tree operations.

## Token Budget Analysis

Current worst case (15-task seed with notes): ~1800 chars / ~450 tokens
Proposed worst case (same seed): ~400 chars / ~100 tokens

Breakdown of proposed output:
```
Applied 15 operations (15 add). 0 errors.

Tasks: 15 total (15 pending)
Next:
  [ ] #1 — Design API
  [ ] #8 — Set up CI
  (+4 more)
```
~180 chars / ~45 tokens. Well within budget.

## File Map

| File | Change |
|------|--------|
| `src/task_tree.ml` | Collapse per-op confirmations for batches >3; cap output |
| `src/task_tree_core.ml` | Add `render_compact_budgeted` with max_next/nudge params |
| `test/test_task_tree.ml` | Tests for batched summary and budget cap |

## Risks

- Agents that parse per-op confirmation lines may need adjustment. Mitigated:
  the compact summary always includes task IDs, and small batches still show
  per-op lines.
- The 800-char default may be too aggressive for some workflows. The budget
  should be configurable via `runtime_config` if needed (future work).

## Not In Scope

- Changing the system prompt task tree injection (already compact via `render_focus`)
- Adding pagination/scrolling to task tree output
- Modifying the summarizer behavior for other tools
