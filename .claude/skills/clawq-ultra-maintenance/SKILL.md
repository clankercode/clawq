---
name: clawq-ultra-maintenance
description: Comprehensive, file-coordinated maintenance sweep of clawq — fan-out subagents find bugs AND refactoring opportunities, triage into a grouped master backlog, then fix each group in an isolated worktree behind a review+codex gate, auto-merging to master. Use for a thorough "ultra" maintenance pass over the codebase (or an optional focus path). Resumable: all state lives on disk under .maint/<run-id>/.
---

# clawq-ultra-maintenance

A maintenance workflow built by hand out of files + subagents — **no Workflow tool required**. A gitignored
`.maint/<run-id>/` directory is the journal: it holds all state so the run is **resumable**, and each phase reads
the prior phase's files (handoff) while a claims dir gives fixers mutual exclusion (sync).

You are the **coordinator** (this main session). You drive phase transitions and maintain the ledger; subagents do
the per-unit work and report **by writing files**, never by messaging you.

**Core principle:** parallelism = several subagent dispatches in one message (they return when all finish). Files are
durable state + handoff + inter-agent sync — *not* a polling channel. Treat a missing/incomplete output file exactly
as a workflow treats a `null` agent result: re-dispatch that unit; never advance a phase on partial output.

## Inputs

- **`focus-path`** (optional arg): restrict scope to a path/component (e.g. `src/discord.ml`, `src/`). Default: whole
  `src/` + `test/`.
- On invoke, detect resume: if the newest `.maint/*/ledger.json` has `phase != "done"` and a matching scope, **resume
  it** (start at the first incomplete phase). Otherwise mint a fresh `run_id` = `date +%Y%m%d-%H%M%S`.

## State layout

```
.maint/<run-id>/
  ledger.json          # phase + per-phase status + run config; the resume anchor (you maintain it with Write/Edit)
  partition.md         # P1: every target file assigned to exactly one finder bucket
  findings/<finder>.md # P2: append-only; entries = file:line · bug|maint · sev(H/M/L) · desc · suggested direction
  backlog.md           # P3: deduped master list, grouped into mostly-disjoint conceptual areas
  groups/<g>.plan.md   # P4: concrete fix plan per group (what / where / why / which test suites)
  claims/<g>.lock/     # P5: atomic claim dir (mkdir) → one fixer per group; survives resume
  reports/<g>.md       # P5/6: files changed, tests run, gate result, ending with `=== DONE ===`
  LOG.md               # your human-readable narrator log of phase transitions and any dropped/skipped items
```

**DONE marker:** every finder/fixer output file MUST end with a line `=== DONE ===`. A file lacking it = incomplete =
re-dispatch.

**ledger.json shape:**
```json
{ "run_id": "...", "scope": "src/ test/", "mode": "both", "concurrency": 3,
  "phase": "find",
  "phases": {"init":"done","partition":"done","find":"in_progress","triage":"todo","plan":"todo","fix":"todo","gate":"todo","final":"todo"},
  "finders": ["..."], "groups": ["..."] }
```

## Workflow

Run phases in order. After each, set its status `done` in `ledger.json` and advance `phase`. Append a one-line entry
to `LOG.md` at every transition.

### P0 — Init
- Create `.maint/<run-id>/` and subdirs (`findings/ groups/ claims/ reports/`).
- Ensure `.maint/` is gitignored (it is NOT by default) — append `.maint/` to `.gitignore` and `.dockerignore` if
  absent. (`.worktrees/` is already ignored.)
- Write the initial `ledger.json`.

### P1 — Survey & partition
- List target files (`*.ml`/`*.mli` under scope, plus `test/`). Bucket them by component/directory so **coverage is
  complete** — every file in exactly one bucket. Scale finder count to size (~1 finder per 15–25 files or per
  cohesive component; sensible cap ~8–10).
- Write `partition.md` (bucket → file list) and record `finders` in the ledger.

### P2 — Find (parallel)
- Dispatch one subagent per bucket **in a single message**. Each gets the finder prompt (see
  `references/prompt-pack.md` → *Finder*), its file list, and its output path `findings/<finder>.md`.
- **Validate on return:** every dispatched finder's file exists and ends with `=== DONE ===`. Re-dispatch any
  missing/incomplete before advancing. Log any finder that produced nothing.

### P3 — Triage & group
- Read all `findings/*.md`. Dedupe, drop false-positives and low-value noise, keep legitimate bug + maintainability
  items. (Large finding set → dispatch one triage subagent with the *Triage* prompt; otherwise do it yourself.)
- Write `backlog.md`: master list, each item `file:line · bug|maint · sev · description`, organized into **mostly
  disjoint conceptual groups** (so groups can be fixed as independent batches without colliding). Record `groups`.

### P4 — Plan per group
- For each group write `groups/<g>.plan.md`: concrete plan — what changes, where (file:line), why, and the **exact
  test suites** to run (`make test-run ARGS="test <regex>"`). Respect file-size limits (split >1000 LoC files).
- This is the handoff that informs P5.

### P5 — Fix (parallel, isolated, capped)
- Dispatch fixers in **waves of `concurrency` (default 3)** — system-load norm; each fixer builds/tests in its own
  worktree. Give each the *Fixer* prompt, the group plan path, and `backlog.md`.
- Each fixer: `mkdir .maint/<run-id>/claims/<g>.lock` (atomic — if it fails, the group is already claimed → skip) →
  create worktree `.worktrees/maint-<g>` on branch `maint-<run-id>-<g>` → implement the plan → run **only the named
  test suites** (never `make test-all`; never run `dune` in parallel within a worktree) → commit at intervals → run
  `review-and-fix` over its own diff (use `pirfl` for hard/multi-step items) → write `reports/<g>.md` ending
  `=== DONE ===`. The fixer does **not** merge.
- Validate each wave's reports (DONE marker) before the next wave.

### P6 — Gate + merge (sequential per group)
- Merges must be serialized (one branch into master at a time). For each group with a DONE report:
  1. Run an external review: invoke `ccc-review-cx` over the group's diff + `groups/<g>.plan.md` + relevant
     `backlog.md` rows. (If `ccc-review-cx` is unavailable, fall back to a second `review-and-fix` pass with the
     reviewer model `openai-codex:gpt-5.5`; if no external reviewer at all, a self `review-and-fix` pass.)
  2. **PASS** → merge `maint-<run-id>-<g>` into master, then remove the worktree (first inspect
     `git status --porcelain --ignored` and rescue any untracked/gitignored scratch — never blind `rm -rf`/`--force`).
  3. **FAIL** → dispatch a follow-up fixer to address the review findings in the same worktree, then re-gate.
- Append the gate result to `reports/<g>.md` and mark the group merged in the ledger.

### P7 — Final pass
- Run `review-and-fix` over the whole merged diff. Run a `ccc-review-cx` review of `backlog.md` for **completeness**
  (was every legitimate item actually fixed?). Fix gaps via a fixer agent; re-gate.
- When the review PASSes, run `/carm` to commit-all + rebase master. Write a final summary to `LOG.md` and set
  `phase: done`.

## Cross-cutting rules

- **Resumability:** on every invoke, read the ledger and resume at the first incomplete phase. Claimed/merged groups
  are skipped (claims dir + ledger). Persist artifacts to disk *as each unit finishes*, not only at the end.
- **Validate, never silently truncate:** missing output / absent `=== DONE ===` ⇒ re-dispatch. If you intentionally
  drop or defer anything (a finding, a group, a file), record it in `LOG.md` — no silent gaps.
- **Subagents must NEVER use `attn`.** Put this line in every dispatch prompt. Agents report by writing their file.
- **Dune/tests:** never two `dune` commands at once in the same checkout; run only the focused suites a change
  touches; never `make test-all` in a subagent; if you must run broad tests, capture output to a temp file.
- **Dependency floor:** the orchestration needs only bash + subagent dispatch. `review-and-fix`, `pirfl`,
  `ccc-review-cx`, `carm` are invoked by agents and **degrade gracefully** if absent (fall back to self-review).

## Agent prompts

Verbatim, reusable prompt templates (Finder / Triage / Fixer / Gate, plus the boilerplate every dispatch must carry)
live in [`references/prompt-pack.md`](references/prompt-pack.md). Design rationale and constraints are in
[`references/methodology.md`](references/methodology.md).

Every dispatch prompt MUST include: the output file path + `=== DONE ===` contract; "report by writing your file, not
by messaging"; "**never use `attn`**"; and the relevant dune/test rules above.

## Done checklist

- [ ] `.maint/` + `.worktrees/` gitignored; ledger reaches `phase: done`.
- [ ] Every target file covered by exactly one finder bucket (P1) and every finder validated (DONE).
- [ ] `backlog.md` deduped and grouped; every legitimate item lands in a group.
- [ ] Each group fixed in its own worktree, focused tests green, self-reviewed, gated (external review PASS), merged,
      worktree removed.
- [ ] Final `review-and-fix` + completeness review PASS; `/carm` run; summary in `LOG.md`.
