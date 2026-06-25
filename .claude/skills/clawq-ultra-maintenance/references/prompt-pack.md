# Prompt pack — clawq-ultra-maintenance

Reusable subagent prompt templates. Substitute `{{...}}` placeholders. Every dispatch carries the **boilerplate**
first, then its role-specific body. Use opus for all subagents (per repo convention).

## Boilerplate (prepend to EVERY dispatch)

```
You are a subagent on a clawq maintenance run (run-id {{RUN_ID}}). Working dir: {{WORKTREE_OR_REPO}}.
Report by WRITING your output file — do not message the coordinator with results.
Output file: {{OUT_PATH}}. When fully finished, the LAST line of that file MUST be exactly: === DONE ===
NEVER use the `attn` utility. If blocked, write the blocker into your output file and still emit === DONE ===.
Dune rule: never run two `dune` commands at once in one checkout; run only the specific test suites your work touches
via `make test-run ARGS="test <regex>"`; never run `make test-all`.
```

## Finder (P2)

```
Scope: review ONLY these files for issues — {{FILE_LIST}}.
Hunt for BOTH:
  • bugs/correctness — logic errors, unhandled cases, race/lifetime issues, resource leaks, incorrect error handling,
    config/state desync, security gaps.
  • maintainability — duplication, weak abstractions, inconsistent conventions, oversized files (>1000 LoC soft,
    >2000 hard), tangled responsibilities, dead code, missing/over-broad types.
Investigate usage and neighbours: how each symbol is used, similar/duplicated logic elsewhere, adjacent tests.
Append each finding to {{OUT_PATH}} as ONE line:
  `path:line · bug|maint · H|M|L · <one-sentence problem> · <suggested direction>`
Only legitimate, actionable findings — no style nits the formatter already handles. End with === DONE ===.
```

## Triage (P3, optional — only if findings are large)

```
Read every file in {{FINDINGS_DIR}}. Produce {{OUT_PATH}} = backlog.md:
  1. Drop false-positives, duplicates, and low-value noise (note count dropped).
  2. Keep legitimate bug + maintainability items; merge duplicates referencing the same root cause.
  3. Organize survivors into mostly-DISJOINT conceptual groups (by subsystem/concern) so each group is a batch that
     can be fixed without colliding with another group. Name groups in kebab-case.
Format each item: `- [ ] path:line · bug|maint · H|M|L · <description>` under a `## group: <name>` heading.
End with === DONE ===.
```

## Fixer (P5)

```
Group: {{GROUP}}. Plan: {{PLAN_PATH}}. Master backlog: {{BACKLOG_PATH}}.
Steps (stop and write the blocker if any step is impossible):
  1. Claim: run `mkdir {{CLAIMS_DIR}}/{{GROUP}}.lock`. If it FAILS, the group is already claimed — write "already
     claimed, skipping" to {{OUT_PATH}}, emit === DONE ===, and stop.
  2. Worktree: `git worktree add .worktrees/maint-{{GROUP}} -b maint-{{RUN_ID}}-{{GROUP}}` and work there.
  3. Implement every item in the plan. Preserve behaviour unless an item explicitly requires a semantic change.
     Split files over the size limit. Match existing OCaml style; run `make fmt` if you touched formatting.
  4. Test: run ONLY the suites named in the plan. Then `make test` if the change is broad.
  5. Commit at convenient intervals with clear messages.
  6. Self-review: run the `review-and-fix` skill over your diff (use `pirfl` for hard/multi-step items). Fix what it
     finds.
  7. Do NOT merge. Write {{OUT_PATH}}: items addressed, files changed, suites run + result, branch name. End with
     === DONE ===.
```

## Gate / re-fix (P6)

External reviewer is the coordinator's job via `ccc-review-cx` — not a subagent. If a gate FAILs, dispatch a fixer
with this body in the existing worktree:

```
Group {{GROUP}} failed external review. Findings: {{REVIEW_FINDINGS}}.
Address every finding in worktree .worktrees/maint-{{GROUP}} (branch maint-{{RUN_ID}}-{{GROUP}}). Re-run the named
test suites. Commit. Append a "re-fix" section to {{OUT_PATH}} and end with === DONE ===.
```

## Coordinator-side prompts (you, not subagents)

- **Invocation (concise):** "Run clawq-ultra-maintenance over {{SCOPE}}; resume if an incomplete run exists."
- **Clarification (only if blocking):** ask exactly one targeted question with a recommended default — e.g. scope
  ambiguity ("whole repo, or just `src/`? default: whole repo") — otherwise proceed on defaults.
- **Validation/report (per phase):** "List each dispatched unit, whether its output file exists and ends with
  `=== DONE ===`, and which were re-dispatched. Then state the phase status written to the ledger."
