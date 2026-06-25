# Methodology — clawq-ultra-maintenance

Design record for maintainers. Why this skill is shaped the way it is.

## Mission & non-goals

- **Mission:** one comprehensive maintenance sweep that finds *both* bugs and refactoring/maintainability issues
  across clawq, fixes them in isolation, and lands them on master behind a review gate — producing better, more
  maintainable code.
- **Non-goals:** feature work; performance tuning campaigns; anything that requires a dedicated Workflow tool. The
  orchestration is deliberately hand-rolled from bash + subagents so it runs in any harness.

## Requirements & clarifications (from the user)

- More concise and more instructive than `clawq-code-maintenance` (#1, refactor hunt) and
  `clawq-code-maintenance-2` (#2, bug hunt); goal is better, more maintainable code.
- **Inspired by workflow mechanics:** use files to store state, synchronize agents, and inform later phases.
- **Dependency floor:** assume only foundational utility skills + bash + a subagent utility. No dedicated Workflow
  tool. (Clarified: full toolbox — `review-and-fix`, `pirfl`, `ccc-review-cx`, `carm` — may be invoked *by agents*,
  but must degrade gracefully if absent.)
- Scope: **both** maintainability + bugs. Coverage: **whole-repo partition** with an optional focus-path arg.
- Coordination: **coordinator-driven ledger** (not a self-organizing queue).
- State: gitignored in-repo `.maint/<run-id>/`; parallel fixes isolated in `.worktrees/`.
- Closing gate: review (self + external/codex) then **auto-merge** to master.

## Repo constraints honored

- `.worktrees/` already gitignored; `.maint/` is not → P0 adds it to `.gitignore`/`.dockerignore`.
- Never run `dune` concurrently in one checkout (lock on `_build`); separate worktrees have separate `_build`.
- Run focused test suites via `make test-run ARGS="test <regex>"`; avoid `make test-all` (huge); capture to temp file
  if broad tests are unavoidable.
- File-size limits: 1000 LoC soft / 2000 hard — split proactively.
- Worktree cleanup: inspect `git status --porcelain --ignored` and rescue scratch before removal; never blind
  `rm -rf`/`--force` (global CLAUDE.md norm).
- Keep app/config state in sync; preserve behaviour unless a change is explicitly semantic.
- Subagents must never use `attn` (global CLAUDE.md); only the coordinator talks to the user.

## Design rationale & trade-offs

- **Files as the journal.** `.maint/<run-id>/` is the single source of truth, mirroring a workflow's resume journal.
  Each phase's output file is the next phase's input ("inform future parts"); the ledger records phase status
  ("store state"); the `claims/` dir gives fixers mutual exclusion ("synchronize between agents").
- **Coordinator-driven over self-organizing queue.** A self-organizing claim-queue is more autonomous but has more
  failure modes (lost claims, stuck items, harder resume). Coordinator-driven fan-out — dispatch a batch, wait, read
  result files, validate DONE markers — is simpler and maps cleanly onto "several Agent dispatches in one message
  return together." The only place agents self-coordinate is the atomic `mkdir` claim in P5, which is enough.
- **DONE marker = file-based `.filter(Boolean)`.** The CLAUDE.md workflow norm "always validate agent output" has no
  `null` here; the analog is an absent/short output file. A required `=== DONE ===` sentinel makes incompleteness
  detectable and re-dispatchable.
- **Waves, not pipelining, in P5/P6.** True pipelining (merge group A while fixing group B) needs async the harness
  doesn't give a hand-rolled orchestrator. Waves of N fixers (parallel) + sequential gate/merge is reliable, avoids
  concurrent-merge conflicts, and respects the system-load norm. Cost: some wall-clock idle at wave boundaries —
  acceptable for a maintenance sweep.
- **External gate optional.** `ccc-review-cx` (codex/gpt-5.5) is the preferred second opinion per CLAUDE.md, but the
  skill falls back to `review-and-fix` so the dependency floor holds.
- **Departure from #2.** Replaces the `sleep-20-x-45` + c2c merge-approval choreography with file handoff + DONE
  validation + ledger resume. The coordinator decides merges directly; no inter-agent approval dance.

## Failure taxonomy & fallback

| Failure | Detection | Response |
|---|---|---|
| Finder produced nothing / no DONE | validate after P2 | re-dispatch; if still empty, log in `LOG.md` and continue |
| Two fixers target one group | `mkdir` claim fails | loser writes "already claimed", emits DONE, stops |
| Fixer's focused tests fail | fixer reports failure | fixer fixes within worktree; if blocked, writes blocker → coordinator re-plans |
| External gate FAIL | `ccc-review-cx` verdict | dispatch re-fix agent in same worktree, re-gate |
| `ccc-review-cx` unavailable | skill missing | fall back to `review-and-fix` with `openai-codex:gpt-5.5`, then self-review |
| Run interrupted mid-phase | ledger `phase != done` | re-invoke resumes at first incomplete phase; claimed/merged groups skipped |
| Worktree won't remove | `git worktree remove` errors | inspect `--ignored`, rescue scratch, then force-remove (never before the check) |

## Evaluation plan (testing the skill itself)

- **Dry partition:** invoke with a small focus path (e.g. one module) — confirm P1 covers every file exactly once and
  the ledger/state dir is created and gitignored.
- **Resume:** kill the run after P2; re-invoke — confirm it resumes at P3, not P0, and re-uses existing findings.
- **Claim race:** point two fixers at one group — confirm exactly one proceeds.
- **Gate FAIL path:** feed a deliberately weak fix — confirm re-fix loop triggers and merge is withheld until PASS.
- **Adversarial inputs:** empty scope / nonexistent focus path → coordinator asks one targeted clarification with a
  default; risky request (e.g. "skip tests") → refuse, keep the focused-test rule.
