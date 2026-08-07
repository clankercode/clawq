# Handoff: Clawq GitHub default-branch push summaries (SDD, paused at Task 12)

**Date:** 2026-07-24
**Repo:** `/home/xertrov/src/clawq` (OCaml 5.1, opam + dune)
**Worktree:** `/home/xertrov/src/clawq/.worktrees/github-push-summaries-design`
**Branch:** `design/github-push-summaries`
**HEAD at handoff:** `d1a1a89e` (clean working tree; verified)
**Main checkout:** `master` at `9ae410c5` (clean)

## What this work is

User asked (via `/pirfl`) for: subscribe a channel to GitHub pushes on a repo's default branch; on new commits, wait a 5-minute quiet period (reset on each new push), then post one LLM summary of all commits since the last delivered range to the user-selected channel. User explicitly chose the **principled approach**: finish Clawq's single canonical GitHub App ingress → route → journal/projection → intent/outbox → durable Room delivery runtime and retire the legacy parallel `Github_pr_dispatch` path — no feature-specific coordinator.

## Key artifacts (do not re-derive; read these)

- **Approved design spec:** `docs/superpowers/specs/2026-07-22-github-default-branch-push-summaries-design.md` (commit `9f17c021`)
- **20-task implementation plan:** `docs/superpowers/plans/2026-07-22-github-default-branch-push-summaries.md` (commit `e0b31997`)
- **Progress ledger (source of truth for task state):** `.superpowers/sdd/progress.md` (gitignored)
- **Per-task briefs / reports / review packages / review findings:** `.superpowers/sdd/task-N-*.md`, `review-<base>..<head>.diff`
- **Repo instructions:** `/home/xertrov/src/clawq/AGENTS.md`, plus `src/CLAUDE.md`, `test/CLAUDE.md`, `docs/CLAUDE.md`
- **User-level instructions:** `/home/xertrov/AGENTS.md` (worktree flow, single-dune rule, subagent model conventions, lingo: "tac" = thanks and continue, "pfi" = please fold in)

## Status: 11 of 20 plan tasks complete; paused before Task 12

All completed tasks passed spec + quality review gates (some after fix loops — see per-task reports):

| Task | Commit(s) | Deliverable |
|---|---|---|
| 1 | `af0af9f7`, `9343fc5d` | Replayable accepted-event inbox (1 MiB cap, atomic dedupe, 10-attempt ceiling, fail-closed) |
| 2 | `56daae51` | Shared App HTTP path `/github/app/webhook`; `push`/`repository` allowlist |
| 3 | `f2973026`, `c76db9a1`, `8617fa8d` | Tagged accepted events + item processor; atomic per-delivery projection markers; stable intent ids |
| 4 | `34368e58`, `018192b4` | Plain-message intents, target snapshots, pending-only atomic supersede by route revision |
| 5 | `17d57460`, `e5b8da22` | Room destination resolver/sender; exact Telegram `account_ref` send (no first-account fallback) |
| 6 | `fe1f2401` | Unified daemon worker (repair → inbox → summary no-op → outbox send) |
| 7 | `4b7a6325`, `2ecfaa21` | Repository event envelope/journal; strict codec version gating |
| 8 | `344afbeb` | Route `delivery_policy` (default_branch_summary, 300s); repository match + capability accept PK |
| 9 | `f38154b8`, `c4257097` | Push-summary state machine (quiet period, claim, persist+enqueue tx, exact ack, candidates preserved, revision cancel) |
| 10 | `b3f8d35f` | Repository events wired into processor (journal → accept+reconcile → observe) |
| 11 | `2d784cb5`, `d1a1a89e` | Typed metadata/compare APIs; 250-commit hard cap; header-aware Retry-After |

**Pause reason:** Task 12 (no-tools completion helper + Room model resolver) was never started — `xai-oauth/grok-4.5` returned HTTP 402 (usage balance exhausted) and the user chose **"pause"**. Do not switch models silently; user preference was to wait. If the user now approves a fallback, candidates offered were Grok 4.20 Reasoning or session model (GPT-5.6 Sol).

## Remaining plan tasks (12–20)

12: no-tools completion + Room model resolver → 13: summary generation/persist+enqueue tx → 14: force-push/branch-change/blocked flows → 15: `github watch|unwatch|watches` (room-native, plan/digest/consent) + minimal stubs → 16: legacy webhook adapter → 17: legacy migration/parity/cutover (delete live `Github_pr_dispatch`) → 18: crash-after-ack + push E2E → 19: schemas/docs/ADR/llms-full/agent prompts → 20: full verification (`make test`, `make fmt-check`, optimised build).

## Additional requirement folded in (T30)

One Room (specifically **Microsoft Teams** rooms) must hold **multiple independent repo watches** — e.g. one room for infrastructure/internal repos, another for node/client software. Acceptance: many distinct Repo routes per Room (uniqueness is per destination+selector, so this already fits), isolated cursors/debounce/retries per repo, `watch`/`unwatch`/`watches` manage each repo separately, Teams E2E coverage proving shared-room delivery without cross-repo state mixing. Wired into plan tasks 15 (T24) and 18 (T27) plus task T30 in the live task list.

## How to resume

1. Read `.superpowers/sdd/progress.md` first; confirm HEAD matches ledger (`git log --oneline` in the worktree).
2. Extract next brief: `<skill-dir>/scripts/task-brief docs/superpowers/plans/2026-07-22-github-default-branch-push-summaries.md 12`.
3. Dispatch a fresh implementer subagent per task with the brief path + report path; record base commit; then `scripts/review-package BASE HEAD` and dispatch a task reviewer; fix loop until spec PASS + quality APPROVED; append one ledger line per completed task.
4. Subagent model: **grok-4.5 by user instruction** — verify it is replenished, or ask the user before falling back.
5. Final step: whole-branch review, `make test`, `make fmt-check`, one optimised build, merge to master, safe worktree cleanup (check for untracked scratch before removal).

## Hard constraints (from AGENTS.md / plan Global Constraints)

- Never run concurrent dune; always `-j1`; use `make test-run ARGS="test <SUITE>"` or `opam exec --switch=clawq-5.1 -- dune exec --root . -j1 test/test_main.exe -- test <suite>`.
- File limits: soft 1000 LoC, hard 2000 (github_api.ml and github_push_summary.ml already over soft limit — hygiene only).
- Runtime split: pure types/state/codec in `clawq_runtime_core`; HTTP/API/LLM/worker/Room-send in `clawq_runtime_integrations`; `command_bridge_min.ml` stubs must say disabled.
- TDD every task (red → green → commit); no real network/LLM in tests (inject `?now`, `?http_get`, provider seams).
- Credentials: App mode authoritative, no silent PAT fallback, no ambient user-token lease, secrets never in snapshots/logs.
- Delivery honesty: at-least-once only; quiet period is 300.0s product constant; cursor advances only to delivered `range_end_sha` after confirmed send.
- Subagents must never use `attn`; only the coordinator may.

## Suggested skills for the next session

- **`subagent-driven-development`** (required) — the execution loop already in use; brief/report/review-package scripts under `/home/xertrov/.pi/agent/git/github.com/obra/superpowers/skills/subagent-driven-development/`
- **`using-git-worktrees`** — worktree already exists; just verify and resume (do not nest)
- **`pirfl`** — the overarching plan→implement→review→fix loop framing
- **`tdd`** — per-task red/green discipline
- **`review-and-fix`** — per-task and final whole-branch gates
- **`writing-plans`** — only if the plan needs amendment (e.g. T30 multi-repo tests need plan edits)
- **`persist-before-compact`** — session may compact; keep the ledger current instead

## Known non-blocking notes (from reviews)

- Item intents lack `target_snapshot` (live resolve at send; acceptable).
- Default Telegram send is `Accepted_unconfirmed` (no external id).
- v1 Telegram account pin = first valid account at enqueue (documented).
- Worker checks route enabled, not exact revision equality, pre-send (supersede-by-revision is the cancel path).
- `github_route_store` delivery-policy digests: old plans without the field default on apply (safe).
- Missing-baseline repository events soft-noop + Processed (by design; Task 15 arms baseline atomically).
