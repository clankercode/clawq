# Pi Resume Handoff: P14.M1.E2.T003

Audience: a direct `pi` agent resuming this Clawq workflow.

Do not use `ccc` for new Pi/Mimo implementation lanes. If further implementation is needed, run `pi` directly. Review-and-fix lanes should still use an openai-codex/gpt-5.5 reviewer/fixer and must not delegate further.

## Current Stop Point

- Repo: `/home/xertrov/src/clawq`
- Current branch: `master`
- Backlog task: `P14.M1.E2.T003 Refresh derived policy on config reload`
- Backlog status: `in_progress`
- Backlog claim: `workflow-p14-m1-e2-t003-review-fix`
- Working tree on `master`: clean at handoff time
- Do not mark `bl done P14.M1.E2.T003` until the repair branch is merged, verified, and reviewed after merge.

The workflow was intentionally paused here per user request so future implementation can use direct `pi`.

## Important User/Repo Constraints

- Project casing is `Clawq` or `clawq`, never `ClawQ`.
- Backlog lives in `.backlog`; older `.tasks` references are stale.
- Must `bl claim` before assigning work to an agent; this task is already claimed.
- Must `bl done` only after subagent/worker work is merged and verification passes.
- Agents should use worktrees for implementation and cleanup after merge.
- Follow PIRFL: plan, implement, review, fix, verify.
- Never run more than one Dune command at a time in this repo.
- If Dune appears stuck or memory-heavy, check for a dependency cycle before continuing.

## Completed Planning/Backlog Context

The Claude Tag parity bundle has already been inventoried and ingested:

- Inventory: `docs/plans/2026-06-28-claude-tag-parity-bundle-inventory.md`
- Ultra plan tree: `docs/ultra-plans/claude-tag-parity-next/`
- P14-P18 backlog phases exist under `.backlog`
- `bl check --strict` passed during ingest

P14.M1.E2.T001 and T002 were previously completed. T003 was completed once, then reopened because an epic review found a real reload-boundary bug.

## Relevant Runs

Initial T003 implementation:

- Run: `wf-20260628T121500Z-p14-m1-e2-t003-config-reload-derived-policy-861934`
- Branch: `workflow/p14-m1-e2-t003-config-reload-policy`
- Merged into `master`
- Main merge commit: `155d6fdb Merge branch 'workflow/p14-m1-e2-t003-config-reload-policy'`

Epic review that found blocker:

- Run: `wf-20260628T131701Z-p14-m1-e2-epic-review-52ae57`
- Result: `FAIL`
- Finding: `Config_loader.load` swallowed malformed config into defaults before daemon reload rollback could preserve the last valid policy.

Repair implementation:

- Run: `wf-20260628T132227Z-p14-m1-e2-t003-loader-boundary-review-fix-e7c2a1`
- Current status: paused intentionally for direct-pi handoff
- Branch: `workflow/p14-m1-e2-t003-loader-boundary-fix`
- Worktree: `/home/xertrov/.agents/workflow-system/state/runs/wf-20260628T132227Z-p14-m1-e2-t003-loader-boundary-review-fix-e7c2a1/worktrees/p14-m1-e2-t003-loader-boundary-fix`
- Commit on branch: `603b423e fix: daemon reload preserves last valid config on malformed config.json`
- Worktree status at handoff: clean

Repair review:

- Run: `wf-20260628T134148Z-p14-m1-e2-t003-loader-boundary-codex-review-ce654f`
- Reviewer: openai-codex/gpt-5.5 via `@cx-reviewer`
- Result: `PASS`
- Non-blocking test gap: daemon test simulates the load-result branch rather than invoking the actual signal/watch callbacks.

## Repair Branch Summary

Commit `603b423e` changes:

- `src/config_loader.ml`
  - Adds `Config_loader.load_result : ?path:string -> unit -> (Runtime_config.t, string) result`
  - Keeps existing `Config_loader.load` fallback-to-default behavior for legacy/CLI callers.
- `src/daemon.ml`
  - SIGHUP reload path now calls `Config_loader.load_result`.
  - File-watch reload path now calls `Config_loader.load_result`.
  - On `Error`, daemon logs and preserves current config/policy.
- `src/daemon_util.ml`
  - Adds `apply_ec_watcher_toggle` helper moved out of `daemon.ml`.
- `test/test_config_loader.ml`
  - Adds `load_result` valid/malformed/missing/unreadable coverage.
- `test/test_daemon.ml`
  - Adds malformed config preservation regression coverage.

Line counts observed in the repair worktree:

- `src/daemon.ml`: 1960 lines, under the 2000 hard limit.
- `src/daemon_util.ml`: 1700 lines.
- `src/config_loader.ml`: 1638 lines.

## Known Dogfood Issues

- `ccc @pi-mimo25p` repeatedly produced correct commits but did not exit or write final artifacts. The implementation run was manually recovered and then paused.
- `workflow watch-emit --loop` is documented in the skill but this installed command rejected `--loop`.
- Workflow state sometimes fails to surface `latest_output` for `ccc @pi-mimo25p` even while the raw JSONL log is active.
- For future implementation lanes, use direct `pi` instead of `ccc @pi-mimo25p`.

## Resume Steps

1. Confirm no Dune command is running:

   ```bash
   ps -eo pid,ppid,pgid,stat,comm,args | rg 'dune|ocaml|make test|make build' || true
   ```

2. Confirm current repo and branch status:

   ```bash
   cd /home/xertrov/src/clawq
   git status --short --branch
   bl show P14.M1.E2.T003
   ```

3. Inspect the repair branch if needed:

   ```bash
   git -C /home/xertrov/.agents/workflow-system/state/runs/wf-20260628T132227Z-p14-m1-e2-t003-loader-boundary-review-fix-e7c2a1/worktrees/p14-m1-e2-t003-loader-boundary-fix show --stat --oneline HEAD
   ```

4. Merge the repair branch into `master`:

   ```bash
   git merge --no-ff workflow/p14-m1-e2-t003-loader-boundary-fix \
     -m "Merge branch 'workflow/p14-m1-e2-t003-loader-boundary-fix'"
   ```

5. Run verification serially from `/home/xertrov/src/clawq`:

   ```bash
   make test-run ARGS="test daemon 70-76"
   make test-run ARGS="test config_loader"
   make test-run ARGS="test command_bridge 104-105"
   make fmt-check
   make build
   make test
   git diff --check
   git status --short
   ```

6. Re-run a focused P14.M1.E2 epic review or equivalent lead review after merge. The earlier epic review failed; the loader-boundary Codex review passed, but a post-merge epic-level PASS is still the clean closeout.

7. Record workflow verification:

   ```bash
   workflow verify wf-20260628T132227Z-p14-m1-e2-t003-loader-boundary-review-fix-e7c2a1 \
     --record-only \
     --name post-merge-loader-boundary-verification \
     --kind test \
     --status passed \
     --summary "Post-merge T003 loader-boundary checks passed." \
     --evidence-path /tmp/clawq-p14-m1-e2-t003-loader-boundary-verification.md
   ```

   Create `/tmp/clawq-p14-m1-e2-t003-loader-boundary-verification.md` with the exact commands/results before running the command above.

8. Mark workflow runs done:

   ```bash
   workflow resume wf-20260628T132227Z-p14-m1-e2-t003-loader-boundary-review-fix-e7c2a1 \
     --reason "Resuming after direct-pi handoff verification"
   workflow done wf-20260628T132227Z-p14-m1-e2-t003-loader-boundary-review-fix-e7c2a1 \
     --message "Loader-boundary repair merged and verified"
   ```

9. Mark backlog done only after merge, verification, and review:

   ```bash
   bl done P14.M1.E2.T003
   ```

10. Cleanup after the merge and backlog close:

   ```bash
   git worktree remove /home/xertrov/.agents/workflow-system/state/runs/wf-20260628T132227Z-p14-m1-e2-t003-loader-boundary-review-fix-e7c2a1/worktrees/p14-m1-e2-t003-loader-boundary-fix
   git branch -d workflow/p14-m1-e2-t003-loader-boundary-fix
   ```

11. Commit any backlog closeout metadata if `bl done` does not auto-commit.

## Direct Pi Guidance For Future Work

For future implementation lanes after T003, do not launch `workflow apply` with `runner: ccc` and `ccc_runner: @pi-mimo25p`. Use direct `pi` or the workflow `pi-direct` runner after confirming it works.

If using direct `pi` manually, give it a bounded prompt with:

- task ID and already-claimed status,
- worktree branch/path,
- exact acceptance criteria,
- "Do not run `bl done`; the lead will close backlog after merge and verification",
- "Use an openai-codex/gpt-5.5 subagent for review-and-fix when review time arrives",
- required verification commands,
- "Do not run Dune concurrently".

Next backlog candidate after T003 closes appears to be:

- `P14.M1.E3.T001 Persist effective-access snapshots for executable work`

Claim it with `bl claim` before assigning it to any agent.
