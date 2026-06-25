---
name: run-task
description: Runs a backlog item (bug, feature, task, epic, milestone, phase) end-to-end by gathering context, constructing a startup prompt, spawning a worktree-backed subagent, and managing the claim/done/review cycle. Use when the user wants to execute a backlog target like B123, F456, P2.M4.E3, etc.
argument-hint: <TARGET> [extra steering]
---

# run-task

Execute backlog target `$ARGUMENTS` end-to-end: gather context, build a startup prompt, spawn a worktree-backed subagent, and manage the claim/done/review cycle.

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
- Branch names should visibly contain the target ID (e.g. `clawq-bg-B467`).
