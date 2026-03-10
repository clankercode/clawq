# Planning Mode — Phase 2 Deferred Features

Date: 2026-03-10
Relates to: I013 (merged in Phase 1)

## 1. Interactive Planner ↔ Main-Agent Back-and-Forth

### What
Add `clawq plan interact <id> <message>` that injects a question/feedback into
the planner agent's active session. The planner can then refine the plan and
the main agent receives the updated plan.md.

### How
- Planner stage creates a session (`session_key = "plan-pipeline-{id}-planner"`)
  instead of fire-and-forget bg task.
- `plan start` spawns the planner via `Session.turn` (not `Background_task.enqueue`)
  so the session stays live and resumable.
- `plan interact <id> <message>` calls `session inject plan-pipeline-{id}-planner <message>`.
- Add `plan wait-for-plan <id>` to block until planner writes plan.md.
- Key files: `src/session.ml` (turn/inject), `src/command_bridge.ml` (1428-1501 session inject).

### Why deferred
Requires a persistent session per pipeline, changes the planner execution model
from bg-task to session-based, and adds a new UX workflow. Significant scope.

---

## 2. Async / Daemon-Managed Pipelines

### What
Add `clawq plan start --detach` to fire-and-forget a pipeline. The daemon picks
up queued pipelines and advances their stages. `clawq plan status <id>` polls DB.

### How
- Add a daemon polling loop (alongside the bg task poll) that queries
  `plan_pipelines WHERE status = 'running'` and advances the current stage.
- `Plan_pipeline.advance_one_stage ~db ~pipeline ~runner` does a single
  enqueue+wait cycle and updates DB.
- Daemon calls this from its tick loop (similar to Scheduler.tick).
- Key files: `src/daemon.ml` (tick loop), `src/plan_pipeline.ml` (advance_one_stage).

### Why deferred
MVP foreground mode is sufficient for developer use. Daemon integration needs
careful drain/restart handling and adds complexity to daemon.ml.

---

## 3. Default Cheap-Model Configuration

### What
Pre-wire sensible model defaults for pipeline stages:
- Planner: a cheaper/faster model (e.g. gpt-5.3-codex-spark if available)
- Plan reviewer: mid-tier model
- Coder: strong model
- Code reviewer: strong model

### How
- Add `plan_pipeline_model_config` to `runtime_config.ml` with `planner_model`,
  `reviewer_model`, `coder_model` fields (all `string option`).
- `Plan_pipeline.default_model_config` reads from runtime config then falls back
  to `None` (provider default).
- Users configure via config JSON or `--planner-model` flag.
- Key files: `src/runtime_config.ml`, `src/plan_pipeline.ml`.

### Why deferred
No hardcoded model names; model availability depends on user's provider setup.
Can be added once the base pipeline is proven stable.
