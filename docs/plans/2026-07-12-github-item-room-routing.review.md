# GitHub Item Routing Plan Ingest Review Receipt (Historical Snapshot)

Date: 2026-07-12
Result: HISTORICAL PASS — superseded

> This receipt records the original P19/P20 ingest at its 2026-07-12 review
> boundary (50 P19 tasks, 13 P20 tasks). It is not a current inventory or a
> review of the later P19-P22 phase seam. The current integrated review and
> repaired totals are recorded in
> `docs/plans/2026-07-13-p19-p22-integrated-review.md`.

## Reviewed artifacts

- `docs/plans/2026-07-12-github-item-room-routing.md`
- `docs/adr/0002-use-unified-live-github-app-routes.md`
- `docs/adr/0003-require-plan-confirm-apply-for-agent-setup.md`
- `docs/adr/0004-room-session-owns-github-event-context.md`
- `CONTEXT.md`
- Backlog phases P19 and P20, including every task body and dependency
- Ideas I060, I061, and I062
- Existing P16-P18 foundations referenced by the new work

## Ingest result

- P19: 4 milestones, 12 epics, 50 tasks, 212 hours.
- P20: 2 milestones, 4 epics, 13 tasks, 53 hours.
- Combined: 6 milestones, 16 epics, 63 tasks, 265 hours.
- Future intake: 3 Ideas for Teams, Slack, and remaining secret-bearing
  Connector onboarding adapters.

## Review-and-fix iterations

The first independent review returned FAIL with five findings:

1. Remote MCP `list_changed` failure could retain a stale catalog for new turns.
2. Ordinary events did not explicitly guarantee one accepted delivery per
   destination when same-scope routes collided.
3. The live pilot could bypass assisted setup and immediate Room Tool enablement.
4. High-risk GitHub operations specified confirmation without an explicit
   end-to-end execution done-state.
5. The canonical plan omitted the full task-level inventory.

Repairs were applied to the canonical plan and owning backlog tasks. The repaired
contract now requires:

- immediate MCP server/revision quarantine for remote list changes, fail-closed
  relist failure, and final invocation revision validation;
- one active route per destination and canonical selector plus at most one
  accepted routed event per delivery/item/destination;
- a Teams-originated manifest/callback/plan/confirm/apply pilot that observes the
  managed bundle and uses a newly enabled GitHub Tool on the next turn without a
  restart;
- live confirmed Issue creation, Issue/PR close/reopen, typed workflow dispatch,
  code-changing work, constrained PR creation, and merge execution with receipts
  and webhook reconciliation; and
- a checked-in 63-task inventory with estimates and dependency edges.

A fresh independent reviewer then re-read the complete plan and backlog and
returned PASS with no remaining meaningful gap.

## Verification evidence

- `bl check --strict` — consistency check passed with no issues.
- `bl tree P19 P20 --details` — P19 reports 50 tasks and P20 reports 13 tasks.
- Mechanical inventory total — 63 tasks and 265 hours.
- Dependency review — no missing targets or cycles; sufficient implementation
  ordering for setup, ingress, delivery, collaboration, and advanced routing.
- `git diff --check` — passed for reviewed documentation.
- All P19/P20 tasks remain pending; ingest did not mark implementation complete.

The second review was read-only and made no repository or backlog changes.
