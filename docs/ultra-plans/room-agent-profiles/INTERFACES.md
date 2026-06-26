# Cross-Node Interfaces

| Interface | Producer | Consumers | Definition Status |
|---|---|---|---|
| `room_profile` | 01-profile-foundation | all nodes | P11.M1.E1 |
| `room_profile_binding` | 01-profile-foundation | 02-session-routing-threading, 06-scheduler-ambient | P11.M1.E1 |
| `room_workspace_path` | 01-profile-foundation | 02-session-routing-threading, 03-task-delivery, 06-scheduler-ambient | P11.M1.E2 |
| `room_session_key` | 02-session-routing-threading | 03-task-delivery, 05-policy-budget-ledger, 06-scheduler-ambient | P11.M1.E3 |
| `room_origin` | 02-session-routing-threading | 03-task-delivery, 05-policy-budget-ledger, 07-connector-polish-docs | P11.M3.E1 |
| `connector_thread_ref` | 02-session-routing-threading | 03-task-delivery, 07-connector-polish-docs | P11.M3.E2/P11.M3.E3/P13.M3.E1 |
| `task_room_origin_columns` | 03-task-delivery | 05-policy-budget-ledger, 06-scheduler-ambient | P11.M4.E1 |
| `memory_scope_id` | 04-scoped-memory | 02-session-routing-threading, 05-policy-budget-ledger, 06-scheduler-ambient | P12.M1.E1 |
| `memory_grant` | 04-scoped-memory | 05-policy-budget-ledger, 06-scheduler-ambient | P12.M1.E2 |
| `p11_profile_policy_subset` | 02-session-routing-threading | 03-task-delivery | P11.M2/P11.M3: CWD, model/template precedence, admin/guest async context, privacy guard |
| `profile_policy` | 05-policy-budget-ledger | 02-session-routing-threading, 03-task-delivery, 06-scheduler-ambient | P12.M2: full tool/codebase/grant policy |
| `profile_budget_state` | 05-policy-budget-ledger | 02-session-routing-threading, 03-task-delivery, 06-scheduler-ambient | P12.M3.E1/P12.M3.E2 |
| `room_activity_ledger_entry` | 05-policy-budget-ledger | 06-scheduler-ambient, 07-connector-polish-docs | P12.M3.E3 |
| `routine_target` | 06-scheduler-ambient | 03-task-delivery, 05-policy-budget-ledger | P13.M1 |
| `connector_capability_matrix` | 07-connector-polish-docs | 03-task-delivery, 06-scheduler-ambient | P13.M3.E1; P11 uses Slack/current delivery behavior only |

## Dependency DAG

01-profile-foundation -> 02-session-routing-threading -> 03-task-delivery

01-profile-foundation -> 04-scoped-memory -> 05-policy-budget-ledger -> 06-scheduler-ambient

07-connector-polish-docs feeds connector capability decisions used by 03-task-delivery and 06-scheduler-ambient. `P13.M3.E1` is a structural prerequisite for P13 ambient history even though it lives in the connector-polish milestone.
