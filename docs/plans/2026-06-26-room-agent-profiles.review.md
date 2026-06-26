# Final Review — clawq "Room Agent Profiles" Implementation Plan

**Date:** 2026-06-26 · **Reviewer:** multi-agent review workflow (6 dimension reviewers + adversarial verification) · **Plan:** `docs/plans/2026-06-26-room-agent-profiles.md`

## Verdict

**Ready with required changes.** The plan is structurally coherent, unusually accurate on file existence (all ~31 named pre-existing modules and 4 proposed new files check out), and makes several sound high-level choices (reuse the single cron scheduler, build on existing `effective_cwd` plumbing, first-class scoped-memory tables). However it carries two confirmed High findings — a session-key parser regression that silently misroutes the feature's own deliverables, and an unvalidated `workspace_dir` filesystem-escalation vector — plus a backlog-ingestion procedure whose dependency mechanics do not work as written and several task file-lists that point implementers at the wrong modules. None are fatal; all are addressable in a focused correction pass. Do not ingest into `bl` until the Required Changes are applied.

## Strengths

- **File-existence accuracy is excellent.** Every named pre-existing `src/` module and the audit's test files exist; the 4 new files (`room_workspace/room_session/room_origin/room_activity.ml`) and their tests are correctly absent and labelled "new"; zero namesake collisions.
- **Runtime-split rationale is correct.** `clawq_runtime_core` / `clawq_runtime_integrations` are real `src/dune` stanzas; connectors live in integrations; `command_bridge_min.ml` genuinely returns "disabled in minimal build".
- **Additive-migration approach is grounded.** Idempotent try/catch `ALTER ADD COLUMN` is established in `memory_0_schema.ml`, `task_tree_core.ml:200-220`, `background_task.ml:108-138`, `scheduler.ml:52-57`; adding nullable columns is genuinely row-safe.
- **Single-scheduler reuse is real and correct.** `Scheduler.tick` is called only from `daemon.ml:1516`; "extend cron, no parallel scheduler" matches reality.
- **`effective_cwd` substrate exists end-to-end** (mutable agent field → `session_state.effective_cwd` → `Tool.invoke_context` → shell/file tools/task_tree spawn), so "tools see the room CWD through existing context plumbing" is accurate.
- **`agent_bindings` is a correct precedent** for an optional list that round-trips and is behavior-neutral when empty.
- **Scoped memory is correctly modeled** as first-class tables (`memory_scopes`/`scoped_memories`/`memory_grants`) rather than namespaced global keys, and the activity ledger is correctly a separate table, not an overload of the hash-chained `audit_log`.
- **Process scaffolding is solid:** all 12 fold-in IDs and both `--depends-on` targets are real; tag/body conventions match `bl`; explicit TDD order, phase boundaries, and "do not close overlapping ideas automatically" guardrail.

## Findings

### Critical

None.

### High

**Session-key parser regression: new `:room`/`:thread:`/`:routine:` shapes silently misroute routing, binding, and completion delivery** *(convergence: ACC-2, ARCH-4, REG-1, TDD-7, SEC-13 — five reviewers; verified High)*
- **Location:** Target Session Model (plan L44-52); P11.M1.E3 (L145-150).
- **Issue:** The plan centralizes key *construction* but not the existing positional *parsers*. `Restart_notify.parse_channel_from_key` (`restart_notify.ml:52-57`) does `channel :: id :: _ -> Some (channel, id)`, so the example room key `slack:T123:C456:room` returns id=`T123` (the team), dropping `C456:room`. That value feeds agent-binding resolution (`session_core.ml:844`) and — broader than first flagged — resumed-message dispatch (`daemon.ml:382-389`), heartbeat resume (`daemon_util.ml:394`), and background-task completion routing (`background_task.ml:1314-1316`, i.e. the plan's own P11.M4.E3 "completion delivery"). The parser already carries a bespoke `teams` special-case precisely because multi-segment keys break the generic rule — in-repo proof of the trap. `channel_type_of_session_key` (`runtime_config.ml:358-361`) survives (prefix-only); `queueable_channel_key` also survives (checks name at position 1).
- **Recommendation:** Make `room_session.ml` the single authority for BOTH construct and `parse → typed variant`; refactor `restart_notify` (and audit all `String.split_on_char ':'` consumers) to use it; add round-trip + parser-regression tests over room/thread/routine keys for every connector (including Teams' unsanitized form).
- **Confidence:** High.

**Profile `workspace_dir` is a free-form override with no containment validation — filesystem escalation** *(SEC-5; verified High)*
- **Location:** P11.M1.E1 (L126) vs path requirements (L63-67).
- **Issue:** Path-safety rules (slug+hash, encode unsafe chars, no raw URLs) constrain only the *auto-generated default*, not an explicit `workspace_dir`. `effective_cwd` is trusted verbatim (`tools_builtin_util.ml:310-314`); shell_exec runs there (cwd at `tools_builtin_util.ml:1103`); `workspace_only` arg checks validate against the *static* global workspace, not `effective_cwd` (`tools_builtin_util.ml:241-247`). A profile pointing at `~/.ssh`, `/`, or another repo relocates the shared room agent's shell/file root there — and the existing runtime CWD-change path (`tools_builtin_io.ml:1773-1794`) *does* gate against `allowed_cwd_patterns`, so the plan introduces a new route that bypasses an existing security gate. Combined with the missing bind-authz (below), a non-admin could create such a profile.
- **Recommendation:** Validate `workspace_dir` on bind: `realpath` must resolve under `~/.clawq/workspace/rooms/` (or `allowed_cwd_patterns`/`extra_allowed_paths`); reject traversal/symlink escapes; prefer the derived slug+hash path; gate any override behind admin authz.
- **Confidence:** High (severity medium per verifier, but compounded by an LLM steered by room messages).

### Medium

**Authorization model absent for new privileged surfaces; memory-grant creation authority unspecified** *(SEC-2 + SEC-3; both downgraded to medium)*
- **Location:** P11.M1.E4 (L152-158); P12.M1 (L282-294); P12.M3.E3 (L382-387); P13.M1.E3 (L419-424).
- **Issue:** The plan uses "admin/operator" loosely but never names the enforcement primitive (`Admin.is_admin`, `Slash_commands.gate_admin`) for each new surface, nor states that default-trust guests cannot bind profiles, mint grants, trigger routines, request async work, or read the ledger. `gate_admin` only filters commands registered `AdminRequired`, so an in-channel slash command not so registered is guest-runnable. `memory_scope_id` is a free-form per-profile field (L126) with no uniqueness/derivation — two profiles can silently share one scope, merging private memory. (Verifiers note ledger and routine surfaces are already admin-framed and bind is CLI-framed, narrowing this to a specification gap.)
- **Recommendation:** Add an explicit authz row per surface; make grant creation admin-only and never agent/tool-exposed; derive `memory_scope_id` deterministically (or enforce uniqueness at bind); add authorization as a Review Question.

**Memory isolation enforcement is incomplete: unscoped global FTS search + global core-memory reads on un-migrated paths** *(SEC-1 + ARCH-6; both medium)*
- **Location:** P12.M1.E3/E4 (L312-324).
- **Issue:** Two distinct leak channels the scope work does not cover. (1) The only automatic injection path, `agent.ml:1103` `Memory.search` with no `session_key`, hits the global FTS branch (`memory.ml:1782-1790`) and returns top hits across the entire `messages` table (every channel/user/room) — the scope work targets new `scoped_memories`/`core_memories`, never this `messages` index. (2) Global core-memory reads (`recall_core`/`list_core`) are freely callable from five paths — `agent.ml:1143`, `agent_0_compact.ml:584-610` (compaction, agent-reachable, overlooked), `tools_builtin_io.ml`, `command_bridge_helpers.ml`, `slash_commands_fmt.ml` — so "no runtime room-agent path reads raw global memories" (L317) is enforced only by intent.
- **Recommendation:** Thread a scope/session filter into `Memory.search` for room sessions; make global core functions private/legacy-gated and route all five call sites through scoped APIs; add a grep/compile gate; seed channel-B messages and prove a channel-A room turn cannot surface them.

**Memory-injection task and several others point at the wrong files** *(ACC-3 + TDD-4 + FILE-1)*
- **Location:** P12.M1.E4 (L319-324); P13.M1.E1 (L405); P13.M3.E1 (L475).
- **Issue:** `prompt_builder.ml` never reads DB memories (only on-disk `MEMORY.md`, `prompt_builder.ml:287`); injection lives entirely in `agent.ml inject_search_context` (which *is* already listed). The cron task names `slash_commands_fmt.ml` + vague "cron storage modules" but never `scheduler.ml` (the real `cron_jobs` store). The capability-matrix task is presented green-field despite `connector_capabilities.ml` already existing.
- **Recommendation:** Drop `prompt_builder.ml`, keep `agent.ml` (+ `agent_0_compact.ml` for compaction); name `scheduler.ml`; frame P13.M3.E1 as extending `connector_capabilities.ml` with thread/card fields.

**Budget enforcement: wrong placement, no profile attribution, TOCTOU, no negative test** *(ARCH-2 [→low], ARCH-3, ACC-8, BUD-1, TDD-6)*
- **Location:** P12.M3.E1/E2 (L368-380).
- **Issue:** Provider calls happen inside `agent.ml`'s iterative loop (`loop iteration` at `agent.ml:1404`/`1856`), not once per turn — a `session_turn.ml` turn-entry check cannot stop multi-iteration overrun (the task *does* name "provider call wrapper path", so this is a wording sharpening). `request_stats` is session-keyed with no profile/room column (`memory_0_schema.ml:181-193`; record at `agent.ml:1374`/`1826`, also `debate.ml`); per-room queries need prefix-aggregation over the shared key prefix or a new column threaded through those sites. Shared room sessions allow two concurrent turns to both pass a pre-call check. Tests for "blocks before provider" and "no prompt content leaked" are absent.
- **Recommendation:** Make the per-call gate in both `agent.ml` loops authoritative (keep a coarse pre-check); specify attribution (prefix-aggregate or `profile_id` column, add `agent.ml` to the file list); define the budget period window and thread/routine roll-up; serialize check+reserve or document bounded overshoot; add negative tests asserting no prompt content in the error.

**CWD precedence among profile workspace, `/repo` session cwd, and template cwd is undefined** *(ARCH-5 + ACC-9; medium)*
- **Location:** P11.M2.E2 (L180-186); Review Q4 (L563).
- **Issue:** Three sources exist; today the DB session cwd (set by `/repo` via `Session.set_effective_cwd`) *overrides* `agent_template.cwd` (`session_core.ml:891-903`), so a `/repo` in a profiled room silently wins over the room workspace. The plan only says `/repo` "composes with profile CWD policy" and stores the path in "DB/config" (ambiguous).
- **Recommendation:** Define the chain (suggest: `/repo` explicit > profile workspace > template cwd > global) in the single load block; pick one authoritative store (DB for resolved path, config for declarative default); decide whether a fresh room turn resets cwd to the workspace.

**Room↔profile cardinality and source-of-truth unspecified** *(ARCH-1 + LIFE-2; medium)*
- **Location:** P11.M1.E1 (L126) and P11.M1.E2 (L130-136).
- **Issue:** Config carries singular `connector`+`room_id` (implies 1:1) while `room_profile_bindings` permits 1:many; cardinality is never stated. If 1:many, single-valued `workspace_dir`/`memory_scope_id`/`budget` collide across rooms (per-room workspace derivation mitigates CWD but not memory/budget). (Verifier: the `room_profiles` entity table is not a third *binding* authority — entity+join is normal.)
- **Recommendation:** Pin 1 room : 1 profile for v1; one authoritative store for the binding + reconciliation on config reload; if 1:many is ever wanted, move per-room fields onto the binding.

**Profile lifecycle (unbind/rebind/rename/delete) and workspace GC unspecified** *(LIFE-1 + GC-1; medium)*
- **Location:** P11.M1.E4 (L152-158); workspace model (L54-67).
- **Issue:** No spec for what unbind/rebind/delete do to the shared-room session key + persisted `effective_cwd`, scoped memory rows, `profile_id` columns on task/bg/cron rows, the on-disk workspace, or the ledger. Rebinding a room to a different profile would carry the prior `effective_cwd`/dangling rows across — a cross-profile contamination risk. Lazily-created `rooms/.../threads|tasks|routines/` dirs have no retention/GC.
- **Recommendation:** Add tasks/acceptance for unbind/rebind/rename/delete (preserve vs purge, referential behavior of `profile_id`) and a workspace retention/GC policy.

**Migration mechanics omitted; legacy `core_memories` backfill has no idempotency guard or version gate** *(DEP-1 + ACC-5 + DEP-2 + TDD-3 + DEP-3; medium)*
- **Location:** P11.M1.E2, P12.M1.E1/E3, P12.M3, P13.M1 (schema tasks).
- **Issue:** Acceptance says only "initializes idempotently"/"migrate without data loss" but never names the required mechanism: `schema_version=31` (`memory_0_schema.ml:1`); a new TABLE needs `init_*` + `ensure_all_tables`; a new COLUMN needs a `migrate_step` case + `repair_missing_columns` + a version bump. `core_memories` is created *outside* the version chain (`memory.ml:103`, re-ensured every boot), so a row-copy backfill has no precedent and, if unguarded, double-inserts on every init. The migrate loop is not transaction-wrapped, and a DB advanced past 31 hard-fails on an older binary (no downgrade). (Verifier: the plan already offers "copied OR bound" at L315 — the bind/read-in-place option avoids the risky copy entirely.)
- **Recommendation:** Add a per-task migration sub-checklist (bump version, `migrate_step` + `repair_missing_columns`, register in `ensure_all_tables`, coordinate one bump per milestone). Prefer read-in-place for legacy; if copying, version-gate it (v31→32) with a sentinel guard, wrap in `BEGIN/COMMIT`, and test double-init for no duplication (pattern: `test_memory.ml:141`). Note migrations are forward-only; recommend a DB snapshot before first run.

**Shared room session: mixed-trust steering and concurrency/head-of-line blocking** *(SEC-4 + ARCH-8 + CONC-1; medium)*
- **Location:** Target Session Model (L39-52); P11.M2.E1 (L173-178).
- **Issue:** Collapsing per-user keys to one shared key (`slack.ml:290`, `discord.ml:81`, `telegram.ml:67`) merges all participants into one history + one `effective_cwd`, serialized on one `Lwt_mutex`. Two concrete risks: (1) a prior guest turn stays in context and can plant instructions acted on during a later admin turn (`gate_admin` checks only the current sender); `/repo` is *not* admin-gated (`slash_commands.ml:580-585`) so a guest can repoint the shared CWD. (2) A long room turn blocks every other participant's quick reply; the offload-to-thread classifier itself runs behind the mutex. (Verifier: per-message sender attribution already exists via `effective_message_for_turn`, `session_core.ml:1288`; serialization is inherited behavior.)
- **Recommendation:** Tag history turns with sender trust and treat lower-trust turns as untrusted content; default-deny state-mutating actions (CWD change, memory write, budget spend) for non-admins; specify the classify-then-offload concurrency model and fairness; add a two-concurrent-message test; expand Review Question 1.

**Activity ledger is unsigned/mutable with no default redaction; relationship to `audit.ml` unflagged** *(SEC-7 + ACC-11; medium)*
- **Location:** P12.M3.E3 (L382-387).
- **Issue:** The new `room_activity.ml` table is weaker than the hash-chained, signed `audit_log` (`audit.ml` `prev_hash`/`signature`/`verify_chain`), whose schema lacks every ledger field — so it must be a parallel table (not an `audit_log` extension), but `audit.ml`'s retention/export framework is reusable. No default redaction policy is set; a raw-payload default makes it a long-lived plaintext store of cross-user prompts.
- **Recommendation:** Default `audit_policy` to redact/reference-only; reuse `audit.ml` retention/export (or apply append-only signing); state retention + who may read.

**Connector thread support is asymmetric: Matrix/Telegram have no thread substrate** *(CONN-1 + TDD-10; medium)*
- **Location:** Terms table (L36); P11.M2.E1 (L167); P11.M3; P13.M2.E1.
- **Issue:** Telegram has zero `message_thread_id` handling, Matrix none, and neither uses `connector_history`; the thread-bound work-session model degrades to no-op there. Slack needs `thread_ts` added (zero "thread" refs today); Teams already has reply chains. Non-deterministic synthesized thread ids would fork a child session on every replay/restart.
- **Recommendation:** Add a per-connector support matrix up front; scope each milestone's connector list; test thread-less degradation (deterministic key or clean fallback, no replay forks).

**Other medium findings (compact):**
- **Memory grant-graph semantics unspecified** (ARCH-7): transitivity, hierarchy inheritance, deny precedence, cycles undefined for a 7-kind, default-deny model. Specify a concrete resolution algorithm + unit tests.
- **Profile resolution duplicated across 4 connectors** (ARCH-9): provide one `Room_session.resolve ~connector ~team ~room ?thread → {profile; session_key}` to avoid divergent per-connector binding logic.
- **Model override is net-new, not an extension** (ACC-4): channel session path ignores template `model` (`session_core.ml:904-926`); a profile tier is new plumbing into that precedence block.
- **Codebase grants must restate subordination to global security** (SEC-6): P12.M2.E2 omits that repo grants intersect (not add to) `workspace_only`/`allowed_cwd_patterns`/sandbox; add a rejection test.
- **Profile model override must not bypass Anthropic OAuth opt-in** (SEC-10): assert `allow_anthropic_oauth_inference=false` blocks a profile-selected OAuth model across channel, profile-spawned bg tasks, and routines.
- **Ambient watcher: consent/retention/scope undefined** (SEC-8): unaddressed-message capture is passive bystander recording; `connector_history` has no scope/grant model. Make opt-in (default off), bound retention, scope into the room memory scope, skip restricted channels.
- **Missing request-classification task + untestable acceptance** (TST-1): P11.M4.E1 promises auto classification but has no task; "materially changed"/"inspect last ambient decisions" lack predicates/storage. Add the task and measurable criteria.
- **Completion-delivery round trip and restart/replay untested** (TDD-5): add a mention→task→thread-delivery test including a daemon-restart-mid-flight case asserting idempotent re-delivery and stable child-key resolution.
- **P7 dependency over-scoped; in-flight room work resumption unspecified** (ORD-2): scope P7 to P11.M4/P13 (the async-heavy work); specify restart resumption of room-bound bg tasks/thread sessions.
- **bl ingestion mechanics are non-functional as encoded** (PROC-1 + PROC-3 + PROC-4 + PROC-5): phase-level `--depends-on` does not gate child tasks (`bl why` reports "Task can be started"); `bl check --strict` silently passes dangling deps; hardcoded P11/P12/P13 ids break if P8 ships first; nine of twelve fold-in ids lack a target-task mapping. Encode prerequisites as *task-level* deps; verify with `bl why`/`bl blockers`; pin/derive phase ids at ingestion; add an overlap table (existing id → target task → reference/supersede/keep).

### Low / Nits

- **Budget enforcement placement wording** (ARCH-2, →low): the task already names "provider call wrapper path"; just make explicit it wraps each provider call in both `agent.ml` loops. (Folded into the budget Medium above.)
- **P11 forward-references P12 grant/policy/budget terms** (ORD-1, →low): mark P11.M3/M4 grant clauses as field-inheritance/best-effort until P12, and add the cross-phase `bl` edge.
- **Ingestion step ordering** (PROC-2, →low): reorder so phase deps reference already-created task ids (subsumed by switching to task-level deps).
- **Shared-room key merges per-user history/CWD** (ACC-10): add a one-line note that Slack/Discord/Telegram histories merge and Teams is already per-conversation.
- **`org`/`repo` scope kinds are speculative** (ARCH-10): ship v1 with personal/room/thread/workspace/legacy; introduce `repo` with P12.M2, `org` when an org entity exists.
- **Short workspace hash collision risk** (SEC-11): use ≥12 hex chars over full connector+team+room identity; fail closed on a stored-identity mismatch at bind.
- **Soft-budget channel warning leaks cost to guests** (SEC-12): prefer admin-only delivery; never include raw cost internals in guest-visible messages.
- **`session.ml` is a 3-line facade** (ACC-12): note edits belong in `session_core.ml`/`session_turn.ml`/`session_autonomous.ml`.
- **Task-candidate→epic parent mapping unstated** (PROC-6): annotate each task with its parent epic id; RQ6 answer is "no split needed for `bl` breadth" (P2 holds 111 tasks).
- **Progress-state/ledger persistence under-specified** (OBS-1): name where lifecycle states persist, add filter indexes, state non-blocking best-effort ledger-write semantics.
- **Integration TDD step names no files** (TDD-8): map concerns to `test_session_model_override.ml`/`test_session_persistence.ml`/`test_restart.ml`/`test_scheduler.ml`/`test_daemon.ml`.
- **Workspace path-safety tests lack adversarial inputs** (TDD-9): enumerate traversal, length bounds, unicode/control stripping, slug collision; assert path stays under `~/.clawq/workspace/rooms/`.
- **Cron test glob misses the engine test** (TDD-2): replace `test_*cron*` with `test_scheduler.ml` (engine) + `test_setup_cron.ml` (wizard).

*Dropped after verification:* **SEC-9** (legacy-scope leak) — refuted; plan L317 already forbids runtime room paths from reading raw global memories and L324 mandates the absent-leak test, so an unset `memory_scope_id` yields an empty scope, not a full-store inheritance.

## Codebase-Accuracy Corrections

| Plan claim | Reality (file:line) | Fix |
|---|---|---|
| Slack room key `slack:T123:C456:room` is mere centralization of string formatting | Slack key today = `"slack:"^channel^":"^user`, no team id; team never extracted (`slack.ml:286-291`; parser reads only channel/user/text at `:199-234`) | Drop the T-segment or add an explicit per-connector team-id ingestion task |
| New suffix keys parse correctly | `parse_channel_from_key` takes position-2 as id → `slack:T123:C456:room` yields `("slack","T123")` (`restart_notify.ml:52-57`) | Update parser / route all parsing through `room_session.ml`; round-trip tests |
| Memory injection touches `prompt_builder.ml` | Injection is `agent.ml inject_search_context` (`agent.ml:1096-1163`); `prompt_builder.ml:287` only reads on-disk `MEMORY.md` | Target `agent.ml` (+ `agent_0_compact.ml`); drop `prompt_builder.ml` |
| Profile model "overrides channel default" extends template behavior | Channel path precedence is DB > channel > global; template `model` is **not** consulted (`session_core.ml:904-926`) | State it is a net-new tier inserted into that block |
| `core_memories` migrates "without data loss" via existing pattern | Created outside the version chain (`memory.ml:103`); no row-copy precedent; `schema_version=31` (`memory_0_schema.ml:1`) | New `migrate_step` v31→32 + version bump, or read-in-place; guard idempotency |
| Cron files: `slash_commands_fmt.ml` + "cron storage modules" | Real storage is `scheduler.ml` (`cron_jobs` at `:42`, record `:11-21`, INSERT `:218-241`, SELECT `:154-157`) | Name `scheduler.ml`; expand to `command_bridge.ml` cmd_cron + both slash cron variants |
| Connector capability matrix is new | `connector_capabilities.ml` already declares edit/delete/react/type/send_files/parse_mode per connector | Extend it with thread-reply + card/button fields |
| `request_stats` can query by profile/room | Session-keyed only; no profile column (`memory_0_schema.ml:181-193`); record at `agent.ml:1374`/`1826`, `debate.ml` | Add column/prefix-aggregate; add `agent.ml` to file list |
| `src/session.ml` is the session module | 3-line facade (`include Session_core/Turn/Autonomous`) | Edit `session_core.ml`/`session_turn.ml` |
| Cron tests at `test/test_*cron*.ml` | Glob matches only `test_setup_cron.ml`; engine test is `test_scheduler.ml` | Reference both explicitly |
| Background tasks "see room CWD" | Only the spawn-tool repo_path default derives from `effective_cwd`; the running task uses its own worktree (`background_task.ml:1638,1686`) | Clarify CWD does not transparently follow background work |

## Answers to the Author's 6 Review Questions

**Q1 — Does the shared-room + child-thread model carry risks?** Yes, several concrete, evidence-backed ones: (a) per-user histories/CWD merge into one shared session for Slack/Discord/Telegram (a real privacy/continuity change, not a re-key); (b) a **phase-ordering privacy hole** — P11 ships shared rooms while scoped memory only lands in P12, so during P11 `inject_search_context` injects one user's global memories into everyone's shared context; (c) the key-parser regression (High finding); (d) shared `effective_cwd` repointable by any participant via `/repo`; (e) child-session orphaning/replay non-determinism on thread-less connectors; (f) Teams is already per-conversation, hiding the keying change. Resolve (a)/(b) before the first MVP and add a "disable global memory injection in profiled rooms" guard inside P11.

**Q2 — Replace `core_memories`, or keep legacy CLI access?** Replace for all runtime paths, but expose legacy memory to CLI/admin via a `legacy` *scope* over the same scoped store — do not run two parallel backends (they would drift). The lowest-risk form is **not** to bulk-copy: have `recall_scoped('legacy')`/`list_scoped('legacy')` read `core_memories` in place, which avoids the risky backfill entirely (note `core_memories` is created outside the version chain). If you do copy, it must be a version-gated `migrate_step` v31→32 with an idempotency guard.

**Q3 — Lowest-risk cron extension?** Additive nullable `profile_id`/`thread_id`/`routine_workspace_id` columns via the established idempotent `ALTER ADD COLUMN` (`scheduler.ml:52-57`); old jobs get NULL and behave unchanged. Resolve profile → session_key **at tick time** through the same `room_session` path connectors use (do not store a resolved key — it orphans on rebind). Thread the fields through the small cron surface (record type, `add_job`, `list_jobs`, `update_job`, `command_bridge.ml` cmd_cron, slash `CronAdd`/`CronEdit`). Route routine output through `cron_runs` so the degenerate-loop auto-disable still fires. Test in `test_scheduler.ml` + `test_setup_cron.ml`.

**Q4 — Room CWDs in config, DB, or both?** Both. Config holds declarative policy/seed (operator-set, reviewable); the DB (`room_profiles` + `session_state.effective_cwd`) holds the resolved materialized path — required to satisfy "store the path so renames do not orphan state" (L67). Specify precedence: `/repo` explicit session cwd > resolved room workspace (DB) > profile `workspace_dir` policy (config) > `agent_template.cwd` > global. Decide whether a fresh room turn resets cwd to the workspace (recommended yes).

**Q5 — Which connector first?** Slack first as the single end-to-end vertical, Teams as a fast-follow — **not both together**. Slack is harder and more representative: its per-user → shared-room transition exercises all the new machinery (centralized key construction, shared-session routing, net-new `thread_ts` plumbing). Teams-first would hide the keying change (already per-conversation) and entangle pre-existing Teams UX bugs (B424/B457/B499). Resolve the Q1 privacy/migration decisions before the Slack MVP. Replace the plan's ambiguous "at least Slack and Teams first" (L178) with a single-connector commitment.

**Q6 — Are the phases too broad for `bl`?** Phase breadth is fine (existing P2 holds 111 tasks) — no split needed for `bl` mechanics. But specific milestones bundle independent failure domains and should be decomposed: split P11.M1's three pure helpers into a first epic; treat P12.M1 scoped memory (4 tables + grants + migration + injection rewrite) as effectively its own phase with isolation, migration, and injection as separate ingestible epics; split P12.M3 budgets (runtime-critical) from the ledger (observability); make P13.M2 ambient watcher its own deferred phase. Also: several "task candidates" (P11.M2.E1 across 5 connectors, P11.M4.E2) are really multi-task epics. The genuine ingestion risk is dependency mechanics (Q-related Medium finding), not breadth.

## Required Changes Before bl Ingestion

1. **Fix session-key parsing.** Make `room_session.ml` the single construct+parse authority; update `restart_notify.parse_channel_from_key` and audit all `String.split_on_char ':'` consumers; add round-trip + parser-regression tests for room/thread/routine keys across connectors. *(High)*
2. **Validate `workspace_dir` containment** on bind (realpath under rooms dir / `allowed_cwd_patterns`, reject traversal/symlink escapes) and gate any override behind admin authz. *(High)*
3. **Fix `bl` ingestion mechanics:** convert prerequisites/sequencing to **task-level** `--depends-on`; reorder so deps reference already-created task ids; verify with `bl why`/`bl blockers` (not `bl check`); pin/derive phase ids at ingestion (guard against P8 shifting numbers).
4. **Correct task file-lists:** `agent.ml` (not `prompt_builder.ml`) for injection; name `scheduler.ml` for cron; reference `connector_capabilities.ml`; add `agent.ml` (+ `memory_0_schema.ml`) to budget-attribution tasks.
5. **Specify migration mechanics** per schema task (version bump, `migrate_step` + `repair_missing_columns` + `ensure_all_tables`); make the legacy-memory backfill read-in-place or a version-gated, sentinel-guarded, transaction-wrapped one-shot with a double-init test.
6. **Define the authorization model** for bind/unbind, grant creation (admin-only, never agent-exposed), routine trigger, async-work request, and ledger read; state guests cannot perform them; add as a Review Question.
7. **Resolve memory scope semantics:** scope the unscoped `Memory.search` and route all five global core-memory read sites through scoped APIs; confirm an unset `memory_scope_id` yields an empty room scope (never legacy inheritance).
8. **Pin room↔profile cardinality** (recommend 1:1 for v1) and one authoritative store for the binding with a config-reload reconciliation step.
9. **Define CWD precedence** for profiled sessions in the single load block and pick one authoritative store for the workspace path.
10. **Add the missing request-classification task** (concrete heuristic + test) and give every currently-untestable acceptance criterion a measurable predicate and named test file.

## Open Questions for the Author

- **Cardinality & authority:** Is it strictly 1 profile : 1 room for v1? Which store is authoritative for the profile definition and resolved workspace path, and what reconciles config vs DB on reload?
- **Budget window & roll-up:** What is the budget period, and how do thread/routine child-session costs aggregate into the room's shared budget?
- **Grant-graph rules:** Are explicit grants transitive? How does scope-hierarchy inheritance compose with explicit grants, what is deny-vs-allow precedence, and how are cycles handled?
- **Concurrency/steering:** What is the intended behavior when multiple participants address the shared room session simultaneously, and where does offload-to-thread classification run so it does not itself block on the room mutex?
- **Connector scope matrix:** Which connectors get full room-agent support (shared session + threads + ambient history) vs degraded/no-op at each milestone, given Matrix/Telegram lack thread substrate?
- **Restart resumption:** How is a half-finished thread/room-origin background task resumed across a daemon restart, and how does completion delivery behave if the originating thread/connector is gone?
- **P7 dependency scope:** Should the inbound-queue prerequisite gate all of P11 or only the async-heavy P11.M4/P13 work?
