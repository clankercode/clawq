# Huddle Standing Orders P4 Recovery Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> Execution policy overrides that recommendation: this bounded worker is PLAN ONLY, no subagents. A later coordinator assigns implementation and review; this document does not authorize deployment, merge, Slack writes or live DB/config changes.

**Goal:** Recover eligible missed huddles safely, remove notes-ready latency (B852) and excessive live-huddle polling (B853), and complete roadmap E4.1–E4.5.

**Architecture:** Preserve the deterministic executor and audited transport. Add a shared method-aware limiter, durable recovery barrier/cursors/notices, and a reconciler that discovers work but routes relay production through the existing leased fire executor. Separate discovery, pause replay, catch-up notification and own-artifact retention so authority, crash boundaries and restart behavior are testable.

**Tech Stack:** OCaml 5.1, Lwt, SQLite, Alcotest, existing Slack Web API/Socket Mode and upload-v2 adapters; no new dependency.

**Spec:** `docs/superpowers/specs/2026-11-04-slack-huddle-standing-orders-design.md`, r5, especially §§6.1–6.9 and all §7 invariants. Roadmap: `docs/superpowers/plans/2026-11-04-huddle-orders-roadmap.md`, M4. Campaign handoff: `/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/HANDOFF-2026-11-06.md`.

## Global Constraints

- “No LLM turn ever holds grant authority in v1.” Huddle content never reaches an LLM in this path.
- “An **empty `allowed_targets` denies everything.**” All three capabilities (`slack_huddles`, `slack_huddle_content`, `slack_standing_order_fire`) are independently deny-wins; denial means zero HTTP, including reports.
- “Only relay-producing writes” are epoch-fenced. Pause finalises pre-dispatch work and forbids reposts; existing dispatched relays retain identity, silence, tracking and cleanup.
- `date_start ≥ active_since`, notes-ready timestamp ≥ `active_since`, matching epoch; no backfill before activation or catch-up after pause/resume. `active_since` is the Slack click timestamp, never a local clock substitution.
- `UNIQUE(order_id, room_id, date_start)`, `date_start NOT NULL`, unique canvas per order, globally unique transcript. Keep public `T.fire` unchanged and retain both attempt identities plus SQL compatibility projections.
- Every standing-order Slack call uses a persisted `Standing_order` snapshot, explicit `slack:<channel_id>` access key, current config egress and audit labelled with order and fire/reconcile identity. Never log message text, response bodies, tokens or upload capability URIs.
- Full grant revalidation at every fire and before every Slack write remains mandatory; local order/epoch/lease/accepting check is last, without yielding before dispatch. Do not solve B853 by caching write authority.
- Discovery interval **300 seconds**; history **48 hours**; no-notes timeout **60 minutes**; catch-up hold **600 seconds** after confirmed notice; source retention **30 days**, including inactive orders. Existing pickup defaults: T_ack **180 seconds**, T_reply **3600 seconds**, rate-notice repost delay **300 seconds**, K **3**, daily default cap **10**.
- At most **2 relay messages per (order, huddle)**; at most **1 repost ever**. Only proven-no-share `posting_unknown`, or a known rate notice without acknowledgement, permits that repost. CAS and cross-process channel leases fence all transitions.
- Uploads only to `https://files.slack.com/`, no redirects, no Authorization on raw binary uploads. Relay is one new top-level upload-v2 message with files; no fallback ladder.
- Preserve B832/B833 gates: Slackbot never starts turns, retained relay threads remain silent, assistant-thread mention gating and history file-key allowlist remain intact; drain honors `Slack_thread_runs.accepting`.
- Preserve core/integrations split. New network/recovery modules live in `clawq_runtime_integrations` in `src/dune`; no dependency from core to integrations. Keep dynamic reload semantics for config/credentials/egress/allowlists.
- Read applicable root, `src/CLAUDE.md`, `test/CLAUDE.md`, `docs/CLAUDE.md`. Aim <1000 lines/file, never >2000; introduce focused modules rather than extending `dispatch` or `fire_store` indefinitely.
- Implementation validation only via the gate below, x-p35, two jobs, shared flock. Never local dune/opam/make; never parallel gate jobs. Quick tests use injected clocks/HTTP and no real sleeps/network.
- Scope excludes SQLite-busy/schema-cache cleanup, B849–B851, P5 live acceptance and P6 spoken requests. No changes to live services, Slack, config or DB. No push/merge/deploy. Do not silently change model routing away from gpt-6.1-sol.

## Observed evidence and reasoned rulings

Read-only inspection on 2026-11-06 of `/home/xertrov/.clawq/slack-events.jsonl` (554 JSONL rows at inspection) extracted only event type, identity, timestamps and file ids. No token, message text or content bodies were printed or saved. Evidence is a finite sample, not a complete P0-5 acceptance run.

| Huddle root | Root observed | End observed / room date_end | Canvas edit observed | Ready reply observed | Ready reply file hydration |
| --- | --- | --- | --- | --- | --- |
| `1793923274.194789` | `1793923278.212` | `1793928035.283` / `1793928034` | `1793928251.973` | `1793928252.207` | `1793928252.823` |
| `1793943008.303209` | `1793943009.358` | `1793945814.074` / `1793945813` | `1793945990.229` | `1793945990.849` | `1793945991.562` |

Both are channel `C0CA6UR8FFU`. Roots arrive as `message/huddle_thread`; end and canvas association arrive as `message/message_changed`. End-to-canvas delays were approximately **218 s** and **177 s**. Ready replies arrive before their `files[]` hydration; hydration must be treated as another hint and deduplicated. First root's end edit has `thread_ts` equal to its own ts; this does not prove a separately thread-started huddle. Recorded room ids differ (`R0CFCK3LW3U`, `R0CF9GAK4P7`); no observed reuse is claimed.

**T4.2.1 decision:** retain **5 minutes**. Both observed sequences include roots, edits and ready replies; the roadmap's 2-minute condition (missing delivery of roots/edits) is not demonstrated. Normal ready events must wake fetching immediately (B852), independently of this interval. Polling-only recovery may take a pass plus the required hold; do not promise the normal one-minute event SLO during outage/rate limiting.

**Deletion ruling:** three `file_deleted` events exist in the inspected file (only one after cutoff `1793923000`); they carry `file_id` and `event_ts`, no channel. None was proven to be a huddle source deleted through Slack's “Delete AI notes and transcription” action. Polling `files.info` is therefore the P4 correctness path; do not add `file_deleted` subscription or infer source disappearance from unrelated events. Treat unknown/denied/transient file lookup as unknown, not deletion. Revisit the subscription only with matched source evidence in P5.

**Reuse ruling:** synthetic reused-room fixtures remain required. The ledger currently replaces the row for a newer `date_start`; recovery must retain old instance identity in durable candidates/fires before replacement and re-read each pending fire's pinned root, never borrow the newest room's canvas. Delayed old edits cannot overwrite a newer use. Missing start/provenance stays held/unavailable, never guessed.

**Pause ordering ruling:** a per-pass full Grant check precedes ordinary discovery as specified. Pause replay itself uses the fail-safe `Pause.run ~source:Text` membership path, independent of `allow_users` and LLM availability. A local capability refusal performs zero HTTP and leaves recovery dispatch barred. Failed/incomplete pause sweep likewise bars dispatch; lack of scan coverage is not proof that no pause exists.

## Current code map and planned seams

Inspected baseline: `feat/huddle-orders-p4`, `b84d8330ae28644675eb29dc98d8c89c83fca478`.

- `slack_standing_order_exec.ml` (664 lines): `context` has per-worker channel 1/s provisional limiter; `pass` repeatedly calls `resolve_pending`; `start` captures immutable `held`/`read_only` lists and marks all pre-start roots ineligible. Replace those permanent P3 exclusions with durable recovery authorization, not unconditional permission.
- `slack_standing_order_fetch.ml` (697): `resolve_pending` Grant.Resolve, rereads roots and inserts fires even while live; `read_root` already enforces exact ts; `retry` uses 120/300/600/1200 s backoff. Reuse provenance/fetch, avoid repeated live resolution.
- `slack_standing_order_grant.ml` (729): `evaluate`, `stage_rows`, `tracking_call`, `gate`; full membership/identity work every evaluation. Add explicit audited reconcile/cleanup stages, without weakening Fire/write checks.
- `slack_standing_order_slack.ml`: closed `methods`, `make`, `snapshot`, `preflight`, `http` currently returns `(status, body)`; headers are lost. `slack_standing_order_upload.ml` already has raw response headers through `Slack_file_upload`.
- `slack_huddle_observer.ml`: metadata ledger + generic `wake`; no Slack calls. `slack_huddle_ledger.ml` (429): `record_seen`, `record_notes_ready`, `seen_row`, 30-day prune; one row per room, not historical use.
- `slack_standing_order_fire_store.ml`: `runtime`, `due`, `claim`, `renew`, `cas`, `reserve_dispatch`; lease 120 s, renewal 30 s. Keep lease state machine authoritative.
- `slack_standing_order_dispatch.ml`: `deliver`, `track`, `reconcile`, `confirm_no_pickup`; `slack_standing_order_pickup.ml`: retained attempts, `confirm`, `schedule`. Extend through small adapters, not a second executor.
- `slack_standing_order_pause.ml`: exact `parse_command`, fail-safe `run`, code-rendered `receipt`; `cards.ml`/`card_store.ml`: bound `Pause_button`, team/app/channel/message binding. Reuse those bindings for catch-up.
- `slack_standing_order_report.ml`: durable once-per-cause reports, `Reconcile_gap of string`. `slack_socket.ml`: confirmed hello at line 248. `daemon.ml`: executor starts around 860 and stops at 883, currently no reconciler.
- `setup_slack.ml`: wizard hardcodes scopes/events; no `bot_scopes`, `bot_events` or manifest builder. `test/test_main.ml` explicitly registers suites; `src/dune` explicitly lists integration modules. `test/dune` must be checked for any new helper dependencies.

New focused integration files: `slack_read_limiter.ml`, `slack_standing_order_schedule.ml`, `slack_standing_order_recovery_store.ml`, `slack_standing_order_reconcile_scan.ml`, `slack_standing_order_catchup.ml`, `slack_standing_order_reconcile.ml`, `slack_standing_order_retention.ml`. New test files mirror those concerns; shared fault fixtures in `test/slack_standing_order_fault_support.ml`. No separate briefs needed: the tasks below are the briefs.

## Verification contract

All commands below are literal shell invocations from this worktree. Run serially. Gate was inspected: it fixes WT to this checkout, x-p35 mirror `~/bb-src/clawq-huddle-orders-p4`, flock `/tmp/clawq-huddle-orders-gate.lock`, `-j 2`, isolated HOME/TMPDIR. Do not override `HUDDLE_GATE_HOST`.

```bash
GATE=/home/xertrov/src/clawq/.superpowers/sdd/2026-11-06-huddle-orders-p4-recovery/gate-p4.sh
"$GATE" suite slack_standing_order_worker
"$GATE" quick
"$GATE" fmt-check
"$GATE" all
"$GATE" build
"$GATE" minimal
"$GATE" opt
```

Each task defines focused commands using this `GATE`. Newly named suites must be registered in `test/test_main.ml` in that task. A focused red run must fail on the intended contract, not just an unrelated build failure; then green and quick/fmt-check after non-trivial OCaml changes. Final all/build/minimal/opt are mandatory for this runtime change. No tests/builds were executed while authoring this plan.

## Ordered task graph

Execute **1 → 2 → 3 → 4 → 5 → 6 → 7 → 8 → 9**, each with its own review and targeted commit. Dependencies: 2 consumes 1; 3 is the durable contract used by 4–7; 4 consumes 1–3; 5 consumes 1,3,4; 6 integrates 1–5; 7 consumes 1,3,6; 8 validates 1–7; 9 closes manifest/docs and reruns campaign gates. Each task is independently testable with fake HTTP/time and a fresh DB; no intermediate task authorizes recovery relay production before Task 6's barrier is connected.

### Task 1: Shared 429-aware transport limiter and channel post pacing (E4.1)

**Files:** Create `src/slack_read_limiter.ml`, `test/test_slack_read_limiter.ml`; modify `src/slack_standing_order_slack.ml`, `src/slack_standing_order_upload.ml`, `src/slack_standing_order_ctx.ml`, `src/slack_standing_order_exec.ml`, `src/slack_standing_order_dispatch.ml`, `src/slack_standing_order_report.ml`, `src/dune`, `test/test_main.ml`, `test/test_slack_standing_order_slack.ml`, `test/test_slack_standing_order_upload.ml`.

**Interfaces (new):**

```ocaml
(* Slack_read_limiter; opaque t. Keys contain team/app + method, never token. *)
val create : unit -> t
val create_with : now:(unit -> float) -> wait:(seconds:float -> unit Lwt.t) -> t
val wrap : t -> key:string -> unit -> unit Lwt.t
val note_retry_after : t -> key:string -> float -> unit
val post_pacer : t -> channel:string -> unit -> unit Lwt.t
(* Slack_standing_order_slack; retain existing http and ?http fixtures. *)
type response = { status : int; headers : (string * string) list; body : string }
type http_response = verb:string -> token:string -> method_:string ->
  params:(string * Yojson.Safe.t) list -> response Lwt.t
val retry_after_seconds : (string * string) list -> float option
```

Add optional `?http_response` and `?limiter:Slack_read_limiter.t` to `SO.make_scoped`/`SO.make` and `Grant.evaluate`; a supplied old `?http` is adapted with empty headers; specifying both is rejected before I/O. Production uses the header-preserving transport, preserving Lwt-local fixture injection. Extend the optional header seam through Fetch.evaluate, Dispatch.evaluate/track and reconcile pass_with as well, so production cannot silently fall back to header-dropping HTTP. Thread one shared limiter through `Ctx.t` as `limiter : Slack_read_limiter.t option` (default None for existing tests); retain `limit` compatibility. Method-aware limiting belongs in SO.make, not a method-blind Ctx callback. Raw upload uses the same instance and notes 429 headers without exposing its URI. One daemon creates the instance; executor/reconciler/report adapters reuse it.

- [ ] Write red tests using `create_with` and fake timers: same team+method callers reserve serial 1 s slots, different methods/teams proceed; channel pacing shares one slot across relay completion, repost, report, notice and cleanup note. Verify auth.test/canvases.getContent are reads despite POST. Only message-producing writes use channel post pacing.
- [ ] Add header tests: `Retry-After: 17` on 429 blocks the shared key until now+17; missing/malformed/nonfinite/negative header uses conservative 60 s; JSON ratelimited with HTTP 200 also uses fallback. Extend rather than shorten existing cooldown. Waiters recheck cooldown after waking, so an already queued waiter cannot escape a later 429. Cancellation removes/cancels waiter and creates no dangling Lwt task.
- [ ] Implement reservation under an Lwt mutex, waiting outside it, and rechecking absolute deadlines after sleep. Return typed Rate_limited to caller: never blindly retry a write within transport. Recheck current config/egress and final write fence after all limiter waits.

```ocaml
let retry_delay headers =
  match Slack_standing_order_slack.retry_after_seconds headers with
  | Some seconds -> seconds
  | None -> 60.
(* On status 429: note_retry_after limiter ~key (retry_delay headers);
   return Error Rate_limited. Never replay completeUploadExternal here. *)
```

- [ ] Run `"$GATE" suite slack_read_limiter`, `"$GATE" suite slack_standing_order_slack`, `"$GATE" suite slack_standing_order_upload`, `"$GATE" quick`, `"$GATE" fmt-check`. Verify egress redaction and denied capability still yield zero HTTP.
- [ ] Commit only Task 1 paths: `feat(slack): share retry-after budgets and channel post pacing`.

### Task 2: Event-driven fetch scheduling and immediate notes wake (B852, B853)

**Files:** Create `src/slack_standing_order_schedule.ml`, `test/test_slack_standing_order_schedule.ml`; modify `src/slack_huddle_observer.ml`, `src/slack_standing_order_exec.ml`, `src/slack_standing_order_fetch.ml`, `src/dune`, `test/test_main.ml`, `test/test_slack_standing_order_worker.ml`, `test/test_slack_huddle_observer.ml`. Put scheduling SQL in the new module, not additional bulk in fire_store.

**Interfaces:** Schedule consumes current ledger and Store.runtime/claim/CAS; observer still only persists hints. New `Schedule.note_ready : db:Sqlite3.db -> team:string -> channel:string -> thread_ts:string -> canvas_file_id:string -> ready_ts:string -> now:float -> unit`; `Schedule.take_ready : db:Sqlite3.db -> fire_id:string -> lease:Store.lease -> now:float -> bool`; `Schedule.resolution_due : db:Sqlite3.db -> order_id:string -> root_ts:string -> now:float -> bool`; `Schedule.resolved : db:Sqlite3.db -> order_id:string -> root_ts:string -> now:float -> unit`.

- [ ] Write red fake-clock test starting a fetching fire with reason `notes_not_ready` and next_attempt_at=now+300. Record verified/classified Slackbot ready hint plus canvas hydration, wake worker, and assert one fetch before 60 fake seconds, unchanged epoch/lease/cap. Same hint twice produces one effective wake. Spoofed human/other-bot hint produces none.
- [ ] Persist ready hints keyed by installation/channel/thread/canvas/ready ts. Worker consumes only matching hint under a valid fire lease and CAS, making its retry due; never observer-update a live claim's fire fields. Hints cannot bypass catch-up holds, pause barrier, provenance, rate-limit cooldown or stopped accepting. Restrict acceleration to `notes_not_ready` fetching/detected source work; do not clear `posting_unknown`, rate-limit repost or transport-failure backoff.

```ocaml
let accelerate reason dispatched =
  reason = Some "notes_not_ready" && dispatched = None
(* A matching durable hint changes next_attempt_at to Some now by Store.cas;
   it does not fetch content and cannot manufacture a canvas association. *)
```

- [ ] Add durable root scheduling keyed by order/root. Once a live root resolves, schedule its next fallback probe at now+300; changed room/end/ready metadata dirties it immediately. Worker five-second timer checks local due/hints, not Grant.Resolve on every known root. Initial live detection may create one fire as P3 does, but that fire sleeps until end/ready hint or scheduled fallback; do not repeatedly run full Grant and history every 10 s. Fire/write authority remains fresh when actual work runs.
- [ ] Test 300 fake seconds of unchanged live huddle: at most initial resolution plus one fallback full-resolution evaluation (not 30), no content/upload calls; burst ready edits coalesce; unrelated pickup event does not dirty every root. Config change/capability deny locally suspends without waiting for next remote probe; posture hint invalidates relevant scheduling, never grants authority.
- [ ] Run `"$GATE" suite slack_standing_order_schedule`, `"$GATE" suite slack_standing_order_worker`, `"$GATE" suite slack_huddle_observer`, `"$GATE" quick`, `"$GATE" fmt-check`; preserve B848 exception supervision/backoff/drain tests.
- [ ] Commit: `fix(slack): wake notes retries and bound live huddle polling`.

### Task 3: Durable recovery ownership, cursors and restart barriers (E4.2/E4.3 foundation)

**Files:** Create `src/slack_standing_order_recovery_store.ml`, `test/test_slack_standing_order_recovery_store.ml`; modify `src/slack_standing_order_exec.ml`, `src/slack_standing_order_fetch.ml`, `src/dune`, `test/test_main.ml`, `test/test_slack_standing_order_worker.ml`.

**Interfaces:** All new types/functions here belong to Recovery_store (alias `R` below); existing `T.order`, `T.fire`, `Store.lease` remain unchanged.

```ocaml
type owner = { owner_id : string; generation : int; until : float }
type coverage = { oldest : string; latest : string; history_cursor : string;
  pause_cursor : string; last_success : float option }
type batch = { id : string; order_id : string; epoch : int;
  notice_ts : string option; release_at : float option }
val ensure_schema : Sqlite3.db -> unit
val claim : Sqlite3.db -> order_id:string -> owner_id:string -> now:float -> owner option
val renew : Sqlite3.db -> order_id:string -> owner:owner -> now:float -> bool
val load_coverage : Sqlite3.db -> order_id:string -> coverage option
val begin_scan : Sqlite3.db -> order:T.order -> owner:owner ->
  oldest:string -> latest:string -> now:float -> bool
val save_page : Sqlite3.db -> order_id:string -> owner:owner ->
  coverage:coverage -> candidates:(string * string * string) list -> now:float -> bool
val finish_scan : Sqlite3.db -> order_id:string -> owner:owner -> now:float -> bool
val relay_allowed : Sqlite3.db -> fire_id:string -> now:float -> bool
val recover_startup : Sqlite3.db -> now:float -> unit
```

Candidate tuples are `(root_ts, room_id, date_start)` only; durable root identity precedes any replaceable ledger update. `save_page` atomically saves candidates and cursor; no message content. Additional store operations required by Tasks 4–5 are defined there as part of this same module.

- [ ] Write migration/reopen tests for a fresh DB and existing P3 schema; `recover_startup` is idempotent. New tables: recovery passes keyed order (owner/generation/until, fixed scan bounds, cursors, phase, last_success), candidates keyed order/root/start (room, epoch, fire_id nullable, batch_id nullable), batches keyed id (order/epoch/status/notice/card/release), pause receipts keyed installation/channel/message ts/order, and durable thread work keyed pass/root (cursor/completed).
- [ ] `begin_scan` sets a recovery dispatch barrier before I/O, snapshots bounds once, and retains previous last_success. `save_page` and `finish_scan` CAS owner/generation AND unexpired lease plus order epoch; stale instances cannot advance coverage or release work. Persist cursor after fully processed page only. Invalid/repeated cursor starts bounded rescan of same window, retaining dedup candidates, never skips ahead.
- [ ] Startup tags undispatched Detected/Fetching/Uploading as recovery candidates, including P3 `p4_recovery_held`; do not reset dispatch ledger, uploaded identities or attempts. Dispatched work stays read-only/forbidden to repost until ordinary fenced rules prove eligibility. Origins become durable `live` versus `recovery` eligibility, not “anything first seen after startup is live.” Restarts cannot relabel an old root or clear a hold.

```sql
CREATE TABLE IF NOT EXISTS slack_standing_order_recovery_candidates (
 order_id TEXT NOT NULL, root_ts TEXT NOT NULL, room_id TEXT NOT NULL,
 date_start TEXT NOT NULL, epoch INTEGER NOT NULL, fire_id TEXT, batch_id TEXT,
 PRIMARY KEY(order_id, root_ts, date_start));
```

- [ ] Replace immutable captured `held`/`read_only` exclusion checks with `R.relay_allowed` consulted at dispatch, still false until scan complete and confirmed hold elapsed. Keep P3 safety while Tasks 4–5 are not integrated; no unconditional promotion of startup roots. Test close/reopen during each phase and expired owner rejection.
- [ ] Run `"$GATE" suite slack_standing_order_recovery_store`, `"$GATE" suite slack_standing_order_worker`, `"$GATE" suite slack_standing_order_fire_store`, `"$GATE" quick`, `"$GATE" fmt-check`.
- [ ] Commit: `feat(slack): persist recovery cursors and dispatch barriers`.

### Task 4: Complete pause sweep and bounded historical discovery (E4.2.2–5)

**Files:** Create `src/slack_standing_order_reconcile_scan.ml`, `test/test_slack_standing_order_reconcile_scan.ml`; modify `src/slack_standing_order_recovery_store.ml`, `src/slack_standing_order_pause.ml`, `src/slack_standing_order_grant.ml`, `src/slack_standing_order_fetch.ml`, `src/slack_huddle_ledger.ml`, `src/dune`, `test/test_main.ml`.

**Interfaces:** Scan consumes `Grant.validated.call`, current `T.order`, `R.owner` and coverage; produces candidates, pause outcomes and a completed barrier, never upload/relay. Add `Grant.Reconcile of { reconcile_id : string }` stage (requires active order; snapshot order ref; unit label `reconcile:<id>`), keeping fire_id None rather than forging a fire. `Scan.sweep : db:Sqlite3.db -> manager:Session.t -> ctx:Ctx.t -> owner:R.owner -> order:T.order -> call:SO.call -> (unit, Grant.failure) result Lwt.t`; `Scan.discover` has the same arguments/result. Add `Fetch.resolve_with_grant : ctx:Ctx.t -> db:Sqlite3.db -> manager:Session.t -> validated:Grant.validated -> roots:string list -> T.order -> unit Lwt.t`; `resolve_pending` remains its validating wrapper.

- [ ] Red fixtures: pause top-level, huddle reply, relay reply, ordinary parent thread, member outside allow_users, deleted/nonhuman/guest actor, exact parser near miss, changed order epoch, denied capability, incomplete history/thread page. Scan every thread with replies in the bounded history plus known older huddle/relay threads. Fetch complete replies where timestamps may fall inside the window even if root predates it; include paused-order ledger context so resume cannot replay paused huddles.
- [ ] Use conversations.history pages for `[max(active_since, now−172800), fixed_pass_now]`; for pause safety use the downtime window from last_success bounded to 48h, not newly reset active_since. Enumerate all top-level parents in that pause window plus known threads; do not use reply event timestamps as substitutes for parent discovery. For roots older than window that are not known locally, Slack history cannot discover their new replies: document that bounded-coverage limitation in the gap report, keep catch-up hold, and never claim an exhaustive workspace thread search.
- [ ] Collect only exact `Pause.parse_command` matches and actor/ts/thread identities. Sort by numeric Slack timestamp (preserve fractional precision), persist replay records, apply `Pause.run ~source:Text` in timestamp order, post fixed late receipt once. Exclude bot/edit impersonation; verified history response supplies channel/install context, never user-provided nested metadata. Reuse membership check independent of allow_users. Dedup event-delivered pauses and sweep pauses by durable message identity; a receipt failure never rolls back pause.

```ocaml
let lower_bound ~active_since ~now = max active_since (now -. 172800.)
(* Keep exact decimal Slack ts strings in persistence and API params.
   Compare whole/fractional components for ordering; do not round to int. *)
```

- [ ] Full grant revalidation establishes per-pass snapshot before discovery. Sweep must finish across all pages/threads before any candidate becomes dispatchable; any API failure/429, membership indeterminate or truncated paging leaves barrier closed. Add optional `?call:SO.call` to Pause.run and optional `?call:SO.call` to its receipt transport (defaults retain current behavior), without weakening its membership checks, so replay uses the pass audit label and tests never use real HTTP.
- [ ] Discover roots from history, persist instance candidates first, then record ledger hints through existing classifier/record functions. Recheck all unfinalised rows/stubs by pinned root/thread id even outside history window; use `Fetch.read_root` exact equality, page replies and hydrate ready evidence. Missing root != previous message. For ended rows with no notes after 3600 s finalise no_notes once; never finalise a still-live huddle by age. Resolve multiple same room/date instances independently against their own root. Preserve source association/unique transcript checks.
- [ ] Test duplicate page, partial-page crash, cursor replay, stale epoch, >48h long live huddle, late ready hydration, unrelated canvas, ready stub with no room, reused-room late old edit, missing root and missing_scope method-specific suspension. Run `"$GATE" suite slack_standing_order_reconcile_scan`, `"$GATE" suite slack_huddle_ledger`, `"$GATE" suite slack_standing_order_exec`, `"$GATE" quick`, `"$GATE" fmt-check`.
- [ ] Commit: `feat(slack): sweep downtime pauses before discovering recovery work`.

### Task 5: Restart-safe catch-up batches, top-level notice and bound Pause hold (E4.2.8)

**Files:** Create `src/slack_standing_order_catchup.ml`, `test/test_slack_standing_order_catchup.ml`; modify `src/slack_standing_order_recovery_store.ml`, `src/slack_standing_order_cards.ml`, `src/slack_standing_order_render.ml`, `src/slack_standing_order_card_store.ml`, `src/slack_standing_order_report.ml`, `src/dune`, `test/test_main.ml`.

**Interfaces:** `Catchup.prepare : db:Sqlite3.db -> order:T.order -> fire_ids:string list -> now:float -> R.batch`; `Catchup.ensure_notice : ctx:Ctx.t -> db:Sqlite3.db -> manager:Session.t -> ?http:SO.http -> R.batch -> unit Lwt.t`; `Catchup.release_due : db:Sqlite3.db -> batch_id:string -> now:float -> bool`. Store adds `bind_notice : db -> batch_id:string -> owner:R.owner -> ts:string -> release_at:float -> now:float -> bool` (db is `Sqlite3.db`); `find_batch : Sqlite3.db -> batch_id:string -> R.batch option`. Render adds `catchup_notice : T.order -> card_id:string -> count:int -> release_at:float -> rendered`. R.batch is the type from Task 3, not a duplicate record.

- [ ] Write red tests: downtime candidate set gets one persisted batch and one top-level notice, no thread_ts, no target mention or content; existing `Pause_button` is bound to this exact notice ts/install/channel/order. Copy: `clawq was offline; N huddles will be relayed at HH:MM. Pause SO-n to stop them.` Include timezone in rendered release time; button text `Pause`. Do not change active_since or epoch to authorize catch-up.
- [ ] Persist batch and card before post. Reserve a `notice_attempted` state synchronously before HTTP; include opaque batch marker in fixed Block Kit metadata (no secret/nonce or external text). On success bind returned ts to card and set release_at to at least max(advertised time, response-received now+600). If delayed response changes advertised time, extend hold conservatively; never shorten visible opportunity to pause. On restart preserve persisted release_at; don't recreate a notice or restart timer blindly.
- [ ] Ambiguous post/timeout/crash-after-share: never resend automatically. Search paginated bounded channel history under current audited authority for own bot + exact batch/card marker. Adopt only one exact match, bind its card and give a fresh full 600 s from adoption (so a long outage doesn't skip the pause opportunity). No match or incomplete scan stays notice_unknown/held, inspectable and reported once in management thread; do not infer Slack postMessage idempotency from client_msg_id. This chooses safe non-delivery over a duplicate notice when outcome cannot be proved.

```ocaml
let hold_deadline ~advertised ~confirmed_now =
  max advertised (confirmed_now +. 600.)
(* release_due additionally requires complete pause sweep, active same-epoch
   order, confirmed bound notice, no cancellation and a live recovery owner. *)
```

- [ ] Batch is immutable once notice attempted. Newly discovered fires get a later batch and notice, not silent attachment to an already advertised N; duplicate event/poll candidates join existing batch. Any pause/suspend/cancel invalidates pending batches/reposts transactionally; resume has a new epoch and cannot revive them. A new downtime/reconnect gap during hold requires another completed sweep before release. Existing dispatch fence checks R.relay_allowed after limiter wait.
- [ ] Tests at 599/600 s, restart at 200 s, pause before/after notice, pause during complete, notice success then crash before binding, ambiguous missing notice, spoofed marker, two daemon owners and capability reload. Run `"$GATE" suite slack_standing_order_catchup`, `"$GATE" suite slack_standing_order_cards`, `"$GATE" suite slack_standing_order_pause`, `"$GATE" quick`, `"$GATE" fmt-check`.
- [ ] Commit: `feat(slack): hold catch-up behind durable bound pause notices`.

### Task 6: Wire reconciler lifecycle, retries, pickup and gap reporting (E4.2.1–8)

**Files:** Create `src/slack_standing_order_reconcile.ml`, `test/test_slack_standing_order_reconcile.ml`; modify `src/slack_socket.ml`, `src/daemon.ml`, `src/slack_standing_order_exec.ml`, `src/slack_standing_order_dispatch.ml`, `src/slack_standing_order_recovery_store.ml`, `src/slack_standing_order_tools.ml`, `src/slack_standing_order_cli.ml`, `src/slack_standing_order_render.ml`, `src/dune`, `test/test_main.ml`, `test/test_slack_socket.ml`, `test/test_slack_standing_order_worker.ml`.

**Interfaces (roadmap compatibility):**

```ocaml
(* Slack_socket *)
val on_connected : (unit -> unit) ref
(* Slack_standing_order_reconcile *)
val pass : db:Sqlite3.db -> manager:Session.t -> T.order -> unit Lwt.t
val start : db:Sqlite3.db -> manager:Session.t -> unit
val trigger : unit -> unit
val stop : unit -> unit Lwt.t
val pass_with : ctx:Ctx.t -> ?http:SO.http -> db:Sqlite3.db ->
  manager:Session.t -> T.order -> unit Lwt.t
val configure : ctx:Ctx.t -> unit
```

`configure` installs daemon-owned shared dependencies before start; `start` cannot create a second limiter. `pass` delegates to pass_with with configured context. Test injection uses pass_with. Shutdown removes only this worker's hook and cancels/joins owned tasks; default on_connected is ignore.

- [ ] Red lifecycle tests: one startup pass, one trigger per confirmed `hello` (not TCP connect), periodic 300 fake seconds, reconnect storms coalesce to at most one queued rerun while a pass executes. In Slack_socket hello callback catch exceptions and only enqueue; never block receive/ACK path. Startup trigger runs even if hello happened before hook installation.
- [ ] Serialize orders/channels within process; use R.claim for cross-process reconciliation and existing DB channel lease for relay-producing work. Renew pass lease while waiting for limiter/network; cancel and leave barrier closed on renewal loss. Do not hold SQLite transaction across I/O. Bound pages per tick (20) and yield/requeue with persisted cursor; never treat budget exhaustion as success.
- [ ] Pass phases are explicit: local zero-HTTP capability gate → audited Grant.Reconcile → persisted bounds/barrier → full pause sweep → reload order/epoch → history/candidate discovery → unfinalised/stub checks → batch held recovery → due-fire scheduling and retained pickup → success watermark/gap report. Relay dispatcher consults durable barrier even if a previous per-channel job was already running. Existing accepting and final CAS still fence all writes. Within recovery-owned passes, expose only the exact candidate roots after the entire sweep succeeds; event wakes received during the sweep can enqueue work but cannot dispatch it.
- [ ] Remove blanket startup `read_only`/origin false traps only through durable candidate + batch authorization. All persisted undispatched rows recover; already dispatched rows keep original attempt ids and are reconciled/adopted, not unconditionally reposted. Normal live event work remains prompt after sweep and is not delayed 10 min on every ordinary periodic pass; first pass/reconnect/failed-work recovery origins are held. Treat an interrupted pass as incomplete on restart.
- [ ] Due retries feed the existing executor; reconcile does not call deliver in a second execution engine. Pickup uses `Pickup.confirm`/`read_all` on every retained attempt before no_pickup; failures leave pending and do not count failure twice. Track acknowledged reply-delay reporting and fixed rate-notice waits without early 180 s no_pickup.

```ocaml
let uncovered ~last_success ~now =
  if now -. last_success > 172800. then
    Some (last_success, now -. 172800.)
  else None
(* Render exact Slack-ts window for Report.Reconcile_gap and once-per-window key.
   A failed/partial pass never moves last_success. *)
```

- [ ] First install with no prior watermark persists the bounded-coverage status `coverage_before_window_unknown`, visible in order show; it does not fabricate a last-success timestamp or an exact downtime report. Persist uncovered-window report key before advancing success. Downtime >48h reports `[last_success, now−48h)`; scan only allowed 48h/active_since window plus known unfinalised roots. Management-thread fixed copy contains no quoted text. Report failure cannot silently erase pending gap.
- [ ] Wire daemon configure/start alongside executor and stop reconciler before executor shutdown. Both share accepting condition/current Session config; reload triggers a coalesced pass and local gate invalidation, never a cached permission. Keep background worker supervision from B848.
- [ ] Run `"$GATE" suite slack_standing_order_reconcile`, `"$GATE" suite slack_standing_order_tools`, `"$GATE" suite slack_standing_order_cli`, `"$GATE" suite slack_socket`, `"$GATE" suite slack_standing_order_worker`, `"$GATE" suite slack_standing_order_pickup`, `"$GATE" quick`, `"$GATE" fmt-check`.
- [ ] Commit: `feat(slack): reconcile missed huddles on startup reconnect and cadence`.

### Task 7: Audited source retention in every order state (E4.2.7)

**Files:** Create `src/slack_standing_order_retention.ml`, `test/test_slack_standing_order_retention.ml`; modify `src/slack_standing_order_grant.ml`, `src/slack_standing_order_recovery_store.ml`, `src/slack_standing_order_reconcile.ml`, `src/slack_standing_order_render.ml`, `src/dune`, `test/test_main.ml`.

**Interfaces:** `Retention.pass : ctx:Ctx.t -> db:Sqlite3.db -> manager:Session.t -> ?http:SO.http -> T.order -> unit Lwt.t`. Add `Grant.Cleanup of { fire_id : string }`: it creates persisted audit snapshot and checks current credentials/install/channel/egress/capabilities but does not require an active grant, grantor membership or allowlisted target to delete own artifacts. Restrict its call allowlist to source `files.info`, own uploaded `files.delete`, and fixed note `chat.postMessage`; never upload, complete or relay. Capability refusal still means zero HTTP (cleanup retries when allowed); arbitrary inactive-state grant authority is not restored.

- [ ] Red tests parameterize Active/Paused/Suspended/Cancelled. Select fires with actual dispatched relay within last 30 days (not merely fire.created_at); include both retained attempts' uploaded ids. Persist source_canvas_id/source_transcript_id from verified fetch; never use ids from note text or mutable canvas embeds.
- [ ] Poll sources at most once per 300 s, budgeted and cursor-persisted; run this maintenance even when discovery skips nonactive orders. An independent cleanup pass owner/lease is necessary so order-inactive discovery rejection cannot suppress retention. Reconcile.start lists orders with recent relay attempts, including cancelled, and dispatches retention separately.
- [ ] `file_not_found` under valid identity/permissions is disappearance evidence; explicit deleted/tombstone fields require typed parser tests. Missing_scope, no_permission, inaccessible, malformed, 429, 5xx and transport error mean unknown; no delete on those. Since deletion UI shape is unproven, document this best-effort limit. Do not subscribe file_deleted without matched P0-5 evidence.

```ocaml
let source_missing = function
  | Error (Slack_standing_order_slack.Slack_error "file_not_found") -> true
  | _ -> false
```

- [ ] Under cleanup owner CAS, delete only exact own attempt file ids loaded from durable rows, across both attempts. Retry idempotent file_not_found as already removed. Persist per-file deletion status and fixed-note attempt before I/O; ambiguous note is reconciled by own-bot/fire marker, never blindly duplicated. Fixed thread note: `Clawq removed its relay files because the source was deleted. Clawq cannot recall Anthropic's copy.` Pacer applies. Do not delete source files or others' relays, and don't reinterpret pause epoch as a cleanup veto.
- [ ] Keep cleanup bookkeeping longer than the huddle observation TTL until outstanding cleanup closes; pruning ledger rows must not erase verified source ids/attempts. Test day 29/day 31 boundary, order pause after N relays, partial deletion then crash, two attempts, unknown source error and strict egress deny.
- [ ] Run `"$GATE" suite slack_standing_order_retention`, `"$GATE" suite slack_standing_order_grant`, `"$GATE" suite slack_standing_order_pickup`, `"$GATE" quick`, `"$GATE" fmt-check`.
- [ ] Commit: `feat(slack): retain own relay cleanup across inactive orders`.

### Task 8: Recovery crash matrix, shared-DB races and invariant audit (E4.3)

**Files:** Create `test/slack_standing_order_fault_support.ml`, `test/test_slack_standing_order_faults.ml`; modify `test/test_main.ml`, `test/dune` if needed. Fix production defects found only within Tasks 1–7 owned paths; no adjacent cleanup. Keep fault support/individual suites under 1000 lines.

**Interfaces:** Fake Slack owns shared server state independent of daemon DB handles: upload allocations, shares, own posts, source deletion and request log (method/ids only). Provide `with_server : (server -> unit) -> unit`, `server_http : server -> SO.http`, `server_upload : server -> Slack_standing_order_upload.raw_http`, `inject : server -> point -> outcome -> unit`, `relay_count : server -> order_id:string -> room_id:string -> date_start:string -> int`. Define in fault support:

```ocaml
type point = Allocate | Binary_upload | Complete_before_share | Complete_after_share
  | Share_lookup | Notice_before_post | Notice_after_post | Pause_page
  | History_page | Retention_delete | Retention_note
type outcome = Timeout | Status_500 | Crash
let assert_relay_bound count =
  Alcotest.(check bool) "at most two relay messages" true (count <= 2)
```

- [ ] Build red/green parameterized fault matrix over every public fire state plus each durable recovery phase. Close/reopen DB to simulate process loss; no Store global-only fake restart. Server retains shared messages while worker loses all memory. Inject failure both before and after Slack side effect; complete with/without shares, lost event and delayed history visibility.
- [ ] Test pause during backoff/held notice/rate-limited/posting_unknown and racing complete; relays dispatched before pause keep ts and receipt disclosure; none starts afterward. Test expired lease during limiter wait and mid-complete: old holder cannot CAS, second instance adopts posted relay, no third attempt. Use two independent SQLite handles on one temporary DB, not two refs to the same connection.
- [ ] Test same/reused room with different date_start, unique transcript collision, stale earlier canvas, duplicated Socket events and repeated cursors. Count actual server shares, not just DB dispatch debits. Assert <=2 after every interleaving and at completion; unauthorized repost cases stay <=1 or zero. Catch-up notice count <=1 per batch and no relay until confirmed notice hold/sweep.
- [ ] Audit matrix covers each §7.1 authority denial and §7.14 all method callsites: immutable snapshot rows exist, reconcile ids distinguish passes, no message/token/upload URL in logs/audit; local capability denial is zero HTTP. Count limiter/Grant calls for a live huddle over 10 fake minutes and notes-ready wake during 300 s backoff.
- [ ] Preserve all §7 invariants with named regressions: creation/binding/spoofing, fail-safe pause nonallowlisted human, history/silence, fail-closed unknown snapshot, cap/unique active order, raw upload pinning and accepting drain. Do not change unrelated expectations. List any deliberately changed P3 tests (startup hold now releases only via confirmed recovery; pacing test now tests shared limiter).
- [ ] Run `"$GATE" suite slack_standing_order_faults`, then `"$GATE" quick`, `"$GATE" all`, `"$GATE" fmt-check`, `"$GATE" build`, `"$GATE" minimal`, `"$GATE" opt`. Record exact SHA, commands and pass/fail evidence; no live Slack fixtures.
- [ ] Commit: `test(slack): exercise recovery faults and two-daemon relay bounds`.

### Task 9: One-source manifest and complete P4 documentation (E4.4–E4.5)

**Files:** Modify `src/setup_slack.ml`, `test/test_setup_slack.ml`, `docs/setup-guide.md`, `docs/connector-reference.md`, `docs/src/content/docs/channels.mdx`, `docs/public/llms-full.txt`, `docs/public/llms.txt`; create `docs/src/content/docs/standing-orders.mdx`; modify `docs/cli-reference.md` and its existing website source `docs/src/content/docs/cli-reference.mdx`. No live manifest install/reinstall.

**Interfaces:** `Setup_slack.bot_scopes : string list`, `bot_events : string list`, `manifest_json : socket_mode:bool -> Yojson.Safe.t`. Preserve existing wizard API. HTTP manifest uses the explicit editable example URL `https://example.invalid/slack/events` for both request and interactivity URLs, Socket manifest enables socket mode without an HTTP URL; neither contains credentials. Use Slack schema sections `display_information`, `features.bot_user`, `features.agent_view`, `oauth_config.scopes.bot`, `settings.event_subscriptions.bot_events`, `settings.interactivity`, `settings.socket_mode_enabled`.

- [ ] Red tests parse manifests in both modes and compare exact canonical required sets, with duplicates forbidden. Preserve required existing native-agent/DM and I065 canvas support; do not copy incidental Max-only scope grants. Canonical bot scopes:

```ocaml
let bot_scopes = [
 "app_mentions:read"; "assistant:write"; "channels:history"; "channels:read";
 "chat:write"; "groups:history"; "groups:read"; "im:history"; "im:read";
 "im:write"; "mpim:history"; "mpim:read"; "users:read";
 "files:read"; "files:write"; "canvases:read"; "canvases:write";
 "reactions:read"; "reactions:write" ]
let bot_events = [
 "app_mention"; "message.channels"; "message.groups"; "message.im";
 "agent_session_stopped"; "app_home_opened"; "app_context_changed";
 "reaction_added"; "member_joined_channel"; "channel_shared" ]
```

App-level `connections:write` stays separate. `file_deleted` is absent pending matched huddle-source evidence; correctness does not depend on it. Do not add message.mpim merely because a scope exists: preserve current subscribed event behavior. Wizard lists and manifest derive from these same constants.
- [ ] Replace drifting required scope/event lists in setup-guide §6, connector-reference and channels with canonical generated/checked blocks; explicitly label optional scope discussion outside those blocks. Tests extract only canonical blocks, compare sorted sets to Setup_slack, and check wizard output matches. Preserve explanation of existing canvas requirements.
- [ ] Write standing-orders page covering create/confirm, deterministic relay, normal vs held recovery, 5-min discovery, 48h coverage gaps/older unknown threads, no pause catch-up, Pause outside allow_users, epoch/restart behavior, two-attempt limit, no guarantee of recalling Claude copies, retention unknowns, and inspectable notice_unknown state. No promise that deletion is fully observed or that pickup means downstream completion.
- [ ] Derive egress table from `SO.methods` + raw upload: include actual `users.list`, `chat.update`, `files.delete`, POST reads auth.test/canvases.getContent and actual files.getUploadURLExternal GET. `users.conversations` is mentioned in r5 as preferred but current Grant uses conversations.members/users.list; do not add a dependency or claim it is called unless implementation actually changes. Explain `files.info include_transcription=true` is the narrow undocumented P0-verified exception; flag it in table. Files.slack.com POST is pinned and sensitive URI redacted. Unknown snapshot decode rollback caveat: older binary can decode standing_order as Room_turn; do not roll back to one without review.
- [ ] Update connector/CLI references and llms-full twin for recovery status/reasons without changing public fire fields, and add llms.txt link. Keep P5 acceptance and unknown Claude notice wording risk explicit. Mention B852/B853 verification, and leave SQLite cleanup/B849–B851 outside this delivery.
- [ ] Run `"$GATE" suite setup_slack`, `"$GATE" suite slack_standing_order_faults`, `"$GATE" quick`, `"$GATE" all`, `"$GATE" fmt-check`, `"$GATE" build`, `"$GATE" minimal`, `"$GATE" opt`. The inspected `docs/package.json` defines `build` as `astro build`: run `"$GATE" raw 'npm --prefix docs run build'` only if the remote mirror has Node/npm and provisioned docs dependencies. The gate excludes node_modules and supplies a minimal PATH; if unavailable, record that docs rendering was not verified, without installing dependencies or building locally. Canonical documentation parity tests remain mandatory.
- [ ] Commit: `docs(slack): consolidate manifest and explain huddle recovery`.

## Review Focus

1. Recovery authorization must be durable and narrower than discovery. Never remove P3 held/origin protections before pause barrier and notice hold are checked at actual dispatch.
2. History pagination, thread pagination and pause replay success are different watermarks. Failure/cursor truncation/429 cannot become “covered”; advancing last_success early hides gaps.
3. A Slack post and local commit are not atomic. Ambiguous catch-up/cleanup notices are adopted read-only or stay held, never resent automatically. Release waits a full safe hold after confirmation/adoption.
4. B852 only accelerates matching notes-not-ready work; B853 avoids unnecessary reads by scheduling, never by caching permission for relay writes. Shared limiter must see real headers and preserve fences after waits.
5. Inactive-state cleanup has narrow own-artifact authority; it neither depends on active grant nor restores relay permission. Local capability denial remains zero HTTP. Sources are verified ids, not mutable canvas text.
6. Both relay attempts remain observable and silent across pause/restart/repost. Count actual messages under faults; DB attempts alone do not prove <=2.
7. Reused room and source deletion are not fully characterized live. Test conservative synthetic shapes without claiming empirical coverage. Distinguish a root self-thread_ts from a truly thread-started root.
8. Avoid module cycles: low-level store/schedule/ctx cannot depend on executor, reconcile, daemon, Slack or ingress. Socket callback points outward through ref; integration modules may consume core types, never the reverse.
9. Split code by responsibility before files exceed 1000 lines (hard 2000). Preserve current-config reads during wait/reload and B848 worker supervision. Baseline dispatch is 951 lines, fire_store 856 and test_slack_standing_order_exec 877: place new behaviors in the focused adapters/suites listed above; extract existing cohesive share-resolution helpers into `src/slack_standing_order_share.ml` if required to keep dispatch below 1000, preserving signatures/reexports and registering it in integrations. This extraction belongs to Task 6 if needed, never a broad cleanup.

## Self-review: coverage and remaining concerns

| Roadmap requirement | Concrete task / regression |
| --- | --- |
| T4.1.1 shared limiter, 429 | 1 header seam + queued waiter cooldown tests; 8 cross-worker call audit |
| T4.1.2 post pacing | 1 all message-producing writes; 5 notice and 7 cleanup share pacer |
| T4.2.1 startup/hello/5min | observed evidence ruling + 6 lifecycle fake clock |
| T4.2.2 per-pass Grant/snapshot | 4 Reconcile stage + 6 order/epoch reload; 8 authority matrix |
| T4.2.3 pause first | 3 barrier, 4 paginated sorted replay/late receipts, 5 release guard |
| T4.2.4 48h history | 3 fixed bounds/cursors, 4 discovery, 6 gap windows |
| T4.2.5 unfinalised/stubs/no_notes | 4 exact root checks + known roots outside window, 2 scheduled live probes |
| T4.2.6 retries/pickup | 6 existing executor routing, 8 retained attempts/lost events |
| T4.2.7 30-day all-state retention | 7 own-artifact cleanup/error classification/independent lifecycle |
| T4.2.8 gaps/10min notice | 3 durable watermarks, 5 bound notice ambiguity, 6 reporting |
| T4.3.1 faults/reused room | 4 instance candidates, 8 server-persistent crash matrix |
| T4.3.2 two instances/expired owner | 3 owner fencing, 6 pass renewal, 8 separate DB handles |
| T4.3.3 <=2 messages | 8 actual-server count across every fault, existing dispatch reservation |
| T4.4.1 scopes/events/manifest | 9 canonical sets, conservative file_deleted exclusion |
| T4.4.2 documentation parity | 9 parsed canonical blocks and wizard/manifest tests |
| T4.5.1 dedicated page | 9 privacy/flow/pause/egress/rollback and limits |
| T4.5.2 references/twins/link | 9 connector/CLI/llms and actual method table |
| B852 notes-ready backoff | 2 durable matching hints + <60fake-second test |
| B853 repeated live polling | 2 due scheduling + 8 ten-minute request budget |
| §7.1–21 unchanged invariants | Global Constraints; 8 explicit matrix plus entire existing suite |

Plan authored with no implementation edits, subagents, live writes or build/test commands. There are no omitted M4 epics. Remaining external evidence: real reused room, separately thread-started huddle, participant deletion UI/file semantics and exact Claude failure notices still need P5 evidence; no new live acceptance is claimed. Slack cannot enumerate unknown old thread roots with recent replies from a bounded history window: coverage/reporting is explicitly limited rather than advertised as exhaustive. Ambiguous notice with no adoptable match can remain held for operator inspection; this is a deliberate no-duplicate safety ruling, not silent automatic success.
