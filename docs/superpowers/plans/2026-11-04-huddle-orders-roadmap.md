# Slack Huddle Standing Orders: Campaign Roadmap

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking. This is a **roadmap**: phases 1–2 have their own task-level plans, and each of phases 3–6 gets one written from its tree below. Phase 0 has settled every gate except P0-5 and Claude's notice wordings.

**Goal:** after each huddle in a channel with an active standing order, clawq relays that huddle's Slackbot AI notes and transcript to @Claude in a new top-level message, exactly once, under a scoped and revocable grant. Requests spoken in the huddle are confirmed by a person before anyone acts on them (Phase 6).

**Architecture:** a side-tap observer on `Slack_thread_ingress.handle_event` and a polling reconciler both feed a durable fire ledger. A deterministic, LLM-free executor drains that ledger. It runs under a `Standing_order` access snapshot (order scope ∩ grantor's live authority ∩ room profile ∩ channel posture) and proves provenance from Slack-controlled records (Slackbot share records, and the room and canvas agreeing on the transcript id), never from file ownership. It uploads with upload v2 and tracks pickup. Management goes through agent tools and Block Kit cards following ADR 0003 plan → confirm → apply. The only LLM step is the tool-less spoken-request extractor (Phase 6).

**Tech Stack:** OCaml 5.1, dune, Lwt, cohttp-lwt, Yojson, sqlite3, Alcotest. Slack Web API, Events API over Socket Mode, and Block Kit.

**Spec:** `docs/superpowers/specs/2026-11-04-slack-huddle-standing-orders-design.md` (r4 approved 2026-11-04, plus §4 "Results so far"; where those results contradict older spec text, the results win — see "Spec text superseded"). Research: `/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/research/` (Phase 0: `live-workspace-probe.md`). Phase plans: `2026-11-04-huddle-orders-p1-hardening-observer.md`, `2026-11-04-huddle-orders-p2-grants-management.md`.

## Global Constraints

- All new code goes in `clawq_runtime_integrations` (`src/dune`, after `slack_huddle_tools` at :533); the phase plans name the few core exceptions (config types in `Slack_config_types`, the `Access_snapshot` variant). Integration-only CLI gets `"disabled in minimal build"` stubs in `src/command_bridge_min.ml` (and `src/main_min.ml` for a new top-level command).
- The observer must not depend on `Slack` or `Slack_thread_ingress`. A module cycle can make dune use 20–30 GB. Cross-module wake-ups go through `ref` hooks.
- v1 has no LLM in the fire path, and no LLM turn ever holds grant authority. The human huddle cell (`Slack_huddle_context`) stays untouched.
- Fixed values: `max_fires_per_day` is a per-order column (default 10), not a config key; T_ack 3 min (P0-3 saw the reaction in ≤ 11 s, so ample margin); T_reply 60 min (information only); K = 3 consecutive failures auto-pause; ≤ 1 re-post per fire, ever; the no-notes backoff is +2, 5, 10, then 20 min, with `no_notes` after 60 min; the reconciler runs every 5 min over a 48 h window; catch-up fires are held 10 min; posts are paced ≤ 1/s per channel; `instruction_text` is ≤ 1000 code points; cards expire in 10 minutes; the ledger has a 30-day TTL; ≤ 10 spoken candidates; spoken-request buttons expire after 7 days.
- Uploads go only to `https://files.slack.com/`, with no redirects followed. An empty `allowed_targets` denies everything. No message text is ever logged; the one exception is Claude's replies in `#clawq-huddle-test`, through the event log's exact channel + bot-id `capture_text_from` rule.
- **One undocumented Slack parameter:** `files.info … include_transcription=true` (Max approved 2026-11-04). It is feature-detected: an absent or empty `huddle_transcription` degrades the fire to labelled notes and is reported once. It never fails a fire.
- **Own-relay detection is structural:** own bot user **and** own `bot_id` + non-empty `files[]` + the fixed nonce footer. P0-4 showed the relay message has `subtype = null`, so nothing ever keys on `file_share`.
- Config lives under `channels.slack.standing_orders.*` (spec shorthand `slack.standing_orders.*`): `enabled` (default `true`, a kill switch; the empty allowlist keeps the feature off), `allowed_targets` (`{app_id, label}` entries, default `[]`), and `creator_policy` (`member` by default, Max's choice; `admin` is stricter). There is no cap key: the daily cap is the order's `max_fires_per_day`. Private channels are used locally through `private_channel_policy = allow_if_listed` (Max's decision; B836 tracks the product follow-up; no campaign code).
- Fixtures use fake ids (`U0CLAUDE`/`B0CLAUDE`/`A0CLAUDE`, `UBOT`/`BOWN`, `C0TEST`). Real ids appear only in docs and config examples: clawq app `A0CA40GBWTX`, bot user `U0C9TU04K5M`, bot `B0CB3LLV07J`, team `T0CA9091NAD`; Claude user `U0CA6U11ADQ`, bot `B0CA0SS6NJF`, app `A08SF47R6P4`. Fixtures cover a **private channel with a `C…` id** whose shares sit under `shares.private`, like the test channel.
- Files stay ≤ 1000 LoC. Every new `src/*.ml` is listed in `src/dune`; every new `test/test_*.ml` in `test/dune` and in `test/test_main.ml` as `("<suite>", Test_<x>.suite);` beside the `slack_*` suites (~:274-294).
- Verification uses V1–V5 (one suite, quick tests, `@fmt`, minimal build, `@check` cycle grep) and `gen-agents`/`gen-agents-check`, each run through the x-p35 gate (`/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate.sh`) from `/home/xertrov/src/clawq/.worktrees/huddle-orders`, one at a time. The gate rsyncs the worktree to a standalone tree on x-p35 and runs the command there with `-j 2`. **Never run `build2`, `make` or `dune` locally** (Max: local builds grind xsm to a halt; inside the worktree the Makefile's plain `dune` would also pick the parent checkout's `dune-workspace`).
- TDD per leaf task: write the named failing test, run its suite and see FAIL, implement, run it and see PASS, then commit. Each epic ends with the quick test run and the format check.

## Review Focus

1. **Timestamp units.** `room.date_start` is in integer seconds, while `active_since` and notes-ready are Slack `ts` strings (`"1793750473.714339"`). Eligibility must compare them numerically. Owner T3.3.3: `eligibility compares slack ts numerically across digit-length boundaries`.
2. **Two notes-ready replies for one huddle** (notes-only first, then "notes and transcript"). Exactly one relay, with the transcript if it arrives before dispatch. Owner T3.3.4: `second notes-ready reply before dispatch upgrades payload; after dispatch is ignored`.
3. **Large transcripts or unresolvable speakers** (3 h huddles; a 2–3 min test transcript was already 45 KB). Lines carry `user_id`; a deleted or external user renders as `Unknown speaker (U…)`, never a failed fire. Owner T3.2.6: `render tolerates unknown speakers and 200k-char transcripts`.
4. **Timezone and DST in the relay header** ("4 Nov 11:01–12:05 AEDT"): the daemon's zone with the right abbreviation either side of an AU DST change. Owner T3.4.2: `header window renders AEST/AEDT correctly across the April and October transitions`.
5. **A spoken-request card with nothing to show, or no thread to show it in.** No message, and an honest status in `show`. Owner T6.2.2: `zero candidates or missing thread posts nothing and records status`.
6. **Pickup while notice wordings are uncaptured.** `picked_up` = Claude's first reply that is not a known notice, and the notice matchers stay empty until captured. A notice as Claude's first reply would read as pickup; `spend_limit`/`disabled` on any reply must still give `not_delivered`. Owner T3.5.2: `first reply matching a notice never sets picked_up; an unmatched first reply does`. T5.3.1 gates #core-sync on this.
7. **Notes-only huddles and the room→canvas link.** Live evidence (2026-11-04, 22/22 rooms): `room.transcript_file_id` holds the **canvas id**, so the canvas rule is `canvas id = room.transcript_file_id`, and the transcript id comes only from the verified canvas's `huddle_transcript_file_id`. A notes-only huddle may have no `huddle_transcript_file_id` (relay labelled notes) and possibly no room link. Recommendation, to confirm on a P0-5 notes-only capture: an absent, null or empty room link passes on the other canvas rules; a link naming any other file rejects the canvas. Owner T3.2.2: `notes-only huddle without a room link passes; room link naming another file fails`.

---

## Sequencing

`Phase 1 commit 1 (metadata event log, flag off) merged, deployed, flag on` → `P0-5 + notice capture` → Phase 1 remainder (classifier finalised on P0-5 payloads) ∥ Phase 2 → Phase 3 → Phase 4 → Phase 5 → Phase 6. The old stop rule (P0-2 **and** P0-1b fail) is cleared: both passed. The new standing risk is the undocumented `include_transcription`; if Slack removes it, relays go notes-only (reported) and Phase 6 parks.

## Phase 0: live checks (spec §4)

The coordinator runs API calls with the bot token from a scratch script; subagents never do. Only `#clawq-huddle-test` (`C0CEN7X8KL6`, **private**) data is used, never #core-sync. Results so far are in spec §4 and the probe file.

**Done (2026-11-04).** Setup (Max): the test channel exists with clawq and Claude, AI notes auto-start, Claude set to Respond automatically, and one test huddle was run.

| Check | Result | Facts the build now relies on |
|---|---|---|
| P0-6 scopes | ✅ | Reinstalled. 36 bot scopes incl. `canvases:read`, `reactions:read`, `files:read`, `files:write`; Max's extras (`canvases:write`, `lists:*`, `pins:*`, …) are not requirements. Events `reaction_added`, `member_joined_channel`, `channel_shared` added. Bot token unchanged. |
| P0-7 identity | ✅ | Claude user `U0CA6U11ADQ`, app `A08SF47R6P4`, bot `B0CA0SS6NJF` (the `allowed_targets` seed). |
| P0-8 org gates | ✅ (Max) | Recorded by Max. #core-sync is re-checked at T5.3.1. |
| Root shape | ✅ | `subtype = "huddle_thread"`, `user = USLACKBOT`, no `bot_id`, `text = ""`, `files = [canvas]`. `room`: `call_family = "huddle"`, `id R…`, `has_ended`, `date_start`/`date_end`, `thread_root_ts = canvas_thread_ts = root ts`, **`attached_file_ids = []`** (the canvas is in root `files[]`), `transcript_file_id` key present. |
| Notes-ready | ✅ | ~20 s after the end: `user = USLACKBOT`, `subtype = null`, `bot_id = null`, `thread_ts = root ts`, `files = [same canvas]`. It reached clawq's turn gate (refused only by `private_channel_policy`), so the Phase 1 USLACKBOT exclusion is required. |
| P0-1 canvas | ✅ revised | `files.info`: `user` = the **huddle starter** (a human); **`canvas_metadata = null`** (no `originating_huddle_id`); `is_huddle_canvas = true`; `huddle_transcript_file_id` set; `shares.{private,public}[C]` entries with `share_user_id = USLACKBOT` at the root ts and the ready-reply ts. |
| P0-2 notes | ✅ | `canvases.getContent(markdown)` works with the bot token; it only references the transcript, never inlines it. |
| P0-1b transcript | ✅ undocumented | Documented `files.info` has no `huddle_transcription`; `url_private_download` → 302 login. With `include_transcription=true`: `huddle_transcription {blocks, channel_id, date_start, date_end, lines[{contents, start_time_ms, line_id, user_id}], transcription_time_ranges}`, `channel_id` = the channel, dates = the room's. `mimetype application/vnd.slack-huddle-transcript`; `user` = huddle starter; shared only into the canvas's own file channel. |
| P0-3 mention | ✅ | Claude reacted `eyes` (user `U0CA6U11ADQ`) in ≤ 11 s and replied in-thread ~9 s later. Reply: `user U0CA6U11ADQ`, `bot_id B0CA0SS6NJF`, `bot_profile.app_id A08SF47R6P4`, `username = null`. No `Claude [<task>]` working signal. |
| P0-4 bot file | ✅ | Upload host `files.slack.com`, no redirect. The `completeUploadExternal` response already had `files[].shares.private[C][0].ts`; `files.info` agreed ~1 s later. Relay message: `subtype = null`, `user` = clawq's bot user, `bot_id` = clawq's bot, `files[]`, `text` = initial_comment. Escaped `&lt;!channel&gt;` stayed inert. Claude quoted line 3 exactly, so **the §6.6 ladder is not built.** |

**Remaining**
- [ ] **Prerequisite:** Phase 1 commit 1 (event log) merged and deployed, with `event_log.enabled = true` and `capture_text_from = [{channel C0CEN7X8KL6, bot_id B0CA0SS6NJF}]`. Before any restart, run `ps -o pid,lstart,args -p 3924361` (orphaned `ec-run`); if alive, ask Max before stopping it.
- [ ] **P0-5 event shapes (Max runs the huddles).** Four huddles: a 2-minute one; one started inside a thread; one where a participant shares a canvas in the huddle thread; one restarted. Then delete one huddle's AI notes and transcript. Record from the log: type and subtype sequence (root, `message_changed` at end, notes-ready, `thread_broadcast`), `reaction_added` and Claude-reply payloads, room-id reuse, whether `file_deleted` fires, and `files.info` on the deleted ids. Read-only extras from the same huddles: (a) the **value** of `room.transcript_file_id` equals the canvas's `huddle_transcript_file_id` (the probe recorded only the key); (b) a notes-only huddle's transcript-id fields (Review Focus 7); (c) the `start_time_ms` base (offset from the huddle start, or epoch ms). **Drives:** the Phase 1 classifier, T4.2.1, T4.4.1, T4.2.7, T4.3.1, T3.2.2 and T3.2.6.
- [ ] **Notice wordings** (rate-limited, spend-limit, disabled, not-delivered), captured through the text exception when cheap. **Drives:** T3.5.1 matchers and the T5.3.1 gate.
- [ ] **Fixtures:** redacted, text-free, fake-id `test/fixtures/slack_huddle_events/p0-*.json` from the recorded shapes (none exist yet; `test/dune` has `(source_tree fixtures)`).
- [ ] **Spec r5:** fold in the P0-5 results and the superseded text below. If P0-5 adds `file_deleted`, Max adds it at api.slack.com/apps → clawq → Event Subscriptions → Subscribe to bot events, saves, and reinstalls if the banner asks.

**Spec text superseded by Phase 0 (for r5; this roadmap and the phase plans already follow the results):** §6.2 and §6.5 step 3 and §6.7 "`file_share`" (use the structural own-relay rule); §6.2 `working` and §6.5 step 4 and §7.19 "unclassified never counts" (`picked_up` = first non-notice reply); §6.5 step 0 `originating_huddle_id` (room from the thread root); §7.4 "not Slackbot-owned" and `originating_huddle_id` (use the §6.5 revised rules); §6.6 and §7.20 (ladder not built); F-c/F-d (transcript via `include_transcription`).

**Deltas the phase 1–2 plans must absorb** (owned there, listed so none is lost): P1 fixtures: `own_relay` gets `subtype` null (not `"file_share"`) with `bot_id` set; `huddle_root` carries `files = [canvas_file ()]`, `room.attached_file_ids = []` and `room.transcript_file_id`; `canvas_file.user` is a human, not USLACKBOT. P1 `is_own_relay_message` stays the single definition and adds `bot_id = own bot id` there only (P0-4 settled its open note). P2 config examples seed `allowed_targets = [{app_id "A08SF47R6P4", label "Claude (Anthropic)"}]`.

## Phases 1–2 (task-level plans)

- **Phase 1, hardening and observation:** `2026-11-04-huddle-orders-p1-hardening-observer.md`. Commit 1 is the metadata-only event log behind a flag that defaults to off. Also §6.7 (USLACKBOT turn-gate exclusion, relay-thread silence, `untrusted_source` labelling, the history file-key allowlist), the pure classifier, the observer and `slack_huddles` ledger, the observed-channels interface, and fail-closed `Access_snapshot` decoding.
- **Phase 2, grants and management:** `2026-11-04-huddle-orders-p2-grants-management.md`. Orders, fire-ledger and card tables; the grant evaluator (§6.4); seven tools (§6.8's six plus change-instruction, `slack_standing_order_change`, an atomic cancel-and-create card); confirm, change, resume and pause cards; the deterministic `pause SO-n` parser; the Slack consent check for `creator_policy = member`; `Slack_huddle_watch.of_db`; `clawq slack orders …` with min stubs; `ops.md` plus `make gen-agents`.

**Interfaces (cross-phase contract; a phase plan wins on names, and this block is updated in the same commit):**
- Phase 1 produces: `Slack_huddle_classify.classify : context -> Yojson.Safe.t -> observation` (pure; `room`, `text_kind`, `matchers`, `target`, `context`); `Slack_huddle_observer.{observe; observe_db}` (never raises, never calls Slack; **no executor wake yet**); `Slack_huddle_ledger` (table `slack_huddles`, rooms and notes stubs); `Slack_huddle_watch.{t; of_db; source}`; `Slack_relay_guard.{footer; nonce_of_text; file_ids; is_own_relay_message; registry; current_registry; silent}`; `Slack_event_log`; `Access_snapshot` `Known | Unknown` decode. Matchers are empty, so every reply is `Unclassified`.
- Phase 2 produces: `Slack_standing_order_types.order` (incl. `spoken_requests`, `card_version`, `bot_user_id`, `bot_id`, `epoch`, `active_since`, `management_thread_ts`); `Slack_standing_order_store` (§6.1 schema incl. fires and cards, `clock`, `deactivate`/`deactivate_in_tx`, `insert_fire`, `dispatches_since`, `observed_orders`; fire claim and state transitions are Phase 3's); `Slack_standing_order_grant.{gate; evaluate}` (`Plan | Confirm | Replace | Resume | Fire`; `Hard | Transient | Inactive | Capped`); `Slack_standing_order_slack.{methods; snapshot; make; error_code}` (key `slack:<channel>`); `Slack_standing_order_pause.run`; `Access_snapshot.Standing_order`; action ids `clawq_so_confirm|cancel|resume|pause|change`, dispatched from `Slack_thread_runs.handle`.

## Phase 0 result → decision it drives (phases 3–6)

| P0 | Result | Phase | Decision |
|---|---|---|---|
| P0-1 | ✅ revised | 3, 4 | Canvas provenance from Slackbot shares + transcript-id agreement, never owner or `canvas_metadata` (T3.2.2); stub room from the thread root (T3.3.0, T4.2.5) |
| P0-1b | ✅ undocumented | 3, 6 | Retrieval via `include_transcription`, feature-detected (T3.2.4); line shape for T3.2.6 and T6.1.1; Phase 6 viable |
| P0-2 | ✅ | 3 | Notes via `getContent` (T3.2.5); stop rule cleared |
| P0-3 | ✅ | 3, 5, 6 | `acknowledged` = reaction, `picked_up` = first non-notice reply (T3.5.2); T_ack stays 3 min (T3.5.3); [Send to Claude] works (T6.3.3); rung 2 unneeded |
| P0-4 | ✅ | 1, 3, 4 | File path is primary, rung 1 dropped; `relay_ts` from the complete response first (T3.4.5); own-relay match ignores subtype (P1, T3.4.3, T3.4.6) |
| P0-5 | open | 1, 3, 4 | Classifier (P1); T4.2.1 interval; T4.4.1 `file_deleted`; T4.2.7 deletion; T4.3.1 reuse fixtures; T3.2.2 notes-only; T3.2.6 time base |
| P0-6 | ✅ | 4, 5 | Manifest lists the required set only (T4.4.1); deploy check (T5.1.2) |
| P0-7 | ✅ | 3, 5 | Target keys (T3.5.2); `allowed_targets` seed (T5.1.2) |
| P0-8 | ✅ | 5 | #core-sync preconditions (T5.3.1) |
| Notices | open | 3, 5 | T3.5.1 matchers; #core-sync gate (T5.3.1) |

## Phase 3: Executor (gates passed; P0-5 shapes only fixtures and Review Focus 7)

**Interfaces (Phase 3 produces):** `Slack_standing_order_slack.methods` gains `conversations.history`, `conversations.replies`, `files.info` (incl. `include_transcription`), `canvases.getContent` (POST), `reactions.get`, `files.getUploadURLExternal`, `files.completeUploadExternal` and `files.delete` (other verbs per Slack docs), plus `upload_prefix = "https://files.slack.com/"` and a `?limit:(unit -> unit Lwt.t)` hook (no-op until Phase 4) · `Slack_huddle_transcript.line = { line_id : string; start_ms : int; user_id : string; text : string }`, `of_file_info : Yojson.Safe.t -> line list option` (None = feature absent or empty), `render : speaker_name:(string -> string Lwt.t) -> date_start:int -> line list -> string Lwt.t` · `Slack_standing_order_provenance.verify : … -> (verified, reason) result Lwt.t`, `verified = { root_ts; room_id; date_start; canvas_id; notes_md; transcript : transcript option }`, `transcript = { file_id : string; lines : Slack_huddle_transcript.line list; markdown : string }` · `Slack_standing_order_relay.nonce : secret:string -> order_id:string -> room_id:string -> date_start:string -> string` and `initial_comment : … -> string` · `Slack_standing_order_pickup.matchers : Slack_huddle_classify.matchers` (notice wordings; `working_username_prefixes = []`) and `apply : db:Sqlite3.db -> Slack_huddle_classify.observation -> unit` · `Slack_standing_order_exec.start : db:Sqlite3.db -> manager:Session.t -> unit`, `wake : unit -> unit`, `resolve_pending : db:Sqlite3.db -> Slack_standing_order_types.order -> unit Lwt.t` · `Slack_standing_order_report.post_once : db:Sqlite3.db -> Slack_standing_order_types.order -> cause -> unit Lwt.t`, `cause = Missing_scope of string | Target_not_member | No_pickup | Not_delivered | Grantor_unauthorised | Posture_changed | Repeated_failures | Cap_hit | Transcript_unavailable | Reconcile_gap of string`.

```
M3 Executor
├── E3.1 Grant HTTP and egress (suite slack_standing_order_slack, extended)
│   ├── T3.1.1 append the Phase 3 methods to Slack_standing_order_slack.methods; audit rows tagged standing_order:<id>:<fire|reconcile>
│   ├── T3.1.2 methods is the only list call sites may use; test `every executor call site is covered by an egress rule` (§7.14)
│   └── T3.1.3 upload POST pinned to upload_prefix; test `302 from upload url fails without following` (§7.16)
├── E3.2 Provenance and fetch (suite slack_standing_order_provenance; fixtures follow the P0 shapes above)
│   ├── T3.2.1 root re-read: latest=oldest=root, inclusive, limit 1; returned ts = root_ts; call_family=huddle, room id matches, has_ended
│   ├── T3.2.2 canvas: in root files[] (or attached_file_ids) and on the notes-ready reply; is_huddle_canvas; a shares.{public|private}[channel] entry with share_user_id=USLACKBOT at root or ready ts; canvas id = room.transcript_file_id (Slack's room→canvas link; 22/22 live rooms); owner and canvas_metadata never read; tests `human-owned canvas with Slackbot shares passes`, `participant-shared canvas fails` (+Review Focus 7)
│   ├── T3.2.3 transcript id only from files.info(verified canvas).huddle_transcript_file_id (the room names the canvas, never the transcript); test `markdown embed pointing at an older transcript relays nothing` (§7.4)
│   ├── T3.2.4 transcript via files.info(include_transcription=true): mimetype application/vnd.slack-huddle-transcript, huddle_transcription.channel_id = order channel, date_start = room.date_start, unique per fire; owner and the transcript's own shares not checked; absent or empty → labelled notes + one Transcript_unavailable; test `missing huddle_transcription degrades to labelled notes and reports once`
│   ├── T3.2.5 notes via canvases.getContent(markdown); test `human or other-bot notes-ready lookalike never passes provenance` (§7.17)
│   └── T3.2.6 render lines `[mm:ss] Speaker: text` from start_ms (base per P0-5) and user_id via cached users.info (+Review Focus 3)
├── E3.3 Fire state machine (suite slack_standing_order_exec)
│   ├── T3.3.0 resolve_pending: stub room from the thread root (thread_ts = root ts; canvas_metadata is null); read root for date_start; Store.insert_fire or skipped_cap (Phase 4 reuses it)
│   ├── T3.3.1 claim/CAS and lease added to Slack_standing_order_store (P2 made the columns); one fire per channel; stops when not Slack_thread_runs.accepting (§7.18)
│   ├── T3.3.2 detected→fetching: Grant.evaluate ~stage:(Fire …) first; Hard suspends with zero HTTP, Transient backs off, Inactive gives skipped_inactive, Capped gives skipped_cap; no-notes backoff 2/5/10/20, then no_notes at 60 min
│   ├── T3.3.3 eligibility: date_start ≥ active_since ∧ notes_ready_ts ≥ active_since ∧ fire.epoch = order.epoch (+Review Focus 1)
│   ├── T3.3.4 when_no_transcript: relay_notes (labelled) or skip, giving skipped_no_transcript (+Review Focus 2)
│   ├── T3.3.5 epoch fence on upload, complete and re-post only; test `pause racing complete records dispatched fire and receipt discloses it` (§7.5)
│   └── T3.3.6 start from src/daemon.ml beside Daemon_runtime_loops.start (:853); add observer fire effects + wake ref; fire-ledger body for P1's Slack_relay_guard registry; test `fire path never calls Provider` (§7.2)
├── E3.4 Relay via upload v2 (suite slack_standing_order_relay; primary path confirmed by P0-4)
│   ├── T3.4.1 nonce = HMAC(local secret, order_id‖room_id‖date_start); test `nonce absent from list, show and reports`
│   ├── T3.4.2 fixed §5.2 template: instruction escaped (&,<,>) and ≤1000 chars, do-not-act clause always present, P1 relay footer (+Review Focus 4)
│   ├── T3.4.3 pre-upload adoption: conversations.history since fire creation; own bot user ∧ own bot_id ∧ files[] ∧ nonce footer is adopted as posted; test `adoption ignores subtype`
│   ├── T3.4.4 getUploadURLExternal → POST → persist uploaded_file_ids → re-evaluate → completeUploadExternal(channel_id, no thread_ts, initial_comment)
│   ├── T3.4.5 relay_ts from the complete response files[].shares.{public|private}[C][0].ts, then Own_relay, then a files.info shares poll ≤2 min (kept fallback); else posting_unknown; test `relay_ts read from complete response shares.private`
│   └── T3.4.6 re-post ≤1 ever (write-ahead), counts toward the cap; only after posting_unknown proves no share (structural match, never file_share) or a rate_limited notice without acknowledgement
├── E3.5 Pickup and accounting (suite slack_standing_order_pickup)
│   ├── T3.5.1 matchers: rate_limited/not_delivered on the first reply, spend_limit/disabled on any reply, from captured wordings (empty until captured); no working-username signal
│   ├── T3.5.2 acknowledged = target reaction (any emoji; observed eyes) on relay_ts or a relay file; picked_up = first target reply in the relay thread that is not a notice; a reply with no reaction sets both (+Review Focus 6, §7.19 revised)
│   ├── T3.5.3 no_pickup only after T_ack and a reactions.get + conversations.replies confirmation; T_reply is information only
│   ├── T3.5.4 rate_limited: fixed 5-min re-post if no acknowledgement reaction; spend_limit/disabled gives not_delivered even after picked_up
│   └── T3.5.5 K=3 consecutive no_pickup|not_delivered|failed auto-pauses; cap gives skipped_cap, reported once a day (§7.15)
└── E3.6 Reporting (suite slack_standing_order_report)
    ├── T3.6.1 fixed templates (ids and enumerated reasons), once per cause, in the management thread, mentioning the grantor (§7.13)
    └── T3.6.2 hard failure suspends and reports; one-time no-notes hint; Transcript_unavailable once; expected non-events go to show only
```

## Phase 4: Reconciler, limiter, recovery, manifest, docs (gated on P0-5)

**Interfaces (Phase 4 produces):** `Slack_read_limiter.t`, with `create : unit -> t`, `wrap : t -> key:string -> unit -> unit Lwt.t` (plugs into `?limit`), `note_retry_after : t -> key:string -> float -> unit` and `post_pacer : t -> channel:string -> unit -> unit Lwt.t` (≤ 1/s) · `Slack_standing_order_reconcile.pass : db:Sqlite3.db -> manager:Session.t -> Slack_standing_order_types.order -> unit Lwt.t`, `start : db:Sqlite3.db -> manager:Session.t -> unit` and `trigger : unit -> unit` · `Slack_socket.on_connected : (unit -> unit) ref` (default `ignore`) · `Setup_slack.bot_scopes : string list`, `Setup_slack.bot_events : string list` and `Setup_slack.manifest_json : socket_mode:bool -> Yojson.Safe.t`.

```
M4 Reconciler and hardening
├── E4.1 Limiter (suite slack_read_limiter)
│   ├── T4.1.1 shared read limiter honouring 429 Retry-After; executor and reconciler share one instance
│   └── T4.1.2 post pacer ≤1/s per channel, wired into T3.4.4 and T3.4.6
├── E4.2 Reconciler pass (suite slack_standing_order_reconcile)
│   ├── T4.2.1 triggers: start, on_connected (at "hello received", slack_socket.ml:246-249), every 5 min (2 min if P0-5 shows roots or edits missing; notes-ready already arrives as an event)
│   ├── T4.2.2 Grant.evaluate under a per-pass snapshot; on failure skip and apply §6.4 handling; never logs text
│   ├── T4.2.3 pause sweep first via Slack_standing_order_pause.run ~source:Text: exact `pause SO-n` in the window (top-level, huddle and relay threads), ts order, late receipt (§7.5 downtime)
│   ├── T4.2.4 history scan over [max(active_since, now−48h), now], recording roots and room state
│   ├── T4.2.5 unfinalised rows and stubs by id: root re-read with ts equality (stub room from thread_ts), replies for notes-ready, no_notes after 60 min
│   ├── T4.2.6 retry due fires; pickup check (reactions.get + replies) before any no_pickup
│   ├── T4.2.7 retention: files.info on sources ≤30 days, in any order state; when deleted, files.delete own relay files and post the fixed note [P0-5]
│   └── T4.2.8 gap >48 h reports the uncovered window; catch-up fires held 10 min with a top-level notice carrying [Pause]
├── E4.3 Crash recovery and fault injection (suite slack_standing_order_faults)
│   ├── T4.3.1 fake Slack: timeouts and 5xx at upload, complete and share (complete with and without shares); crash in every state; restart resumes; reused room-id fixture [P0-5]
│   ├── T4.3.2 two daemon instances on one DB; test `expired lease holder cannot write state`
│   └── T4.3.3 test `at most 2 relay messages per order and huddle across every injected fault` (§7.9)
├── E4.4 Manifest from one source (suite setup_slack)
│   ├── T4.4.1 bot_scopes = the required set (live since P0-6; Max's extra scopes are not listed); bot_events add reaction_added, member_joined_channel, channel_shared, and file_deleted per P0-5; manifest_json for both modes
│   └── T4.4.2 test `docs scope and event lists equal Setup_slack sets` over setup-guide.md §6, connector-reference.md and channels.mdx
└── E4.5 Docs
    ├── T4.5.1 docs/src/content/docs/standing-orders.mdx: flow, pause, privacy, egress list from methods (flag include_transcription as undocumented), rollback caveat
    └── T4.5.2 P2 Task 15 already documents the tools, config keys, CLI and the P2 egress list (connector-reference.md "Standing orders (huddle relay)", cli-reference.md, public/llms-full.txt); add the Phase 3–4 methods and the files.slack.com upload rule there, and the public/llms.txt link
```

## Phase 5: Live acceptance (needs Max; P0-5 and notice wordings)

```
M5 Live acceptance
├── E5.1 Deploy
│   ├── T5.1.1 merge to master; restart the xsm daemon with Max's approval (no systemd unit); test channel only; local config private_channel_policy=allow_if_listed with C0CEN7X8KL6 listed
│   └── T5.1.2 allowed_targets=[{A08SF47R6P4, "Claude (Anthropic)"}]; auth.test team T0CA9091NAD and user U0C9TU04K5M; bots.info(B0CB3LLV07J).app_id = A0CA40GBWTX; scopes still match P0-6
├── E5.2 #clawq-huddle-test scenarios (record each fire status from `show SO-n`)
│   ├── T5.2.1 natural-language create → card → grantor click → activation notice with [Pause]; a non-grantor click is refused
│   ├── T5.2.2 2-minute huddle: one relay with 2 files, relay_ts from the complete response; Claude reacts and replies; picked_up well inside T_ack
│   ├── T5.2.3 notes-only huddle: labelled notes relay; restarted huddle: one relay each
│   ├── T5.2.4 pause by text, by button, and while the daemon is down: zero posts and a receipt; resume card: held huddles not relayed
│   └── T5.2.5 member outside allow_users can pause; a guest join (if Max adds one) suspends the order and reports it
└── E5.3 #core-sync
    ├── T5.3.1 P0-8 gates hold for #core-sync; notice matchers captured, or Max explicitly accepts Review Focus 6; Max creates the order (destination still open; only the instruction text changes)
    └── T5.3.2 watch the first 3 real huddles; retune T_ack and the matchers from what is observed; fold results into spec r5
```

## Phase 6: Spoken requests, §6.10 (P0-1b and P0-3 passed; parks if `include_transcription` disappears)

**Interfaces (Phase 6 produces):** `Slack_spoken_requests.line = { idx : int; line_id : string; offset_s : int; speaker : string; text : string }`, built from `Slack_huddle_transcript.line` (`speaker` = Slack's `user_id`, so speaker separation comes from Slack, not the model) · `candidate = { line_refs : int list; addressee : Clawq | Claude | Ambiguous; proposed_action : string }` · `extract : config:Runtime_config.t -> line list -> Yojson.Safe.t Lwt.t` (one `Provider.complete ~config ~messages ()` call with no `?tools`, no history and no memory) · `validate : line list -> Yojson.Safe.t -> item list` (quote and speaker from the lines, via `Structured_pipeline_schema`) · table `slack_spoken_requests(id, fire_id, item_no, line_refs, addressee, proposed_action, card_ts, executed_by, executed_at, expires_at)` · action ids `clawq_sr_do`, `clawq_sr_edit`, `clawq_sr_swap`, `clawq_sr_send` and `clawq_sr_ignore` · `Slack_standing_order_types.current_card_version = 2`.

```
M6 Spoken requests (orders with spoken_requests = 1 ∧ card_version ≥ 2, else a re-confirm card first (P2 Decision 13); async after E3.3 fetches a transcript; never blocks the relay; skipped for notes-only)
├── E6.1 Extractor (suite slack_spoken_requests)
│   ├── T6.1.1 input is only the transcript lines (idx, [mm:ss], user_id, contents); test `extractor has no tools, history, grant or memory`
│   ├── T6.1.2 ≤10 candidates; the prompt flags likely mishearings ("claw", "clock", "cloud", names swapped) as Ambiguous
│   └── T6.1.3 validate: out-of-range or duplicate refs dropped; test `output cannot introduce speakers or quotes absent from referenced lines`
├── E6.2 Card in the huddle thread, not the relay thread
│   ├── T6.2.1 each item reads `<@speaker> at mm:ss — "quote" → proposed action`, with [Do it]/[Send to Claude], [Edit], [It was for …] and [Ignore]
│   ├── T6.2.2 zero candidates or a missing thread posts nothing (+Review Focus 5)
│   ├── T6.2.3 easy confirm and correct: show the speaker's neighbouring line(s) under the quote, so nobody has to remember what was said
│   └── T6.2.4 same bindings as the §6.8 cards; 7-day expiry; test `an item executes at most once`
└── E6.3 Clicks: any non-guest channel member, acting with the clicker's authority, never the grant's
    ├── T6.3.1 [Do it] starts a B833 native task in the huddle thread as the clicker's Human_request, quote attached as marked untrusted context. OPEN: a click is not a message; the plan must define how a verified click mints the with_request cell, and security review gates it
    ├── T6.3.2 [Edit] asks the clicker to reply with the correction (an ordinary human turn). Proposed, needs Max: a modal pre-filled with the proposal (views.open, initial_value)
    ├── T6.3.3 [Send to Claude] posts a new top-level "<@Claude> Confirmed by <@clicker> from the huddle at 12:31: …" via relay pinning and pacing (P0-3: Claude answers a bot's top-level mention)
    ├── T6.3.4 [It was for …] swaps the addressee and re-renders; [Ignore] dismisses; test `click runs with clicker authority, not grant`
    └── T6.3.5 test `relay comment always contains the do-not-act clause` (shared with T3.4.2)
```

## Proposed backlog entries (spec §10; not filed; `bl bug --title … --body … --simple`)

1. **B833 turn gate drops human file_share and thread_broadcast messages.** Any subtyped message is dropped at `slack_thread_ingress.ml:52-55`, so Slack audio and file transcription (`slack.ml:842-962`) cannot be reached in production.
2. **send_to_session lacks target authorization.** It delivers to any session key's notifier (`tools_builtin_session.ml:284-520`); it needs target authorization or denial in granted contexts.
3. **Slack scope and manifest docs have drifted.** Five lists disagree (`setup_slack.ml`, setup-guide, connector-reference, channels.mdx, `docs/dist` manifest); `message.mpim` and `files:read` are missing. Phase 4 E4.4 closes this if it lands first.
4. **setup-guide says an empty Slack allowlist means "all".** The code treats an empty list as deny-all; fix the doc wording.
5. **The xsm daemon has no supervision.** It was down from 2026-11-03 17:05 until a manual restart on master (2026-11-04 14:53). B829 supervision was never deployed: there is no systemd unit. An orphaned `ec-run` (pid 3924361) was still running.
6. **Slack_huddles.content misses thread-started huddles and never gets a transcript.** It has an 8-file hydrate cap; its `originating_huddle_id` check is always skipped (P0-1: `canvas_metadata` is null), and it never passes `include_transcription`, so `huddle_transcription` never comes back (P0-1b). Its provenance should follow the §6.5 revised rules.
7. (idea, `bl idea`) **A per-channel "respond only when mentioned" mode for clawq**, the follow-up §6.7 recommends.

Already filed: B836 (private-channel policy product follow-up; the campaign only uses `allow_if_listed` locally).

--- SUMMARY ---

- **Shape:** 7 phases. Phase 0 is Max plus the coordinator. Phases 1–2 have their own plans. Phases 3–6 have 67 leaf tasks here: Phase 3 = 29, Phase 4 = 17, Phase 5 = 9, Phase 6 = 12 (each leaf is one TDD commit). Rung 1 (T3.4.7) is gone because P0-4 passed.
- **Phase 0** is done except P0-5 (event shapes, plus three read-only extras: the room/canvas transcript-id value, notes-only fields, the `start_time_ms` base) and Claude's notice wordings. Both need Phase 1's event log deployed. The stop rule is cleared.
- **What Phase 0 changed:** provenance uses Slackbot share records and room↔canvas transcript-id agreement, never ownership or `canvas_metadata`; the transcript comes from the undocumented, Max-approved `include_transcription=true`, feature-detected; own-relay detection is structural and ignores subtype (it is null); `relay_ts` comes from the complete response first; `acknowledged` = Claude's reaction, `picked_up` = first non-notice reply; no fallback ladder.
- **Phase 3** is the deterministic executor; **Phase 4** the reconciler, limiter, fault injection (≤ 2 relays), single-source manifest and docs; **Phase 5** live acceptance (test channel via `allow_if_listed`, then #core-sync once notices are captured or Max accepts the risk); **Phase 6** tool-less extraction over Slack's speaker-separated lines, gated on `card_version ≥ 2`.
- **Decisions here:** interface names synced to the P1/P2 plans (they win); Phase 3 extends `Slack_standing_order_slack` instead of a new HTTP module; Phase 3 adds the executor wake (Phase 1 has none); verification uses V1–V5 through the x-p35 gate, never local `build2`/`make`/`dune`.
- **Open:** Review Focus 7's notes-only rule (recommendation given; confirm on P0-5); Phase 6 click → `with_request` path needs its own design and security review; the pre-filled [Edit] modal needs Max's OK; Claude's write destination is undecided (does not block); spec r5 must fold in the superseded-text list.
