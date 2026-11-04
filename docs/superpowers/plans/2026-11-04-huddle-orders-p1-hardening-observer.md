# Huddle Standing Orders, Phase 1 (Hardening and Observation): Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship delivery phase 1 of the Slack huddle standing-orders design: a metadata-only Slack event log (off by default), a Slackbot turn exclusion, a relay-thread silence gate, hardened thread history, a pure huddle classifier with an observer tap and a `slack_huddles` ledger, and fail-closed access-snapshot decoding.

**Architecture:** Everything hangs off the one ingress convergence point, `Slack_thread_ingress.handle_event`. Two side-taps, the event log and the huddle observer, run after the event-id dedup mark and before routing, and never change routing. The conversational path gains a Slackbot exclusion and a relay-thread silence gate, and history rendering becomes a hardened pure function. Phases 2 and 3 provide orders and fires; until then each dependency is a small typed interface (`Slack_huddle_watch.t`, `Slack_relay_guard.registry`) whose phase-1 implementation observes nothing, so a later phase replaces one function body.

**Tech Stack:** OCaml 5.1, dune, Lwt, Yojson.Safe, sqlite3-ocaml, Alcotest.

**Spec:** `docs/superpowers/specs/2026-11-04-slack-huddle-standing-orders-design.md` (r4+). This plan covers §10 item 1: §4 "Metadata log fields", §6.1 (`slack_huddles` only), §6.2, §6.4 "Snapshot" (the fail-closed decode only), §6.7, and §7 items 10–13 and 21. **§4 "Results so far" (Phase 0, 2026-11-04) is authoritative where older spec text disagrees**, including: §6.2 `working`/`unclassified` and "Own relay" (structural, Task 3, not a file-id match); §6.7 "a `file_share`"; §6.5 step 0 (a stub's room id from `canvas_metadata.originating_huddle_id`) and every other `canvas_metadata` use; and **§7.4 ownership** ("not Slackbot-owned", `originating_huddle_id` must match). Canvas and transcript are starter-owned and `canvas_metadata` is null, so no phase may reintroduce an ownership or `originating_huddle_id` check (see Out of scope for what phase 3 uses instead). Live notes: `/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/research/live-workspace-probe.md` (main checkout only; not in the worktree). Executors read the spec and this plan.

**Out of scope here:**
- Phases 2–4: the `Standing_order` work type, orders, fires, executor, reconciler, management tools and cards. Creator policy (default `member`, Max's decision) is phase 2.
- Phase 3 facts this plan must not contradict. `relay_ts` comes from the `completeUploadExternal` response (`files[].shares.private[C][0].ts`), and the `files.info` fallback is kept. Canvas provenance uses `files.info.shares[channel]` entries with `share_user_id = USLACKBOT` at the root and ready-reply ts, plus `huddle_transcript_file_id = room.transcript_file_id`. It never uses ownership, because the canvas and transcript are owned by the huddle starter and `canvas_metadata` is null. The transcript is read with the undocumented `files.info include_transcription=true` (Max approved), feature-detected. Phase 1 stores these ids as hints only.
- §6.6 fallback ladder: not needed (P0-3 and P0-4 passed). §6.9 manifest consolidation.
- §6.10 spoken requests are delivery phase 6. Phase 1 never reads transcript text and never acts on speech. Its only mention test (Task 3) matches the literal `<@UBOT>` / `<@UBOT|…>` token Slack inserts, so a typed or misheard name ("clawq" vs "Claude") never addresses clawq.
- Private channels: Max allows the private test channel locally with `private_channel_policy = "allow_if_listed"` plus `allow_private_channels`; B836 tracks the product follow-up. No code here.
- Live Phase 0 work and §10 item 1's "admin registration verified" (`/register_as_admin_otc`, an operational step for Max). See the checkpoint after Task 2.

## Phase 0 results applied

| P0 fact (spec §4) | Applied in |
|---|---|
| The relay message has `subtype` null (not `file_share`), `user` = clawq bot user, `bot_id` = clawq bot, `files[]`, and `text` = `initial_comment` | Fixture `own_relay` (no subtype, `bot_id = "BOWN"`). Task 3 `is_own_relay_message` keys on own bot user + `files[]` + footer and never reads `subtype`; tests pin both. Known gaps: the footer survives verbatim |
| `completeUploadExternal` already returns `shares.private[C][0].ts` | Out of scope (phase 3); `files.info` fallback kept |
| Root: `huddle_thread`, USLACKBOT, no `bot_id`, empty text, `files[] = [canvas]`, `attached_file_ids = []`, `room.transcript_file_id` present | Fixtures `huddle_root`/`room`. Task 1 room projection adds `transcript_file_id`. Task 5 adds `Huddle_seen.file_ids` and `room.transcript_file_id`. Task 6 adds `root_file_ids_json` and `room_transcript_file_id` |
| Ready reply: USLACKBOT, `subtype`/`bot_id` null, `thread_ts` = root ts, same canvas, about 20 s after the end | Fixture `notes_ready`. Task 5 explicit-null case. Task 6 notes-before-ended-edit case. Review Focus 1 |
| Canvas owned by the huddle starter, `canvas_metadata` null, shared by USLACKBOT | Fixture `canvas_file` (`user = "U0STARTER"`). Task 5 asserts ownership is never read. Out of scope (phase 3 provenance) |
| Transcript is `application/vnd.slack-huddle-transcript`, starter-owned; `huddle_transcription` only via `include_transcription=true` | Fixture `transcript_file` with planted lines. Tasks 1 and 4 prove its content never reaches the log or history. Out of scope |
| Claude reacts `eyes` and replies with `bot_id` and `bot_profile.app_id`, `username` null, no working signal. Acknowledged = reaction; picked_up = first reply that is not a known notice | Fixture `claude_reply` (no `username`). Task 5 `text_kind` becomes `Notice of notice \| Reply`; `working_username_prefixes` is removed |
| Real ids | Global Constraints (fake ids in fixtures); checkpoint config example |
| Max's decisions: creator policy `member`; §6.10 is phase 6; private channels are local config (B836) | Out of scope; checkpoint |

## Global Constraints

- Run everything from `/home/xertrov/src/clawq/.worktrees/huddle-orders`, one command at a time. Never run two `dune` commands in parallel. Builds and tests go through `build2` (the 2-core slice).
- **Always pass `--root .`; never use the Makefile's dune targets here.** Both `/home/xertrov/src/clawq/dune-workspace` and the worktree's exist, and dune takes the outermost as root, so `make test-suite`, `make fmt-check` and `make build-minimal` from this worktree silently build, test or format **master**. `make test` (Docker) mounts the worktree but `build2` caps only the docker client, so don't use it either. Steps below name these commands; expand them in full (shell functions don't persist between tool calls):

  ```bash
  # T <suite>   one suite, exact name from test/test_main.ml; one suite per command (the filter is a Str regex; no alternation)
  OCAMLRUNPARAM=o=120,O=120 build2 opam exec --switch=clawq-5.1 -- dune exec --root . -j2 test/test_main.exe -- test '^<suite>$'
  # FMT         first rewrites (may exit non-zero after promoting); the second must exit 0
  build2 opam exec --switch=clawq-5.1 -- dune build --root . -j2 @fmt --auto-promote
  build2 opam exec --switch=clawq-5.1 -- dune build --root . -j2 @fmt
  # QUICK       whole quick run (make test-nocontainer at 2 cores)
  OCAMLRUNPARAM=o=120,O=120 build2 opam exec --switch=clawq-5.1 -- dune exec --root . -j2 test/test_main.exe -- test --quick-tests
  # SLACK       every suite whose name starts with slack (prefix regex; covers slack_fix1_*, slack_finalfix, slack_transport_credentials, ...)
  OCAMLRUNPARAM=o=120,O=120 build2 opam exec --switch=clawq-5.1 -- dune exec --root . -j2 test/test_main.exe -- test '^slack'
  # CYCLES      must print nothing
  OCAMLRUNPARAM=o=120,O=120 build2 opam exec --switch=clawq-5.1 -- dune build --root . -j2 @check 2>&1 | grep -i "Dependency cycle"
  ```
  If output says `Entering directory '/home/xertrov/src/clawq'` or `No test suite matches`, the command lost `--root .` and ran against master. A held `_build/.lock`: run `./scripts/clean_stale_dune_locks.sh _build` and retry; if it persists another dune is running, so wait. `OCAMLRUNPARAM=o=120,O=120` is the Makefile's GC cap.
- All new code lives in `clawq_runtime_integrations` (`src/dune` modules list, next to `slack_huddle_tools` at line 533). The one exception is `src/slack_config_types.ml` (Task 1), which is core because `slack_config` needs its type; it goes in the `clawq_runtime_core` list before `runtime_config_types` (line 16).
- No new opam dependencies and no new CLI commands, so `src/command_bridge_min.ml` and `src/main_min.ml` are unchanged.
- `Slack_huddle_observer`, `Slack_relay_guard`, `Slack_huddle_classify` and everything they call must not depend on `Slack` or `Slack_thread_ingress` (a dune module cycle can OOM).
- Event log: **"No text is logged, with one exception: the full text of Claude's replies in the test channel only."** The exception needs an exact `C…`/`G…` channel id **and** an exact `B…` bot id; no wildcards. The flag defaults to off.
- The observer **"never calls Slack and never runs an LLM"**, **"returns unit, catches all exceptions and never changes routing"**, and **"ignores D channels"**.
- The classifier is **"a pure function over fixed top-level JSON paths only: never a recursive search, never metadata, blocks or attachments text."** Notes ready means **`user = USLACKBOT`, no `bot_id`, `thread_ts` present, and at least one `F…` file id**, as a hint only.
- The history file-key allowlist is exactly `id`, `name`, `title`, `mimetype`, `filetype`, `size`, `user`, `created`.
- `slack_huddles` rows are written only for observed channels and expire after **30 days**.
- Every B832/B833 invariant holds: authority only from verified ingress; synthetic and background turns get no huddle authority; decoding fails closed. All other B832/B833 suites still pass.
- No test makes a network call. Ingress tests use `Test_slack_threads.with_ingress` (a mock transport whose unknown methods fail the test).
- **Unit fixtures use fake ids** (clawq `T123`/`A123`/`UBOT`/`BOWN`; Claude `U0CLAUDE`/`B0CLAUDE`/`A0CLAUDE`; channel `C0TEST`; huddle starter `U0STARTER`). Real ids appear only in docs and config examples: clawq app `A0CA40GBWTX`, bot user `U0C9TU04K5M`, bot `B0CB3LLV07J`, team `T0CA9091NAD`; Claude user `U0CA6U11ADQ`, bot `B0CA0SS6NJF`, app `A08SF47R6P4`; test channel `C0CEN7X8KL6` (`#clawq-huddle-test`, private).
- Keep files under 1000 lines. New test files are used because `test/test_access_snapshot.ml` (1390 lines) and `test/test_slack_huddles.ml` (835) are already large. Use ocamlformat 0.28.1 and never fight it.
- Each task ends with FMT, then the listed `git add` and `git commit` on `feat/huddle-standing-orders`. Don't push, merge, restart a daemon or change live config yourself.
- Two phase-1 stubs are test seams held in a ref: `Slack_relay_guard.registry_source` and `Slack_huddle_watch.source`. A test that overrides one restores it in `Fun.protect`/`Lwt.finalize`. Production never assigns them; later phases replace the default function's body.

## Review Focus

1. **Out-of-order or repeated root edits, and notes-ready timing.** An older "not ended" edit arrives after the "ended" one; a delayed or retried edit from the *previous* use of a reused room id arrives after the new use was recorded (Socket Mode redelivery, 24 h HTTP retries); the ready reply (about 20 s after the end, P0) arrives before the root's ended edit. The ledger never regresses `has_ended`, `date_end` or `room_transcript_file_id`, never resets a row to an older use, and notes attached before the ended edit survive it. Tests: Task 6 `room lifecycle merge is monotonic`, `notes attach, newer wins`, `reused room id: newer start resets, older start is stale`.
2. **Malformed or hostile payload values:** `room` as a list, lowercase or 65-character ids, `files` not a list, numbers as digit `String`/`Intlit`, an object where a string belongs, explicit JSON `null` for `bot_id`/`subtype`/`username` (as P0 shows), a `Null` envelope. Each gives `Irrelevant` or a safe record, never raises, never changes routing. Tests: Task 1 `malformed payloads never raise`, Task 5 `posture events and malformed shapes are irrelevant` and `over-long ids and numeric strings`, Task 7 `store failure never escapes; malformed ignored`.
3. **A thread history read fails on a reply** (HTTP 5xx, or `not_in_channel` at HTTP 200). The gate can't see the root, so it consults only the registry: a recorded relay stays silent, and an unknown root gives the human a normal turn. Test: Task 3 `replies failure falls back to registry`.
4. **HTTP Events envelopes carry the legacy `token` and `authorizations`; interactive payloads carry `actions[].value`.** None is ever logged. Test: Task 1 `secrets and action values never logged`.
5. **The operator toggles `event_log.enabled` while the daemon runs.** It takes effect on the next event, without restart (CLAUDE.md: keep app state in sync with config). Test: Task 1 `tap off by default, logs, keeps routing, runtime toggle takes effect`.

---

## File Structure

| File | Status | Responsibility |
|---|---|---|
| `src/slack_config_types.ml` | new (core) | The single Slack id rule (`valid_id`) and the `event_log` config type, default and JSON codec. Keeps `runtime_config_types.ml` (1025 lines) from growing; phase 2's `standing_orders` config types can live here too |
| `src/slack_event_log.ml` | new | Pure metadata projection of an ingress payload, JSONL append and rotation, config-gated `observe` |
| `src/slack_relay_guard.ml` | new | Relay footer, structural own-relay detection, relay-registry interface (phase-1 stub), silence predicate |
| `src/slack_huddle_classify.ml` | new | Pure classifier from envelope to observation (§6.2) and notice text kinds |
| `src/slack_huddle_ledger.ml` | new | `slack_huddles` table: schema, upserts, stubs, TTL prune, reads |
| `src/slack_huddle_watch.ml` | new | Observed-channels interface and classifier context source (phase 2 implements `of_db`) |
| `src/slack_huddle_observer.ml` | new | Ingress tap: envelope gate → classify → ledger effects; never raises |
| `src/runtime_config_types.ml` | mod | One field: `slack_config.event_log : Slack_config_types.event_log` |
| `src/config_loader_channels.ml`, `src/runtime_config_json_sections.ml`, `src/config_set.ml`, `src/setup_slack.ml` | mod | Load, serialise, `config set` path and wizard default for `event_log` |
| `src/slack_huddles.ml` | mod | `valid_id` aliases `Slack_config_types.valid_id`; `slackbot_user_id` constant |
| `src/slack_thread_api.ml` | mod | `history` splits into `fetch_thread` + pure `render_history`; file allowlist; untrusted labelling |
| `src/slack_thread_ingress.ml` | mod | Two taps, Slackbot exclusion, silence gate |
| `src/daemon_runtime_loops.ml` | mod | Hourly cleanup also prunes `slack_huddles` |
| `src/access_snapshot.ml` | mod | Fail-closed unknown-work-type decode, `list_rows` |
| `test/test_slack_event_fixtures.ml` | new | Shared Slack payloads in the documented and P0-observed shapes |
| `test/test_slack_{event_log,turn_gate,relay_guard,history_hardening,huddle_classify,huddle_ledger,huddle_observer}.ml`, `test/test_access_snapshot_decode.ml` | new | One suite per task |
| `docs/connector-reference.md`, `docs/public/llms-full.txt`, `docs/src/content/docs/channels.mdx` | mod | Operator-facing behaviour phase 1 actually changes |

**Test registration (every task).** Add each new test module to `(modules …)` in `test/dune` after `test_slack_socket` (line 310), and its suite to `all_suites` in `test/test_main.ml` after `("slack_private_policy", …)` (line 288) as `("<suite>", Test_<suite>.suite)`. A suite is a list of `(name, `Quick, f)`; each backticked test name in a step is the exact Alcotest case name. Ingress tests return `unit Lwt.t` inside `with_ingress`, which runs `Lwt_main.run` itself. Test files open `Test_slack_huddle_fixtures` (`obj`, `str`, `arr`, `get`, `update`, `contains`, `check_bool`) and, for ingress, `Test_slack_threads` and `Lwt.Syntax`; `module F = Test_slack_event_fixtures`.

---

### Task 1: Metadata-only Slack event log (behind a config flag)

**Files:**
- Create: `src/slack_config_types.ml` (core; in `src/dune` directly before `runtime_config_types`, line 16; depends only on `Yojson.Safe` and `Logs`).
- Modify: `src/runtime_config_types.ml:134-150`: add only `event_log : Slack_config_types.event_log;` after `default_model`, with a one-line doc comment.
- Modify: `src/slack_huddles.ml:21-25`: `let valid_id = Slack_config_types.valid_id` (one Slack id rule; integrations may call core).
- Modify: `src/config_loader_channels.ml:95-163` (parse), `src/runtime_config_json_sections.ml:205-219` (`slack_json`), `src/config_set.ml:66-77` (slack schema), `src/setup_slack.ml:136-148` (`default_slack_config`).
- Modify: every full `slack_config` literal (the compiler names each: "Some record fields are undefined: event_log"), adding `event_log = Slack_config_types.default_event_log;`. Known: `test/test_channel_formats.ml` 83, 102, 121, 140, 499, 518; `test/test_commands.ml:388`; `test/test_config_set.ml:497`; `test/test_connector_tool_calls.ml:102`; `test/test_daemon.ml` 603, 1028, 1089, 1153, 1434; `test/test_service.ml` 468, 551; `test/test_slack.ml:41-53` (`make_config`); `test/test_slack_socket.ml:231`.
- Create: `src/slack_event_log.ml` (in `src/dune` after `slack_huddle_tools`). Modify: `src/slack_thread_ingress.ml:44-46` (the tap).
- Create: `test/test_slack_event_fixtures.ml`, `test/test_slack_event_log.ml` (suite `slack_event_log`).
- Docs: `docs/connector-reference.md:550-562` Config Fields row; `docs/public/llms-full.txt:441` channels.slack field list; `docs/src/content/docs/channels.mdx` "Key Features (Both Modes)" (around line 333), one bullet. `docs/setup-guide.md` stays unchanged: its Slack section is a setup example, not a field list.

**Interfaces:**
- Produces:
  ```ocaml
  (* Slack_config_types (core) *)
  val valid_id : string -> string -> bool  (* moved verbatim from Slack_huddles: 2..64 chars, first char in prefixes, all [A-Z0-9] *)
  type event_log_text_rule = { text_channel_id : string; text_bot_id : string }
  type event_log = { enabled : bool; capture_text_from : event_log_text_rule list }
  val default_event_log : event_log                  (* { enabled = false; capture_text_from = [] } *)
  val event_log_of_json : Yojson.Safe.t -> event_log
  val event_log_to_json : event_log -> Yojson.Safe.t
  (* Slack_event_log *)
  val file_name : string                  (* "slack-events.jsonl" *)
  val default_path : unit -> string       (* Dot_dir.sub file_name *)
  val default_max_bytes : int             (* 16 * 1024 * 1024 *)
  val record : now:float -> rules:Slack_config_types.event_log_text_rule list -> Yojson.Safe.t -> Yojson.Safe.t
  val append : ?max_bytes:int -> path:string -> Yojson.Safe.t -> unit
  val observe : ?path:string -> manager:Session.t -> Yojson.Safe.t -> unit
  ```
- JSON shape: `channels.slack.event_log = { "enabled": bool, "capture_text_from": [ { "channel": "C…|G…", "bot_id": "B…" } ] }`. `event_log_of_json`: `Null` (missing) → default; a non-object → default with a warning; missing or non-bool `enabled` → `false`; keeps only rules whose `channel` passes `valid_id "CG"` and `bot_id` passes `valid_id "B"`, warning once per dropped rule without printing values. `event_log_to_json` always emits both keys. The loader sets `event_log = Slack_config_types.event_log_of_json (s |> member "event_log")`; `slack_json` appends `("event_log", Slack_config_types.event_log_to_json s.event_log)`; the `config_set` slack schema gains `("event_log", O [ ("enabled", L); ("capture_text_from", L) ])` (`config set` already parses a leaf starting with `[` as JSON).
- Produces (fixtures, used by Tasks 2–7): the table in Step 1.

- [ ] **Step 1: Write the shared fixtures** in `test/test_slack_event_fixtures.ml` (opens `Test_slack_huddle_fixtures`). Each returns a `Yojson.Safe.t`. An optional argument without a default, or set to `None`, omits its key; the payload never carries `null` unless a test adds it with `update`. Shapes follow `/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/research/slack-events.md` §1.2–1.4 and §2.2 as corrected by P0; values marked *planted* exist only to prove they are dropped. Define them in dependency order (`canvas_file` before `huddle_root`). `\|` in the table is a Markdown escape for `|`.

| Fixture | Arguments (defaults) | Payload |
|---|---|---|
| `envelope` | `?team="T123" ?app="A123" ?id="Ev1" event` | `{type:"event_callback", team_id, api_app_id, event_id, event_time:1700000000, event}` |
| `room` | `?id="R0ROOM1" ?call_family=(Some "huddle") ?has_ended=false ?date_start=1700000000 ?date_end=0 ?thread_root_ts="1700000000.000100" ?attached=[] ?transcript=(Some "F0TRANSCRIPT") ()` | `{id, name:"Secret planning"` *planted*`, created_by:"U123", date_start, date_end, participant_history:["U123"], thread_root_ts, canvas_thread_ts:thread_root_ts, channels:["C0TEST"], has_ended, attached_file_ids:attached, call_family?, transcript_file_id?}`. P0: `attached_file_ids` is `[]` live |
| `huddle_root` | `?channel="C0TEST" ?ts="1700000000.000100" ?subtype=(Some "huddle_thread") ?room=(room ()) ?files=[canvas_file ()] ()` | `{type:"message", user:"USLACKBOT", text:"", ts, channel, room, files, blocks:[{type:"rich_text", text:"A huddle started"}]` *planted*`, subtype?}`; no `bot_id` |
| `root_changed` | `?channel="C0TEST" ?ts="1700003600.000200" message` | `{type:"message", subtype:"message_changed", hidden:true, channel, ts, event_ts:ts, message, previous_message:message}` |
| `thread_started_huddle` | `()` | `huddle_root ~subtype:(Some "thread_broadcast") ~ts:"1700000500.000300" ~room:(room ~id:"R0ROOM2" ~thread_root_ts:"1699990000.000100" ()) ()` plus `thread_ts:"1699990000.000100"` (third-party shape; P0-5 open) |
| `canvas_file` | `?id="F0CANVAS" ?user="U0STARTER" ()` | `{id, name:"Huddle AI notes", title:"Huddle AI notes & transcript", mimetype:"application/vnd.slack-docs", filetype:"quip", user, preview:"SECRET NOTES PREVIEW"}`. P0-1: owner is the huddle starter |
| `transcript_file` | `?id="F0TRANSCRIPT" ()` | `{id, mimetype:"application/vnd.slack-huddle-transcript", filetype:"huddle_transcript", user:"U0STARTER", huddle_transcription:{channel_id:"C0TEST", lines:[{contents:"SECRET SPOKEN LINE", start_time_ms:0, line_id:"L1", user_id:"U123"}]}}` (P0-1b shape, *planted* text) |
| `md_file` | `?id="F0MD" ()` | `{id, name:"notes.md", title:"notes", mimetype:"text/markdown", filetype:"markdown", user:"UBOT", size:10, created:1700000000, preview:"SECRET MD PREVIEW", preview_plain_text:"SECRET PLAIN", lines:3, title_blocks:[{text:"SECRET TITLE BLOCK"}], url_private:"https://files.slack.com/files-pri/T123-F0MD/notes.md"}` |
| `notes_ready` | `?channel="C0TEST" ?user="USLACKBOT" ?bot_id ?thread_ts=(Some "1700000000.000100") ?ts="1700003900.000400" ?files=[canvas_file ()] ?text="AI huddle notes and transcript are ready. <https://x.slack.com/docs/T123/F0CANVAS\|View AI Notes & Transcript>" ()` | `{type:"message", user, channel, ts, text, files, thread_ts?, bot_id?}`; no subtype |
| `claude_reply` | `?channel="C0TEST" ?bot_id="B0CLAUDE" ?app_id="A0CLAUDE" ?top_app_id ?username ?thread_ts=(Some "1700004000.000500") ?ts="1700004010.000600" ?text="ok" ()` | `{type:"message", user:"U0CLAUDE", bot_id, bot_profile:{app_id, name:"Claude"}, channel, ts, text, app_id:top_app_id?, username?, thread_ts?}`. P0-3: no `username`, no subtype |
| `reaction` | `?user="U0CLAUDE" ?item={type:"message", channel:"C0TEST", ts:"1700004000.000500"} ()` | `{type:"reaction_added", user, reaction:"eyes", item_user:"UBOT", item, event_ts:"1700004005.000700"}` |
| `posture` | `?channel="C0TEST" kind` | `{type:kind, channel, user:"U999", event_ts:"1700005000.000800"}` |
| `file_event` | `?id="F0CANVAS" ?with_file=true ?channel_id ?user_id kind` | `{type:kind, file_id:id, event_ts:"1700006000.001000", file:(canvas_file ~id ())` when `with_file`, `channel_id?, user_id?}`. Shapes of `file_shared`/`file_deleted`/`file_change` (P0-5 deletion check); the full canvas object is *planted* to prove name, title and preview are dropped |
| `human` | `?channel="C0TEST" ?thread_ts ?ts="1700002000.000900" ?user="U123" text` | `{type:"message", channel, user, ts, text, thread_ts?}` |
| `block_actions` | value | `{type:"block_actions", team:{id:"T123"}, api_app_id:"A123", user:{id:"U123"}, token:"VERIFICATION", actions:[{action_id:"clawq_stop_run", value:"opaque-secret"}]}` |
| `read_lines` | `path` | non-empty lines of the file; `[]` if it doesn't exist |
| `relay_nonce` (Task 3) | value | `"0123456789abcdef0123456789abcdef"` |
| `own_relay` (Task 3) | `?channel="C0TEST" ?user="UBOT" ?bot_id=(Some "BOWN") ?subtype ?ts="1700004000.000500" ?thread_ts ?files=[md_file ()] ?footer=(Some (Slack_relay_guard.footer ~nonce:relay_nonce)) ()` | `{type:"message", user, channel, ts, files, text:"<@U0CLAUDE> Huddle notes from this channel." ^ ("\n" ^ footer when present), bot_id?, subtype?, thread_ts?}`. P0-4: no subtype |
| `target` (Task 5) | value | `{ Slack_huddle_classify.target_user_id = "U0CLAUDE"; target_bot_id = "B0CLAUDE"; target_app_id = "A0CLAUDE" }` |

- [ ] **Step 2: Write the failing tests** in `test/test_slack_event_log.ml`. Helpers: `rule = { Slack_config_types.text_channel_id = "C0TEST"; text_bot_id = "B0CLAUDE" }`; `record ?(rules = []) e = Slack_event_log.record ~now:1.0 ~rules (F.envelope e)`; `ev r = get "event" r`; `has r s = contains (Yojson.Safe.to_string r) s`.
  - `human text never logged`: `record ~rules:[ rule ] (F.human "the secret plan")`: `has _ "secret plan" = false`; `ev` has `user = str "U123"` and `channel = str "C0TEST"`.
  - `room metadata and key names only`: `get "room" (ev (record (F.huddle_root ())))` has `id = str "R0ROOM1"`, `call_family = str "huddle"`, `has_ended = `Bool false`, `date_start = `Int 1700000000`, `attached_file_ids = arr []`, `transcript_file_id = str "F0TRANSCRIPT"`; its `keys` list contains `str "participant_history"`; the record contains neither `"Secret planning"` nor `"A huddle started"`.
  - `message_changed nested metadata`: `record (F.root_changed (F.huddle_root ~room:(F.room ~has_ended:true ~date_end:1700003600 ()) ()))`: `ev.message.room.has_ended = `Bool true`; `ev.previous_message <> `Null`.
  - `file and bot metadata only`:
    - `ev (record (F.notes_ready ()))` has `files = arr [ obj [ ("id", str "F0CANVAS"); ("mimetype", str "application/vnd.slack-docs"); ("filetype", str "quip"); ("user", str "U0STARTER") ] ]`; the record contains no `"SECRET"`, `"Huddle AI notes"` or `"are ready"`.
    - `record (update (F.human "x") [ ("files", arr [ F.transcript_file () ]) ])` has `files = arr [ obj [ ("id", str "F0TRANSCRIPT"); ("mimetype", str "application/vnd.slack-huddle-transcript"); ("filetype", str "huddle_transcript"); ("user", str "U0STARTER") ] ]` and no `"SECRET SPOKEN"` or `"huddle_transcription"`.
    - `ev (record (F.claude_reply ~top_app_id:"A0CLAUDE" ~username:"Claude [Ingest]" ()))` has `bot_id = str "B0CLAUDE"`, `bot_profile_app_id = str "A0CLAUDE"`, `app_id = str "A0CLAUDE"`, `username = str "Claude [Ingest]"`.
    - Neither `F.claude_reply ()` nor `update (F.claude_reply ()) [ ("username", `Null) ]` yields a `username` key (P0-3 shape).
    - `ev (record (F.human "x"))` has none of the keys `item`, `bot_profile_app_id`, `files`, `room`, `bot_id`: absent keys are omitted, never `null` or empty.
  - `reaction metadata`: `ev (record (F.reaction ()))` has `reaction = str "eyes"`, `user = str "U0CLAUDE"`, `item_user = str "UBOT"`, `item = obj [ ("type", str "message"); ("ts", str "1700004000.000500"); ("channel", str "C0TEST") ]`.
  - `file events keep ids only`:
    - `ev (record (F.file_event ~channel_id:"C0TEST" ~user_id:"U0STARTER" "file_shared"))` has `type = str "file_shared"`, `file_id = str "F0CANVAS"`, `file_obj_id = str "F0CANVAS"`, `channel_id = str "C0TEST"`, `user_id = str "U0STARTER"`; the record contains none of `"Huddle AI notes"`, `"SECRET"` or `"preview"`.
    - `ev (record (F.file_event ~with_file:false "file_deleted"))` has `file_id = str "F0CANVAS"` and `event_ts`, and no `file_obj_id` key.
  - `text exception exact channel and bot`: with `t ?(rules = [ rule ]) ?(channel = "C0TEST") ?(bot_id = "B0CLAUDE") () = has (record ~rules (F.claude_reply ~channel ~bot_id ~text:"Spend limit reached" ())) "Spend limit reached"`: `t ()` true; `t ~channel:"C0OTHER" ()`, `t ~bot_id:"B0OTHER" ()` and `t ~rules:[] ()` false; `has (record ~rules:[ rule ] (F.root_changed (F.claude_reply ~text:"Spend limit reached" ()))) "Spend limit reached"` true.
  - `secrets and action values never logged`: `Slack_event_log.record ~now:1.0 ~rules:[] (update (F.envelope (F.human "x")) [ ("token", str "VERIFICATION"); ("authorizations", arr [ obj [ ("user_id", str "UBOT") ] ]) ])` contains neither `"VERIFICATION"` nor `"authorizations"`. `Slack_event_log.record ~now:1.0 ~rules:[] F.block_actions` has `envelope.type = str "block_actions"`, `envelope.team_id = str "T123"`, and no `"opaque-secret"` or `"VERIFICATION"`.
  - `malformed payloads never raise`: `record` with `[ rule ]` returns for `` `Null `` and for `obj [ ("type", str "event_callback"); ("event", str "x") ]`. `F.envelope (obj [ ("type", str "message"); ("room", arr []); ("files", str "F1"); ("user", obj [ ("real_name", str "Leaky") ]); ("item", `Int 3) ])` gives no `"Leaky"`.
  - `append rotation and permissions`: in `Test_helpers.with_temp_home (fun dir -> …)` with `path = Filename.concat dir "events.jsonl"`, two `append ~path` calls give 2 lines and `(Unix.stat path).st_perm land 0o777 = 0o600`. Then `append ~max_bytes:1 ~path` leaves 2 lines in `path ^ ".1"` and 1 in `path`.
  - `config load, roundtrip and set path`, and the tap test:

```ocaml
let test_config () =
  let base = [ ("bot_token", str "xoxb-x"); ("app_token", str "xapp-x"); ("socket_mode", `Bool true) ] in
  let parse fields = Option.get (Config_loader.parse_config ~resolve_secrets:false
                                   (obj [ ("channels", obj [ ("slack", obj fields) ]) ])).channels.slack in
  let r c b = obj [ ("channel", str c); ("bot_id", str b) ] in
  let sc = parse (base @ [ ("event_log", obj [ ("enabled", `Bool true); ("capture_text_from",
             arr [ r "C0TEST" "B0CLAUDE"; r "*" "B0CLAUDE"; r "D0DM" "B0CLAUDE"; r "C0TEST" "U0CLAUDE" ]) ]) ]) in
  check_bool "enabled; only exact C/G + B rules" true (sc.event_log.enabled && sc.event_log.capture_text_from = [ rule ]);
  check_bool "default off" true ((parse base).event_log = Slack_config_types.default_event_log);
  check_bool "non-object is default" true ((parse (base @ [ ("event_log", str "on") ])).event_log = Slack_config_types.default_event_log);
  let cfg = { Runtime_config.default with channels = { Runtime_config.default.channels with slack = Some sc } } in
  let round = Config_loader.parse_config ~resolve_secrets:false (Runtime_config.to_json cfg) in
  check_bool "json roundtrip" true ((Option.get round.channels.slack).event_log = sc.event_log);
  check_bool "settable" true (Config_set.validate_set_path [ "channels"; "slack"; "event_log"; "enabled" ] Config_set.config_schema)

let set_log (base : fixture) enabled =
  let cfg = base.manager.config in
  let sc = Option.get cfg.channels.slack in
  base.manager.config <- { cfg with channels = { cfg.channels with
    slack = Some { sc with event_log = { Slack_config_types.enabled; capture_text_from = [] } } } }
let log () = F.read_lines (Slack_event_log.default_path ())

let test_tap () =
  with_ingress (fun base calls _ seen _ run ->
      let* _ = run (F.huddle_root ()) in
      check_bool "off by default" true (log () = []);
      set_log base true;
      let* _ = run ~id:"Ev2" (F.huddle_root ~ts:"1700000001.000100" ()) in
      check_bool "huddle root makes no Slack call" true (!calls = []);
      let* _ = run ~id:"Ev3" (message "please help") in
      check_bool "human turn still runs" true (!seen <> []);
      Alcotest.(check int) "both enabled events logged" 2 (List.length (log ()));
      check_bool "turn text not logged" false (List.exists (fun l -> contains l "please help") (log ()));
      set_log base false;
      let* _ = run ~id:"Ev4" (F.huddle_root ~ts:"1700000002.000100" ()) in
      Alcotest.(check int) "runtime toggle takes effect" 2 (List.length (log ()));
      Lwt.return_unit)
```

  The tap test's Alcotest case name is `tap off by default, logs, keeps routing, runtime toggle takes effect`. Register `test_slack_event_fixtures`, `test_slack_event_log` and suite `slack_event_log`.

- [ ] **Step 3: Run to verify it fails.** `T slack_event_log` → FAIL at compile time: `Unbound module Slack_event_log` (or `Slack_config_types`).

- [ ] **Step 4: Implement the config record.** `src/slack_config_types.ml` to the Interfaces; register it in core before `runtime_config_types`; add the field; make `Slack_huddles.valid_id` the alias; update the loader, `slack_json`, the `config_set` schema, `setup_slack.default_slack_config` and every literal under Files.

- [ ] **Step 5: Implement `src/slack_event_log.ml`** with these exact projection rules:
  - **Envelope:** copy the scalars `type`, `team_id`, `api_app_id`, `event_id`, `event_time`. For a non-`event_callback` payload, take `team_id` from `team.id` when the top-level field is absent. Copy nothing else (no `token`, `authorizations`, `actions`, `user`).
  - **Message-level projection** of `event`, and one level deep of `message` and `previous_message`. Every key is emitted only when its source is present with the stated shape; never `null`, `{}` or `[]` as a placeholder.
    - Scalars only (`String`, `Bool`, `Int`, `Intlit`, `Float`; `Null` is not a scalar): `type subtype user bot_id app_id username channel channel_type ts thread_ts event_ts hidden deleted_ts reaction item_user file_id channel_id user_id`. The last three are the ids `file_shared`/`file_deleted`/`file_change` carry (spec §4 "file ids"; P0-5 deletion).
    - `file_obj_id` when `file` is an object with a scalar `id` (never any other key of it).
    - `bot_profile_app_id` when `bot_profile` is an object with a scalar `app_id`.
    - `item` when `item` is an object: its present scalars `type`, `ts`, `file`, `channel`, in that order.
    - `files` when a list: object elements only, each with its scalars `id`, `mimetype`, `filetype`, `user`, in that order.
    - `room` when an object: `keys` (sorted key names), the scalars `id`, `call_family`, `has_ended`, `date_start`, `date_end`, `thread_root_ts`, `canvas_thread_ts`, `transcript_file_id`, and `attached_file_ids` restricted to string elements (a present empty list is copied as `[]`; P0 roots have one).
    - `text` only when the **outer** event `channel` equals some `rule.text_channel_id` and this message's `bot_id` equals that same rule's `text_bot_id`.
  - **Output:** `{"logged_at": now, "envelope": {...}, "event": {...}}`; omit `event` when it is not an object.
  - **`append`:** if `path` exists with size ≥ `max_bytes` (default `default_max_bytes`), `Sys.rename path (path ^ ".1")`; then `open_out_gen [Open_wronly; Open_append; Open_creat] 0o600` and write one line.
  - **`observe`:** read `(Session.get_config manager).channels.slack` on every call; act only when `event_log.enabled`; `ignore (Dot_dir.ensure ())` (it returns the dir; dev builds use `-strict-sequence`); append `record ~now:(Unix.gettimeofday ()) ~rules` to `Option.value path ~default:(default_path ())`. Catch every exception: `Logs.warn "Slack event log write failed: %s"` with `Printexc.to_string`.

- [ ] **Step 6: Wire the tap.** In `src/slack_thread_ingress.ml`, immediately after `Channel_util.Lru_dedup.mark event_dedup identity;` (line 45) and before the `block_actions` branch: `Slack_event_log.observe ~manager:session_manager json;`.

- [ ] **Step 7: Run to verify they pass,** one at a time: `T slack_event_log`, `T config_set`, `T setup_slack`, `T slack_huddles` (the `valid_id` alias). Expected: all PASS.

- [ ] **Step 8: Update the docs.**
  - `connector-reference.md` Config Fields row: `event_log` | Phase 0 diagnostic: metadata-only JSONL of every Slack event at `$CLAWQ_HOME/slack-events.jsonl` (0600, rotates at 16 MiB to `.1`); `capture_text_from` adds full text only for an exact `{channel, bot_id}` pair | `{"enabled": false, "capture_text_from": []}`. Add the same to `llms-full.txt:441`.
  - `channels.mdx` Key Features: "**Diagnostic event log** (off by default) --- `event_log.enabled` writes one metadata-only JSON line per Slack event (ids, types, timestamps, room and file ids; no message text) to `slack-events.jsonl`. `capture_text_from` adds text only for an exact channel and bot id pair. Toggling it takes effect without a restart."

- [ ] **Step 9: FMT, then commit.** `git add src/slack_config_types.ml src/runtime_config_types.ml src/slack_huddles.ml src/config_loader_channels.ml src/runtime_config_json_sections.ml src/config_set.ml src/setup_slack.ml src/slack_event_log.ml src/slack_thread_ingress.ml src/dune test/ docs/connector-reference.md docs/public/llms-full.txt docs/src/content/docs/channels.mdx`; `git commit -m "feat(slack): metadata-only event log behind channels.slack.event_log (default off)"`.

---

### Task 2: Exclude Slackbot from conversational turns

**Files:**
- Modify: `src/slack_huddles.ml` (constant near the helpers, lines 1-30) and `src/slack_thread_ingress.ml:52-55` (the turn gate).
- Create: `test/test_slack_turn_gate.ml` (suite `slack_turn_gate`).
- Docs: in `docs/connector-reference.md` §"Native agent threads" (after line 276) and the `llms-full.txt` Slack task/thread lifecycle bullets (around line 1072), add: "Slackbot (`USLACKBOT`) messages never start tasks."

**Interfaces:**
- Produces: `Slack_huddles.slackbot_user_id : string` (= `"USLACKBOT"`), used by Task 5.

- [ ] **Step 1: Write the failing tests.** Each runs in `with_ingress (fun _ calls _ seen _ run -> …)`.
  - `slackbot thread reply starts no turn`: `run (update (message ~user:"USLACKBOT" ~thread:"100.000001" ~ts:"100.000009" "AI huddle notes are ready") [ ("files", arr [ F.canvas_file () ]) ])` → `check_bool "notes-ready reply: no Slack call" true (!calls = [])` and `!seen = []`.
  - `slackbot mention starts no turn`: `run (message ~kind:"app_mention" ~user:"USLACKBOT" "<@UBOT> hello")` → `!calls = []` and `!seen = []`.
  - `human message still starts a turn`: `run (message "please help")` → `!seen <> []`.

- [ ] **Step 2: Run to verify it fails.** `T slack_turn_gate` → FAIL with `notes-ready reply: no Slack call`: today the reply reaches `Slack_thread_api.authorize`, which calls `auth.test` and `users.info`. P0 saw exactly this live (daemon log 15:39:32, refused only by `private_channel_policy`).

- [ ] **Step 3: Implement.** `let slackbot_user_id = "USLACKBOT"` in `slack_huddles.ml`; add `&& field "user" event <> slackbot_user_id` to the turn-gate condition. Slackbot events still reach both taps, which run before the gate.

- [ ] **Step 4: Run to verify they pass:** `T slack_turn_gate`, then `T slack_threads`, then `QUICK`. Expected: all PASS. `QUICK` is required here because Tasks 1–2 go live at the checkpoint: Task 1 changes the `slack_config` record (about 15 test files build literals) and taps ingress, and Task 2 changes the turn gate that other suites (`slack_fix1_*`, `slack_finalfix`, `slack_transport_credentials`, …) drive through the ingress fixture.

- [ ] **Step 5: FMT, then commit.** `git add src/slack_huddles.ml src/slack_thread_ingress.ml test/test_slack_turn_gate.ml test/dune test/test_main.ml docs/connector-reference.md docs/public/llms-full.txt`; `git commit -m "fix(slack): Slackbot messages never start conversational turns"`.

---

### Checkpoint: land the event log for Phase 0 (after Task 2)

Phase 0 has already settled P0-1, 1b, 2, 3, 4, 6 and 7 with direct API calls (spec §4 "Results so far"). **What remains needs this event log running in the daemon:** P0-5 (event shapes for a thread-started huddle, a participant-shared canvas, room-id reuse and deletion) and Claude's notice wordings. They finalise Task 5's rules and fill its matchers. Tasks 1 and 2 don't depend on Tasks 3–8, and Task 2 fixes a live bug.

- [ ] **Executor: stop and report** the two commit hashes and the Task 1–2 results (including the Task 2 `QUICK` pass) to the coordinator, then continue with Task 3 on the branch. Don't merge, restart or reconfigure anything.
- [ ] **Coordinator or Max lands Tasks 1–2 on master:** in `/home/xertrov/src/clawq`, `git cherry-pick <task1> <task2>` (a merge would bring later work in progress), then `build2 make build` and `build2 make test-nocontainer` (the main checkout is the outermost dune root, so the Makefile targets master correctly there). **`test-nocontainer` must PASS before the daemon is restarted**; on a failure, stop and fix on the branch, not on master.
- [ ] **Coordinator or Max restarts the xsm daemon on master** to load them. It was last restarted on master at 14:53 AEDT on 2026-11-04 (spec §11). If the orphaned `main.exe ec-run --daemon-mode` (pid 3924361) is still running, inspect it with `ps -o pid,lstart,cmd -p 3924361` and stop it only with Max's agreement. Check `clawq service status`, then `clawq service start`. Installing B829 supervision (`clawq service install`) is Max's call.
- [ ] **Coordinator or Max turns the log on for the test channel only.** It takes effect on the next event:
  ```bash
  clawq config set channels.slack.event_log.capture_text_from '[{"channel":"C0CEN7X8KL6","bot_id":"B0CA0SS6NJF"}]'
  clawq config set channels.slack.event_log.enabled true
  ```
  The tap runs before authorization, so it logs the private test channel even under `private_channel_policy: deny`. Letting clawq *answer* there is Max's local config (`allow_if_listed`, B836) and is not needed for the log. To check: send one human message in the test channel; `~/.clawq/slack-events.jsonl` exists with mode 0600 and holds that event's metadata but not its text.
- Until this checkpoint is done, P0-5 and the notice captures can't start. Record that as a blocker for Max, not a failure of this plan.

---

### Task 3: Relay-thread silence gate (structural detection + stub registry)

**Files:**
- Create: `src/slack_relay_guard.ml` (add to `src/dune`).
- Modify: `src/slack_thread_api.ml:249-348`: replace `history` with `fetch_thread` + `render_history` + `root_message` (its only caller is `slack_thread_ingress.ml:105`). Modify `src/slack_thread_ingress.ml:89-125` (the gate).
- Modify: `test/test_slack_event_fixtures.ml` (add `relay_nonce`, `own_relay`). Create: `test/test_slack_relay_guard.ml` (suite `slack_relay_guard`).
- Docs: none in phase 1; the registry is empty and clawq posts no relays until phase 3. Phase 3 adds to the `connector-reference.md` native-threads section and the `llms-full.txt` lifecycle bullets: "Clawq starts no task in a thread rooted at one of its own standing-order relay posts unless the reply mentions clawq."

**Interfaces:**
- Produces (`Slack_relay_guard`):
  ```ocaml
  val footer_prefix : string                      (* "clawq-relay/1 " *)
  val valid_nonce : string -> bool                (* exactly 32 chars of [0-9a-f] *)
  val footer : nonce:string -> string             (* footer_prefix ^ nonce; Invalid_argument "Slack_relay_guard.footer" if not valid_nonce *)
  val nonce_of_text : string -> string option     (* split on '\n', String.trim each, drop empty lines; the last
                                                     remaining line must equal footer_prefix ^ n with valid_nonce n *)
  val file_ids : Yojson.Safe.t -> string list     (* files[].id passing valid_id "F", deduped, in order *)
  val is_own_relay_message : bot:string -> Yojson.Safe.t -> bool
     (* valid_id "UW" bot && user = bot && file_ids <> [] && nonce_of_text text <> None; never reads subtype or bot_id *)
  val mentions : bot:string -> string -> bool     (* text contains "<@"^bot^">" or "<@"^bot^"|" *)
  type registry = {
    recorded_relay : channel:string -> ts:string -> bool;          (* DB: a fire's relay_ts *)
    in_flight_file : channel:string -> file_id:string -> bool;     (* DB: an in-flight fire's uploaded_file_ids *)
    unconfirmed_since : channel:string -> float option;            (* DB: earliest upload start among posting_unknown / awaiting-relay_ts fires *)
  }
  val no_relays : registry                        (* all false / None *)
  val phase1_registry : Session.t -> registry     (* fun _ -> no_relays; phase 3 replaces this body *)
  val registry_source : (Session.t -> registry) ref   (* default phase1_registry; test seam only *)
  val current_registry : Session.t -> registry    (* !registry_source manager *)
  val silent : registry:registry -> bot:string -> channel:string -> root_ts:string ->
               root:Yojson.Safe.t option -> text:string -> bool
  ```
  `silent` is false whenever `mentions ~bot text`. Otherwise it is true iff `registry.recorded_relay ~channel ~ts:root_ts`, or `root = Some m` and one of: `is_own_relay_message ~bot m`; some id in `file_ids m` satisfies `in_flight_file`; or `user m = bot`, `file_ids m <> []`, `unconfirmed_since ~channel = Some t`, `Slack_huddles.valid_ts (ts m)` and `float_of_string (ts m) >= t`. An invalid root ts makes that last clause false; `silent` never raises.

  **`is_own_relay_message` is the only definition of "own relay".** Task 4's history labelling and Task 5's classifier call it and add nothing except the classifier's top-level check. P0-4 settled the live shape: `subtype` null, `user` = clawq's bot user, `bot_id` = clawq's bot, `files[]`, `text` = `initial_comment`. The predicate keys on the own bot user plus `files[]` plus the fixed footer. It **never reads `subtype`**, so neither `null` nor `file_share` matters. It does not also require `bot_id`: Slack sets `user` and `bot_id` together from the posting token, so `bot_id` adds no proof. Requiring it would only narrow the fail-safe uses, so that a relay missing `bot_id` would be rendered as `assistant`. Don't add a requirement in one caller only.
- Produces (`Slack_thread_api`), replacing `history`:
  ```ocaml
  type thread = { messages : Yojson.Safe.t list; overflow : bool; limited : bool; next_cursor : string; has_more : bool }
  val fetch_thread : manager:Session.t -> token:string -> route:Slack_thread_route.t -> current:string ->
                     (thread, string) result Lwt.t
  val root_message : route:Slack_thread_route.t -> thread -> Yojson.Safe.t option   (* List.find_opt (ts = route.root) *)
  val render_history : route:Slack_thread_route.t -> bot:string -> current:string -> thread -> string  (* Task 4 adds ~untrusted *)
  ```
  `fetch_thread` holds the current paging loop under `with_route`, with the same calls and limits, and post-processes **in the current order** (`slack_thread_api.ml:268-285`): `overflow = List.length messages > 500`; take the first 500 raw messages; keep a valid ts that is not `current`; `List.sort_uniq` by float ts. That order keeps output byte-for-byte unchanged for threads over 500 messages and for invalid or duplicate ts. `overflow`, `limited`, `next_cursor` (last page's `response_metadata.next_cursor`) and `has_more` (last page's flag) keep their meaning. `render_history` is the current budget, role and `complete` logic, byte-for-byte unchanged in output.

- [ ] **Step 1: Write the failing tests.** Helpers: `module G = Slack_relay_guard`; `reg ?(recorded = false) ?(in_flight = []) ?since ()` builds a `G.registry` answering `recorded`, `List.mem file_id in_flight` and `since`; `silent ?(registry = G.no_relays) ?(root = Some (F.own_relay ())) text = G.silent ~registry ~bot:"UBOT" ~channel:"C0TEST" ~root_ts:"1700004000.000500" ~root ~text`.
  - `relay footer format`, with `n = F.relay_nonce`:
    - `G.nonce_of_text ("intro\n" ^ G.footer ~nonce:n ^ "\n  ") = Some n`;
    - `None` for each of `"clawq-relay/1 " ^ String.uppercase_ascii n`, `"clawq-relay/1 abc"`, `G.footer ~nonce:n ^ "\nmore text"` and `""`;
    - `G.footer ~nonce:"bad"` raises `Invalid_argument "Slack_relay_guard.footer"`.
  - `silence predicate`:
    - **true:**
      - `silent "thanks"`;
      - `silent "<@U0CLAUDE> go on"` (mentioning Claude stays silent);
      - `silent "clawq, summarise"` (a typed name is not a mention);
      - `silent ~root:(Some (F.own_relay ~subtype:"file_share" ())) "x"` (subtype never read);
      - `silent ~registry:(reg ~recorded:true ()) ~root:None "x"`;
      - `silent ~registry:(reg ~in_flight:[ "F9" ] ()) ~root:(Some (F.own_relay ~footer:None ~files:[ F.md_file ~id:"F9" () ] ())) "x"`;
      - `silent ~registry:(reg ~since:1700003000. ()) ~root:(Some (F.own_relay ~footer:None ())) "x"`.
    - **false:**
      - `silent "<@UBOT> summarise"`;
      - `silent "<@UBOT|clawq> hi"`;
      - `silent ~root:(Some (F.human "hi")) "x"`;
      - `silent ~root:(Some (F.own_relay ~user:"U123" ())) "x"` (a human-authored footer is no relay);
      - `~root:(Some (F.own_relay ~footer:None ()))`;
      - `~root:(Some (F.own_relay ~files:[] ()))`;
      - `~registry:(reg ~since:1700005000. ()) ~root:(Some (F.own_relay ~footer:None ()))`;
      - `~registry:(reg ~since:1700003000. ()) ~root:(Some (F.own_relay ~footer:None ~ts:"bad" ()))` (invalid root ts: false, no exception);
      - `silent ~root:None "x"` (the stub registry knows no relays).
    - Also `G.is_own_relay_message ~bot:"UBOT" (F.own_relay ())` and `(F.own_relay ~bot_id:None ())` are both true.
  - Ingress tests use `relay_root handler`. It wraps `!handler` so that `conversations.replies` returns `ok [ ("messages", arr [ F.own_relay ~channel:"C123" ~ts:"100.000001" (); message ~ts:"100.000002" "earlier reply" ]); ("has_more", `Bool false) ]`, and it delegates every other method.
    - `relay thread reply starts no turn`: `run (message ~thread:"100.000001" ~ts:"100.000004" "what now")` gives `!seen = []`, no `chat.postMessage` or `agents.sessions.setStatus` in `!calls`, and exactly 1 `conversations.replies` call.
    - `explicit mention in relay thread runs`: the same with `"<@UBOT> summarise"` gives `!seen <> []`.
  - `replies failure falls back to registry`, using the two helpers below. `reply id ts = run ~id (message ~thread:"100.000001" ~ts "still there?")`.
    - Under `failing_replies ~status:500 ~error:"internal_error"`:
      - `with_registry (reg ~recorded:true ()) (fun () -> reply "Ev1" "100.000004")` gives `!seen = []`;
      - then `reply "Ev2" "100.000005"` with no override gives `!seen <> []`.
    - Then `seen := []`. Under `failing_replies ~status:200 ~error:"not_in_channel"`, `with_registry (reg ~recorded:true ()) (fun () -> reply "Ev3" "100.000006")` gives `!seen = []`.
    - Event ids and ts differ per run, because ingress dedups both.

```ocaml
(* HTTP-level failure for conversations.replies only; the with_ingress fixture always answers 200, so wrap the
   transport (Slack_transport has no .mli; key and the record are public). *)
let failing_replies ~status ~error f =
  match Lwt.get Slack_transport.key with
  | None -> Alcotest.fail "with_ingress transport missing"
  | Some outer ->
      let get ~uri ~headers =
        if Filename.basename (Uri.path (Uri.of_string uri)) = "conversations.replies" then
          Lwt.return (status, Yojson.Safe.to_string (obj [ ("ok", `Bool false); ("error", str error) ]))
        else outer.Slack_transport.get ~uri ~headers in
      Slack_transport.with_http { outer with Slack_transport.get = get } f
let with_registry registry f =
  let old = !G.registry_source in
  G.registry_source := (fun _ -> registry);
  Lwt.finalize f (fun () -> G.registry_source := old; Lwt.return_unit)
```

- [ ] **Step 2: Run to verify it fails.** `T slack_relay_guard` → FAIL at compile time: `Unbound module Slack_relay_guard`.

- [ ] **Step 3: Implement `src/slack_relay_guard.ml`** to the Interfaces. It depends only on `Slack_huddles` helpers and `Session`.

- [ ] **Step 4: Split `Slack_thread_api.history`** into `fetch_thread`, `root_message` and `render_history` as specified, and remove `history`.

- [ ] **Step 5: Wire the gate into `Slack_thread_ingress`.** This replaces the `let* context = …` block at lines 101–107. The `handle_text` consumption stays first, because phase 2's pause parser also runs before this gate. From line 108 on, everything stays as it is: the `accepting` check, the "Slack thread context unavailable" fallback, `app_context` and run creation.
  - `let reply = root <> ts`. Fetch the thread **once**. Skip the fetch only when `command && not reply`, which keeps a top-level `/`-command free of history calls as today. A `/`-command posted as a reply now reads the thread once, so the gate can see the root.
  - `registry = Slack_relay_guard.current_registry session_manager`. `root_message` comes from `Slack_thread_api.root_message ~route t` on a successful fetch, and is `None` otherwise.
  - If `reply && Slack_relay_guard.silent ~registry ~bot ~channel ~root_ts:root ~root:root_message ~text`, log `Logs.info "Slack relay thread %s/%s: no turn without an explicit mention"` (channel, root) and return `"ok"`.
  - Otherwise `context` is:
    - `Ok ""` for a command or a skipped fetch;
    - `Ok (render_history …)` for a fetched thread;
    - `Error e` for a failed fetch, which flows into the existing fallback.

  Top-level messages make exactly the same Slack calls as before.

- [ ] **Step 6: Run to verify they pass:** `T slack_relay_guard`, then `SLACK`, then `CYCLES`. Expected: all PASS and no cycle output; `paginated chronological deduplicated history` still sees exactly 2 replies calls, and `slack_fix1_boundaries` `test_mixed_append` (a `/followup-append` reply, which now reads the thread once) still passes.

- [ ] **Step 7: FMT, then commit.** `git add src/slack_relay_guard.ml src/slack_thread_api.ml src/slack_thread_ingress.ml src/dune test/`; `git commit -m "feat(slack): stay silent in standing-order relay threads unless mentioned"`.

---

### Task 4: History hardening: file-key allowlist and untrusted relay labelling

**Files:**
- Modify: `src/slack_thread_api.ml` (`render_history`), `src/slack_relay_guard.ml` (add `untrusted_in_history`), `src/slack_thread_ingress.ml` (pass `~untrusted`).
- Create: `test/test_slack_history_hardening.ml` (suite `slack_history_hardening`).
- Docs, for the file-key allowlist only, which changes behaviour now. In `llms-full.txt:1072`, after "only messages from this app's verified bot identity are labeled assistant content.", add the sentence below. Add the same sentence to `connector-reference.md` §"Native agent threads". The relay-labelling sentence waits for phase 3.
  > Attached-file metadata in thread context is limited to id, name, title, mimetype, filetype, size, user and created (no previews, lines or transcriptions).

**Interfaces:**
- Consumes: `Slack_relay_guard.{is_own_relay_message; registry}` and `Slack_thread_api.thread` (Task 3). `Slack_thread_api` now depends on the guard, which creates no cycle because the guard depends only on `Slack_huddles` and `Session`.
- Produces:
  ```ocaml
  val Slack_thread_api.history_file_keys : string list   (* ["id"; "name"; "title"; "mimetype"; "filetype"; "size"; "user"; "created"] *)
  val Slack_thread_api.project_file : Yojson.Safe.t -> Yojson.Safe.t option  (* `Assoc -> its allowlisted scalar keys; else None *)
  val Slack_thread_api.render_history :
    route:Slack_thread_route.t -> bot:string -> current:string -> untrusted:(Yojson.Safe.t -> bool) -> thread -> string
     (* role = if Slack_relay_guard.is_own_relay_message ~bot m || untrusted m then "untrusted_source"
               else if user m = bot then "assistant" else "untrusted_source";
        extras "files" = List.filter_map project_file, omitted when empty; "blocks"/"attachments" unchanged *)
  val Slack_relay_guard.untrusted_in_history : registry:registry -> channel:string -> Yojson.Safe.t -> bool
     (* registry.recorded_relay ~channel ~ts:(field "ts" m): the secondary DB check only *)
  ```
  **Structural relay recognition lives in `render_history` itself, so every caller gets it** (§6.7: the DB lookup is secondary and survives pruning and other instances). No caller can render clawq's own relay as `assistant`. `~untrusted` only *adds* the registry lookup; `fun _ -> false` still labels a structural relay `untrusted_source`.
- **B833 tests whose expectations change: none.** Checked `slack_thread_edges` (`paginated chronological deduplicated history`, `own assistant versus untrusted other bots`, `limited empty history coverage`) and `slack_threads` (`authorized thread bootstrap and cleanup`). None asserts on file metadata. The own-bot message in `own assistant versus untrusted other bots` has no files or footer, so it stays `assistant`. The deliberate change is that human file previews and snippet text no longer reach history (§6.7, §7.21).

- [ ] **Step 1: Write the failing tests.** Helpers (`module G = Slack_relay_guard`):
  - `route = { Slack_thread_route.team = "T123"; app = "A123"; channel = "C123"; root = "100.000001" }`;
  - `render ?(untrusted = fun _ -> false) messages = Slack_thread_api.render_history ~route ~bot:"UBOT" ~current:"100.9" ~untrusted { Slack_thread_api.messages; overflow = false; limited = false; next_cursor = ""; has_more = false }`;
  - `own_reply = update (message ~ts:"100.2" ~user:"UBOT" "own response") [ ("bot_id", str "BOWN") ]`.

  Tests:
  - `history file-key allowlist`: `render [ update (message ~ts:"100.000001" "see file") [ ("files", arr [ F.md_file (); F.transcript_file (); str "junk" ]) ] ]`:
    - contains `"F0MD"`, `"notes.md"`, `"text/markdown"`, `"F0TRANSCRIPT"` and `"application/vnd.slack-huddle-transcript"`;
    - contains none of `"SECRET"`, `"title_blocks"`, `"files-pri"`, `"preview"`, `"huddle_transcription"` or `"junk"`.
  - `empty file projection omitted`: `render [ update (message ~ts:"100.3" "hi") [ ("files", arr [ str "junk" ]) ] ]` does not contain `"Attached source metadata"`.
  - `own relay labelled untrusted`:
    - `render [ F.own_relay ~channel:"C123" ~ts:"100.000001" () ]`, with the default `~untrusted`, contains `"\"role\":\"untrusted_source\""` and no `"\"role\":\"assistant\""`;
    - `render [ own_reply ]` contains `"\"role\":\"assistant\""`;
    - `render ~untrusted:(fun m -> get "ts" m = str "100.2") [ own_reply ]` contains `"\"role\":\"untrusted_source\""`;
    - with `recorded = { G.no_relays with recorded_relay = (fun ~channel ~ts -> channel = "C123" && ts = "100.2") }`, `untrusted_in_history ~registry:recorded ~channel:"C123" own_reply` is true, and the same call with `G.no_relays` is false.
  - `mentioned relay thread context hardened`: in `with_ingress`, wrap the handler so `conversations.replies` returns `ok [ ("messages", arr [ F.own_relay ~channel:"C123" ~ts:"100.000001" () ]); ("has_more", `Bool false) ]`. Then `run (message ~thread:"100.000001" ~ts:"100.000004" "<@UBOT> summarise")`. Let `text` be the concatenation of every `(m : Provider.message).content` in `!seen`:
    - it contains `"\"role\":\"untrusted_source\",\"text\":\"<@U0CLAUDE> Huddle notes"`;
    - it does not contain `"SECRET"`.

- [ ] **Step 2: Run to verify it fails.** `T slack_history_hardening` → FAIL at compile time: `render_history` has no `~untrusted` argument.

- [ ] **Step 3: Implement** `history_file_keys`, `project_file`, the new `render_history` (with the structural check built in) and `untrusted_in_history`. In ingress, pass `~untrusted:(Slack_relay_guard.untrusted_in_history ~registry ~channel)`, using the gate's `registry`.

- [ ] **Step 4: Run to verify they pass:** `T slack_history_hardening`, then `SLACK`, then `CYCLES`. Expected: all PASS and no cycle output (`Slack_thread_api` now depends on `Slack_relay_guard`).

- [ ] **Step 5: FMT, then commit.** `git add src/slack_thread_api.ml src/slack_relay_guard.ml src/slack_thread_ingress.ml test/test_slack_history_hardening.ml test/dune test/test_main.ml docs/connector-reference.md docs/public/llms-full.txt`; message: `fix(slack): history keeps only allowlisted file keys; own relays are untrusted`, a blank line, then `B833 behaviour change: human file previews/snippets no longer enter thread history.` and `No existing test expectations change (checked slack_thread_edges, slack_threads).`

---

### Task 5: Pure huddle classifier

**Files:**
- Create: `src/slack_huddle_classify.ml` (add to `src/dune`). Modify: `test/test_slack_event_fixtures.ml` (add `target`).
- Create: `test/test_slack_huddle_classify.ml` (suite `slack_huddle_classify`).

**Interfaces:**
- Consumes: `Slack_huddles.{valid_id; valid_ts; get; field; text; list; slackbot_user_id}`, `Slack_relay_guard.{file_ids; nonce_of_text; is_own_relay_message}`, `String_util.contains`.
- Produces (`Slack_huddle_classify`):
  ```ocaml
  type room = { room_id : string; has_ended : bool; date_start : int option; date_end : int option;
                thread_root_ts : string option; canvas_thread_ts : string option; attached_file_ids : string list;
                transcript_file_id : string option }  (* room.transcript_file_id passing valid_id "F"; a hint (P0) *)
  type notice = Rate_limited | Spend_limit | Disabled | Not_delivered
  type text_kind = Notice of notice | Reply      (* Reply = matches no known notice *)
  type notes_variant = Notes_and_transcript | Notes_only | Variant_unknown
  type observation =
    | Huddle_seen of { channel : string; root_ts : string; file_ids : string list (* root files[]: the canvas is here, P0 *); room : room }
    | Notes_ready of { channel : string; thread_ts : string; ts : string; file_ids : string list;
                       canvas_file_id : string; variant : notes_variant }
    | Own_relay of { channel : string; ts : string; file_ids : string list; nonce : string }
    | Target_reaction of { channel : string option (* None: documented file item has no channel *); user : string;
                           item_ts : string option; item_file : string option }
    | Target_reply of { channel : string; thread_ts : string; ts : string; bot_id : string; app_id : string option;
                        username : string option; kind_if_first : text_kind; kind_if_later : text_kind }
    | Posture_change of { channel : string }
    | Irrelevant
  type target = { target_user_id : string; target_bot_id : string; target_app_id : string }
  type matchers = { first_reply : (string * notice) list; any_reply : (string * notice) list }
  type context = { self_user_id : string option; targets : target list; matchers : matchers }
  val no_matchers : matchers     (* all empty: every reply is Reply until the notice wordings are captured *)
  val empty_context : context    (* { self_user_id = None; targets = []; matchers = no_matchers } *)
  val posture_event_types : string list  (* member_joined_channel; member_left_channel; channel_shared; channel_unshared *)
  val notes_variant_to_string : notes_variant -> string   (* "notes_and_transcript" | "notes_only" | "unknown" *)
  val text_kind : matchers -> first:bool -> string -> text_kind
  val event_channel : Yojson.Safe.t -> string option      (* envelope -> event.channel, else event.item.channel, if strings *)
  val classify : context -> Yojson.Safe.t -> observation  (* takes the whole envelope *)
  val channel_of : observation -> string option           (* Target_reaction -> its option; Irrelevant -> None *)
  ```
- **Pickup vocabulary (P0-3, supersedes spec §6.2 `working`/`unclassified`).** No working signal exists, so `working_username_prefixes` and `Working`/`Unclassified` are gone. Phase 3 maps: acknowledged = target reaction, or the first reply when no reaction is seen. picked_up = the first target reply whose `kind_if_first = Reply`. `Notice Spend_limit | Disabled` on any reply = not_delivered.
- **Rules.** Read only the fixed paths named here; the first match wins.
  0. `type` ≠ `event_callback`, or `event` not an object → `Irrelevant`.
  1. **`reaction_added`:** `item` is an object and `event.user` is some `target_user_id`.
     - `item.type = "message"`: `item.channel` passes `valid_id "CG"` and `item.ts` is valid → `Target_reaction {channel = Some; item_ts = Some; item_file = None}`.
     - `item.type = "file"`: `item.file` passes `valid_id "F"`. **`item.channel` is not required** (Slack documents `{type, file}`; fact-recheck r2 n6) → `{channel = Some c` only if present and valid, else `None; item_ts = None; item_file = Some}`. Phase 3 resolves it by `item.file ∈ uploaded_file_ids`.
     - otherwise `Irrelevant`.
  2. `event.type ∈ posture_event_types` and `event.channel` passes `valid_id "CG"` → `Posture_change`.
  3. **`message`:** `event.channel` passes `valid_id "CG"`. `m` = `event.message` (must be an object) when `subtype = "message_changed"`; `message_deleted` → `Irrelevant`; otherwise `m = event`. Any other subtype, or `null`, is never read again.
     - a. **Huddle:** `m.room` is an object, `room.id` passes `valid_id "R"`, `room.call_family = "huddle"` **or** `m.subtype = "huddle_thread"`, and `m.ts` is valid → `Huddle_seen {root_ts = m.ts; file_ids = file_ids m}`. `has_ended = (room.has_ended = true || date_end > 0)`, and `date_end` is `None` when absent or 0. Integers accept `Int`, an integral `Float`, and a `String`/`Intlit` of 1–18 ASCII digits; anything else (signs, exponents, fractions) is `None`. Timestamps are kept only when `valid_ts`, file ids only when `valid_id "F"`. P0 shows the root is USLACKBOT with no `bot_id`, but neither is required: the ledger is a hint, and thread-started roots (P0-5) are still open.
     - b. **Notes ready:** `m.user = slackbot_user_id`; `m.bot_id` absent or `Null`; `m.ts` valid; `m.thread_ts` valid and ≠ `m.ts`; `file_ids m ≠ []` → `Notes_ready`. `canvas_file_id` is the first file with `mimetype = "application/vnd.slack-docs"`, else the first id. File ownership is never read (P0-1: the canvas belongs to the huddle starter). `variant` comes from the lower-cased `m.text`: `"notes and transcript are ready"` → `Notes_and_transcript`, else `"notes are ready"` → `Notes_only`, else `Variant_unknown`.
     - c. **Own relay:** `ctx.self_user_id = Some u`, `Slack_relay_guard.is_own_relay_message ~bot:u m`, `m.ts` valid, and `m.thread_ts` absent or equal to `m.ts` → `Own_relay {file_ids = file_ids m; nonce}` from `nonce_of_text m.text`. Only the top-level check is added (Task 3).
     - d. **Target reply:** `m.bot_id` is some `target_bot_id`; every present value of `m.bot_profile.app_id` and `m.app_id` equals that target's `target_app_id`; `m.thread_ts` is valid and ≠ `m.ts` → `Target_reply` with `kind_if_first = text_kind ~first:true`, `kind_if_later = text_kind ~first:false`, `app_id` = `bot_profile.app_id`, else `app_id`, else `None`, and `username` only when a string (P0-3: null).
     - e. Otherwise `Irrelevant`.
  4. Otherwise `Irrelevant`.

  **`text_kind`:** with `t = String.trim text`, an exact `any_reply` match comes first, then (only when `first`) an exact `first_reply` match, else `Reply`. Spec placement: `rate_limited`/`not_delivered` texts go in `first_reply`, `spend_limit`/`disabled` texts in `any_reply`.

- [ ] **Step 1: Write the failing tests.** Add `target` to the fixtures. Helpers: `module C = Slack_huddle_classify`; `ctx = { C.empty_context with self_user_id = Some "UBOT"; targets = [ F.target ] }`; `classify ?(ctx = ctx) e = C.classify ctx (F.envelope e)`; `irrelevant ?ctx e = classify ?ctx e = C.Irrelevant`. "`X` → `Huddle_seen {…}`" means a match whose listed fields equal those values.
  - `huddle roots and edits`:
    - `F.huddle_root ()` → `Huddle_seen {channel = "C0TEST"; root_ts = "1700000000.000100"; file_ids = [ "F0CANVAS" ]}`, whose room has `room_id = "R0ROOM1"`, `has_ended = false`, `date_start = Some 1700000000`, `date_end = None`, `canvas_thread_ts = Some "1700000000.000100"`, `attached_file_ids = []` and `transcript_file_id = Some "F0TRANSCRIPT"` (P0 root shape).
    - Not irrelevant: `F.huddle_root ~room:(F.room ~call_family:None ()) ()` (subtype alone, the B832 fixture shape) and `F.huddle_root ~subtype:None ()` (call_family alone).
    - `F.thread_started_huddle ()` → `root_ts = "1700000500.000300"`, `room.thread_root_ts = Some "1699990000.000100"`.
    - `F.root_changed (F.huddle_root ~room:(F.room ~has_ended:true ~date_end:1700003600 ()) ())` → `has_ended` and `date_end = Some 1700003600`.
    - `F.huddle_root ~room:(F.room ~date_end:1700003600 ()) ()` → `has_ended = true`.
    - `~room:(F.room ~transcript:None ())` and `~room:(F.room ~transcript:(Some "f0bad") ())` → `transcript_file_id = None`.
    - Irrelevant: `~room:(F.room ~id:"r0room1" ())`, and `update (F.root_changed (F.huddle_root ())) [ ("subtype", str "message_deleted") ]`.
  - `over-long ids and numeric strings`:
    - Irrelevant: `F.huddle_root ~room:(F.room ~id:("R" ^ String.make 64 'A') ()) ()` and `F.huddle_root ~channel:("C" ^ String.make 64 'A') ()`.
    - With `date_start v` = the room's `date_start` for `F.huddle_root ~room:(update (F.room ()) [ ("date_start", v) ]) ()` (it must still be `Huddle_seen`):
      - `str "1700000000"`, `` `Intlit "1700000000" `` and `` `Float 1700000000. `` → `Some 1700000000`;
      - `str "17e8"`, `str "-5"`, `str ""`, `` `Float 1.5 `` and `obj []` → `None`.
  - `notes ready and spoofs`:
    - `F.notes_ready ()` → `Notes_ready {thread_ts = "1700000000.000100"; canvas_file_id = "F0CANVAS"; file_ids = [ "F0CANVAS" ]; variant = Notes_and_transcript}`. Its canvas user is `U0STARTER`, so ownership is not read.
    - `update (F.notes_ready ()) [ ("bot_id", `Null); ("subtype", `Null) ]` → `Notes_ready` (P0 explicit nulls).
    - `F.notes_ready ~files:[ F.md_file (); F.canvas_file () ] ~text:"AI huddle notes are ready." ()` → `canvas_file_id = "F0CANVAS"`, `variant = Notes_only`.
    - Irrelevant:
      - `~user:"U123"` (human imitation);
      - `~bot_id:"B1"`;
      - `~thread_ts:None`;
      - `~files:[]`;
      - `~files:[ obj [ ("id", str "f1") ]; obj [ ("id", str "X1") ] ]`;
      - `update (F.human "x") [ ("blocks", arr [ obj [ ("room", obj [ ("id", str "R1") ]) ] ]); ("metadata", obj [ ("user", str "USLACKBOT") ]) ]`.
  - `own relay structure`:
    - `F.own_relay ()` → `Own_relay {nonce = F.relay_nonce; file_ids = [ "F0MD" ]}` (P0-4: no subtype, `bot_id` BOWN).
    - `F.own_relay ~subtype:"file_share" ()` and `F.own_relay ~bot_id:None ()` → `Own_relay`.
    - Irrelevant: `~ctx:C.empty_context`, `~footer:None`, `~user:"U123"`, `~thread_ts:"1700000000.000100"`.
  - `target replies and reactions`:
    - Replies:
      - `F.claude_reply ()` → `Target_reply {bot_id = "B0CLAUDE"; app_id = Some "A0CLAUDE"; username = None; kind_if_first = Reply; kind_if_later = Reply}` (P0-3);
      - `F.claude_reply ~top_app_id:"A0CLAUDE" ()` → `Target_reply`;
      - irrelevant: `~app_id:"A0EVIL"`, `~top_app_id:"A0EVIL"`, `~thread_ts:None`, `~bot_id:"B0OTHER"`, `~ctx:C.empty_context`.
    - Reactions:
      - `F.reaction ()` → `Target_reaction {channel = Some "C0TEST"; item_ts = Some "1700004000.000500"; item_file = None}`;
      - `fr = F.reaction ~item:(obj [ ("type", str "file"); ("file", str "F0MD") ]) ()` → `{channel = None; item_ts = None; item_file = Some "F0MD"}`, and `C.event_channel (F.envelope fr) = None`;
      - irrelevant: `F.reaction ~item:(obj [ ("type", str "message"); ("ts", str "1700004000.000500") ]) ()` (a message reaction needs its channel), and `F.reaction ~user:"U123" ()`.
  - `notice text kinds`: with `m = { C.first_reply = [ ("Rate limited.", C.Rate_limited) ]; any_reply = [ ("Spend limit reached", C.Spend_limit) ] }` (test strings, not captured wordings) and `k ?(first = true) t = C.text_kind m ~first t`:
    - `C.text_kind C.no_matchers ~first:true "ok" = C.Reply`;
    - `k " Rate limited. " = C.Notice C.Rate_limited`, but `k ~first:false "Rate limited." = C.Reply`;
    - `k "Spend limit reached"` and `k ~first:false "Spend limit reached"` are both `C.Notice C.Spend_limit`;
    - `k "Spend limit reached!" = C.Reply` (exact match only).
  - `posture events and malformed shapes are irrelevant`:
    - Each kind in `C.posture_event_types`: `classify (F.posture kind) = C.Posture_change { channel = "C0TEST" }`.
    - Irrelevant: `F.posture "channel_rename"` and `F.huddle_root ~channel:"D0DM" ()`.
    - `C.classify ctx j = C.Irrelevant` for each of:
      - `` `Null ``;
      - `F.block_actions`;
      - `obj [ ("type", str "event_callback"); ("event", str "x") ]`;
      - `F.envelope (obj [ ("type", str "message"); ("channel", str "C0TEST"); ("ts", `Int 5); ("room", arr []); ("files", str "F1"); ("user", obj []) ])`.
    - `C.event_channel (F.envelope (F.reaction ())) = Some "C0TEST"`, and `C.channel_of (classify (F.huddle_root ())) = Some "C0TEST"`.

- [ ] **Step 2: Run to verify it fails.** `T slack_huddle_classify` → FAIL at compile time: `Unbound module Slack_huddle_classify`.

- [ ] **Step 3: Implement `src/slack_huddle_classify.ml`** to the Interfaces and Rules: no Lwt, no DB, no Slack calls; dependencies `Slack_huddles`, `Slack_relay_guard`, `String_util` only.

- [ ] **Step 4: Run to verify it passes.** `T slack_huddle_classify` → PASS.

- [ ] **Step 5: FMT, then commit.** `git add src/slack_huddle_classify.ml src/dune test/test_slack_huddle_classify.ml test/test_slack_event_fixtures.ml test/dune test/test_main.ml`; `git commit -m "feat(slack): pure huddle event classifier over P0-observed payload shapes"`.

---

### Task 6: `slack_huddles` observation ledger

**Files:**
- Create: `src/slack_huddle_ledger.ml` (add to `src/dune`).
- Modify: `src/daemon_runtime_loops.ml`, in the hourly cleanup block (`if now -. !last_memory_cleanup >= 3600.0`, around line 119), directly after `Memory.cleanup_teams_dedup ~db ~max_age_days:30;`. This keeps the 30-day TTL even when no events arrive. Phase 4's reconciler may also call `prune`.
- Create: `test/test_slack_huddle_ledger.ml` (suite `slack_huddle_ledger`).

**Interfaces:**
- Consumes: `Slack_huddle_classify.{room; notes_variant; notes_variant_to_string}`, `Sql_util`.
- Produces (`Slack_huddle_ledger`):
  ```ocaml
  val ensure_schema : Sqlite3.db -> unit          (* idempotent; called by every write *)
  val ttl_seconds : float                         (* 30. *. 86400. *)
  type row_kind = Room | Notes_stub
  type row = { kind : row_kind; team_id : string; channel_id : string; room_id : string option;
               root_ts : string option; thread_root_ts : string option; canvas_thread_ts : string option;
               attached_file_ids : string list; root_file_ids : string list; room_transcript_file_id : string option;
               date_start : int option; date_end : int option; has_ended : bool;
               notes_thread_ts : string option; notes_ready_ts : string option; notes_file_ids : string list;
               canvas_file_id : string option; transcript_file_id : string option;
               notes_variant_hint : string option; finalised : bool;
               first_seen_at : float; last_seen_at : float; source : string }
  type notes_attachment = Attached of string (* room_id *) | Stub
  val record_seen : db:Sqlite3.db -> team:string -> channel:string -> source:string -> now:float ->
                    root_ts:string -> file_ids:string list -> Slack_huddle_classify.room -> unit
  val record_notes_ready : db:Sqlite3.db -> team:string -> channel:string -> source:string -> now:float ->
                           thread_ts:string -> ts:string -> file_ids:string list -> canvas_file_id:string ->
                           variant:Slack_huddle_classify.notes_variant -> notes_attachment
  val prune : db:Sqlite3.db -> now:float -> int
     (* deletes rows with last_seen_at < now -. ttl_seconds and returns the count; when sqlite_master has no
        slack_huddles it returns 0 without creating it, so the hourly tick adds no table to DBs that never observe *)
  val list_rows : db:Sqlite3.db -> team:string -> channel:string -> row list   (* ORDER BY id *)
  ```
- Schema. `room_transcript_file_id` is the root's Slack-controlled hint. `transcript_file_id` is the verified id, set only by the phase-3 executor:
  ```sql
  CREATE TABLE IF NOT EXISTS slack_huddles (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    row_kind TEXT NOT NULL CHECK (row_kind IN ('room','notes_stub')),
    team_id TEXT NOT NULL, channel_id TEXT NOT NULL, room_id TEXT,
    root_ts TEXT, thread_root_ts TEXT, canvas_thread_ts TEXT,
    attached_file_ids_json TEXT NOT NULL DEFAULT '[]', root_file_ids_json TEXT NOT NULL DEFAULT '[]',
    room_transcript_file_id TEXT,
    date_start INTEGER, date_end INTEGER, has_ended INTEGER NOT NULL DEFAULT 0,
    notes_thread_ts TEXT, notes_ready_ts TEXT, notes_file_ids_json TEXT NOT NULL DEFAULT '[]',
    canvas_file_id TEXT, transcript_file_id TEXT, notes_variant_hint TEXT,
    finalised INTEGER NOT NULL DEFAULT 0,
    first_seen_at REAL NOT NULL, last_seen_at REAL NOT NULL,
    source TEXT NOT NULL CHECK (source IN ('event','reconcile')));
  CREATE UNIQUE INDEX IF NOT EXISTS slack_huddles_room ON slack_huddles(team_id, channel_id, room_id) WHERE row_kind = 'room';
  CREATE UNIQUE INDEX IF NOT EXISTS slack_huddles_stub ON slack_huddles(team_id, channel_id, notes_thread_ts, canvas_file_id) WHERE row_kind = 'notes_stub';
  CREATE INDEX IF NOT EXISTS slack_huddles_seen ON slack_huddles(last_seen_at);
  ```
- Each write is `ensure_schema` → `BEGIN IMMEDIATE` → `prune ~now` → read-merge-write → `COMMIT`. The DB is the daemon's shared handle, so `BEGIN IMMEDIATE` runs **outside** the `try`: if it fails (for example another fiber's transaction is open on the handle), the write raises without running `ROLLBACK`, so it can never roll back someone else's transaction; the observer's `try` catches that. Only after this write's own `BEGIN` succeeded does an exception run `ROLLBACK` and re-raise. The JSON lists use `Yojson.Safe`.
- **`record_seen`** works on `Room` rows keyed by (team, channel, room_id). The cases are checked in order:
  - **No row:** insert one.
  - **Reuse** (stored and new `date_start` both `Some`, new > stored): the room id was reused for a later huddle.
    - Replace the lifecycle fields with the new values: `root_ts`, `thread_root_ts`, `canvas_thread_ts`, `attached_file_ids`, `root_file_ids`, `room_transcript_file_id`, `date_start`, `date_end`, `has_ended`.
    - Clear `notes_*`, `canvas_file_id`, `transcript_file_id` and `notes_variant_hint`.
    - Set `finalised = 0`, `first_seen_at = last_seen_at = now`, and `source`.
  - **Stale** (both `Some`, new < stored): the event is from an *earlier* use, for example a delayed or retried edit, Socket Mode redelivery, or an HTTP retry up to 24 h late. Set only `last_seen_at = now`.
  - **Merge** (otherwise):
    - `root_ts` and `date_start` keep the stored value, or else take the new one.
    - `thread_root_ts`, `canvas_thread_ts`, `date_end` and `room_transcript_file_id` take the new value when `Some`, or else keep the stored one.
    - `attached_file_ids` and `root_file_ids` become the stored list followed by the new ids not already present.
    - `has_ended` becomes stored `||` new.
    - Set `last_seen_at = now` and `source`.
- **`record_notes_ready`** looks for a `Room` row in (team, channel) where `thread_ts ∈ {root_ts, thread_root_ts, canvas_thread_ts}` **and** (`date_start IS NULL` or `date_start <= float ts`), ordered by `date_start IS NULL, date_start DESC, id DESC`. A ready reply always comes after its huddle started, so a late reply from the previous use of a reused room id becomes a stub. The match does not require `has_ended`, because the reply can precede the ended edit (P0: about 20 s after the end).
  - **A matching row:** set `notes_thread_ts`, `notes_ready_ts`, `notes_file_ids`, `canvas_file_id` and `notes_variant_hint`, but only when the stored `notes_ready_ts` is `None` or `float ts >` the stored value. Always set `last_seen_at`. Return `Attached room_id`.
  - **No match:** upsert a `Notes_stub` keyed on (team, channel, `notes_thread_ts = thread_ts`, `canvas_file_id`) with the same newer-wins rule, and return `Stub`.
  - The observer never sets `transcript_file_id` or `finalised`.

- [ ] **Step 1: Write the failing tests.** Helpers (`module L = Slack_huddle_ledger`, `module C = Slack_huddle_classify`):
  - `with_db f` opens `:memory:`, runs `L.ensure_schema`, and closes the DB in `Fun.protect`.
  - `room ?(id = "R0ROOM1") ?(has_ended = false) ?(date_start = Some 1700000000) ?(date_end = None) ?(thread = "1700000000.000100") ?(attached = []) ?(transcript = Some "F0TRANSCRIPT") () : C.room` sets both thread fields to `Some thread`.
  - `seen ?(team = "T123") ?(channel = "C0TEST") ?(now = 1000.) ?(root_ts = "1700000000.000100") ?(files = [ "F0CANVAS" ]) db r = L.record_seen ~db ~team ~channel ~source:"event" ~now ~root_ts ~file_ids:files r`.
  - `notes ?(now = 2000.) ?(thread_ts = "1700000000.000100") ?(ts = "1700003900.000400") ?(variant = C.Notes_and_transcript) db = L.record_notes_ready ~db ~team:"T123" ~channel:"C0TEST" ~source:"event" ~now ~thread_ts ~ts ~file_ids:[ "F0CANVAS" ] ~canvas_file_id:"F0CANVAS" ~variant`.
  - `rows ?(team = "T123") ?(channel = "C0TEST") db`, and `one db`, which fails unless there is exactly one row.

  Tests:
  - `room lifecycle merge is monotonic`:
    1. `seen db (room ())` gives one row with `kind = Room`, `room_id = Some "R0ROOM1"`, `has_ended = false`, `root_ts = Some "1700000000.000100"`, `root_file_ids = [ "F0CANVAS" ]`, `room_transcript_file_id = Some "F0TRANSCRIPT"`, `attached_file_ids = []` and `source = "event"`.
    2. `seen ~now:1100. ~files:[ "F0CANVAS"; "F0Y" ] db (room ~has_ended:true ~date_end:(Some 1700003600) ~attached:[ "F0X" ] ())`.
    3. A stale edit arrives late: `seen ~now:1200. ~files:[] db (room ~transcript:None ())`.
    4. Expect `has_ended`, `date_end = Some 1700003600` (a stale edit cannot reopen an ended huddle), `attached_file_ids = [ "F0X" ]`, `root_file_ids = [ "F0CANVAS"; "F0Y" ]`, `room_transcript_file_id = Some "F0TRANSCRIPT"` (`None` never clears it) and `last_seen_at = 1200.`.
  - `notes attach, newer wins`:
    1. `seen db (room ())`; `notes db = L.Attached "R0ROOM1"`, although the room has not ended yet.
    2. The row has `notes_ready_ts = Some "1700003900.000400"`, `canvas_file_id = Some "F0CANVAS"`, `notes_variant_hint = Some "notes_and_transcript"` and `transcript_file_id = None`.
    3. `seen ~now:2100. db (room ~has_ended:true ~date_end:(Some 1700003600) ())` gives `has_ended` with `notes_ready_ts` still `Some "1700003900.000400"`: notes before the ended edit survive it.
    4. `notes ~ts:"1700003800.000100" ~variant:C.Notes_only db` leaves the hint `"notes_and_transcript"`. Then `notes ~ts:"1700004500.000100" ~variant:C.Notes_only db` sets it to `"notes_only"`.
  - `thread-started huddle attaches by canvas thread`: `seen ~root_ts:"1700000500.000300" db (room ~id:"R0ROOM2" ~thread:"1699990000.000100" ())`, then `notes ~thread_ts:"1699990000.000100" db = L.Attached "R0ROOM2"`.
  - `reused room id: newer start resets, older start is stale`:
    1. `seen db (room ~has_ended:true ())`; `notes db`.
    2. `seen ~now:5000. db (room ~date_start:(Some 1700090000) ~transcript:(Some "F0T2") ())` gives `date_start = Some 1700090000`, `has_ended = false`, `notes_ready_ts = None`, `canvas_file_id = None` and `room_transcript_file_id = Some "F0T2"`.
    3. A late edit from the first use: `seen ~now:6000. ~files:[ "F0OLD" ] db (room ~has_ended:true ~date_end:(Some 1700003600) ~attached:[ "F0OLD" ] ())`.
    4. Expect `date_start = Some 1700090000`, `has_ended = false`, `date_end = None`, `attached_file_ids = []`, `root_file_ids = [ "F0CANVAS" ]`, `room_transcript_file_id = Some "F0T2"`, `notes_ready_ts = None` and `last_seen_at = 6000.`.
    5. `notes ~ts:"1700003900.000400" db = L.Stub`, and some `Room` row still has `notes_ready_ts = None`.
  - `unknown thread stores one stub`: `notes ~thread_ts:"1690000000.000100" db = L.Stub`. Calling it twice leaves exactly one row, with `kind = Notes_stub` and `room_id = None`.
  - `team/channel isolation and 30-day TTL`:
    1. `seen` for (T123, C0TEST), (T123, C0OTHER) and (T999, C0TEST) gives `rows db` of length 1 and `rows ~channel:"C0OTHER" db` of length 1.
    2. `L.prune ~db ~now:(1000. +. L.ttl_seconds +. 1.) = 3`. `L.ensure_schema` again leaves `rows db = []`.
    3. On a bare `:memory:` DB, `L.prune ~db:bare ~now:1e9 = 0`, and `SELECT name FROM sqlite_master WHERE type='table' AND name='slack_huddles'` returns no row.

- [ ] **Step 2: Run to verify it fails.** `T slack_huddle_ledger` → FAIL at compile time: `Unbound module Slack_huddle_ledger`.

- [ ] **Step 3: Implement** `src/slack_huddle_ledger.ml` to the Interfaces, schema and rules. In `daemon_runtime_loops.ml`, after `Memory.cleanup_teams_dedup`, add `(try ignore (Slack_huddle_ledger.prune ~db ~now) with exn -> Logs.warn (fun m -> m "slack_huddles prune failed: %s" (Printexc.to_string exn)));`.

- [ ] **Step 4: Run to verify:** `T slack_huddle_ledger` → PASS. Then `CYCLES` → prints nothing.

- [ ] **Step 5: FMT, then commit.** `git add src/slack_huddle_ledger.ml src/daemon_runtime_loops.ml src/dune test/test_slack_huddle_ledger.ml test/dune test/test_main.ml`; `git commit -m "feat(slack): slack_huddles observation ledger with stubs and 30-day TTL"`.

---

### Task 7: Huddle observer tap and the observed-channels interface

**Files:**
- Create: `src/slack_huddle_watch.ml`, `src/slack_huddle_observer.ml` (add both to `src/dune`).
- Modify: `src/slack_thread_ingress.ml`: the second tap, on the line after the Task 1 `Slack_event_log.observe` call.
- Create: `test/test_slack_huddle_observer.ml` (suite `slack_huddle_observer`).
- Docs: none. Phase 1 observes no channel in production; phase 2 documents the feature.

**Interfaces:**
- Consumes: `Slack_huddle_classify.{context; empty_context; classify; event_channel}`, `Slack_huddle_ledger.{record_seen; record_notes_ready}`, `Slack_huddles.valid_id`.
- Produces:
  ```ocaml
  (* Slack_huddle_watch *)
  type t = { observed : team:string -> app:string -> channel:string -> bool;
             context : team:string -> app:string -> channel:string -> Slack_huddle_classify.context }
  val none : t                    (* observed = false; context = Slack_huddle_classify.empty_context *)
  val of_db : Sqlite3.db -> t     (* phase 1: none. Phase 2: channels with an active or paused order whose
                                     team_id/api_app_id match; context = those orders' targets + clawq's bot user id *)
  val source : (Sqlite3.db -> t) ref   (* default of_db; test seam only; phase 2 changes of_db, not this ref *)
  (* Slack_huddle_observer *)
  val observe_db : db:Sqlite3.db -> watch:Slack_huddle_watch.t -> now:float -> Yojson.Safe.t -> unit
  val observe : manager:Session.t -> Yojson.Safe.t -> unit
  ```
- `observe` wraps `Session.get_db` (`None` → `()`), the watch lookup (`!Slack_huddle_watch.source db`) and `observe_db ~now:(Unix.gettimeofday ())` in **one** `try … with exn -> Logs.warn "Slack huddle observer skipped an event: %s"` (with `Printexc.to_string`). This matters because phase 2's `of_db` queries the DB. `observe_db` is public too, so it wraps its own whole body in the same `try`. The body:
  1. Requires `type = event_callback`, `team_id` passing `valid_id "T"`, `api_app_id` passing `valid_id "A"`, and `event_channel = Some c` with `valid_id "CG" c`. This makes D channels unreachable.
     - **Channel-less file reactions are dropped here in phase 1.** This loses nothing yet, because `Target_*` effects are no-ops.
     - Phase 3 must route such a reaction past this gate. It resolves `item.file` through the fire ledger (`uploaded_file_ids` → fire → channel), then applies the observed-channel check to that channel. Otherwise the §6.2 pickup path `item.file ∈ uploaded_file_ids` is unreachable.
  2. Requires `watch.observed ~team ~app ~channel:c`.
  3. Classifies with `watch.context ~team ~app ~channel:c`.
  4. Dispatches the result:
     - `Huddle_seen {root_ts; file_ids; room}` → `record_seen ~source:"event" ~root_ts ~file_ids room`;
     - `Notes_ready` → `record_notes_ready ~source:"event"`, ignoring the result;
     - `Own_relay`, `Target_reaction`, `Target_reply`, `Posture_change` and `Irrelevant` do nothing in phase 1. The match stays exhaustive, so phase 3 adds their effects (fires, posture cache) there.

  It makes no Slack or LLM call and doesn't depend on `Slack` or `Slack_thread_ingress`.

- [ ] **Step 1: Write the failing tests.** Helpers (`module L = Slack_huddle_ledger`):
  - `watch_all ?(log = ref []) ()`: its `observed` records `(team, app, channel)` in `log` and returns `team = "T123"`; its `context` is `empty_context`.
  - `with_db` is as in Task 6.
  - `observe ?(watch = watch_all ()) ?team db e = Slack_huddle_observer.observe_db ~db ~watch ~now:1000. (F.envelope ?team e)`.
  - `rows db = L.list_rows ~db ~team:"T123" ~channel:"C0TEST"`.
  - `with_watch w f` swaps `Slack_huddle_watch.source := (fun _ -> w)` and restores it in `Lwt.finalize`.

  Tests:
  - `observed channel records huddle and notes`: `observe db (F.huddle_root ())`, then `observe db (F.notes_ready ())`, gives exactly one row with `room_id = Some "R0ROOM1"`, `root_file_ids = [ "F0CANVAS" ]` and `notes_ready_ts <> None`.
  - `envelope, watch and dm gates`:
    1. `observe ~watch:Slack_huddle_watch.none db (F.huddle_root ())` leaves `rows db = []`.
    2. With `log`, `observe ~watch:(watch_all ~log ()) ~team:"T999" db (F.huddle_root ())` gives `!log = [ ("T999", "A123", "C0TEST") ]`, and there are no T999 rows.
    3. With a fresh `log`, `F.huddle_root ~channel:"D0DM" ()` leaves `!log = []`, and so does `F.reaction ~item:(obj [ ("type", str "file"); ("file", str "F0MD") ]) ()`, which is dropped before the watch in phase 1.
    4. `(Slack_huddle_watch.of_db db).observed ~team:"T123" ~app:"A123" ~channel:"C0TEST" = false`.
  - `store failure never escapes; malformed ignored`:
    - `observe` on an already-closed DB returns.
    - `observe_db` over `` `Null ``, `F.block_actions` and `obj [ ("type", str "event_callback"); ("event", arr []) ]` leaves `rows db = []`.
  - `tap writes the ledger, keeps routing and makes no Slack call`, in `with_ingress (fun base calls _ seen _ run -> …)`, after `L.ensure_schema base.db`, with `ledger () = L.list_rows ~db:base.db ~team:"T123" ~channel:"C0TEST"`:
    1. `run (F.huddle_root ())` leaves `ledger () = []` (the phase-1 default watch).
    2. Under `with_watch (watch_all ())`, `run ~id:"Ev2" (F.huddle_root ~ts:"1700000001.000100" ~room:(F.room ~id:"R0ROOM9" ()) ())` and `run ~id:"Ev3" (F.notes_ready ())` give a row with `room_id = Some "R0ROOM9"` and `notes_ready_ts <> None`, and `!calls = []`.
    3. Under `with_watch { (watch_all ()) with Slack_huddle_watch.observed = (fun ~team:_ ~app:_ ~channel:_ -> failwith "watch exploded") }`, `run ~id:"Ev4" (F.huddle_root ~ts:"1700000002.000100" ())`. Then `run ~id:"Ev5" (message "please help")` gives `!seen <> []`.
  - `watch lookup failure never escapes the tap`: with `Slack_huddle_watch.source := (fun _ -> failwith "phase-2 query failed")`, restored in `Lwt.finalize`, `run (message "please help")` gives `!seen <> []`.

- [ ] **Step 2: Run to verify it fails.** `T slack_huddle_observer` → FAIL at compile time: `Unbound module Slack_huddle_watch`.

- [ ] **Step 3: Implement** both modules to the Interfaces. In `slack_thread_ingress.ml`, add `Slack_huddle_observer.observe ~manager:session_manager json;` after the event-log call.

- [ ] **Step 4: Run to verify they pass:** `T slack_huddle_observer`, then `SLACK`, then `CYCLES`. Expected: all PASS and no cycle output (ingress now depends on the observer). The `tap writes the ledger…` test fails if the ingress call is missing, so it proves the wiring.

- [ ] **Step 5: FMT, then commit.** `git add src/slack_huddle_watch.ml src/slack_huddle_observer.ml src/slack_thread_ingress.ml src/dune test/test_slack_huddle_observer.ml test/dune test/test_main.ml`; `git commit -m "feat(slack): huddle observer tap gated by observed-channels interface"`.

---

### Task 8: Fail-closed `Access_snapshot` unknown-work-type decode

**Files:**
- Modify: `src/access_snapshot.ml`: `row_of_stmt` (lines 430-507), `query` (509-563), `get_by_id` (565-581) and `export_json` (655-668).
- Create: `test/test_access_snapshot_decode.ml` (suite `access_snapshot_decode`).
- Docs: add a bullet to `docs/public/llms-full.txt` §"Effective Access Snapshots" (around line 811):
  > **Fail-closed decode:** a stored work type this binary doesn't know is never treated as authority. Lookups return nothing, so tools deny, and exports list it as `unknown`. Binaries older than this change decoded unknown types as `room_turn`, so once newer work types (e.g. `standing_order`) exist in the database, don't roll back past this change.

**Interfaces:**
- Produces:
  ```ocaml
  type listed =
    | Known of t
    | Unknown of { id : string; timestamp : string; stored_work_type : string;
                   session_key : string option; room_id : string option }
  val list_rows : db:Sqlite3.db -> ?work_type:work_type -> ?session_key:string -> ?room_id:string ->
                  ?config_hash:string -> ?limit:int -> unit -> listed list     (* same filters and order as query *)
  val listed_to_json : listed -> Yojson.Safe.t
     (* Known s -> to_json s; Unknown u -> {"id","timestamp","work_type":"unknown","stored_work_type","session_key","room_id"} *)
  (* unchanged signatures, changed behaviour: query -> Known rows only; get_by_id -> None for Unknown, with a Logs.warn
     naming the id and stored type; export_json -> writes listed_to_json for every row, Unknown included, returns the count *)
  ```
  `work_type_of_string` is unchanged (it already returns `None` for unknown strings); `row_of_stmt` returns `listed`. The inline-record labels don't shadow `t`'s. `create`, `persist` and `work_type` are unchanged.
- **Tests whose expectations change: none.** Checked `test/test_access_snapshot.ml` (`work_type invalid` already expects `None` from `work_type_of_string`), `test/test_slack_huddles.ml` and `test/test_background_task.ml`. No test persists an unknown `work_type` string.

- [ ] **Step 1: Write the failing tests.** The setup (the column is `room_classification`, the 25th in `snapshot_select_columns` at `access_snapshot.ml:354-361`):

```ocaml
let cfg = Config_loader.parse_config (Yojson.Safe.from_string {|{ "workspace": "/tmp/test" }|})
let with_db f =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Access_snapshot.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)
let persisted db = Access_snapshot.create_and_persist ~db ~config:cfg ~work_type:Room_turn
    ~session_key:"slack:C1" ~room_id:"C1" ~room_policy_decision:"allow" ()
let set db sql id = Sql_util.exec_with_params db sql [ Sqlite3.Data.TEXT id ]
let to_unknown db id = set db "UPDATE access_snapshots SET work_type='standing_order' WHERE id=?" id
```
  - `unknown work type fails closed; listing shows unknown`:
    1. `known = persisted db` and `unknown = persisted db`; then `to_unknown db unknown.id`.
    2. `get_by_id ~db unknown.id = None`, and `get_by_id ~db known.id <> None`.
    3. The ids from `query ~db ()` are `[ known.id ]`, and `list_rows ~db ()` contains `Unknown {stored_work_type = "standing_order"}`.
    4. In `Test_helpers.with_temp_home (fun dir -> …)`, with `path = Filename.concat dir "snapshots.jsonl"`, `export_json ~db ~path () = 2`, and the file contains `{|"work_type":"unknown"|}` and `{|"stored_work_type":"standing_order"|}`.
  - `corrupt classification decodes Rm_unknown`: with `s = persisted db`, after `set db "UPDATE access_snapshots SET room_classification='not json' WHERE id=?" s.id`, `get_by_id` returns `Some s` with `s.room_classification = Runtime_config_types.Rm_unknown`.
  - `huddle tools deny unknown work type`: `with_fixture (fun f -> f.turn (fun () -> …))`. Run `ignore (Sqlite3.exec f.db "UPDATE access_snapshots SET work_type='standing_order'")`, then `let* result = f.invoke "slack_huddles" in expect_error result`, and check `!(f.calls) = []`: denied before the network.
  - `github room tools deny unknown work type`: with `s = persisted db`, `to_unknown db s.id`. Then `Github_room_tools.room_for_context ~db ~tool_name:"github_room_issue" (Some { Tool.default_context with snapshot_id = Some s.id })` is `Error e` with `contains e "unavailable"`. Before the change it returns `Ok "C1"` (an allow decision with an empty allowlist).

- [ ] **Step 2: Run to verify it fails.** `T access_snapshot_decode` → FAIL at compile time: `Unbound constructor Access_snapshot.Unknown`.

- [ ] **Step 3: Implement** `listed`, `list_rows`, `listed_to_json` and the new `row_of_stmt`/`query`/`get_by_id`/`export_json` behaviour.

- [ ] **Step 4: Run to verify they pass:** `T access_snapshot_decode`, `T access_snapshot`, `T slack_huddles`, `T background_task`. Expected: all PASS.

- [ ] **Step 5: FMT, then commit.** `git add src/access_snapshot.ml test/test_access_snapshot_decode.ml test/dune test/test_main.ml docs/public/llms-full.txt`; message: `fix(access): unknown snapshot work types decode fail-closed`, a blank line, then `get_by_id returns None (authorization denies); exports list them as unknown.` and `No existing test expectations change (checked access_snapshot, slack_huddles, background_task).`

---

## Final verification (whole branch)

From `/home/xertrov/src/clawq/.worktrees/huddle-orders`, one at a time:

- [ ] `QUICK` → all suites PASS, including every B832/B833 suite (`slack*`, `access_snapshot`), so §7.21 holds.
- [ ] The second `FMT` command → exit 0, no diff.
- [ ] Minimal build: core (with `Slack_config_types` and the new field) builds without integrations:
  ```bash
  CLAWQ_BUILD_MINIMAL=true OCAMLRUNPARAM=o=120,O=120 build2 opam exec --switch=clawq-5.1 -- dune build --root . -j2 src/main_min.exe
  exe=_build/default/src/main_min.exe; echo "$exe $((($(stat -c%s "$exe") + 1023) / 1024)) KB"
  ```
  Expected: the build succeeds, and the last line is `_build/default/src/main_min.exe <n> KB` (inside the worktree, so not master's binary).
- [ ] `CYCLES` → no output.
- [ ] `wc -l src/slack_{config_types,event_log,relay_guard,huddle_classify,huddle_ledger,huddle_watch,huddle_observer}.ml test/test_slack_{event_fixtures,event_log,turn_gate,relay_guard,history_hardening,huddle_classify,huddle_ledger,huddle_observer}.ml test/test_access_snapshot_decode.ml` → each under 1000. `src/runtime_config_types.ml` grew by only the one field.

**Known gaps, left on purpose:**
- `Slack_huddle_watch.of_db` and `Slack_relay_guard.phase1_registry` return "nothing". Phases 2 and 3 replace their bodies; the refs stay as test seams.
- A notes-ready reply seen before its root (daemon restarted mid-huddle, or a missed thread-started root) stays a `Notes_stub` row. A later `record_seen` for that root never merges the stub into the room row; the phase-4 reconciler resolves stubs by id under its snapshot (spec §6.1, §6.3 step 3). Until then a stub expires with the 30-day TTL.
- Observer effects for `Own_relay`, `Target_*` and `Posture_change` are no-ops until phase 3. Channel-less file reactions are dropped at the observer gate until phase 3 resolves `item.file` through the fire ledger (Task 7).
- The observer doesn't "wake the executor" or "create the fire after the room is resolved" (§6.2 Effects): neither exists yet. §6.7 "Fixed report templates" ship with the first report in phase 3.
- User docs for relay-thread silence and untrusted relay labelling come in phase 3, when clawq first posts relays. Phase 1 documents only the event log, the Slackbot exclusion and the file-key allowlist.
- The notice matchers stay empty until Claude's notice wordings are captured (still open in §4). Until then every target reply classifies as `Reply`, and phase 3's picked_up rule (the first non-notice reply) counts an uncaptured notice as pickup. Phase 3 review should check that the captures exist before #core-sync is enabled.
- Classifier rules are re-checked against P0-5 payloads: a thread-started huddle, a participant-shared canvas, room-id reuse and deletion.
- The relay footer `clawq-relay/1 <32 hex>` is fixed. P0-4 confirmed that the message `text` is `initial_comment` verbatim, and the footer has no `&`, `<` or `>` for Slack to escape, so structural detection works on live relays. Phase 3 must emit exactly `Slack_relay_guard.footer ~nonce`.
- §6.10 spoken requests are phase 6 (see Out of scope).
