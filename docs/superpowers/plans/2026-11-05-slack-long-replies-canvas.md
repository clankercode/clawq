# I065 Slack long-reply canvas implementation plan

> **For agentic workers:** Execute this plan task-by-task using the superpowers writing-plans checklist structure. This run is design-only. Implementation requires a subsequent implementation assignment; the current instruction prohibits subagents. Preserve that restriction unless Max changes it.

**Goal:** Deliver Clawq's long final Slack answers as an opening paragraph plus a read-only canvas link, with lossless Markdown-file/plain-message fallback and an explicit research canvas tool.

**Architecture:** Pure policy/config and a neutral final-delivery callback in core; policy-checked Slack canvas/upload adapters, a SQLite delivery journal and final-only routing in integrations. Buffer answer deltas while existing native streams show status. A durable create-attempt fence enforces at-most-one canvas even when Slack's response is lost.

**Tech Stack:** OCaml 5.1, Lwt, Yojson.Safe, SQLite3, existing Slack_transport/Policy_http_client, Alcotest fake HTTP, dune and ocamlformat 0.28.1 through the serialized remote gate.

**Spec:** `docs/superpowers/specs/2026-11-05-slack-long-replies-canvas-design.md` (binding behaviour and public Slack API citations); approved requirements at `.superpowers/sdd/2026-11-05-slack-canvas-replies/requirements.md`.

## Global Constraints

- Default threshold **2500**, strictly greater-than, Unicode scalar values. Modes **canvas|file|off**, default **canvas**; threshold validation **1..40000**. Opening block summary at most **600** scalar values, title at most **120**. Canvas payload conservative ceiling **1048576 bytes and scalar values**, tables **300 cells**. Exact extraction/conversion rules are in the spec.
- Core modules: existing `Slack_config_types`, new **Slack_reply_types**, **Slack_long_reply_policy**, **Session_final_reply**. Integrations modules: new **Slack_canvas**, **Slack_file_upload**, **Slack_reply_store**, **Slack_reply_delivery**, **Slack_canvas_tool**, **Slack_message_transport**. Add module names to the correct explicit lists in `src/dune`. No HTTP/network dependency enters `clawq_runtime_core`; the real tool belongs in `clawq_runtime_integrations`. Core's registered placeholder returns **disabled in minimal build**.
- Soft file-size limit **1000 lines**, hard **2000 lines**; each new module below **1000**. `src/slack.ml` starts at **1121** and must shrink when touched. Extract transport wrappers rather than append policy to it. Avoid Session↔Slack cycles.
- All build/test/format/generation commands use **`/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh`**. Never run dune locally or in parallel. The gate isolates HOME and serializes remote work; no direct live Slack, local daemon, token, `~/.clawq`, attn, other worktree, push, merge or rebase. Only fake HTTP with injectable zero-delay clocks. Temporary SQLite/config data stays under test temp roots.
- Register every new suite in **both `test/dune` and `test/test_main.ml`**, even when adding more cases to a suite created in an earlier task. Stage only the task's explicit files; list generated `.ml` paths explicitly after inspecting their diff. No `git add -A`.
- Final-only semantics include native/legacy, root/thread, DM/MPDM, queue replay and autonomous/agent-command finals. Existing string notifiers remain progress/notice senders. Preserve B845 blank/suppression checks, current authorization, private-channel policy, owner/stop rules and response-sent semantics.
- Public docs require link sharing before access-set and user grants for DM/MPDM. Do not fabricate a DM channel grant, permalink or Slack idempotency key. A failed permission operation is a canvas failure. Coordinator verifies live permissions after deployment; implementers never call live Slack.
- Upload v2 is **absent in this checkout**. Task 2 creates its adapter; do not depend on future standing-order code. If it appears before implementation, reconcile its actual contract and reuse only a policy-safe implementation.
- Two spec entries are **Default pending Max** (DM membership lifetime; share-before-read interval). Implement the stated defaults unless Max directs otherwise, and retain coordinator acceptance checks.

## Review Focus

| Input most likely to bite | Owning task and proof |
| --- | --- |
| Slack access API rejects DM/MPDM IDs and requires prior sharing; successful create is insufficient | Task 2 `dm uses users after share`, `mpdm incomplete members falls back`, `grant failure is not success`; assert operation order and exact recipients. Task 3 proves recovery of carrier/fallback. |
| `turn_verified_stream` is provider streaming; native streams currently contain status only | Task 4 `native long final never streams body`; inspect start/append/stop/post calls across live, drain and replay; assert one final and no answer body in stream calls. |
| Crash after remote create but before saving ID; durable replay regenerating an answer or using another turn's identity | Task 3 `lost create response never creates twice`, Task 4 `replay resumes stored final before model`; assert one create, zero replay model calls, stable request slot despite different generated text/attempt count. |
| Captured config and mixed progress/final notifiers; private-channel/egress revocation during fallback | Task 1 `reload changes next final`, Task 3 `policy revocation stops fallback`, Task 4 `relay and notices bypass overflow`; assert effective config and no unauthorized writes. |
| Slack Markdown/byte limits and unsupported assumed upload implementation | Task 1 `unicode boundary and fenced opening`, Task 2 `upload v2 uses byte length and parent`, Task 3 `unsupported markdown goes to exact file`; assert original byte equality, form encoding and no lossy canvas. |

## Task 1: pure policy, config and neutral delivery contract

**Files**

- Create: `src/slack_reply_types.ml`, `src/slack_long_reply_policy.ml`, `src/session_final_reply.ml`, `test/test_slack_long_reply.ml` (suite `slack_long_reply`).
- Modify: `src/slack_config_types.ml`, `src/runtime_config_types.ml`, `src/config_loader_channels.ml`, `src/runtime_config_json_sections.ml`, `src/config_set.ml`, `src/config_tree.ml`, `src/config_search.ml`, `src/config_show.ml` where needed for actual enumeration; `src/session_types.ml`, `src/session_core.ml`, `src/dune`, `test/dune`, `test/test_main.ml`.
- Test/fixture modifications: `test/test_config_loader.ml`, `test/test_config_set.ml`, `test/test_slack.ml` and every explicit `slack_config` record constructor found by compiler/search; add `long_reply` defaults, without unrelated fixture edits.

**Interfaces — consumes JSON/current config; produces typed policy and a core callback seam**

```ocaml
(* Slack_config_types additions *)
type long_reply_mode = Canvas | File | Off
type long_reply = { mode : long_reply_mode; threshold : int }
val default_long_reply : long_reply
val long_reply_of_json : Yojson.Safe.t -> long_reply
val long_reply_to_json : long_reply -> Yojson.Safe.t
val validate_long_reply : Yojson.Safe.t -> (unit, string) result
(* Runtime_config_types.slack_config adds long_reply : Slack_config_types.long_reply *)

(* Slack_reply_types: no dependency on Session or integration modules *)
type context = {
  connector : string; session_key : string; channel_id : string;
  thread_ts : string option; requester : string option;
  request_id : string; ordinal : int;
}
type target = {
  team_id : string; app_id : string; workspace_url : string;
  channel_id : string; thread_ts : string option;
  requester : string; context : context;
}
type slot = Final of int | Tool of string
type artifact = { canvas_id : string; permalink : string; shared_ts : string }
type outcome = Canvas_sent of artifact | File_sent of string | Text_sent | Suppressed
type error = Policy_denied | Storage_unavailable | Cancelled | Failed of string
type sender = context -> string -> unit Lwt.t

(* Slack_long_reply_policy *)
val code_points : string -> (int, string) result
val summary : string -> string
val overflows : Slack_config_types.long_reply -> string -> bool
val prepare_markdown : string -> (string, string) result
val automatic_title : request_id:string -> string

(* Session_types.t adds mutable final_reply_sender : Slack_reply_types.sender option *)
(* Session_final_reply uses Session_types.t, never Session or Slack_canvas *)
val with_context : Slack_reply_types.context -> (unit -> 'a Lwt.t) -> 'a Lwt.t
val current_context : unit -> Slack_reply_types.context option
val send : Session_types.t -> context:Slack_reply_types.context ->
  fallback:(string -> unit Lwt.t) -> string -> unit Lwt.t
```

`Session_final_reply.send` invokes the installed sender for Slack, otherwise fallback; an absent sender also uses fallback. It does not classify arbitrary progress. Its context key supplies verified connector metadata for later prompt wiring. Initialize the field to `None`, and ensure the metadata is scoped/cleared with `Lwt.finalize`. It must not imply tool authority (the tool separately resolves ingress/snapshot).

**Test-first steps**

- [ ] Register `slack_long_reply`; write `threshold equality stays in chat` (2499/2500 do not overflow; 2501 does), `unicode boundary and fenced opening` (multibyte scalars, CRLF, blank line inside fence, heading/list first block, no paragraph break, 599+ellipsis, whitespace and literal mentions), `markdown limits preserve original` (300/301 cells, code untouched, h4/HTML rejected, bytes/scalars ceiling).
- [ ] Write `long reply config round trips` (missing defaults; all three modes; unknown mode warns once with no raw value), `config set rejects malformed nested values` (leaf, whole object and parent-object routes; 0/40001/float/bool/null/unknown key; actionable examples), `reload changes next final` (swap manager config before final; sender reads new mode/threshold).
- [ ] Run the new suite red. Implement pure policy and JSON codec/schema/serialization; add `channels.slack.long_reply` leaves and whole-object/ancestor validation in config_set. Use the existing runtime reload flow; do not cache policy in a connector closure.
- [ ] Implement/test `final callback is separate from progress` and `neutral context is restored`: callback called once for Slack final, non-Slack fallback once, a progress notifier untouched, nested context unwinds on exception.
- [ ] Run green and inspect the core module graph for references to Slack network modules. Commit only explicit files.

**Verification** (each command runs sequentially):

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_long_reply
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite config_set
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite config_loader
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh minimal
```

**Commit:** `feat(slack): add long-reply policy and live config`

## Task 2: policy-safe canvas and upload-v2 adapters

**Files**

- Create: `src/slack_canvas.ml`, `src/slack_file_upload.ml`, `test/test_slack_canvas.ml` (suite `slack_canvas`), `test/test_slack_file_upload.ml` (suite `slack_file_upload`).
- Modify: `src/slack_thread_api.ml` (safe reusable explicit-context checks, without weakening authorization), `src/slack_transport.ml` (injectable form/raw requests if missing), `src/dune`, `test/dune`, `test/test_main.ml`.

**Interfaces — consumes a server-resolved target and policy-checked transport; produces IDs/receipts, never user-facing raw errors**

```ocaml
(* Slack_canvas; request encapsulates current config + egress + audit *)
type call = string -> (string * Yojson.Safe.t) list ->
  (Yojson.Safe.t, Slack_reply_types.error) result Lwt.t
type recipients = Channel of string | Users of string list
val resolve_target : manager:Session.t -> context:Slack_reply_types.context ->
  (Slack_reply_types.target * recipients, Slack_reply_types.error) result Lwt.t
val create : call:call -> title:string -> markdown:string ->
  (string, Slack_reply_types.error) result Lwt.t
val permalink : call:call -> target:Slack_reply_types.target -> canvas_id:string ->
  (string, Slack_reply_types.error) result Lwt.t
val set_read : call:call -> canvas_id:string -> recipients:recipients ->
  (unit, Slack_reply_types.error) result Lwt.t

(* Slack_file_upload; network methods remain in integrations *)
type ticket = { file_id : string; upload_url : string }
type transport = {
  api : verb:string -> encoding:[ `Form | `Json ] -> method_:string ->
    params:(string * Yojson.Safe.t) list ->
    (Yojson.Safe.t, Slack_reply_types.error) result Lwt.t;
  upload : uri:string -> body:string ->
    (unit, Slack_reply_types.error) result Lwt.t;
}
val ticket : transport:transport -> filename:string -> length:int ->
  (ticket, Slack_reply_types.error) result Lwt.t
val put : transport:transport -> ticket:ticket -> body:string ->
  (unit, Slack_reply_types.error) result Lwt.t
val complete : transport:transport -> ticket:ticket ->
  target:Slack_reply_types.target -> summary:string ->
  (string, Slack_reply_types.error) result Lwt.t
```

`put` names the adapter operation; wire method is **POST**, not HTTP PUT. Callers own journalling/once-only decisions. `call` dispatches GET for files.info/metadata and POST for create/access/owned-canvas cleanup; the upload ticket method uses form encoding even though most other writes are JSON. Resolve current policy before every call, including rechecking metadata/membership before grants. Resolve legacy installation identity from auth.test plus bots.info when there is no verified envelope/native route; validate returned bot/app/team and never guess an app ID. Bound DM/MPDM pagination and reject incomplete/>20 member lists; distinguish `is_mpim` from a private channel. Use a closed method list and sanitized error codes. An injected fake exercises the same serialization and policy path as production.

**Test-first steps**

- [ ] Register suites and write `standalone create has no channel id` (exact title/document_content, no token in JSON; returns ID), `permalink requires matching private file` (files.info GET, returned permalink, missing scope/mismatched host/public-only result errors), `read channel grant` (one channel, `read`, no org/public/write).
- [ ] Write `dm uses users after share` and `mpdm incomplete members falls back`: typed metadata selects users, remove self, dedupe, no `D…` or MPDM ID in channel_ids, paginated completeness, 20/21 boundary, policy/private-channel deny before create. Task 3 will test the full share-before-grant order.
- [ ] Write `upload v2 uses byte length and parent`: ticket form fields, multibyte byte count, returned URL POST exact original bytes, complete JSON uses one channel, original parent, summary comment. `unsafe upload host never receives token` asserts HTTPS/host checks, egress denial, no redirects/bearer forwarding/logged signed URL. `api failures are sanitized` covers 429, missing_scope, disabled/free-team create, malformed response and content detail containing a secret.
- [ ] Run red; implement adapters and metadata resolution. Never use create(channel_id) as a free-plan recovery path. Never treat the requirements' paid-plan inference as verified.
- [ ] Run green and commit explicit paths.

**Verification:**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_canvas
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_file_upload
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
```

**Commit:** `feat(slack): add scoped canvas and upload-v2 adapters`

## Task 3: durable artifact delivery and fallback

**Files**

- Create: `src/slack_reply_store.ml`, `src/slack_reply_delivery.ml`, `test/test_slack_reply_delivery.ml` (suite `slack_reply_delivery`).
- Modify: `src/dune`, `test/dune`, `test/test_main.ml`; add recovery cases to `test/test_slack_canvas.ml` and `test/test_slack_file_upload.ml` where they own adapter behaviour.

**Interfaces — consumes Task 1 policy/context and Task 2 adapters; produces persisted delivery outcome**

```ocaml
(* Slack_reply_store; row is abstract, persisted with original body and all stages *)
type row
val init : Sqlite3.db -> unit
val ensure : db:Sqlite3.db -> target:Slack_reply_types.target ->
  slot:Slack_reply_types.slot -> policy:Slack_config_types.long_reply ->
  title:string -> body:string -> (row, Slack_reply_types.error) result
val claim_canvas : db:Sqlite3.db -> row -> bool
val save_canvas : db:Sqlite3.db -> row -> canvas_id:string -> unit
val save_carrier : db:Sqlite3.db -> row -> ts:string -> unit
val lookup : db:Sqlite3.db -> context:Slack_reply_types.context ->
  slot:Slack_reply_types.slot -> row option
val body : row -> string
val outcome : row -> Slack_reply_types.outcome option
val tombstone_session : db:Sqlite3.db -> session_key:string -> unit

(* Slack_reply_delivery *)
type deps = {
  call : Slack_canvas.call;
  upload : Slack_file_upload.transport;
  post : target:Slack_reply_types.target -> text:string ->
    (string, Slack_reply_types.error) result Lwt.t;
  edit : target:Slack_reply_types.target -> ts:string -> text:string ->
    (unit, Slack_reply_types.error) result Lwt.t;
  delete : target:Slack_reply_types.target -> ts:string ->
    (unit, Slack_reply_types.error) result Lwt.t;
}
val deliver : manager:Session.t -> deps:deps ->
  context:Slack_reply_types.context -> body:string ->
  (Slack_reply_types.outcome, Slack_reply_types.error) result Lwt.t
val create_explicit : manager:Session.t -> deps:deps ->
  context:Slack_reply_types.context -> title:string -> markdown:string ->
  (Slack_reply_types.artifact, Slack_reply_types.error) result Lwt.t
val resume : manager:Session.t -> deps:deps -> context:Slack_reply_types.context ->
  (Slack_reply_types.outcome option, Slack_reply_types.error) result Lwt.t
```

Store internal methods additionally persist permalink, ticket/stage, original policy, intent/outcome and safe error using CAS. `claim_canvas` is atomic and one-way; `false` never authorizes another create. New worker leases cannot clear it. The explicit tool uses `Tool digest` slots; automatic finals use `Final ordinal`. `resume` returns `None` only when no persisted final exists; otherwise it completes/reuses that final. Original bytes never change on retry. Outcome errors propagate rather than setting sent markers. The explicit method shares title/link and grants read, but never silently returns a file receipt.

**Test-first steps**

- [ ] Register the suite. Write `canvas success posts summary link once`: exact order create → files.info → post → access-set; one visible carrier, read access, no full text posted. `grant failure is not success`: file fallback, original bytes, summary comment, carrier cleanup; failed delete triggers safe edit. `failed canvas cleanup is scoped` asserts only this row's known new canvas is deleted, retained body survives, failed deletion leaves a cleanup flag and never blocks fallback.
- [ ] Write `canvas failure uploads markdown`, `both artifacts fail posts full text`, `file and off modes skip canvas`, `unsupported markdown goes to exact file`, `blank or suppressed reply sends nothing`. Assert exact file body/plain body and no raw API error text.
- [ ] Write `lost create response never creates twice`: crash at each fence/response/persist boundary, reopen DB, retry with changed input text and attempt number; one create total and persisted original answer used. Also `concurrent workers share one canvas`, `known canvas resumes grant`, `explicit duplicate reuses receipt`, `identical explicit writeup reuses final link`, `tool and different final have distinct slots`.
- [ ] Write `ambiguous upload completion is not completed twice` (known file shares reconcile; no new ticket when uncertainty forces full text), `carrier failure retains delivery`, `truncated text is not sent`, `policy revocation stops fallback` (no upload/post after revocation), `cancel does not start fallback`, `journal unavailable never creates canvas`, `reset retains fence but removes answer`. Use temporary DBs and fake faults, no real sleeps.
- [ ] Run red, implement additive schema/transactions and coordinator; keep HTTP outside transactions. Persist body and stage intent before writes. Retain stage state on errors and expose only sanitized failure codes.
- [ ] Run green. Review that private/access/egress errors do not trigger a less-restricted fallback, and commit explicit files.

**Verification:**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_reply_delivery
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_canvas
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_file_upload
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
```

**Commit:** `feat(slack): journal long-reply delivery and lossless fallback`

## Task 4: wire final paths, buffered streaming and durable replay

**Files**

- Create: `src/slack_message_transport.ml`, `test/test_slack_reply_paths.ml` (suite `slack_reply_paths`).
- Modify: `src/slack.ml`, `src/slack_thread_execution.ml`, `src/slack_thread_runs.ml` only where needed to preserve finish ordering, `src/slack_thread_route.ml`, `src/daemon_inbound_replay.ml`, `src/daemon_startup.ml`, `src/daemon_resume.ml`, `src/session_turn.ml`, `src/session_queue.ml`, `src/session_autonomous.ml`, `src/session_management.ml`, `src/memory.ml` at clear-session hook, `src/dune`, `test/dune`, `test/test_main.ml`.
- Test: existing `test/test_slack_empty_reply.ml`, `test/test_slack_relay_guard.ml`, `test/test_slack_threads.ml`; locate replay's current suite in `test/test_main.ml` and add regression there if fixture reuse makes it smaller.

**Interfaces — consumes final callbacks and durable request identity; produces one chosen delivery before sent/queue deletion**

```ocaml
(* Slack_message_transport: extracted top-of-handle_message wrappers *)
type t = {
  post : text:string -> unit Lwt.t;
  post_with_id : text:string -> string Lwt.t;
  edit : ts:string -> text:string -> unit Lwt.t;
  delete : ts:string -> unit Lwt.t;
}
val make : manager:Session.t -> route:Slack_thread_route.t option ->
  channel_id:string -> thread_ts:string option ->
  legacy_send:(bot_token:string -> channel_id:string -> text:string -> unit Lwt.t) -> t

(* Slack_reply_delivery additions *)
val install : Session.t -> unit
val send_final : manager:Session.t -> context:Slack_reply_types.context ->
  fallback:(string -> unit Lwt.t) -> string -> unit Lwt.t
val resume_before_model : manager:Session.t ->
  context:Slack_reply_types.context -> bool Lwt.t
```

`install` assigns the neutral callback; it never wraps the progress registry. `send_final` returns only on confirmed success/suppression; safe errors raise retained-delivery failure. Coordinator post/edit adapters must decode Slack `ok`, errors and truncation warnings: the current unit-returning plain-send helpers alone cannot establish successful delivery. `resume_before_model` returns true for a journalled final that has been delivered/reused, false only for no final; errors retain the row. Context construction uses original queued message metadata/ID or persisted synthetic ID; freeze a final ordinal per request, not per attempt. Populate durable ID in enqueue/payload code before executing any synthetic continuation. Preserve original native root and legacy optional parent on every artifact stage.

**Test-first steps**

- [ ] Register `slack_reply_paths`. Write `native long final never streams body`: fake provider emits >2500 characters in multiple deltas; start/maintenance/stop contain status only, no append of answer, one summary/link post. Exercise native stream unavailable and long maintenance rotation too.
- [ ] Write `posted root thread dm and mpdm preserve target`: exact channel/parent matrix, metadata-driven sharing, no reply_broadcast, file fallback same destination. `drain uses queued request identity`: queue A/B with different humans; final B never uses A's IDs/authority. Test response_sent and before_drain produce only one delivery.
- [ ] Write `replay resumes stored final before model`: generate/create once, crash before queue-delete, reopen DB/replay; zero second model invocation and zero second create. Also replay without original message ID uses stable queue ID; changed attempt count does not change key. `synthetic continuation id survives retry` covers autonomous/agent-command final slots.
- [ ] Write `relay and notices bypass overflow`: long standing-order post, tool progress, thinking, pause receipt and card cause no artifact; long actual final does. `reload during generation changes delivery` and `reload after claim keeps chosen artifact` assert mode/threshold timing and refreshed authorization. `clear prevents pending answer resurrection` covers reset/clear journal tombstones.
- [ ] Run red. Extract transport wrappers and rewire only actual finals: live before_drain/result, agent-command send_reply (split debug_notify), queue-final emission in Session, autonomous on_response, native replay before_drain, resumed final sends. Keep `Slack_thread_execution.notify` and registered progress string callbacks unchanged. Use neutral final context scope around model execution for Task 5 prompt metadata.
- [ ] Install delivery at daemon startup/resume before queue replay. Call `resume_before_model` before executing replayed turns and before recoverable queued model reruns. Preserve native preparation/authorization even when reusing a delivered result; suppress duplicates without bypassing current policy.
- [ ] Ensure `slack.ml` shrinks below 1121 lines. Inventory every final callback and every generic notify use; retain exclusions. Run green, full/optimized validation for runtime reshaping, then commit explicit files.

**Verification:**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_reply_paths
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_empty_reply
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_relay_guard
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh raw 'dune exec -j 2 test/test_main.exe -- test'
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh raw 'dune build -j 2 --profile release-speed --build-dir _build_opt_speed src/main.exe'
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh minimal
```

**Commit:** `feat(slack): route final answers through durable canvas delivery`

## Task 5: explicit tool, contextual guidance and templates

**Files**

- Create: `src/slack_canvas_tool.ml`, `test/test_slack_canvas_tool.ml` (suite `slack_canvas_tool`).
- Modify: `src/tools_builtin.ml` (core placeholder/schema), `src/daemon_startup.ml`, `src/daemon_util.ml` only if reload requires rebinding, `src/slack_huddle_context.ml` (DM-compatible validated resolution), `src/room_tool_availability.ml`, `src/agent_profile.ml`, `src/prompt_builder.ml`, `src/agent_1_context.ml`, `src/agent_turn_setup.ml`, `src/session_turn.ml` for verified metadata propagation, `src/dune`, `test/dune`, `test/test_main.ml`.
- Modify: `docs/builtin-agent-prompts/researcher.md`, `docs/builtin-agent-prompts/documenter.md`, generated `src/agent_template_builtins_research.ml` and any other generated group actually changed by regeneration. Test: `test/test_prompt_builder.ml` and B844's existing availability suite.

**Interfaces — consumes current Slack authority and journal; produces schema-compatible tool and per-request prompt section**

```ocaml
(* Tools_builtin: core placeholder, exact schema/description from spec *)
val slack_canvas_create : unit -> Tool.t
(* Slack_canvas_tool: full implementation replaces placeholder *)
val make : current_config:(unit -> Runtime_config.t) -> Tool.t
val register : current_config:(unit -> Runtime_config.t) -> Tool_registry.t -> unit
(* Prompt_builder: reads neutral turn context, no integration import *)
val slack_reply_guidance : connector:string option ->
  config:Runtime_config.t -> tool_available:bool -> string option
(* Room_tool_availability addition, composed with existing checks *)
val slack_canvas_available : connector:string option -> bool
```

The pure `slack_reply_guidance` helper feeds `build` using `Session_final_reply.current_context ()`; this avoids changing every `build` caller and keeps guidance tied to verified execution, including rebuild after compaction. `Agent_profile` uses the same neutral connector metadata to hide the tool outside Slack; the invocation guard still uses Slack_huddle_context plus access snapshot. Add placeholder to core registration; full startup replaces via `Tool_registry.replace` (not duplicate register). Respect tools_enabled and template filters; current config access must survive reload. Task 1's neutral context is presentation metadata, never authority.

**Test-first steps**

- [ ] Register `slack_canvas_tool`; write `outside slack gives actionable error` (exact repair guidance, zero network), `arguments cannot select destination` (unknown fields/token/channel rejected), `valid slack tool shares read only` (title/link carrier, read grant, returned JSON only after success), `repeated call reuses artifact` (same request/title/body, one create), `failed explicit canvas returns error not fake link`.
- [ ] Write `dm tool resolves verified recipients` without widening existing standing-order/huddle grants. Authorization refactor must keep their previous restrictions intact. `stale or injected context cannot publish` proves snapshot/context invalidation remains effective.
- [ ] Add prompt cases `slack canvas guidance includes threshold`, `file and off guidance match behaviour`, `non slack omits guidance`, `hidden tool omits advice`, `static prompt still includes delivery guidance`, `compaction and reload refresh guidance`. Use neutral context scoped around each build; prevent Slack guidance leaking between tests/turns.
- [ ] Add `minimal tool is disabled` at the core placeholder; assert exact schema/description contract and B844 filtering. Run red; implement and replace at startup. Apply the spec's exact description/prompt strings, with mode interpolation only.
- [ ] Review every built-in prompt's allow/disallow lists. Grant only researcher/documenter by default; preserve existing restrictions. Generate built-ins through gate, inspect explicit changed paths, verify generated output and minimal build, then commit.

**Verification:**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_canvas_tool
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite prompt_builder
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh gen-agents
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh gen-agents-check
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh minimal
```

**Commit:** `feat(slack): expose canvas tool and contextual reply guidance`

## Task 6: operator documentation and final acceptance

**Files**

- Modify: `docs/src/content/docs/channels.mdx`, `docs/setup-guide.md`, `docs/src/content/docs/configuration.mdx`, `docs/public/llms-full.txt`.
- Test: extend `test/test_slack_reply_paths.ml`, `test/test_slack_canvas_tool.ml` only if acceptance exposes missing behavioural coverage; no new suite, and no implementation-mirroring documentation tests.

**Interfaces — consumes final public configuration/tool contract; produces operator instructions**

No new OCaml API. Document these existing produced signatures and their operational contract:

```ocaml
Slack_config_types.long_reply_of_json : Yojson.Safe.t -> Slack_config_types.long_reply
Slack_reply_delivery.deliver : manager:Session.t -> deps:Slack_reply_delivery.deps ->
  context:Slack_reply_types.context -> body:string ->
  (Slack_reply_types.outcome, Slack_reply_types.error) result Lwt.t
Slack_canvas_tool.make : current_config:(unit -> Runtime_config.t) -> Tool.t
```

**Test-first/acceptance steps**

- [ ] Before docs, run Task 4's `native long final never streams body`, `replay resumes stored final before model`, `relay and notices bypass overflow`, and Task 5's `outside slack gives actionable error` suites. Add a failing case first if the final behaviour differs from the approved spec; fix it in the owning task's module and report the deviation.
- [ ] Document defaults/modes, threshold count, opening-block rule, runtime config-set examples, file fallback, off-vs-explicit-tool distinction, private/DM/MPDM scope and limits, static prompt guidance, retained failure/journal behaviour and no-public-sharing policy. Use the same public API citations as the spec.
- [ ] Setup: list `canvases:write`, `files:write`, **`files:read`**, existing `chat:write`, `users:read` for installation/identity checks, and conversation read scopes; explain reinstall after OAuth scope additions. `canvases:read` is not required by this create/permalink flow. Document Slack API POST/GET and upload-host egress separately, no broad wildcard credential forwarding. Explain free-team fallback and that native canvas unfurl layout is not guaranteed.
- [ ] Record a coordinator-only live acceptance checklist: long root/thread/private/DM/MPDM, read-only recipients, summary/link rendering, share-before-grant behaviour, then file fallback. State that Max/coordinator does this after deployment; never execute it with tokens during implementation. Carry both pending-Max defaults into the handoff.
- [ ] Run all acceptance commands below sequentially. Confirm every new suite appears in the test list, generated agents are clean, minimal links without integrations, new files <1000 lines and `slack.ml` <1121. Check explicit diff for extraction/generated drift and secrets. If all checks pass, commit only the four docs (and explicitly identified regression files if changed); report exact commands/results and deliberate limits. No deployment/push/merge/rebase.

**Verification:**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh raw 'dune exec -j 2 test/test_main.exe -- list'
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_reply_paths
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_canvas_tool
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh minimal
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh gen-agents-check
```

**Commit:** `docs(slack): document canvas replies scopes and fallback`
