# Formal Verification: Spec-Implementation Gap Audit (2026-03-08)

This audit focuses on the gap between what Coq proves and what the OCaml runtime
actually uses. The existing audit (2026-03-07) covers proof-level gaps and
theorem quality; this document covers the **wiring gap** — extracted code that
isn't called, runtime logic that has no Coq backing, and concrete remediation
steps to close the loop.

Related documents:
- `docs/src/content/docs/formal-verification-audit-2026-03-07.mdx` — proof quality audit
- `docs/src/content/docs/formal-verification-roadmap-2026-03-07.mdx` — proof expansion roadmap
- `old-docs/FORMALIZATION_PLAN.md` — phase history and lessons learned
- `docs/AGENT_LOOP_RUNTIME_CROSSCHECK.md` — AgentLoop model-to-runtime mapping
- `docs/CHANNEL_AUTH_RUNTIME_MAP.md` — ChannelAuth model-to-runtime mapping
- `docs/SECRET_STORE_RUNTIME_MAP.md` — SecretStore model-to-runtime mapping

---

## Executive Summary

The project has 27 Coq modules with 374 verified theorems/lemmas across 22
feature areas (F0–F22). Only 1 admitted statement remains (string parsing
round-trip in McpFraming.v). All LandlockPolicy.v bitwise arithmetic lemmas
are now fully proved.

However, **only 4 of 16 extracted functions are called at runtime**. The
remaining 8 spec-only modules prove properties about abstract models that the
OCaml runtime reimplements independently. This means most proofs serve as
documentation rather than enforcement.

---

## Extraction Usage: What's Actually Wired

### Extracted AND called at runtime (high assurance)

| Extracted Function | Coq Source | Runtime Call Site | What It Guards |
|---|---|---|---|
| `Clawq_core.is_path_safe_segs` | PathSafety.v | `tools_builtin.ml:is_path_safe_coq` | Directory traversal |
| `Clawq_core.is_shell_safe` | QuoteParsing.v | `tools_builtin.ml:has_unsafe_shell_syntax` | Shell injection |
| `Clawq_core.split_words` | QuoteParsing.v | `tools_builtin.ml:split_command_words` | Command tokenization |
| `Clawq_core.dispatch` | Cli.v | `command_bridge.ml` | CLI routing |

### Extracted but NOT called at runtime (dead extracted code)

| Extracted Function | Coq Source | Why Not Called |
|---|---|---|
| `Clawq_core.validate_config` | Config.v | Runtime does its own validation in config_loader.ml |
| `Clawq_core.validate_config_full` | Config.v | Same — never wired into config loading |
| `Clawq_core.valid_weights` | Config.v | Same |
| `Clawq_core.valid_port` | Config.v | Same |
| `Clawq_core.valid_temperature` | Config.v | Same |
| `Clawq_core.default_config` | Config.v | Runtime uses Runtime_config.default instead |
| `Clawq_core.parse_command` | Cli.v | Runtime uses command_bridge.ml directly |
| `Clawq_core.normalize` | PathSafety.v | Called indirectly via is_path_safe_segs |
| `Clawq_core.is_prefix` | PathSafety.v | Called indirectly via is_path_safe_segs |
| `Clawq_core.is_allowed` | ChannelAuth.v | Used at runtime for channel auth |
| `Clawq_core.is_allowed0` | ShellSafety.v | Used via is_allowed in tools_builtin.ml |

Note: `is_allowed` and `is_allowed0` are extracted from different modules
(ChannelAuth.v and ShellSafety.v respectively) with different semantics.

### Spec-only modules (not extracted, runtime reimplements)

| Module | Coq Theorems | Runtime Equivalent | Gap Type |
|---|---|---|---|
| AuditChain.v | 7 | src/audit.ml | Parameterized over abstract hash/hmac; runtime uses Digestif directly |
| AuditRetention.v | 12 | src/audit.ml (purge_old) | Depends on AuditChain parameters |
| RateLimiter.v | 10 | src/rate_limiter.ml | Uses Z (integers); runtime uses floats |
| AgentLoop.v | 22 | src/agent.ml | Fuel-based model; runtime uses Lwt async |
| SessionIsolation.v | 18 | src/session.ml | FMapAVL model; runtime uses Hashtbl + Lwt_mutex |
| SecretStore.v | 25 | src/secret_store.ml | Crypto axiomatized; runtime uses mirage-crypto C FFI |
| LandlockPolicy.v | 12 | src/landlock.ml + landlock_stubs.c | Policy spec; runtime uses C FFI |
| ToolSafety.v | 24 | src/tools_builtin.ml | Risk levels, authorization, allowlist policy |
| McpFraming.v | 30 | src/mcp_server.ml | JSON-RPC framing, dispatch, ID pairing |
| SandboxPolicy.v | 16 | src/sandbox.ml | Backend detection, command wrapping policy |
| PairCoding.v | 34 | src/pair_coding_types.ml | Phase transitions, approval protocol |
| PmodelParsing.v | 13 | src/pmodel.ml | Format classification, conversion, deprecation |
| TaskTree.v | 20 | src/task_tree_core.ml | Status transitions, terminal/stuck states |
| SchedulerCron.v | 20 | src/scheduler.ml | Cron field matching, validity ranges |
| DiscordGateway.v | 20 | src/discord_gateway.ml | Gateway protocol state machine |

---

## Defense-in-Depth Drift Analysis

`tools_builtin.ml` implements a dual-check pattern: both Coq-extracted and
OCaml-native versions of path safety and shell safety checks run, with drift
warnings logged when they disagree. This is good practice, but:

1. **The OCaml-only checks have no formal backing.** Functions like
   `is_path_safe_ocaml` (realpath-based), `is_workspace_safe_arg`,
   `has_workspace_unsafe_args`, and `is_workspace_safe_command_token` are
   security-critical but unverified.

2. **If OCaml is more permissive than Coq on any input, and the system uses the
   OCaml result, the Coq proof doesn't protect.** The drift warning is
   informational only.

3. **No automated conformance testing exists.** The drift detection relies on
   runtime encounters. Property-based tests comparing Coq and OCaml outputs on
   random inputs would be more reliable.

---

## Security-Critical OCaml Code with No Coq Backing

### HTTP Authentication (src/http_server.ml, src/pairing.ml)

- Bearer token extraction and validation
- Constant-time comparison via Eqaf
- Pairing code generation (SHA256 + Base64)
- Lockout logic with mutable state

No Coq spec exists for any of this.

### Protocol State Machines

- `src/discord_gateway.ml` — Hello/Identify/Resume/Heartbeat/Dispatch state machine
  **Now covered by DiscordGateway.v** (20 theorems: heartbeat lifecycle, session
  persistence, reconnect/resume safety, phase transitions)
- `src/slack_api.ml` — HMAC-SHA256 signature verification
- `src/slack_socket.ml` — Envelope parsing and ACK protocol
- `src/telegram.ml` — Long-polling and rate limiting

The ChannelAuth.v proofs cover allowlist and freshness abstractions but not
the actual protocol handling for Slack/Telegram.

### Database/Persistence (src/memory.ml, src/audit.ml)

- SQLite schema management and migrations
- Audit chain maintained in SQL with no runtime verification against Coq model
- Session state persistence ordering

### Tool System (src/tool.ml, src/tool_registry.ml, src/mcp_*.ml)

- Risk level semantics — **Now covered by ToolSafety.v** (24 theorems:
  authorization requirements, allowlist/auth monotonicity, risk ordering)
- Tool search correctness
- MCP JSON-RPC framing and dispatch — **Now covered by McpFraming.v** (30 theorems:
  framing length, ID pairing, dispatch correctness)

---

## Concrete Remediation Plan

### Quick wins (low effort, high value)

1. **Wire `validate_config_full` into config loading.**
   Call `Clawq_core.validate_config_full` from `config_loader.ml` after
   parsing. The function is already extracted; it just needs to be called.
   This immediately gives formally verified config validation.

2. **Add property-based conformance tests for path/shell safety.**
   Generate random path segments and shell strings. Assert that
   `Clawq_core.is_path_safe_segs` and `is_path_safe_ocaml` agree.
   Assert that `Clawq_core.is_shell_safe` and `has_unsafe_shell_syntax_ocaml`
   agree. Run as part of `make test`.

3. **Promote drift warnings to test failures.**
   If the dual-check drift detection in `tools_builtin.ml` ever fires in
   tests, it should fail the test. Currently drift is only logged at runtime.

### Medium effort (meaningful assurance improvement)

4. **Instantiate AuditChain and call extracted verify_chain.**
   The AuditChain module is parameterized over abstract `hash`/`hmac`.
   Instantiate with concrete SHA256/HMAC-SHA256 (as simple string wrappers
   for extraction), extract `verify_chain` and `make_entry`, and call them
   from `src/audit.ml`. This closes the biggest spec-implementation gap.

5. **Extract and call ChannelAuth allowlist functions.**
   `is_allowed` from ChannelAuth.v is already extracted. Wire it into the
   actual channel authorization paths in `src/slack.ml`, `src/discord.ml`,
   `src/telegram.ml` (replacing or augmenting the OCaml implementations).

6. **Add runtime assertions for RateLimiter invariants.**
   The Coq proofs establish `tokens <= max_tokens` after refill. Add
   `assert (bucket.tokens <= bucket.max_tokens)` in the OCaml rate limiter
   as a runtime check that the float implementation respects the proved
   invariant.

### Longer term (architecture-level)

7. **Investigate extractable audit chain.**
   Instantiate AuditChain.v with concrete crypto (possibly via extraction
   hooks that map to Digestif) so the entire chain construction and
   verification logic comes from Coq.

8. **Model HTTP auth in Coq.**
   Bearer token validation and pairing logic are small enough to formalize.
   The pairing state machine (code generation, lockout, expiry) is a good
   candidate for spec-only verification.

9. **Conformance test suite for all spec-only modules.**
   For each spec-only Coq module, write OCaml tests that exercise the same
   scenarios the Coq theorems prove, ensuring the runtime implementation
   matches the proved specification even without extraction.

---

## Theorem Count Summary

| Feature | Coq File(s) | Theorems | Admitted | Extracted | Runtime Use |
|---|---|---|---|---|---|
| F0 CLI | CliProofs.v | 22 | 0 | Yes | dispatch only |
| F1 Config | ConfigProofs.v | 15 | 0 | Yes | Not called |
| F2 PathSafety | PathSafety.v | 19 | 0 | Yes | Called |
| F3 AuditChain | AuditChain.v | 7 | 0 | No | Spec-only |
| F4 RateLimiter | RateLimiter.v | 10 | 0 | No | Spec-only |
| F5 ConfigExt | Config.v + ConfigProofs.v | 15 | 0 | Yes | Not called |
| F6 ShellSafety | QuoteParsing.v + ShellSafety.v | 9 | 0 | Yes | Called |
| F7 SecretStore | SecretStore.v | 25 | 0 | No | Spec-only |
| F8 ChannelAuth | ChannelAuth.v | 20 | 0 | Yes | Partially called |
| F9 AuditRetention | AuditRetention.v | 12 | 0 | No | Spec-only |
| F10 AgentLoop | AgentLoop.v | 22 | 0 | No | Spec-only |
| F11 SessionIsolation | SessionIsolation.v | 18 | 0 | No | Spec-only |
| F12 LandlockPolicy | LandlockPolicy.v | 12 | 0 | No | Spec-only |
| F13 ToolSafety | ToolSafety.v | 24 | 0 | No | Spec-only |
| F14 ConfigMerge | ConfigMerge.v | 12 | 0 | No | Spec-only |
| F15 McpFraming | McpFraming.v | 30 | 1 | No | Spec-only |
| F17 SandboxPolicy | SandboxPolicy.v | 16 | 0 | No | Spec-only |
| F18 PairCoding | PairCoding.v | 34 | 0 | No | Spec-only |
| F19 PmodelParsing | PmodelParsing.v | 13 | 0 | No | Spec-only |
| F20 TaskTree | TaskTree.v | 20 | 0 | No | Spec-only |
| F21 SchedulerCron | SchedulerCron.v | 20 | 0 | No | Spec-only |
| F22 DiscordGateway | DiscordGateway.v | 20 | 0 | No | Spec-only |
| **Total** | **27 files** | **374** | **1** | | |

---

## Key Takeaway

The formal verification foundation is strong and continues to expand (374
theorems across 27 modules, up from 160+ across 17 at the time of this audit).
New modules cover ToolSafety, ConfigMerge, McpFraming, SandboxPolicy,
PairCoding, PmodelParsing, TaskTree, SchedulerCron, and DiscordGateway.
Only 1 Admitted proof remains (down from 4).

The highest-leverage next step remains **closing the loop between proofs and
runtime**: calling extracted functions that already exist, adding conformance
tests, and wiring spec-only models into runtime assertions.
