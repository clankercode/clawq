# P2-11: Test Coverage Expansion

## Context

clawq has ~226 tests (Alcotest). nullclaw has 4,975+ tests embedded throughout its source. The gap is large and primarily a function of nullclaw's policy of co-locating comprehensive unit tests with every module. This plan establishes a strategy to systematically expand test coverage to reach parity in depth (if not in raw count, given language differences).

## Current Test Structure

Tests are in `test/test_main.ml` and `test/test_*.ml`, organized as Alcotest suites. The CLAUDE.md documents the test runner invocation:

```bash
opam exec --switch=clawq-5.1 -- dune exec test/test_main.exe -- list
opam exec --switch=clawq-5.1 -- dune exec test/test_main.exe -- test <SUITE_REGEX>
```

## Strategy

### Principle: Co-locate where practical, separate when complex

For OCaml, unlike Zig, inline tests are less idiomatic. The existing `test/` directory approach is correct. Goal: **one test file per major module** with comprehensive coverage of that module's behavior, plus contract tests for interfaces.

### Test Suites to Add

#### Tier 1: Pure logic modules (high ROI, no I/O needed)

| Module | Suite | Target tests |
|--------|-------|-------------|
| `pairing.ml` | `pairing` | ~40 (code gen, brute-force, token hash, states) |
| `rate_limiter.ml` | `rate_limiter` | ~20 (token bucket, burst, cleanup) |
| `sandbox.ml` | `sandbox` | ~15 (wrap_command, is_available, detect) |
| `resilience.ml` | `resilience` | ~20 (timeout, retry backoff, fallback) |
| `audit.ml` | `audit` | ~30 (event types, chain verify, retention logic) |
| `secret_store.ml` | `secret_store` | ~20 (encrypt/decrypt roundtrip, PBKDF2, `$ENC:` prefix) |
| `vector.ml` | `vector` | ~25 (cosine similarity, merge weights, embedding parse) |
| `prompt_builder.ml` | `prompt_builder` | ~15 (section composition, redaction) |
| `scheduler.ml` | `scheduler` | ~25 (cron parse, interval, next-run calculation, overdue detection) |

#### Tier 2: I/O modules (use SQLite in-memory `:memory:`)

| Module | Suite | Target tests |
|--------|-------|-------------|
| `memory.ml` | `memory` | ~40 (store/load/search/cleanup, core memories, FTS ranking) |
| `migrate.ml` | `migrate` | ~15 (schema version, upgrade path, idempotency) |
| `config_loader.ml` | `config_loader` | ~30 (JSON parse, env substitution, defaults, bad input) |
| `tools_builtin.ml` | `tools_builtin` | ~35 (path safety, allowlist, file ops, sandbox wrapping) |
| `skills.ml` | `skills` | ~20 (load/parse skill JSON, template substitution) |

#### Tier 3: Protocol/format modules (pure parse/format, no network)

| Module | Suite | Target tests |
|--------|-------|-------------|
| `provider.ml` | `provider_format` | ~30 (message format, tool-call parse, streaming chunk parse) |
| `signal.ml` | `signal_parse` | ~20 (SSE event parse, JSON-RPC format, chunking) |
| `matrix.ml` | `matrix_parse` | ~20 (sync response parse, send format, token file) |
| `irc.ml` | `irc_parse` | ~20 (IRC line parse, SASL encoding, chunk boundary) |
| `email_channel.ml` | `email_parse` | ~25 (IMAP response, RFC 2047 decode, HTML strip, SMTP format) |
| `discord.ml` | `discord_parse` | ~15 (gateway event parse, rate limit headers) |
| `telegram.ml` | `telegram_parse` | ~20 (update format, allow_from, polling offset) |
| `slack.ml` | `slack_parse` | ~15 (event format, HMAC verify) |
| `whatsapp.ml` | `whatsapp_parse` | ~15 (webhook JSON parse, send format, dedup) |
| `nostr.ml` | `nostr_parse` | ~10 (event ID dedup, process arg construction) |
| `onebot.ml` | `onebot_parse` | ~15 (array message format, send endpoint selection) |

#### Tier 4: Contract tests

| Test | Purpose |
|------|---------|
| `channel_contract` | All Channel.S implementations satisfy the interface |
| `provider_contract` | All provider backends handle common request shapes |
| `tool_contract` | All registered tools have valid JSON Schema parameters |

#### Tier 5: Command integration tests

| Test | Purpose | Count |
|------|---------|-------|
| `cmd_integration` | Each CLI command returns non-error output | ~18 (one per command) |
| `cmd_status` | Status output contains expected fields | ~10 |
| `cmd_doctor` | Doctor checks provider reachability stubs | ~5 |
| `cmd_audit` | Audit verify/export/purge with in-memory DB | ~10 |
| `cmd_cron` | Cron create/list/delete via command surface | ~10 |

### Target Test Count

| Tier | Suites | Estimated tests |
|------|--------|-----------------|
| Pure logic | 9 | ~190 |
| I/O (SQLite `:memory:`) | 5 | ~140 |
| Protocol parse | 11 | ~185 |
| Contract | 3 | ~30 |
| Command integration | 5 | ~53 |
| **Total new** | 33 | **~598** |
| **Existing** | — | ~226 |
| **Total** | — | **~824** |

This reaches ~824 tests — a significant improvement, though still below nullclaw's ~5000. Further tests can be added incrementally with each new feature.

### Test File Structure

Add one file per module being tested:
```
test/test_pairing.ml
test/test_rate_limiter.ml
test/test_sandbox.ml
test/test_resilience.ml
test/test_audit.ml
test/test_secret_store.ml
test/test_vector.ml
test/test_memory.ml
test/test_migrate.ml
test/test_config_loader.ml
test/test_tools.ml
test/test_scheduler.ml
test/test_provider.ml
test/test_channel_formats.ml    (* all channel parse tests *)
test/test_contracts.ml
test/test_commands.ml
```

Each file exports a list of `Alcotest.test` suites. `test/test_main.ml` registers them all.

### Test Infrastructure Helpers

Add `test/test_helpers.ml`:
```ocaml
(* In-memory SQLite for tests *)
val with_memory_db : (Sqlite3.db -> 'a) -> 'a

(* Temp directory for file-based tests *)
val with_temp_dir : (string -> 'a Lwt.t) -> 'a Lwt.t

(* Load a fixture JSON file *)
val fixture : string -> Yojson.Safe.t

(* Assert OCaml result *)
val assert_ok : ('a, string) result -> 'a
val assert_error : ('a, string) result -> string
```

### Coverage Measurement

Add `make coverage` target using `bisect_ppx`:
```makefile
coverage:
	opam exec --switch=clawq-5.1 -- \
	  BISECT_ENABLE=yes dune runtest && \
	  bisect-ppx-report html
```

Add `bisect_ppx` as a dev/test dependency in `clawq.opam`.

### Approach for Systematic Test Writing

For each new module being implemented (from P2-01 through P2-10), write tests **in the same PR** as the implementation. The test writing checklist per module:

1. Happy path for each public function
2. Edge cases: empty input, max bounds, UTF-8 boundaries
3. Error cases: bad input, DB failure simulation, network error stubs
4. Regression: add a test for every bug fixed

## Files to Create/Modify

- **Create**: `test/test_helpers.ml` — shared test utilities
- **Create**: `test/test_pairing.ml`, `test/test_rate_limiter.ml`, etc. (one per module)
- **Modify**: `test/test_main.ml` — register new suites
- **Modify**: `test/dune` — add new test modules
- **Modify**: `clawq.opam` — add `bisect_ppx` as test dependency
- **Modify**: `Makefile` — add `coverage` target

## Implementation Order

Write tests alongside each P2-0x plan's implementation — not as a separate sprint. This file defines the scope and structure; the actual test writing is tracked via each feature plan's "Test Strategy" section.

The one thing to do now (independently): add `test/test_helpers.ml` with the shared utilities, which all future test files will use.
