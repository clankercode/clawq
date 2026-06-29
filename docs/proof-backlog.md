# Proof Backlog

**Status**: Backlog (future work)  
**Created**: 2026-06-30  
**Owner**: TBD  

This document catalogues invariants from the scope-resolution and memory-policy
subsystems that are candidates for formal proof or stronger verification. It is
intended as a roadmap for future proof work — **none of these items block
P14–P18 implementation**. Most listed invariants are enforced by runtime code
and (where noted) covered by executable conformance tests. One known gap
exists: INV-REDACT-3b (FTS redaction) is documented in the memory-policy
invariant spec and prioritized in Section 3.1 below.

---

## How to Use This Document

Each entry follows this structure:

- **INV-xxx**: Invariant name (links to authoritative spec)
- **Current enforcement**: How the invariant is enforced today (code, test, or both)
- **Proof candidate**: What a formal proof or stronger verification would look like
- **Effort estimate**: Rough complexity (low / medium / high)
- **Dependencies**: Other invariants or infrastructure needed first

---

## 1. Scope Resolution Invariants

Source: [`docs/scope-resolution-invariants.md`](scope-resolution-invariants.md)

### 1.1 Determinism

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-DET-1** | Code + test (`test_legacy_snapshot_matches_resolver`) | Prove `resolve_effective_access` is pure (no IO, no mutation) given stable `HOME`. Requires auditing `expand_home` / `expand_cwd_pattern` for side effects. | Low |
| **INV-DET-2** | Code + conformance test (`test_same_level_scopes_ordered_lexicographically`, `test_mixed_level_scopes_ordered_by_rank_then_lexicographic`) | Prove `sort_scopes` produces a total order consistent with `(level_rank, id)` comparison. Standard sort-correctness proof. | Low |

### 1.2 Precedence

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-PREC-1** | Code + test | Prove `access_scope_level_rank` is injective and monotonic. Trivial enumeration proof. | Low |
| **INV-PREC-2** | Code + test (`test_legacy_room_profile_bundle_is_room_layer`) | Prove profile bundles are always appended after scope bundles in `resolve_effective_access`. Requires tracing concatenation point. | Low |
| **INV-PREC-3** | Code + conformance test (`test_same_scope_bundle_declaration_order_preserved`) | Prove `List.filter_map` preserves list order. Standard OCaml stdlib property. | Low |

### 1.3 Conflict Resolution

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-CONF-1** | Code + test | Prove deny-filter is applied after all allows are collected. Requires tracing merge order. | Low |
| **INV-CONF-2** | Code + test | Prove same-bundle deny wins over allow. Structural property of the filter pipeline. | Low |
| **INV-CONF-3** | Code + test | Prove room-level deny overrides workspace-level allow for the same tool. Requires showing deny list is global across all bundles. | Medium |
| **INV-CONF-4** | Code + tests (2) | Prove missing bundle reference causes fail-closed (zero grants from that scope). Requires tracing `find_access_bundle` None path. | Low |
| **INV-CONF-5** | Code + test | Prove hash-table merge deduplicates by value while preserving provenance. Standard merge-correctness argument. | Low |

### 1.4 Global Security Caps

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-SEC-1** | Code + test | Prove `blocked_by_global_security` moves out-of-workspace grants to blocked list when `workspace_only = true`. Requires path-prefix comparison correctness. | Medium |
| **INV-SEC-2** | Code + test | Prove inherited grants are checked independently (no weakening through inheritance). Requires showing each grant passes security check regardless of source scope. | Medium |
| **INV-SEC-3** | Code + test | Prove `allowed_cwd_patterns` acts as an additional ceiling (empty list = no restriction). Requires glob-matching correctness. | Medium |

### 1.5 Codebase Grant Expansion

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-CG-1** | Code + test | Prove `$CLAWQ_WORKSPACE` expansion is idempotent and produces an absolute path. String-substitution correctness. | Low |
| **INV-CG-2** | Code + test | Prove tilde expansion is idempotent and produces an absolute path. Requires `expand_home` correctness. | Low |

### 1.6 Repo Grant Resolution

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-REPO-1** | Code + test | Prove explicit `repo_grants` suppress legacy `repositories` for the same key. Requires `Hashtbl.mem` precedence check. | Low |
| **INV-REPO-2** | Code + test | Prove repo grants are subject to global security checks regardless of path type (local vs non-local). Requires tracing `repo_path_for_grant_item` and `blocked_by_global_security` interaction. | Medium |
| **INV-REPO-3** | Code + test | Prove repo grants require codebase grant coverage when `codebase_grants` is non-empty. Glob-matching correctness. | Medium |
| **INV-REPO-4** | Code + test | Prove path normalization (`normalize_path`) eliminates traversal before codebase intersection check. Standard path-normalization properties. | Low |
| **INV-REPO-5** | Code + test | Prove wildcard repo grants require exact codebase grant match (not glob). Requires showing `repo_grant_has_glob_metachar` switches to string equality. | Low |
| **INV-REPO-6** | Code + test | Prove legacy `repositories` become read-only (`[Read]`) repo grants. Trivial field-mapping proof. | Low |

### 1.7 Memory Grants

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-MEM-1** | Code + test | Prove memory grants are direct (no recursive traversal). Requires showing `resolve_grants` queries only the specified scope. | Low |
| **INV-MEM-2** | Code + test | Prove missing `memory_grants` field defaults to empty. Standard JSON-parsing default behavior. | Low |

### 1.8 Channel Isolation

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-CHAN-1** | Code + test | Prove room scopes with `channel` selector only match sessions from that channel type. Requires `scope_matches` correctness. | Medium |
| **INV-CHAN-2** | Code + test | Prove missing layer selectors are not wildcards. Requires showing `scope_matches` fails on missing required fields. | Low |

### 1.9 Snapshot Invariants

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-SNAP-1** | Code + test (`test_snapshot_immutable_after_persist`) | Prove `Access_snapshot.create` returns an immutable record. Structural property of OCaml records. | Low |
| **INV-SNAP-2** | Code + test | Prove snapshot `tool_denial` matches resolver output exactly. Requires showing both use the same deny-then-allow logic. | Medium |
| **INV-SNAP-3** | Code + tests (3) | Prove `config_hash` is deterministic and collision-resistant (SHA-256 property). Standard cryptographic hash argument. | Low |
| **INV-SNAP-4** | Code + tests (2) | Prove `bundle_sources` records all provenance entries. Requires walking `extract_bundle_sources` completeness. | Medium |

### 1.10 Egress Rule Ordering

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-EGR-1** | Code + conformance test (`test_egress_rule_ordering_profile_before_scope`) | Prove egress rules are ordered profile > room > channel > workspace > default. Requires tracing `resolve_effective_access` construction of `scope_groups`. | Medium |

### 1.11 Active/Deleted Filtering

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-ACT-1** | Code + conformance tests (3: case-insensitive, room scope, default scope) | Prove `scope_active` filters deleted scopes regardless of case. Requires `String.lowercase_ascii` correctness. | Low |

### 1.12 Reload Invariants

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-RLD-1** | Code + tests (3) | Prove config reload re-parses from disk and produces identical OCaml values for unchanged JSON. Requires JSON-parse roundtrip property. | Medium |
| **INV-RLD-2** | Code + tests (2) | Prove room profiles survive reload intact. Requires showing `parse_config` is deterministic for same input. | Medium |
| **INV-RLD-3** | Code + test | Prove invalid JSON does not crash the runtime. Requires showing `load_result` catches all parse errors. | Low |
| **INV-RLD-4** | Code + test | Prove tool access decisions are stable across reload for unchanged config. Requires combining INV-RLD-1 and INV-DET-1. | Medium |
| **INV-RLD-5** | Code + test | Prove malformed access policy triggers fail-closed. Requires showing `fail_closed_access_policy` is applied on validation failure. | Low |

### 1.13 Provenance Tracking

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-PROV-1** | Code + test (`assert_all_provenance`) | Prove every effective access item carries non-empty provenance. Requires showing `add_bundle_items` always attaches entries. | Low |
| **INV-PROV-2** | Code + test | Prove provenance records layer, source_id, and field. Structural property of `access_provenance` type. | Low |

### 1.14 Backward Compatibility

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-COMP-1** | Code + test | Prove pure legacy configs (no `access_bundles`/`access_scopes`) resolve correctly through new resolver. Requires showing empty bundles/scopes produce no scope-level grants. | Low |
| **INV-COMP-2** | Code + test | Prove legacy effective access matches legacy `room_profile_tool_denial_for_session`. Requires showing both paths use same bundle-derived lists. | Medium |
| **INV-COMP-3** | Code + test | Prove hybrid profiles produce effective access from both explicit and implicit bundles. Requires `access_bundles_for_profile` correctness. | Medium |
| **INV-COMP-4** | Code + test | Prove legacy codebase grants match `room_profile_codebase_grants_for_profile`. Requires showing both paths collect from same source. | Low |
| **INV-COMP-5** | Code + test | Prove multiple legacy profiles do not interfere. Requires showing `resolve_room_profile` matches by session_key independently. | Low |

---

## 2. Memory Policy Isolation Invariants

Source: [`docs/memory-policy-isolation-invariants.md`](memory-policy-isolation-invariants.md)

### 2.1 Memory Scope Isolation

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-MEM-ISO-1** | Code + tests (5) | Prove tool-layer scope filtering is applied before any CRUD operation. Requires tracing `check_room_access` call sites. | Medium |
| **INV-MEM-ISO-2** | Code + test (`test_three_room_isolation`) | Prove scope boundaries are absolute (no cross-room visibility regardless of visibility level). Requires showing all query paths filter by scope. | Medium |
| **INV-MEM-ISO-3** | Code + tests (2) | Document that raw API intentionally does not enforce isolation (admin escape hatch). No proof needed — design decision. | N/A |
| **INV-MEM-ISO-4** | Code + test | Prove FTS and content-search are scoped by `scope_kind`/`scope_key`. Requires showing SQL WHERE clauses are always applied. | Medium |

### 2.2 Memory Visibility Levels

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-VIS-1** | Code + test | Prove three visibility levels exist with correct default. Type-system argument (algebraic type). | Low |
| **INV-VIS-2** | Code + tests (2) | Prove private memory is visible only to scope owner. Requires `can_see_memory` correctness for `Private` case. | Medium |
| **INV-VIS-3** | Code + tests (2) | Prove team memory requires explicit grant. Requires `has_team_grant` query correctness. | Medium |
| **INV-VIS-4** | Code + test | Prove team grants do not override scope isolation. Requires showing scope check precedes visibility check in tool layer. | Medium |
| **INV-VIS-5** | Code + tests (2) | Prove visibility is preserved on upsert when not explicitly provided. Requires `upsert_scoped_memory` field-update logic. | Low |

### 2.3 Grant Resolution

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-GRANT-1** | Code + test | Prove grants are direct (no recursive traversal). Requires showing `resolve_grants` queries only specified scope. | Low |
| **INV-GRANT-2** | Code + test | Prove grant mutations require admin privileges. Requires `require_memory_grant_admin` gate correctness. | Low |
| **INV-GRANT-3** | Code + conformance tests (3: expired, non-expired, null) | Prove expired grants are excluded from resolution. SQL clause correctness (`datetime(expires_at) > datetime('now')`). | Low |
| **INV-GRANT-4** | Code + conformance tests (3: revoked, non-revoked, no column) | Prove revoked grants are excluded when `revoked_at` column exists. Dynamic SQL construction correctness. | Low |

### 2.4 Credential Non-Disclosure

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-CRED-1** | Code + test | Prove Bearer tokens are redacted in content previews. Regex correctness (`Bearer [A-Za-z0-9._+/=-]+`). | Low |
| **INV-CRED-2** | Code + test | Prove content previews are truncated to 200 chars. String-slicing correctness. | Low |
| **INV-CRED-3** | Code + test | Prove redacted memories have content set to NULL and provenance scrubbed. Requires tracing `redact_scoped_memory` SQL. | Medium |

### 2.5 Egress Default-Deny

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-EGR-1** | Code + conformance tests (`test_egress_unmatched_destinations_default_deny`, `test_egress_rules_order_matters`) | Prove `evaluate` returns `Deny` when no rule matches. Structural property of fallthrough case. | Low |
| **INV-EGR-2** | Code (structural) | Prove unmatched destinations remain denied regardless of rule changes. Requires showing new rules only affect their specific matches. | Medium |
| **INV-EGR-3** | Code + conformance test (`test_egress_rules_first_match_wins`) | Prove rules are evaluated in declaration order. Requires showing `find_first_match` iterates in list order. | Low |

### 2.6 MCP Filter Soundness

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-FILTER-1** | Code + tests (2) | Prove deny wins over allow in merged bundles. Requires tracing filter pipeline order. | Low |
| **INV-FILTER-2** | Code (no dedicated test) | Prove `Skills.filter_visible_tools` is an additional filtering layer. Requires showing it runs after agent template filtering. | Medium |
| **INV-FILTER-3** | Code (no dedicated test) | Prove MCP client tools are filtered at connection time via `filter_map`. Structural property of discovery pipeline. | Low |

### 2.7 Budget Invariants

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-BUDG-1** | Schema constraint (no dedicated test) | Prove `profile_id` is unique primary key with FK cascade. Schema-level argument. | Low |
| **INV-BUDG-2** | Schema CHECK constraints (no dedicated test) | Prove non-negative limits are enforced. Schema-level argument. | Low |
| **INV-BUDG-3** | Code (no dedicated test) | Prove cumulative usage tracking and limit enforcement. Requires showing `room_budget.ml` correctly compares usage vs limits. | Medium |
| **INV-BUDG-4** | Code (no dedicated test) | Prove soft warning threshold defaults to 80%. Constant-value argument. | Low |
| **INV-BUDG-5** | Code (no dedicated test) | Prove budget period reset advances `period_started_at` and recalculates `limit_exceeded`. Requires showing `get_profile_budget` derives limit status. | Medium |

### 2.8 Session Lifecycle

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-SESS-1** | Code (no dedicated test) | Prove `clear_session` is idempotent and deletes from all tables. Requires tracing delete statements and re-invocation safety. | Medium |
| **INV-SESS-2** | Code (no dedicated test) | Prove `cleanup_session` preserves tool-group integrity during trimming. Requires `Message_history.ensure_tool_group_integrity` correctness. | High |
| **INV-SESS-3** | Code (no dedicated test) | Prove `replace_session_messages` archives before deleting. Requires showing `archive_session_epoch` is called before `DELETE`. | Medium |
| **INV-SESS-4** | Schema + code (no dedicated test) | Prove FK cascade with `ON DELETE CASCADE` is active (`PRAGMA foreign_keys = ON`). Schema and init-code argument. | Low |

### 2.9 Redaction (Forgetting)

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-REDACT-1** | Code + conformance test | Prove redaction is idempotent. Requires showing `WHERE redacted_at IS NULL` prevents double-redaction. | Low |
| **INV-REDACT-2** | Code + test | Prove redacted memories cannot be corrected. Requires showing `redacted_at` check in correct path. | Low |
| **INV-REDACT-3** | Code + conformance test | Prove redacted memories are excluded from `query_scoped_memories`. Requires showing `redacted_at IS NULL` clause is mandatory. | Low |
| **INV-REDACT-3b** | **Known gap** (documented in invariant spec) | Fix and prove: `Memory.search` FTS path does not filter `sm.redacted_at IS NULL`. Requires either adding the filter or clearing `messages.content` during redaction. **This is the highest-priority proof candidate.** | High |
| **INV-REDACT-4** | Code + conformance tests (2) | Prove redaction clears content to NULL, sets `redacted_at`, and handles NULL reference. SQL correctness argument. | Low |

### 2.10 Ledger Audit Trail

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-LEDGER-1** | Code + tests (5) | Prove all mutation operations emit ledger events when ledger function is provided. Requires tracing `emit_memory_event` call sites. | Medium |
| **INV-LEDGER-2** | Code + tests (3) | Prove ledger events record all required metadata fields. Structural property of event construction. | Low |
| **INV-LEDGER-3** | Code + tests (2) | Prove grant/revoke events record principal and capability. Structural property of `emit_grant_event`. | Low |

### 2.11 Unowned Scope Access

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-UNOWNED-1** | Code + test | Prove unowned scopes deny access by default. Requires showing `check_room_access` requires profile binding or explicit grant. | Low |
| **INV-UNOWNED-2** | Code + test | Prove unowned scopes can be accessed via explicit grants. Requires showing grant lookup fallback works. | Low |

### 2.12 Scope Resolver Isolation

| Invariant | Current Enforcement | Proof Candidate | Effort |
|-----------|---------------------|-----------------|--------|
| **INV-SRES-1** | Code + test | Prove distinct session keys produce distinct room ids. Requires `resolve_room_id_for_context` injectivity. | Medium |

---

## 3. Priority Recommendations

### 3.1 High Priority (Known Gaps)

1. **INV-REDACT-3b** (FTS redaction gap): The `Memory.search` FTS path does not
   filter `sm.redacted_at IS NULL`. This is a documented security gap. A fix
   and accompanying proof should be prioritized.

2. **INV-GRANT-3 / INV-GRANT-4** (expired/revoked grant tests): Conformance
   tests were added in P18.M3.E1.T003, but the underlying SQL correctness
   arguments could be strengthened with property-based tests.

### 3.2 Medium Priority (Structural Properties)

3. **INV-SEC-1 / INV-SEC-2 / INV-SEC-3** (global security caps): These are
   critical security invariants. The path-comparison and glob-matching logic
   would benefit from property-based testing or lightweight formal verification.

4. **INV-SESS-2** (tool-group integrity during cleanup): This is the most
   complex session lifecycle invariant. A proof that `ensure_tool_group_integrity`
   correctly preserves coherent tool-call/result groups would increase confidence
   in context window management.

5. **INV-EGR-1 / INV-EGR-2 / INV-EGR-3** (egress default-deny): Egress
   security is critical. The evaluator's structural properties (default-deny,
   first-match-wins) would benefit from property-based testing.

### 3.3 Low Priority (Already Well-Tested)

6. Most scope-resolution invariants (INV-DET, INV-PREC, INV-CONF, INV-CG,
   INV-REPO, INV-MEM, INV-CHAN, INV-SNAP, INV-PROV, INV-COMP) have
   comprehensive runtime test coverage. Formal proofs would add marginal value
   unless the codebase undergoes significant refactoring.

7. Memory visibility and redaction invariants (INV-VIS, INV-REDACT except 3b)
   have good test coverage and straightforward enforcement logic.

---

## 4. Proof Infrastructure Notes

### 4.1 Current State

- Most invariants are enforced by OCaml runtime code; one known gap exists
  (INV-REDACT-3b: FTS redaction path does not filter `sm.redacted_at IS NULL`)
- Most invariants have executable conformance tests (Alcotest)
- No formal proof infrastructure (Coq, Why3, etc.) is currently in use
- The `src/extracted/` directory contains Coq extraction artifacts, but these
  are for the core logic engine, not for invariant verification

### 4.2 Recommended Approach

1. **Property-based testing** (QCheck): Add property-based tests for invariants
   with complex input spaces (e.g., INV-SEC path comparison, INV-REPO glob
   matching). This provides stronger guarantees than example-based tests without
   the overhead of full formal proof.

2. **Lightweight formal methods**: Consider using `ocaml-diff` or similar tools
   to verify that refactored code preserves invariant-carrying functions.

3. **Coq extraction**: For critical invariants (INV-REDACT-3b fix, INV-SEC),
   consider writing Coq proofs that extract to OCaml test code. The existing
   `src/extracted/` infrastructure provides a foundation.

### 4.3 Effort Summary

| Effort | Count | Examples |
|--------|-------|---------|
| Low | ~35 | INV-DET, INV-PREC, INV-CG, INV-MEM, INV-REPO-1/4/5/6, INV-REDACT-1/2/4 |
| Medium | ~20 | INV-SEC, INV-CHAN, INV-SNAP-2/4, INV-VIS-2/3/4, INV-SESS-1/3, INV-SRES |
| High | ~3 | INV-REDACT-3b, INV-SESS-2, INV-EGR-2 |

---

## 5. Decision Log

| Date | Decision | Rationale |
|------|----------|-----------|
| 2026-06-30 | Created proof backlog from invariant specs | P18.M3.E1.T003 added conformance tests; this backlog documents remaining proof work |
| 2026-06-30 | INV-REDACT-3b marked high priority | Known security gap in FTS redaction path |
| 2026-06-30 | No proof work blocks P14–P18 | Most invariants are enforced by runtime code (except INV-REDACT-3b); proofs are future hardening |

---

## 6. Links

- [Scope Resolution Invariants](scope-resolution-invariants.md)
- [Memory Policy Isolation Invariants](memory-policy-isolation-invariants.md)
- [Conformance Tests](../test/test_invariant_conformance.ml) (P18.M3.E1.T003)
- [Formal Verification Spec Gap Audit](FV_SPEC_IMPL_GAP_AUDIT_2026_03_08.md)
