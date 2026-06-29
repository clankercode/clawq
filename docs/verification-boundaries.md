# Verification Boundaries

**Status**: Authoritative  
**Last updated**: 2026-06-30  
**Owner**: P18.M3 (Memory Policy)

This document provides a cross-cutting view of verification boundaries across
all security-relevant subsystems in Clawq. It complements the per-subsystem
invariant specs and proof backlog by showing, at a glance, what is enforced
where.

---

## Verification Status Legend

| Tag | Meaning |
|-----|--------|
| **[RUNTIME]** | Enforced by runtime code logic (the code path itself prevents violation) |
| **[TEST]** | Covered by one or more executable conformance tests (Alcotest) |
| **[PROOF-CANDIDATE]** | Candidate for formal proof or stronger verification (see proof-backlog.md) |
| **[GAP]** | Known gap: neither runtime code nor test enforces this invariant |

An invariant may carry multiple tags (e.g. `[RUNTIME] [TEST]` means both code
and tests enforce it). Tags reflect the *current* state; gaps are tracked in
the proof backlog.

---

## 1. Scope Resolution

**Source**: [`scope-resolution-invariants.md`](scope-resolution-invariants.md)

| Category | Invariants | Runtime | Tests | Proof Candidates | Gaps |
|----------|-----------|---------|-------|-----------------|------|
| Determinism | INV-DET-1, INV-DET-2 | Yes | Yes (4 tests) | Yes (2) | 0 |
| Precedence | INV-PREC-1..3 | Yes | Yes (3 tests) | Yes (3) | 0 |
| Conflict Resolution | INV-CONF-1..5 | Yes | Yes (7 tests) | Yes (5) | 0 |
| Global Security | INV-SEC-1..3 | Yes | Yes (3 tests) | Yes (3) | 0 |
| Codebase Grants | INV-CG-1..2 | Yes | Yes (2 tests) | Yes (2) | 0 |
| Repo Grants | INV-REPO-1..6 | Yes | Yes (6 tests) | Yes (6) | 0 |
| Memory Grants | INV-MEM-1..2 | Yes | Yes (2 tests) | Yes (2) | 0 |
| Channel Isolation | INV-CHAN-1..2 | Yes | Yes (2 tests) | Yes (2) | 0 |
| Snapshots | INV-SNAP-1..4 | Yes | Yes (7 tests) | Yes (4) | 0 |
| Egress Ordering | INV-EGR-1 | Yes | Yes (1 test) | Yes (1) | 0 |
| Active/Deleted | INV-ACT-1 | Yes | Yes (3 tests) | Yes (1) | 0 |
| Reload | INV-RLD-1..5 | Yes | Yes (9 tests) | Yes (5) | 0 |
| Provenance | INV-PROV-1..2 | Yes | Yes (2 tests) | Yes (2) | 0 |
| Backward Compat | INV-COMP-1..5 | Yes | Yes (5 tests) | Yes (5) | 0 |

**Summary**: All scope-resolution invariants are enforced at runtime AND
covered by tests. All are proof candidates for future hardening. No gaps.

---

## 2. Memory Policy Isolation

**Source**: [`memory-policy-isolation-invariants.md`](memory-policy-isolation-invariants.md)

| Category | Invariants | Runtime | Tests | Proof Candidates | Gaps |
|----------|-----------|---------|-------|-----------------|------|
| Scope Isolation | INV-MEM-ISO-1..4 | Yes | Yes (8 tests) | Yes (3) | 0 |
| Visibility | INV-VIS-1..5 | Yes | Yes (7 tests) | Yes (5) | 0 |
| Grant Resolution | INV-GRANT-1..4 | Yes | Yes (7 tests) | Yes (4) | 0 |
| Credential | INV-CRED-1..3 | Yes | Yes (3 tests) | Yes (3) | 0 |
| Egress Default-Deny | INV-EGR-1..3 | Yes | Yes (2 of 3 tested) | Yes (3) | 0 |
| MCP Filter | INV-FILTER-1..3 | Yes | Yes (1 of 3 tested) | Yes (3) | 0 |
| Budget | INV-BUDG-1..5 | Yes | No | Yes (5) | 0 |
| Session Lifecycle | INV-SESS-1..4 | Yes | No | Yes (4) | 0 |
| Redaction | INV-REDACT-1..4, 3b | Yes (4 of 5) | Yes (4 of 5 tested) | Yes (5) | **1 (REDACT-3b)** |
| Ledger | INV-LEDGER-1..3 | Yes | Yes (5 tests) | Yes (3) | 0 |
| Unowned Scope | INV-UNOWNED-1..2 | Yes | Yes (2 tests) | Yes (2) | 0 |
| Scope Resolver | INV-SRES-1 | Yes | Yes (1 test) | Yes (1) | 0 |

**Summary**: Most memory-policy invariants are enforced at runtime AND covered
by tests. Budget and session lifecycle invariants lack dedicated tests (enforced
by code only). One known gap exists: INV-REDACT-3b (FTS redaction).

---

## 3. Credential Security

**Source**: [`credential-callsite-inventory.md`](credential-callsite-inventory.md)

| Domain | Callsites | Runtime Redaction | Proof Candidates | Gaps |
|--------|----------|-------------------|-----------------|------|
| LLM Providers | ~20 | `Http_debug` redacts in debug logs | Request-path redaction | Normal log paths unredacted |
| Connector Credentials | ~60 | Most unredacted in request path | Centralized redaction middleware | Most connectors |
| GitHub | ~10 | Existing (via `auth_headers`) | Low priority | 0 |
| Telegram | ~18 | Token in URL path | URL-path redaction | URL-path exposure |
| Config Display | ~5 | Existing (config_show) | Low priority | 0 |
| HTTP Debug | ~5 | Existing (redact_token) | Low priority | 0 |
| Secret Store | ~2 | Existing (AES-256-GCM) | Low priority | 0 |
| Runner Relay | ~2 | Existing (SHA256 hash) | Low priority | 0 |

**Summary**: Config display and HTTP debug redaction are well-implemented.
Most connector and provider credentials lack redaction in normal log paths.
No centralized credential redaction middleware exists.

---

## 4. Network Egress

**Source**: [`network-callsite-inventory.md`](network-callsite-inventory.md)

| Transport | Callsites | Egress Enforceable | Notes |
|-----------|----------|-------------------|-------|
| HTTP (Http_client) | ~115 | Existing (static) / Dynamic | Most outbound calls |
| HTTP-direct (Cohttp) | 3 | Dynamic | Bypasses Http_client |
| WebSocket | 8 | Dynamic | Gateway, socket, etc. |
| TCP/TLS (raw) | 6 | Dynamic | IRC, email |
| Subprocess | ~20 | Not enforceable | Nostr, tunnels, etc. |

**Summary**: Most static-host callsites (Discord, Slack, Telegram, etc.) are
fully enforceable. Dynamic-host callsites need host injection at eval time.
Subprocess-based calls (Nostr, tunnels) are not interceptable by OCaml-level
egress evaluator.

---

## 5. MCP Tool Filtering

**Source**: [`memory-policy-isolation-invariants.md`](memory-policy-isolation-invariants.md) (Section 6)

| Invariant | Runtime | Tests | Proof Candidate | Gap |
|-----------|---------|-------|-----------------|-----|
| INV-FILTER-1 (deny wins) | Yes | Yes (2 tests) | Yes | 0 |
| INV-FILTER-2 (skills filter) | Yes | No | Yes | 0 |
| INV-FILTER-3 (MCP filter_map) | Yes | No | Yes | 0 |

**Summary**: Deny-wins-over-allow is well-tested. Skills and MCP filter layers
lack dedicated tests but are enforced by code.

---

## 6. Session Lifecycle

**Source**: [`memory-policy-isolation-invariants.md`](memory-policy-isolation-invariants.md) (Section 8)

| Invariant | Runtime | Tests | Proof Candidate | Gap |
|-----------|---------|-------|-----------------|-----|
| INV-SESS-1 (clear_session) | Yes | No | Yes | 0 |
| INV-SESS-2 (cleanup_session) | Yes | No | Yes | 0 |
| INV-SESS-3 (archive before replace) | Yes | No | Yes | 0 |
| INV-SESS-4 (FK cascade) | Yes | No | Yes | 0 |

**Summary**: All session lifecycle invariants are enforced by code but lack
dedicated tests. Integration paths exercise them indirectly.

---

## 7. Known Gaps

### Critical

| ID | Subsystem | Description | Status |
|----|-----------|-------------|--------|
| **INV-REDACT-3b** | Memory Redaction | `Memory.search` FTS path does not filter `sm.redacted_at IS NULL` | Known security gap, fix needed |

### Moderate

| ID | Subsystem | Description | Status |
|----|-----------|-------------|--------|
| INV-BUDG-* | Budget | No dedicated invariant tests | Enforced by code, tests needed |
| INV-SESS-* | Session Lifecycle | No dedicated invariant tests | Enforced by code, tests needed |
| INV-FILTER-2 | MCP/Skills | No dedicated skills filter test | Enforced by code, test needed |
| INV-EGR-2 | Egress Evaluator | Unmatched-destinations-remain-denied not directly tested | Enforced by code, test needed |

### Low

| ID | Subsystem | Description | Status |
|----|-----------|-------------|--------|
| Credential redaction | Connectors | Most connector credentials unredacted in normal log paths | Infrastructure gap, no centralized middleware |
| Subprocess egress | Network | Nostr, tunnels bypass OCaml egress evaluator | OS-level controls needed |

---

## 8. Links

- [Scope Resolution Invariants](scope-resolution-invariants.md)
- [Memory Policy Isolation Invariants](memory-policy-isolation-invariants.md)
- [Proof Backlog](proof-backlog.md)
- [Credential Callsite Inventory](credential-callsite-inventory.md)
- [Network Callsite Inventory](network-callsite-inventory.md)
- [Conformance Tests](../test/test_invariant_conformance.ml)
