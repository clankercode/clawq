# Scope Resolution Invariants

**Status**: Authoritative  
**Last updated**: 2026-06-30  
**Owner**: P14 (Access Scope Inheritance)

This document enumerates the invariants guaranteed by the scope resolver
(`Runtime_config.resolve_effective_access`) and access snapshots
(`Access_snapshot`).

**Proof backlog**: See [`proof-backlog.md`](proof-backlog.md) for a catalogue
of invariants that are candidates for formal proof or stronger verification.
**Verification boundaries**: See [`verification-boundaries.md`](verification-boundaries.md)
for a cross-cutting view of all security-relevant subsystems.

---

## Verification Status Legend

Each invariant is classified by its verification boundary:

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

## 1. Resolver Determinism

**INV-DET-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: The resolver is a function
of `(config, session_key, ?room_profile)`. Given identical inputs, it produces
identical effective access within the same process invocation. The resolver
depends on `HOME` (via `expand_home` / `expand_cwd_pattern`) for tilde
expansion; determinism assumes a stable `HOME` environment variable.

- **Code**: `resolve_effective_access` in `runtime_config.ml` (line 768).
- **Test**: `test_legacy_snapshot_matches_resolver` — snapshot created from the
  same config carries identical allowed/denied tools as a direct resolver call.
- **Proof candidate**: Prove `resolve_effective_access` is pure (no IO, no
  mutation) given stable `HOME`.

**INV-DET-2** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Scope bundles are merged in
deterministic order: level rank ascending
(`Default=0 < Workspace=1 < Channel=2 < Room=3`), then scope id lexicographic
within the same level.

- **Code**: `sort_scopes` uses `compare` on `(access_scope_level_rank, id)`.
- **Test**: `test_same_level_scopes_ordered_lexicographically`,
  `test_mixed_level_scopes_ordered_by_rank_then_lexicographic`,
  `test_layers_merge_deterministically_and_deny_wins`.
- **Proof candidate**: Prove `sort_scopes` produces a total order consistent
  with `(level_rank, id)` comparison.

---

## 2. Precedence Rules

**INV-PREC-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Scope levels have fixed
precedence: `Default (0) < Workspace (1) < Channel (2) < Room (3)`.

- **Code**: `access_scope_level_rank` in `runtime_config.ml` (line 614).
- **Test**: `test_layers_merge_deterministically_and_deny_wins` — room scope
  tools appear after workspace/channel tools in the merged list.
- **Proof candidate**: Prove `access_scope_level_rank` is injective and
  monotonic. Trivial enumeration proof.

**INV-PREC-2** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Profile bundles (from
`room_profiles`) are treated as the `"room"` layer. In the resolver, profile
bundles are appended after scope bundles
(`scope_bundles @ profile_bundles`).

- **Code**: `resolve_effective_access` concatenates
  `scope_bundles @ profile_bundles`.
- **Test**: `test_legacy_room_profile_bundle_is_room_layer` — legacy profile
  provenance shows `room:room_profile:<id>`. Note: this test verifies the
  layer label, not the ordering relative to room-scope bundles.
- **Proof candidate**: Prove profile bundles are always appended after scope
  bundles in `resolve_effective_access`.

**INV-PREC-3** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Within the same scope,
bundles referenced by `access_bundle_ids` are merged in declaration order (list
order preserved by `List.filter_map`).

- **Code**: `scope.access_bundle_ids |> List.filter_map ...` preserves list
  order.
- **Test**: `test_same_scope_bundle_declaration_order_preserved`.
- **Proof candidate**: Prove `List.filter_map` preserves list order. Standard
  OCaml stdlib property.

---

## 3. Conflict Resolution

**INV-CONF-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Deny wins over allow. If
a tool appears in `denied_tools` anywhere in the merged bundles, it is removed
from `allowed_tools`.

- **Code**: `allowed_tools |> List.filter (fun item -> not (List.mem item.value
  denied_tool_values))`.
- **Test**: `test_layers_merge_deterministically_and_deny_wins` — `shared_tool`
  denied by room scope is removed from allowed.
- **Proof candidate**: Prove deny-filter is applied after all allows are
  collected. Requires tracing merge order.

**INV-CONF-2** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Same-tool allow+deny
within a single bundle: deny wins, allow entry is removed.

- **Code**: The deny filter runs after collection, removing any allowed item
  whose value appears in the denied list.
- **Test**: `test_allow_and_deny_same_tool_denies` — `shell_exec` in both
  allowed and denied of the same bundle results in empty allowed, explicit deny.
- **Proof candidate**: Prove same-bundle deny wins over allow. Structural
  property of the filter pipeline.

**INV-CONF-3** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Room deny overrides
workspace allow for the same tool.

- **Code**: Deny list is collected from all bundles (including room-level), then
  applied as a filter on allowed.
- **Test**: `test_room_deny_overrides_workspace_allow` — `deploy_tool` allowed
  by workspace scope but denied by room scope is removed from effective allowed.
- **Proof candidate**: Prove room-level deny overrides workspace-level allow
  for the same tool.

**INV-CONF-4** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Invalid bundle references
cause fail-closed behavior. If a scope or profile references a non-existent
bundle id, all grants from that scope/profile are suppressed.

- **Code**: `find_access_bundle cfg bundle_id = None` causes the entire scope
  to yield no bundles. `profile_missing_access_bundle_ids` returns non-empty
  for invalid profile bundle refs.
- **Tests**:
  - `test_invalid_scope_bundle_denies_scope_grants`
  - `test_invalid_profile_bundle_denies_effective_profile_grants`
- **Proof candidate**: Prove missing bundle reference causes fail-closed (zero
  grants from that scope).

**INV-CONF-5** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Duplicate bundle
references across scopes merge provenance but do not duplicate values.

- **Code**: `merge_effective_items` uses a hash table keyed by value, appending
  provenance on collision.
- **Test**: `test_duplicate_bundle_references_merge_provenance_once` —
  `shared_tool` appears once with both scope provenances.
- **Proof candidate**: Prove hash-table merge deduplicates by value while
  preserving provenance.

---

## 4. Global Security Caps

**INV-SEC-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: When
`security.workspace_only = true`, codebase grants whose expanded path prefix
falls outside the workspace (and `extra_allowed_paths`) are moved to
`blocked_codebase_grants`.

- **Code**: `blocked_by_global_security` checks
  `is_prefix_of ~prefix:workspace`.
- **Test**: `test_global_security_caps_codebase_grants` — `/tmp/outside/**`
  blocked, workspace grant kept.
- **Proof candidate**: Prove `blocked_by_global_security` moves out-of-workspace
  grants to blocked list when `workspace_only = true`.

**INV-SEC-2** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Inherited grants do not
weaken global security. Room-level grants that point outside the workspace are
blocked even if inherited from a lower-level scope.

- **Code**: `blocked_by_global_security` runs on each grant independently.
- **Test**: `test_inherited_grants_do_not_weaken_global_security` — both
  default and room outside grants are blocked.
- **Proof candidate**: Prove inherited grants are checked independently (no
  weakening through inheritance).

**INV-SEC-3** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: `allowed_cwd_patterns`
provides an additional ceiling. Grants must match at least one pattern (or the
pattern list must be empty).

- **Code**: `pattern_ok` check in `blocked_by_global_security`.
- **Test**: `test_global_security_caps_codebase_grants` — pattern list
  restricts grants to matching paths.
- **Proof candidate**: Prove `allowed_cwd_patterns` acts as an additional
  ceiling (empty list = no restriction).

---

## 5. Codebase Grant Expansion

**INV-CG-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: `$CLAWQ_WORKSPACE` is
expanded to the configured workspace path before security checks.

- **Code**: `expand_cwd_pattern` replaces `$CLAWQ_WORKSPACE` with
  `effective_workspace cfg`.
- **Test**: `test_legacy_room_profile_bundle_is_room_layer` —
  `$CLAWQ_WORKSPACE/**` expands to `/tmp/clawq-scope-root/**`.
- **Proof candidate**: Prove `$CLAWQ_WORKSPACE` expansion is idempotent and
  produces an absolute path.

**INV-CG-2** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Tilde (`~`) in workspace
selectors is expanded to the home directory before comparison.

- **Code**: `expand_home` in `workspace_option_required_matches`.
- **Test**: `test_workspace_scope_expands_tilde_selector` — `~/clawq-scope-root`
  matches expanded workspace.
- **Proof candidate**: Prove tilde expansion is idempotent and produces an
  absolute path.

---

## 6. Repo Grant Resolution

**INV-REPO-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Explicit `repo_grants`
suppress legacy `repositories` for the same repo key.

- **Code**: `collect_repo_grants` checks `Hashtbl.mem repo_table repo` before
  adding legacy entries.
- **Test**: `test_explicit_repo_grants_take_precedence_over_legacy` — one grant
  (not duplicated), legacy repositories preserved.
- **Proof candidate**: Prove explicit `repo_grants` suppress legacy
  `repositories` for the same key.

**INV-REPO-2** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Repo grants are subject
to global security checks. For local-path repo grants (starting with `/`, `~`,
`$`, `./`, `../`), the path is checked directly. For non-local repo grants
(e.g., GitHub-style `acme/app`), the resolver maps them to
`$CLAWQ_WORKSPACE/<repo>` before the security check.

- **Code**: `repo_path_for_grant_item` maps non-local repos to workspace paths.
  `repo_grant_allowed` calls `blocked_by_global_security` on the mapped path
  when `repo_grant_is_local_path` is true on the expanded result.
- **Test**: `test_repo_grants_blocked_by_global_security` (second definition,
  line 784) — both `acme/app` and `outside/repo` are mapped under the workspace
  and allowed. Note: a first definition at line 467 tested local-path blocking
  but is shadowed by the second definition; the suite entries both reference
  the second definition.
- **Proof candidate**: Prove repo grants are subject to global security checks
  regardless of path type (local vs non-local).

**INV-REPO-3** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Repo grants require
codebase grant coverage. If `codebase_grants` is non-empty, a repo grant must
be covered by at least one codebase grant pattern.

- **Code**: `repo_grant_covered_by_codebase_grants` checks glob matching.
- **Test**: `test_repo_grants_intersect_codebase_grants` — uncovered repo
  blocked, covered repo kept.
- **Proof candidate**: Prove repo grants require codebase grant coverage when
  `codebase_grants` is non-empty.

**INV-REPO-4** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Path traversal in repo
grants is normalized before codebase intersection check.

- **Code**: `Path_util.normalize_path` on `repo_path_for_codebase_check`.
- **Test**: `test_repo_grants_normalize_traversal_before_codebase_intersection`
  — `allowed/../other/app` is not covered by `allowed/**`.
- **Proof candidate**: Prove path normalization eliminates traversal before
  codebase intersection check.

**INV-REPO-5** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Wildcard repo grants
require exact codebase grant match (not glob matching).

- **Code**: `repo_grant_has_glob_metachar` triggers
  `repo_grant_exactly_covered_by_codebase_grants` (string equality).
- **Test**: `test_wildcard_repo_grants_require_exact_codebase_grant` —
  `allowed/**` blocked, `exact/**` kept when codebase grant is `exact/**`.
- **Proof candidate**: Prove wildcard repo grants require exact codebase grant
  match (not glob).

**INV-REPO-6** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Legacy `repositories`
become read-only (capability `[Read]`) repo grants.

- **Code**: `collect_repo_grants` adds `([ Read ], ...)` for legacy entries.
- **Test**: `test_legacy_repositories_become_read_only_repo_grants` — one repo
  grant with provenance.
- **Proof candidate**: Prove legacy `repositories` become read-only (`[Read]`)
  repo grants. Trivial field-mapping proof.

---

## 7. Memory Grants

**INV-MEM-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Memory grants are direct,
not transitive. A bundle granting access to "child" does not transitively
grant child's memory grants.

- **Code**: Only the directly referenced bundle's `memory_grants` are collected.
- **Test**: `test_memory_grants_are_direct_not_transitive` — only `child`
  appears, not `scope:secret:read`.
- **Proof candidate**: Prove memory grants are direct (no recursive traversal).

**INV-MEM-2** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Missing `memory_grants`
field defaults to empty (no access).

- **Code**: `string_list b "memory_grants"` returns `[]` on missing key.
- **Test**: `test_missing_memory_grants_default_to_no_access`.
- **Proof candidate**: Prove missing `memory_grants` field defaults to empty.
  Standard JSON-parsing default behavior.

---

## 8. Channel Isolation

**INV-CHAN-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Room scopes with a
`channel` selector only match sessions from that channel type.

- **Code**: `scope_matches` checks `string_option_required_matches channel_type
  scope.channel`.
- **Test**: `test_room_scopes_do_not_cross_channel_boundaries` — slack room
  tool not visible to discord session.
- **Proof candidate**: Prove room scopes with `channel` selector only match
  sessions from that channel type.

**INV-CHAN-2** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Missing layer selectors
are not wildcards. A workspace scope without a `workspace` field does not
match any session. A room scope without a `room` field does not match any
session.

- **Code**: `scope_matches` requires `expected = room` for room level; missing
  `workspace` fails `workspace_option_required_matches`.
- **Test**: `test_missing_layer_selectors_are_not_wildcards`.
- **Proof candidate**: Prove missing layer selectors are not wildcards.

---

## 9. Snapshot Invariants

**INV-SNAP-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Snapshots are immutable
once created. The `create` function produces a self-contained record; `persist`
writes to DB but does not mutate the in-memory record.

- **Code**: `Access_snapshot.create` returns a plain record type `t`.
- **Test**: `test_snapshot_immutable_after_persist` in `test_access_snapshot.ml`
  — config_hash and other fields unchanged after persist and reload.
- **Proof candidate**: Prove `Access_snapshot.create` returns an immutable
  record. Structural property of OCaml records.

**INV-SNAP-2** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Snapshot `tool_denial`
matches the resolver's allowed/denied lists exactly.

- **Code**: `tool_denial` checks `denied_tools` then `allowed_tools` using the
  same logic as the resolver's post-filter.
- **Test**: `test_legacy_snapshot_matches_resolver` — for each tool, snap
  denial matches resolver denial.
- **Proof candidate**: Prove snapshot `tool_denial` matches resolver output
  exactly.

**INV-SNAP-3** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Snapshot carries a
`config_hash` (SHA-256 of serialized config) for audit trail. The hash is
deterministic for the same config and differs when config changes.

- **Code**: `config_hash` in `Access_snapshot.create` calls
  `Digestif.SHA256.digest_string`.
- **Tests**: `test_create_snapshot_basic` (non-empty hash),
  `test_config_hash_deterministic` (same config = same hash),
  `test_config_hash_differs_on_config_change` (different config = different
  hash).
- **Proof candidate**: Prove `config_hash` is deterministic and
  collision-resistant (SHA-256 property).

**INV-SNAP-4** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Snapshot preserves bundle
provenance. `bundle_sources` records which bundle/layer/field contributed each
effective grant.

- **Code**: `extract_bundle_sources` walks `effective_access` provenance.
- **Test**: `test_bundle_sources_extracted` and
  `test_bundle_sources_include_non_tool_fields` in `test_access_snapshot.ml`.
- **Proof candidate**: Prove `bundle_sources` records all provenance entries.

---

## 10. Egress Rule Ordering

**INV-EGR-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Egress rules are ordered
with higher-priority scopes first (first match wins). Profile bundles (room
layer) come before scope bundles. Within scope bundles, rules are grouped by
scope and reversed so Room > Channel > Workspace > Default.

- **Code**: `resolve_effective_access` constructs `scope_groups` by reversing
  the scope order, then concatenates `profile_bundles @ scope_groups`.
- **Test**: `test_egress_rule_ordering_profile_before_scope` — profile egress
  allow takes precedence over default deny.
- **Proof candidate**: Prove egress rules are ordered
  profile > room > channel > workspace > default.

---

## 11. Active/Deleted Filtering

**INV-ACT-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Scopes with
`status: "deleted"` are filtered out during resolution via `scope_active`.
Bundles and profiles with deleted status are similarly filtered in validation
and implicit profile resolution.

- **Code**: `scope_active` checks
  `String.lowercase_ascii scope.status <> "deleted"`.
  `resolve_room_profile` filters by profile status.
- **Tests**: `test_deleted_scopes_filtered_during_resolution`,
  `test_deleted_scopes_case_insensitive`, `test_deleted_room_scope_filtered`.
- **Note**: Explicit `?room_profile` passed to `resolve_effective_access` is
  not status-filtered by the resolver itself; callers are responsible for
  passing only active profiles.
- **Proof candidate**: Prove `scope_active` filters deleted scopes regardless
  of case.

---

## 12. Reload Invariants

**INV-RLD-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Config reload re-parses
the config file from disk, picking up changes to bundles, scopes, and profiles.

- **Code**: `Config_loader.load` re-reads and re-parses the JSON file.
- **Tests**:
  - `test_minimal_reload_picks_up_config_change`
  - `test_minimal_scope_bundle_reload_cycle`
  - `test_minimal_new_room_profile_added_on_reload`
- **Proof candidate**: Prove config reload re-parses from disk and produces
  identical OCaml values for unchanged JSON.

**INV-RLD-2** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Existing room profiles
survive a config reload cycle intact (unchanged fields preserved).

- **Code**: `parse_config` re-reads all fields; unchanged JSON produces
  identical OCaml values.
- **Tests**:
  - `test_minimal_room_profiles_survive_reload`
  - `test_minimal_room_profiles_persist_through_config_change`
- **Proof candidate**: Prove room profiles survive reload intact.

**INV-RLD-3** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Invalid config JSON does
not crash the runtime. The minimal binary continues to produce status output.

- **Code**: `load_result` catches parse errors and returns `Error`; `load`
  falls back to defaults.
- **Test**: `test_minimal_reload_from_invalid_json`.
- **Proof candidate**: Prove invalid JSON does not crash the runtime.

**INV-RLD-4** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Tool access decisions are
stable across reload when config is unchanged.

- **Code**: Re-reading the same JSON produces identical `room_profile` records.
- **Test**: `test_minimal_room_profile_tool_access_after_reload`.
- **Proof candidate**: Prove tool access decisions are stable across reload for
  unchanged config.

**INV-RLD-5** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Malformed access policy
(e.g., duplicate bundle ids) triggers validation warnings. The config loader
applies `fail_closed_access_policy` which forces scoped access to deny, but
the minimal binary does not crash.

- **Code**: `validate_room_profiles` returns issues; `load_result` logs
  warnings and applies `fail_closed_access_policy`.
- **Test**: `test_minimal_status_with_malformed_scope_bundles` — minimal binary
  produces status output without crashing. Stronger validation tests exist in
  `test_config_loader.ml`.
- **Proof candidate**: Prove malformed access policy triggers fail-closed.

---

## 13. Provenance Tracking

**INV-PROV-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Every effective access
item carries non-empty provenance.

- **Code**: `add_bundle_items` attaches at least two provenance entries per
  item (direct field + bundle id reference).
- **Test**: `assert_all_provenance` called in most scope resolver tests.
- **Proof candidate**: Prove every effective access item carries non-empty
  provenance.

**INV-PROV-2** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Provenance records the
layer, source_id, and field name for each contribution.

- **Code**: `access_provenance` type with `layer`, `source_id`, `field`.
- **Test**: `test_layers_merge_deterministically_and_deny_wins` — room item
  provenance shows `room:z-room:allowed_tools`.
- **Proof candidate**: Prove provenance records layer, source_id, and field.

---

## 14. Backward Compatibility

**INV-COMP-1** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Pure legacy configs
(P11-P13) with no `access_bundles` or `access_scopes` resolve correctly
through the new resolver.

- **Code**: Empty bundles/scopes lists produce no scope-level grants; profile
  bundles carry legacy inline fields.
- **Test**: `test_legacy_config_no_scopes_no_bundles`.
- **Proof candidate**: Prove pure legacy configs resolve correctly through new
  resolver.

**INV-COMP-2** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Legacy effective access
matches the legacy `room_profile_tool_denial_for_session` path for all tools.

- **Code**: Both paths use the same bundle-derived allowed/denied lists.
- **Test**: `test_legacy_effective_matches_tool_denial`.
- **Proof candidate**: Prove legacy effective access matches legacy
  `room_profile_tool_denial_for_session`.

**INV-COMP-3** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Hybrid profiles (both
`access_bundle_ids` and inline legacy fields) produce effective access from
both sources.

- **Code**: `access_bundles_for_profile` returns explicit bundles + implicit
  legacy bundle.
- **Test**: `test_legacy_hybrid_explicit_and_implicit_bundles`.
- **Proof candidate**: Prove hybrid profiles produce effective access from both
  explicit and implicit bundles.

**INV-COMP-4** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Legacy
`room_profile_codebase_grants` match
`room_profile_codebase_grants_for_profile` output.

- **Code**: Both paths collect from `room_profile_codebase_grants` list.
- **Test**: `test_legacy_codebase_grants_match_codebase_grants_for_profile`.
- **Proof candidate**: Prove legacy codebase grants match
  `room_profile_codebase_grants_for_profile`.

**INV-COMP-5** `[RUNTIME] [TEST] [PROOF-CANDIDATE]`: Multiple legacy profiles
bound to different rooms do not interfere with each other.

- **Code**: `resolve_room_profile` matches by session_key; each profile's
  bundles are independent.
- **Test**: `test_multiple_legacy_profiles_do_not_interfere`.
- **Proof candidate**: Prove multiple legacy profiles do not interfere.

---

## Appendix: Test File Locations

| Test file | Coverage |
|-----------|----------|
| `test/test_scope_resolver.ml` | INV-DET, INV-PREC, INV-CONF, INV-SEC, INV-CG, INV-REPO, INV-MEM, INV-CHAN, INV-PROV, INV-COMP |
| `test/test_minimal_reload.ml` | INV-RLD |
| `test/test_access_snapshot.ml` | INV-SNAP |
| `test/test_config_loader.ml` | INV-RLD-5 (validation/fail-closed) |
| `src/access_snapshot.ml` | Snapshot implementation |
| `src/runtime_config.ml` | Resolver implementation |
| `src/config_loader.ml` | Config parsing and validation |

## Known Test Issues

- `test_repo_grants_blocked_by_global_security` is defined twice in
  `test/test_scope_resolver.ml` (lines 467 and 784). The second definition
  shadows the first; both suite entries reference the second definition. The
  first definition's scenario (local-path repo outside workspace) is not
  currently exercised by the test suite.
