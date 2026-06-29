# Scope Resolution Invariants

**Status**: Authoritative  
**Last updated**: 2026-06-29  
**Owner**: P14 (Access Scope Inheritance)

This document enumerates the invariants guaranteed by the scope resolver
(`Runtime_config.resolve_effective_access`) and access snapshots
(`Access_snapshot`). Most invariants link to runtime tests that enforce them;
exceptions are noted explicitly.

**Proof backlog**: See [`proof-backlog.md`](proof-backlog.md) for a catalogue
of invariants that are candidates for formal proof or stronger verification.

---

## 1. Resolver Determinism

**INV-DET-1**: The resolver is a function of `(config, session_key,
?room_profile)`. Given identical inputs, it produces identical effective access
within the same process invocation. The resolver depends on `HOME` (via
`expand_home` / `expand_cwd_pattern`) for tilde expansion; determinism assumes
a stable `HOME` environment variable.

- **Code**: `resolve_effective_access` in `runtime_config.ml` (line 763).
- **Test**: `test_legacy_snapshot_matches_resolver` — snapshot created from the
  same config carries identical allowed/denied tools as a direct resolver call.

**INV-DET-2**: Scope bundles are merged in deterministic order: level rank
ascending (`Default=0 < Workspace=1 < Channel=2 < Room=3`), then scope id
lexicographic within the same level.

- **Code**: `sort_scopes` uses `compare` on `(access_scope_level_rank, id)`.
- **Test**: `test_layers_merge_deterministically_and_deny_wins` verifies layer
  ordering. Same-level lexicographic ordering is enforced by the sort but not
  directly tested with multiple same-level scopes.

---

## 2. Precedence Rules

**INV-PREC-1**: Scope levels have fixed precedence:
`Default (0) < Workspace (1) < Channel (2) < Room (3)`.

- **Code**: `access_scope_level_rank` in `runtime_config.ml` (line 609).
- **Test**: `test_layers_merge_deterministically_and_deny_wins` — room scope
  tools appear after workspace/channel tools in the merged list.

**INV-PREC-2**: Profile bundles (from `room_profiles`) are treated as the
`"room"` layer. In the resolver, profile bundles are appended after scope
bundles (`scope_bundles @ profile_bundles`).

- **Code**: `resolve_effective_access` concatenates `scope_bundles @ profile_bundles`.
- **Test**: `test_legacy_room_profile_bundle_is_room_layer` — legacy profile
  provenance shows `room:room_profile:<id>`. Note: this test verifies the
  layer label, not the ordering relative to room-scope bundles.

**INV-PREC-3**: Within the same scope, bundles referenced by `access_bundle_ids`
are merged in declaration order (list order preserved by `List.filter_map`).

- **Code**: `scope.access_bundle_ids |> List.filter_map ...` preserves list order.
- **Test**: No dedicated test verifies same-scope declaration order. The
  ordering is enforced by the implementation.

---

## 3. Conflict Resolution

**INV-CONF-1**: Deny wins over allow. If a tool appears in `denied_tools`
anywhere in the merged bundles, it is removed from `allowed_tools`.

- **Code**: `allowed_tools |> List.filter (fun item -> not (List.mem item.value denied_tool_values))`.
- **Test**: `test_layers_merge_deterministically_and_deny_wins` — `shared_tool`
  denied by room scope is removed from allowed.

**INV-CONF-2**: Same-tool allow+deny within a single bundle: deny wins, allow
entry is removed.

- **Code**: The deny filter runs after collection, removing any allowed item
  whose value appears in the denied list.
- **Test**: `test_allow_and_deny_same_tool_denies` — `shell_exec` in both
  allowed and denied of the same bundle results in empty allowed, explicit deny.

**INV-CONF-3**: Room deny overrides workspace allow for the same tool.

- **Code**: Deny list is collected from all bundles (including room-level), then
  applied as a filter on allowed.
- **Test**: `test_room_deny_overrides_workspace_allow` — `deploy_tool` allowed
  by workspace scope but denied by room scope is removed from effective allowed.

**INV-CONF-4**: Invalid bundle references cause fail-closed behavior. If a
scope or profile references a non-existent bundle id, all grants from that
scope/profile are suppressed.

- **Code**: `find_access_bundle cfg bundle_id = None` causes the entire scope
  to yield no bundles. `profile_missing_access_bundle_ids` returns non-empty
  for invalid profile bundle refs.
- **Tests**:
  - `test_invalid_scope_bundle_denies_scope_grants`
  - `test_invalid_profile_bundle_denies_effective_profile_grants`

**INV-CONF-5**: Duplicate bundle references across scopes merge provenance but
do not duplicate values.

- **Code**: `merge_effective_items` uses a hash table keyed by value, appending
  provenance on collision.
- **Test**: `test_duplicate_bundle_references_merge_provenance_once` —
  `shared_tool` appears once with both scope provenances.

---

## 4. Global Security Caps

**INV-SEC-1**: When `security.workspace_only = true`, codebase grants whose
expanded path prefix falls outside the workspace (and `extra_allowed_paths`)
are moved to `blocked_codebase_grants`.

- **Code**: `blocked_by_global_security` checks `is_prefix_of ~prefix:workspace`.
- **Test**: `test_global_security_caps_codebase_grants` — `/tmp/outside/**`
  blocked, workspace grant kept.

**INV-SEC-2**: Inherited grants do not weaken global security. Room-level
grants that point outside the workspace are blocked even if inherited from a
lower-level scope.

- **Code**: `blocked_by_global_security` runs on each grant independently.
- **Test**: `test_inherited_grants_do_not_weaken_global_security` — both
  default and room outside grants are blocked.

**INV-SEC-3**: `allowed_cwd_patterns` provides an additional ceiling. Grants
must match at least one pattern (or the pattern list must be empty).

- **Code**: `pattern_ok` check in `blocked_by_global_security`.
- **Test**: `test_global_security_caps_codebase_grants` — pattern list
  restricts grants to matching paths.

---

## 5. Codebase Grant Expansion

**INV-CG-1**: `$CLAWQ_WORKSPACE` is expanded to the configured workspace path
before security checks.

- **Code**: `expand_cwd_pattern` replaces `$CLAWQ_WORKSPACE` with
  `effective_workspace cfg`.
- **Test**: `test_legacy_room_profile_bundle_is_room_layer` —
  `$CLAWQ_WORKSPACE/**` expands to `/tmp/clawq-scope-root/**`.

**INV-CG-2**: Tilde (`~`) in workspace selectors is expanded to the home
directory before comparison.

- **Code**: `expand_home` in `workspace_option_required_matches`.
- **Test**: `test_workspace_scope_expands_tilde_selector` — `~/clawq-scope-root`
  matches expanded workspace.

---

## 6. Repo Grant Resolution

**INV-REPO-1**: Explicit `repo_grants` suppress legacy `repositories` for the
same repo key.

- **Code**: `collect_repo_grants` checks `Hashtbl.mem repo_table repo` before
  adding legacy entries.
- **Test**: `test_explicit_repo_grants_take_precedence_over_legacy` — one grant
  (not duplicated), legacy repositories preserved.

**INV-REPO-2**: Repo grants are subject to global security checks. For
local-path repo grants (starting with `/`, `~`, `$`, `./`, `../`), the path is
checked directly. For non-local repo grants (e.g., GitHub-style `acme/app`),
the resolver maps them to `$CLAWQ_WORKSPACE/<repo>` before the security check.

- **Code**: `repo_path_for_grant_item` maps non-local repos to workspace paths.
  `repo_grant_allowed` calls `blocked_by_global_security` on the mapped path
  when `repo_grant_is_local_path` is true on the expanded result.
- **Test**: `test_repo_grants_blocked_by_global_security` (second definition,
  line 784) — both `acme/app` and `outside/repo` are mapped under the workspace
  and allowed. Note: a first definition at line 467 tested local-path blocking
  but is shadowed by the second definition; the suite entries both reference
  the second definition.

**INV-REPO-3**: Repo grants require codebase grant coverage. If
`codebase_grants` is non-empty, a repo grant must be covered by at least one
codebase grant pattern.

- **Code**: `repo_grant_covered_by_codebase_grants` checks glob matching.
- **Test**: `test_repo_grants_intersect_codebase_grants` — uncovered repo
  blocked, covered repo kept.

**INV-REPO-4**: Path traversal in repo grants is normalized before codebase
intersection check.

- **Code**: `Path_util.normalize_path` on `repo_path_for_codebase_check`.
- **Test**: `test_repo_grants_normalize_traversal_before_codebase_intersection`
  — `allowed/../other/app` is not covered by `allowed/**`.

**INV-REPO-5**: Wildcard repo grants require exact codebase grant match (not
glob matching).

- **Code**: `repo_grant_has_glob_metachar` triggers
  `repo_grant_exactly_covered_by_codebase_grants` (string equality).
- **Test**: `test_wildcard_repo_grants_require_exact_codebase_grant` —
  `allowed/**` blocked, `exact/**` kept when codebase grant is `exact/**`.

**INV-REPO-6**: Legacy `repositories` become read-only (capability `[Read]`)
repo grants.

- **Code**: `collect_repo_grants` adds `([ Read ], ...)` for legacy entries.
- **Test**: `test_legacy_repositories_become_read_only_repo_grants` — one repo
  grant with provenance.

---

## 7. Memory Grants

**INV-MEM-1**: Memory grants are direct, not transitive. A bundle granting
access to "child" does not transitively grant child's memory grants.

- **Code**: Only the directly referenced bundle's `memory_grants` are collected.
- **Test**: `test_memory_grants_are_direct_not_transitive` — only `child`
  appears, not `scope:secret:read`.

**INV-MEM-2**: Missing `memory_grants` field defaults to empty (no access).

- **Code**: `string_list b "memory_grants"` returns `[]` on missing key.
- **Test**: `test_missing_memory_grants_default_to_no_access`.

---

## 8. Channel Isolation

**INV-CHAN-1**: Room scopes with a `channel` selector only match sessions from
that channel type.

- **Code**: `scope_matches` checks `string_option_required_matches channel_type
  scope.channel`.
- **Test**: `test_room_scopes_do_not_cross_channel_boundaries` — slack room
  tool not visible to discord session.

**INV-CHAN-2**: Missing layer selectors are not wildcards. A workspace scope
without a `workspace` field does not match any session. A room scope without a
`room` field does not match any session.

- **Code**: `scope_matches` requires `expected = room` for room level; missing
  `workspace` fails `workspace_option_required_matches`.
- **Test**: `test_missing_layer_selectors_are_not_wildcards`.

---

## 9. Snapshot Invariants

**INV-SNAP-1**: Snapshots are immutable once created. The `create` function
produces a self-contained record; `persist` writes to DB but does not mutate
the in-memory record.

- **Code**: `Access_snapshot.create` returns a plain record type `t`.
- **Test**: `test_snapshot_immutable_after_persist` in `test_access_snapshot.ml`
  — config_hash and other fields unchanged after persist and reload.

**INV-SNAP-2**: Snapshot `tool_denial` matches the resolver's allowed/denied
lists exactly.

- **Code**: `tool_denial` checks `denied_tools` then `allowed_tools` using the
  same logic as the resolver's post-filter.
- **Test**: `test_legacy_snapshot_matches_resolver` — for each tool, snap
  denial matches resolver denial.

**INV-SNAP-3**: Snapshot carries a `config_hash` (SHA-256 of serialized config)
for audit trail. The hash is deterministic for the same config and differs
when config changes.

- **Code**: `config_hash` in `Access_snapshot.create` calls
  `Digestif.SHA256.digest_string`.
- **Tests**: `test_create_snapshot_basic` (non-empty hash),
  `test_config_hash_deterministic` (same config = same hash),
  `test_config_hash_differs_on_config_change` (different config = different hash).

**INV-SNAP-4**: Snapshot preserves bundle provenance. `bundle_sources` records
which bundle/layer/field contributed each effective grant.

- **Code**: `extract_bundle_sources` walks `effective_access` provenance.
- **Test**: `test_bundle_sources_extracted` and
  `test_bundle_sources_include_non_tool_fields` in `test_access_snapshot.ml`.

---

## 10. Egress Rule Ordering

**INV-EGR-1**: Egress rules are ordered with higher-priority scopes first
(first match wins). Profile bundles (room layer) come before scope bundles.
Within scope bundles, rules are grouped by scope and reversed so Room >
Channel > Workspace > Default.

- **Code**: `resolve_effective_access` constructs `scope_groups` by reversing
  the scope order, then concatenates `profile_bundles @ scope_groups`.
- **Note**: No dedicated egress ordering test exists. The ordering is enforced
  by the resolver implementation.

---

## 11. Active/Deleted Filtering

**INV-ACT-1**: Scopes with `status: "deleted"` are filtered out during
resolution via `scope_active`. Bundles and profiles with deleted status are
similarly filtered in validation and implicit profile resolution.

- **Code**: `scope_active` checks `String.lowercase_ascii scope.status <> "deleted"`.
  `resolve_room_profile` filters by profile status.
- **Note**: Explicit `?room_profile` passed to `resolve_effective_access` is
  not status-filtered by the resolver itself; callers are responsible for
  passing only active profiles. No dedicated deleted-filtering test exists.

---

## 12. Reload Invariants

**INV-RLD-1**: Config reload re-parses the config file from disk, picking up
changes to bundles, scopes, and profiles.

- **Code**: `Config_loader.load` re-reads and re-parses the JSON file.
- **Tests**:
  - `test_minimal_reload_picks_up_config_change`
  - `test_minimal_scope_bundle_reload_cycle`
  - `test_minimal_new_room_profile_added_on_reload`

**INV-RLD-2**: Existing room profiles survive a config reload cycle intact
(unchanged fields preserved).

- **Code**: `parse_config` re-reads all fields; unchanged JSON produces
  identical OCaml values.
- **Tests**:
  - `test_minimal_room_profiles_survive_reload`
  - `test_minimal_room_profiles_persist_through_config_change`

**INV-RLD-3**: Invalid config JSON does not crash the runtime. The minimal
binary continues to produce status output.

- **Code**: `load_result` catches parse errors and returns `Error`; `load`
  falls back to defaults.
- **Test**: `test_minimal_reload_from_invalid_json`.

**INV-RLD-4**: Tool access decisions are stable across reload when config is
unchanged.

- **Code**: Re-reading the same JSON produces identical `room_profile` records.
- **Test**: `test_minimal_room_profile_tool_access_after_reload`.

**INV-RLD-5**: Malformed access policy (e.g., duplicate bundle ids) triggers
validation warnings. The config loader applies `fail_closed_access_policy`
which forces scoped access to deny, but the minimal binary does not crash.

- **Code**: `validate_room_profiles` returns issues; `load_result` logs
  warnings and applies `fail_closed_access_policy`.
- **Test**: `test_minimal_status_with_malformed_scope_bundles` — minimal binary
  produces status output without crashing. Stronger validation tests exist in
  `test_config_loader.ml`.

---

## 13. Provenance Tracking

**INV-PROV-1**: Every effective access item carries non-empty provenance.

- **Code**: `add_bundle_items` attaches at least two provenance entries per
  item (direct field + bundle id reference).
- **Test**: `assert_all_provenance` called in most scope resolver tests.

**INV-PROV-2**: Provenance records the layer, source_id, and field name for
each contribution.

- **Code**: `access_provenance` type with `layer`, `source_id`, `field`.
- **Test**: `test_layers_merge_deterministically_and_deny_wins` — room item
  provenance shows `room:z-room:allowed_tools`.

---

## 14. Backward Compatibility

**INV-COMP-1**: Pure legacy configs (P11-P13) with no `access_bundles` or
`access_scopes` resolve correctly through the new resolver.

- **Code**: Empty bundles/scopes lists produce no scope-level grants; profile
  bundles carry legacy inline fields.
- **Test**: `test_legacy_config_no_scopes_no_bundles`.

**INV-COMP-2**: Legacy effective access matches the legacy
`room_profile_tool_denial_for_session` path for all tools.

- **Code**: Both paths use the same bundle-derived allowed/denied lists.
- **Test**: `test_legacy_effective_matches_tool_denial`.

**INV-COMP-3**: Hybrid profiles (both `access_bundle_ids` and inline legacy
fields) produce effective access from both sources.

- **Code**: `access_bundles_for_profile` returns explicit bundles + implicit
  legacy bundle.
- **Test**: `test_legacy_hybrid_explicit_and_implicit_bundles`.

**INV-COMP-4**: Legacy `room_profile_codebase_grants` match
`room_profile_codebase_grants_for_profile` output.

- **Code**: Both paths collect from `room_profile_codebase_grants` list.
- **Test**: `test_legacy_codebase_grants_match_codebase_grants_for_profile`.

**INV-COMP-5**: Multiple legacy profiles bound to different rooms do not
interfere with each other.

- **Code**: `resolve_room_profile` matches by session_key; each profile's
  bundles are independent.
- **Test**: `test_multiple_legacy_profiles_do_not_interfere`.

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
