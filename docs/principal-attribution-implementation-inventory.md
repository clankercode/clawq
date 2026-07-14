# P21 implementation inventory (task / source / schema / API / test)

Crosswalk of shipped P21 surfaces for operators and reviewers (P21.M4.E3.T002).
Not exhaustive of every helper; names major modules and regression suites.

## T002 acceptance crosswalk

This compact crosswalk is the maintained evidence for the T002 acceptance
categories. The detailed inventories below remain the module-level index.

| Task lineage | Source | Schema/store | API/operator surface | Regression evidence |
|---|---|---|---|---|
| **Trust-adapter readiness** — `P21.M1.E1.T003`, `P21.M1.E1.T005–T009`, `P21.M2.E1.T001` | `principal_bootstrap`, `teams/slack/discord/telegram_principal_ingress`, `github_user_auth_readiness` | `principal_identity_store`: `principals`, `connector_actors`, `identity_links` | `clawq github user-auth readiness` (`Github_user_auth_enablement_cli`) | `principal_bootstrap`, `*_principal_ingress`, `github_user_auth_readiness` |
| **Linking conflicts** — `P21.M1.E1.T004`, `P21.M1.E1.T010–T012`, `P21.M1.E2.T002` | `principal_link_exec`, `principal_merge`, `principal_unlink_split`, `github_account_ownership_policy` | `identity_links`, `github_account_bindings`, binding snapshots | `clawq github account link|relink|unlink` | `principal_link_exec`, `principal_unlink_split`, `github_account_ownership_policy` |
| **Authorization** — `P21.M2.E1.T002–T003`, `P21.M2.E2.T001–T004`, `P21.M2.E3.T001–T003` | `github_user_auth_tx`, `github_user_auth_pkce`, `github_user_auth_device`, `github_user_auth_activate` | `github_user_auth_tx` | `clawq github user-auth status|readiness|repair|enable|disable|apply` | `github_user_auth_tx`, `github_user_auth_pkce_callback`, `github_user_auth_device_poll`, `github_user_auth_activate` |
| **Key lifecycle** — `P21.M2.E4.T001–T008` | `github_user_token_vault`, `github_user_token_master_key`, `github_user_token_rewrap` | `github_user_token_vault`, `github_user_token_rewrap` | `clawq github user-auth readiness`; `Github_user_token_vault_recovery.check_compatibility` | `github_user_token_vault`, `github_user_token_master_key`, `github_user_token_rewrap` |
| **Attribution rollout** — `P21.M3.E2.T001`, `P21.M3.E2.T003–T007` | `github_attribution_authorize`, `github_attribution_rollout`, `github_action_reconcile` | rollout `readiness`/gate JSON records; `github_action_correlations` | `Github_attribution_rollout.readiness_complete`, `cleanup_complete` | `github_attribution_authorize`, `github_attribution_rollout`, `github_action_reconcile` |
| **Delayed-job repair** — `P21.M1.E3.T005`, `P21.M3.E3.T003` | `github_delayed_attribution`, `principal_legacy_migrate` | `principal_legacy_migration_runs`, `principal_legacy_migration_records`, `principal_legacy_invalidated_jobs` | `Principal_legacy_migrate.rollback_run` | `github_delayed_attribution`, `github_durable_job_actor_attribution`, `principal_legacy_migrate` |
| **Revoke/relink** — `P21.M3.E1.T003–T004`, `P21.M4.E1.T001` | `github_user_auth_invalidate`, `github_user_auth_revocation_webhook`, `github_account_cli` | `github_account_bindings`, `github_user_token_vault` | `clawq github account relink|unlink` | `github_user_auth_invalidate`, `github_user_auth_revocation_webhook`, `github_account_cli` |
| **Backup/restore** — `P21.M2.E4.T005`, `P21.M2.E4.T008` | `github_user_token_vault_recovery`, `github_user_token_vault` | `github_user_token_vault`, `github_user_token_vault_recovery_events`, recovery state | `Github_user_token_vault_recovery.export_backup`, `check_compatibility`, `restore` | `github_user_token_vault_recovery`, `github_user_token_vault_security` |
| **Pilot cleanup** — `P21.M3.E2.T006`, `P21.M4.E2.T003–T004` | `github_attribution_rollout` | rollout `rollback_gate`/`cleanup_gate` JSON records | `Github_attribution_rollout.cleanup_complete`, `no_residual_authority` | `github_attribution_rollout`, `github_p21_pilot_dryrun` |
| **Compatibility** — `P21.M4.E1.T004` | `github_app_pat_compat`, `command_bridge_min` | `github_user_auth_enablement` and `github_user_auth_enablement_plans` | `clawq github account` and `clawq github user-auth` (minimal build returns disabled guidance) | `github_app_pat_compat` |

## Identity and linking

| Concern | Source | Schema / store | Tests |
|---------|--------|----------------|-------|
| Principal / actor model | `principal_identity` | types | `principal_identity` |
| Persist Principals/links | `principal_identity_store` | SQLite principals/actors/links | `principal_identity_store` |
| Bootstrap fail-closed | `principal_bootstrap` | — | `principal_bootstrap` |
| Resolve/create | `principal_resolve` | store | `principal_resolve` |
| Ingress adapters | `teams/slack/discord/telegram_principal_*` | — | `*_principal` suites |
| Link protocol | `principal_link_protocol` | types | `principal_link_protocol` |
| Link execution | `principal_link_exec` | `principal_link_tx`, edges | `principal_link_exec` |
| Merge/adopt | `principal_merge`, `principal_merge_persist` | snapshots/receipts | `principal_merge` |
| Unlink/split | `principal_unlink_split` | plans | `principal_unlink_split` |
| Legacy migrate | `principal_legacy_migrate` | migration tables | `principal_legacy_migrate` |
| Isolation proof | — | — | `principal_isolation_attribution` |

## Account binding and preferences

| Concern | Source | Tests |
|---------|--------|-------|
| Bindings + vault ref | `github_account_binding` | `github_account_binding` |
| Ownership policy | `github_account_ownership_policy` | `github_account_ownership_policy` |
| Preferences | `github_account_preference` | `github_account_preference` |
| Eligible resolve | `github_eligible_account_resolve` | `github_eligible_account_resolve` |
| Admin surface | `github_account_admin_surface` | `github_account_admin_surface` |
| CLI / tool | `github_account_cli`, `github_account_tool` | `github_account_cli` |

## Authorization and vault

| Concern | Source | Tests |
|---------|--------|-------|
| Readiness | `github_user_auth_readiness` | `github_user_auth_readiness` |
| Auth tx | `github_user_auth_tx` | `github_user_auth_tx` |
| Private delivery | `github_user_auth_delivery` | `github_user_auth_delivery` |
| PKCE | `github_user_auth_pkce`, `*_callback` | `github_user_auth_pkce*` |
| Device | `github_user_auth_device`, `*_poll` | `github_user_auth_device*` |
| Activate | `github_user_auth_activate` | `github_user_auth_activate` |
| Token records | `github_user_token_store` | `github_user_token` |
| Master key | `github_user_token_master_key` | `github_user_token_master_key` |
| Vault CRUD | `github_user_token_vault` | `github_user_token_vault` |
| CAS | `github_user_token_cas` | `github_user_token_cas` |
| Lease | `github_user_token_lease` | `github_user_token_lease` |
| Refresh / flight | `github_user_token_refresh` | `github_user_token_refresh` |
| Rewrap | `github_user_token_rewrap` | `github_user_token_rewrap` |
| Recovery | `github_user_token_vault_recovery` | `github_user_token_vault_recovery` |
| Invalidate | `github_user_auth_invalidate` | `github_user_auth_invalidate` |
| Revocation webhook | `github_user_auth_revocation_webhook` | `github_user_auth_revocation_webhook` |
| Enablement admin | `github_user_auth_enablement` | `github_user_auth_enablement` |
| Diagnostics | `github_user_auth_diagnostics` | `github_user_auth_diagnostics` |
| App/PAT compat | `github_app_pat_compat` | `github_app_pat_compat` |
| Security suite | — | `github_p21_security`, vault_security |

## Attribution and actions

| Concern | Source | Tests |
|---------|--------|-------|
| Policy | `github_attribution_policy` | `github_attribution_policy` |
| Authorize | `github_attribution_authorize` | `github_attribution_authorize` |
| Fallback | `github_attribution_fallback` | `github_attribution_fallback` |
| Dispatch lease | `github_attribution_dispatch_lease` | `github_attribution_dispatch_lease` |
| Audit | `github_attribution_audit` | `github_attribution_audit` |
| Rollout | `github_attribution_rollout` | `github_attribution_rollout` |
| Actor pin | `actor_snapshot`, `github_action_actor_attribution` | `actor_snapshot`, `github_action_actor_attribution` |
| Delayed pin | `github_delayed_attribution` | `github_delayed_attribution` |
| Family integrators | `github_*_attribution` (collab, PR review, issue, merge, workflow_dispatch, code_change) | matching suites |
| Reconcile | `github_action_reconcile` | `github_action_reconcile` |
| Integration | — | `github_p21_integration` |

## Docs and pilots

| Artifact | Path |
|----------|------|
| Plan | `docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md` |
| ADR boundary | `docs/adr/0009-principal-token-vault-security-boundary.md` |
| Glossary | `docs/glossary-principal-github-attribution.md` |
| Operator contract | `docs/github-user-auth-operator-contract.md` |
| Vault recovery | `docs/github-vault-recovery.md` |
| Pilot runbook/checklist/receipt/backout | `docs/pilots/p21-*` |
| Doc drift tests | `github_p21_docs`, `github_p21_adr_glossary`, `github_p21_pilot_dryrun`, `github_p21_operator_docs` |

## API stability notes

- Prefer pure modules with injectable fetchers/clocks for tests.
- Redacted JSON/export paths must never include access/refresh tokens, client
  secrets, device codes, or raw vault plaintext.
- Minimal build: disabled guidance for integration-only CLI surfaces.
