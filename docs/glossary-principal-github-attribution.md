# Glossary — Principal identity and GitHub user attribution

Terms as shipped for P21 (and P19 action surfaces that consume them). Normative
security boundary: [ADR 0009](adr/0009-principal-token-vault-security-boundary.md).

| Term | Meaning |
|------|---------|
| **Principal** | Stable human identity record, distinct from Room/Session. Created on first verified Connector actor or authenticated bootstrap. |
| **Connector actor** | Namespace + immutable user id from a verified adapter (Teams tenant+oid, Slack workspace+user, …). |
| **Identity link** | Active association of a Connector actor to a Principal (or tombstoned after merge). |
| **Actor snapshot** | Immutable, non-authoritative capture of Principal/lineage/link evidence for intents, jobs, receipts. |
| **Current authority** | Live re-resolution of Principal (following `Merged_into`), active links, and Authorized binding lineage. |
| **GitHub account binding** | Principal-owned record of host/App/numeric user id, mutable login, auth status, lineage, optional vault ref. |
| **Vault record** | Encrypted user access/refresh handles for one binding; schema versioned; no plaintext tokens in exports. |
| **Token generation** | CAS counter for mutable credential material on a vault row (not master-key version). |
| **Master key / key id** | External encryption key with versioned id; staged rotation + rewrap under CAS. |
| **Opaque lease** | Callback-scoped handle + binding + generation for HTTP use only; never runner/shell/Git. |
| **Pending activation** | Post-exchange state: sealed pending credential + private confirmation required before Authorized. |
| **User_required** | Attribution mode: native user token required; no App/PAT fallback. |
| **User_preferred** | Attribution mode: prefer user; App fallback only if policy + preview name App. |
| **App_installation** | Action may run as App installation token under policy. |
| **Attribution rollout stage** | `safe_default`, production/pilot enable, `rollback`, `cleanup` with no residual authority. |
| **Whole-store rollback limitation** | Internally consistent DB restore under the same key may not be detectable without external anchors. |

## Related docs

- [ADR 0005](adr/0005-separate-human-principals-from-room-sessions.md)
- [ADR 0006](adr/0006-use-principal-owned-github-user-tokens.md)
- [Operator contract](github-user-auth-operator-contract.md) (P21.M4.E3.T002)
- [Implementation inventory](principal-attribution-implementation-inventory.md) (P21.M4.E3.T002)
