# 04 Scoped Memory SPEC

## Responsibilities

- Add scoped memory tables and migration mechanics.
- Preserve existing `core_memories` through an explicit legacy read-in-place scope.
- Add scoped memory APIs with actor/profile/session context.
- Resolve direct, non-transitive memory grants.
- Rewrite prompt/search injection to respect scoped memory.

## Non-Responsibilities

- Tool/codebase grants.
- Budget enforcement.
- Connector history capture.

## Backlog Mapping

- P12.M1.E1.T001: Add scoped memory tables and migration mechanics.
- P12.M1.E1.T002: Add legacy memory read-in-place scope.
- P12.M1.E2.T001: Add scoped memory APIs and grant resolution.
- P12.M1.E3.T001: Scope room search and prompt injection.

## Granularity Note

Granularity audit split scoped memory API/search work:

- `P12.M1.E2` now separates scoped CRUD APIs, grant resolution, and admin-only mutation/tool integration.
- `P12.M1.E3` now separates scoped `Memory.search`, agent/compaction prompt wiring, and global callsite audit/legacy routing tests.
