# Group G+I Deferrals

Deferred items from the code duplication and miscellaneous fix pass (2026-06-26).
See `/tmp/clawq-review/master-list.md` for full details.

## Deferred Items

| Item | Category | Reason |
|------|----------|--------|
| G4 | File size | Structural; splitting 2000+ LoC files risks merge conflicts |
| G5 | Slash command dispatch | Major refactor across 4 connector modules |
| G6 | Wizard loop duplication | Low impact, refactor risk |
| I1 | Lru_dedup ordering | Cosmetic; both orderings produce correct final state |
| I5 | Blocking Sys.command | Needs Lwt_process conversion + testing |
| I6 | model_preferences caching | Needs cache invalidation strategy |
| I7 | SSE placeholder | Needs real event forwarding implementation |
| I8 | agent_router naming | Cosmetic naming issue |

## I4 Architectural Note

Full Markdown character escaping for Discord/Slack/Teams is not feasible with
the current `Format_adapter.escape` API. The function is called BEFORE formatting
wrappers (bold, code, italic) are applied by `Content_dsl`. Escaping `*`, `_`,
`` ` `` etc. at the text level would prevent the formatting from taking effect.

A full fix requires refactoring to escape-after-format (post-processing the
final rendered string), which is a larger architectural change.
