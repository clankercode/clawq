# 7. Use one feature catalog with multiple renderers

Date: 2026-07-13
Status: Accepted

## Context

Feature onboarding is spread across slash-command help, agent help, public docs,
and machine-readable references. Copying prose between `/new-features` and
`clawq_help` would drift, while forcing identical output would make one surface
too verbose or the other too shallow.

## Decision

Maintain one versioned feature catalog containing stable batch/feature IDs,
short and expansive material, role-aware getting-started actions, documentation
links, availability, connectors, and source backlog tasks. Generate checked-in
runtime data from the docs-owned source.

The catalog has a top-level `schema_version`. Each batch has `released_at` and
`released_in` fields, and each feature has an explicit order within its batch.
Batches sort by release date and then stable batch ID; features sort by explicit
order and then stable feature ID. This ordering is shared by every renderer.

Feature availability is `planned`, `preview`, `available`, `deprecated`, or
`removed`. Default new-feature discovery promotes only `available` entries.
Preview entries require an explicit preview query and suitable audience;
deprecated entries remain queryable with migration guidance but are not promoted;
removed entries remain deterministic tombstones for historical links and are
never presented as usable. Every preview, available, or deprecated entry must
have a non-empty `source_tasks` list whose references resolve to existing leaf
backlog tasks with status `done`; unknown, parent, cancelled, rejected, or
unfinished references fail validation.

Catalog audience is one of `public`, `authenticated-user`, `room-admin`, or
`operator`. A verified authenticated user sees public and authenticated-user
entries, a verified Room admin also sees room-admin entries, and a verified local
operator may see every audience. Guests, unknown actors, and callers without
verified current-role evidence see public entries only. The catalog never
contains credentials or secret values. `/new-features` and `clawq_help` use the
same visibility decision for the verified current caller; when the tool path
cannot carry that evidence, it must render public entries only rather than infer
a role from a Room, Session, display name, or prior turn.

`/new-features` renders a bounded short summary plus the first setup or docs
action. `clawq_help` renders expanded prerequisites, setup, troubleshooting,
commands/tools, and references. Both use the same ordered visible IDs and links.

Documentation audits consume repository-owned machine-readable inputs. The
GitHub surface coverage manifest maps stable surface IDs and supported aliases to
authoritative code locations, public documentation anchors, machine-reference
anchors, and explicit exclusions. The Claude Tag source manifest stores the
normalized FT definitions with provenance and a content hash; its crosswalk maps
each FT entry to status, code/test/documentation evidence, source tasks, and an
owner or follow-up for non-complete entries. Machine-local downloads and old
prose are not audit inputs.

Audit commands have separate modes. Check mode is deterministic and read-only:
it validates committed inputs and never embeds the current time. An explicit
update mode may refresh evidence and produce a dated receipt; that receipt is not
treated as generated drift during ordinary checks.

The final `docs-check` gate checks catalog and generated-runtime drift, GitHub
surface coverage, Claude Tag parity evidence, `llms.txt` structure and links,
generated outputs, human navigation and orphan pages, and the Astro build. The
same gate runs in change validation and before a documentation deployment is
uploaded.

## Consequences

- The renderers can differ in prose length without semantic drift.
- Unknown roles and unavailable caller evidence fail closed without making
  public discovery unusable.
- Docs/build checks can reject duplicate IDs, invalid versions, missing links,
  stale generated output, incomplete promoted entries, or evidence drift.
- Historical Claude Tag-parity and current GitHub batches are queryable through
  one interface.
- Role-aware guidance can say “ask an admin” without exposing operator-only
  setup material.
- Audit inputs and update receipts remain reproducible in CI and on deployment.
