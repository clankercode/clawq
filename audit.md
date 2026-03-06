use git tags to track audit points so we can audit changes efficiently.
format: `audit-check-<date>-<time>`
After an audit, tag the most recent commit you audited.
Before an audit, check the git log to find a suitable slice of commits to audit, starting with the most recently audited commit.
You should pick at most 5 commits at a time.

Audit points:

- docs are up to date with code:
  - FV status docs
  - Commands
  - APIs
  - Config
  - Channels
  - Dev guide updated
- proofs and code in sync or proofs extracted; nothing with proofs broken
- functionality that should be tested is tested
- code is well structured and avoids duplication
