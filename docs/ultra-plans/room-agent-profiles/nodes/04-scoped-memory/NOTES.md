# Notes

- Review specifically warned about schema migration mechanics: version bump, `migrate_step`, `repair_missing_columns`, and `ensure_all_tables`.
- Consider splitting `P12.M1.E3.T001` into search filter, agent prompt injection, and compaction tasks if implementation starts to sprawl.
