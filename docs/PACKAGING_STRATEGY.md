# Packaging Strategy

Regenerate the current packaging comparison with:

```bash
make packaging-report
```

The generated report lands in `dist/packaging-report/`.

## 2026-03-07 Snapshot

Current size-build packaging measurements:

| Variant | Size Bytes | `--help` | `status` |
|---|---:|---:|---:|
| unstripped | 22,827,200 | 0.028002s | 0.010530s |
| stripped | 12,980,032 | 0.027198s | 0.009445s |
| stripped + UPX | not measured | not measured | not measured |

Observed takeaway:

- Stripping cuts the current size build by about `43.14%` with no startup penalty in this local measurement.
- `UPX` was not installed on this machine, so there is no current data to justify making it part of the default release path.

## Release Guidance

- Ship the stripped size build by default.
- Keep UPX optional and off by default unless a maintainer explicitly installs it and accepts the measured startup/runtime tradeoff for that environment.
- When UPX is available, rerun `make packaging-report` before using it in release automation.
