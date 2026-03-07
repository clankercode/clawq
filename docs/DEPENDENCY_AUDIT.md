# Dependency Audit

Regenerate the current audit with:

```bash
make dependency-audit
```

The generated report lands in `dist/dependency-audit/`.

## 2026-03-07 Snapshot

This audit uses two rough signals for each direct dependency in `clawq.opam`:

- `closure_packages`: unique packages visible from `opam tree <dep>`
- `closure_lib_kb`: sum of installed library directories for that closure

Because shared packages are counted once per direct dependency, the size totals are intentionally approximate. They are useful for ranking, not for exact accounting.

Top weight signals from the current report:

| Package | Closure Packages | Closure Lib KB | Takeaway |
|---|---:|---:|---|
| `tls-lwt` | 22 | 178,708 | Largest integration-side stack; keep isolated from core/minimal builds. |
| `ca-certs` | 21 | 164,776 | Pulls a broad certificate and crypto closure; keep integration-only. |
| `cohttp-lwt-unix` | 15 | 156,248 | High-cost HTTP client/server family; avoid pulling it into new core-only features. |
| `conduit-lwt-unix` | 12 | 158,588 | Another large network transport layer; good candidate for optional packaging boundaries. |
| `sqlite3` | 6 | 132,388 | Material but much smaller closure; only worth isolating if minimal mode can avoid persistent storage entirely. |
| `coq` | 5 | 292,988 | Biggest absolute toolchain footprint, but build-only rather than runtime. |

## Recommendations

- Keep the existing `clawq_runtime_core` vs `clawq_runtime_integrations` split strict; the network/TLS stack is the clearest high-cost family to isolate.
- Treat `tls-lwt`, `ca-certs`, `cohttp-lwt-unix`, and `conduit-lwt-unix` as the first optional-packaging boundary if we later ship slimmer non-network distributions.
- Keep Coq and proof/extraction tooling out of runtime-oriented packaging discussions; it matters for dev/CI footprint, not shipped binaries.
- Do not spend time replacing `cmdliner`, `yojson`, `lwt`, `logs`, or `fmt` for size reasons first; they look foundational and lower-ROI than tightening integration boundaries.
- Only consider making `sqlite3` optional if we are willing to support a reduced persistence story for minimal deployments.
