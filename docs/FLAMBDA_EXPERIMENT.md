# Flambda Experiment

Run the experiment with:

```bash
make flambda-experiment
```

This target:

- reuses the baseline switch from `FLAMBDA_BASE_SWITCH` (default `clawq-5.1`)
- creates `FLAMBDA_SWITCH` (default `clawq-5.1-flambda`) with `ocaml-option-flambda` if needed
- builds speed and size optimized binaries in both switches
- captures stripped and unstripped sizes
- measures startup time via `main.exe help`
- measures a runtime proxy via `main.exe status`

Generated artifacts land in `dist/flambda-experiment/`:

- `results.tsv`
- `report.md`
- copied baseline/flambda binaries for each profile

The current report is intended to be regenerated rather than hand-edited.

## 2026-03-07 Snapshot

Measured with:

```bash
./scripts/run_flambda_experiment.sh --runs 1
```

| Profile | Switch | Speed Unstripped | Speed Stripped | Size Unstripped | Size Stripped | `--help` | `status` |
|---|---|---:|---:|---:|---:|---:|---:|
| Baseline | `clawq-5.1` | 22,890,064 | 13,037,376 | 22,827,200 | 12,980,032 | 0.053684s | 0.016762s |
| Flambda | `clawq-5.1-flambda` | 35,988,728 | 22,734,176 | 30,990,920 | 18,519,584 | 0.068614s | 0.019309s |

Observed deltas versus baseline:

- speed unstripped: `+57.22%`
- speed stripped: `+74.38%`
- size unstripped: `+35.76%`
- size stripped: `+42.68%`
- startup proxy (`--help`): `+27.81%`
- runtime proxy (`status`): `+15.20%`

This experiment does not justify enabling flambda by default for the current clawq binary; in this configuration it is materially larger and slightly slower on both CLI timing proxies.
