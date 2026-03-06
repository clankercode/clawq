# Optimization Maintenance

This document captures the optimization and binary-size maintenance rules.

## Build Modes

- Speed profile: `release-speed` (`-O3`)
- Size profile: `release-size` (`-O2 -compact`)
- Profiles are defined in `dune-workspace`.

## Runtime Split

- `clawq_runtime_core`: core CLI/runtime logic.
- `clawq_runtime_integrations`: network/server integrations.
- Full binary (`clawq`) links both libraries.
- Minimal binary (`clawq-min`) links core only.

## Make Targets

- Full optimized builds:
  - `make build-opt-speed`
  - `make build-opt-size`
- Stripped full builds:
  - `make build-opt-speed-stripped`
  - `make build-opt-size-stripped`
- Minimal builds:
  - `make build-minimal`
  - `make build-opt-minimal`

## Output Contract

Optimization targets must end with one line in this format:

`<relative/path/to/exe> <size_kb> KB`

Example:

`_build_opt_size/default/src/main.exe 19434 KB`

## Guardrails

1. Keep integration features out of `clawq_runtime_core` unless required.
2. Put new network/server features in `clawq_runtime_integrations`.
3. Keep integration-only commands disabled in `src/command_bridge_min.ml`.
4. Evaluate every new dependency for core vs integration placement.
5. Preserve make target behavior and final size-line output.

## Validation

After runtime/dependency changes run:

1. `make test`
2. `make build-opt-size-stripped`
3. `make build-opt-minimal`

Track KB output over time to catch regressions.
