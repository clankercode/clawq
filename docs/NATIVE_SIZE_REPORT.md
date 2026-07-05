# Native Size Analysis

Regenerate the current native size report with:

```bash
make native-size-report
```

The generated data lands in `dist/native-size-report/`.

## 2026-03-07 Snapshot

This workflow analyzes optimized native object files from `_build_opt_size/default/src`.

This is a historical pre-refactor snapshot. Regenerate before using it as
current sizing evidence, especially for config parser modules now split out of
`config_loader`.

- Module ranking uses `.text + .rodata + .data + .bss + .eh_frame` as a rough footprint signal.
- Symbol ranking uses `nm -S --size-sort --print-size` and filters to concrete text symbols, so the list highlights real code bodies rather than linker bookkeeping.

Largest module-level contributors in this snapshot:

| Module | Group | Footprint | Note |
|---|---|---:|---|
| `command_bridge` | integrations | 91,030 | Biggest integration hotspot; broad command surface makes it a strong refactor target. |
| `daemon` | integrations | 75,703 | Large always-on integration path; restart/lifecycle logic is a good size-reduction candidate. |
| `config_loader` | core | 73,226 | Largest core parser path; config compatibility logic is expensive. |
| `tools_builtin` | core | 69,767 | Tool schema + dispatch code is a notable core contributor. |
| `runtime_config` | core | 52,745 | Serialization/deserialization is a major recurring source of size. |
| `email_channel` | integrations | 46,150 | Channel-specific parser/decoder path is large relative to usage frequency. |
| `http_server` | integrations | 36,780 | Server path is a meaningful integration-only contributor and should stay out of minimal/core packaging. |

Largest concrete code symbols from this snapshot:

| Symbol | Module | Size |
|---|---|---:|
| `camlConfig_loader.parse_config_inner_1563` | `config_loader` | 19,973 |
| `camlRuntime_config.to_json_845` | `runtime_config` | 15,861 |
| `camlConfig_wizard_update.update_640` | `config_wizard_update` | 13,190 |
| `camlDaemon.run_1531` | `daemon` | 3,974 |
| `camlHttp_server.handler_987` | `http_server` | 3,789 |
| `camlGithub_webhook.parse_event_541` | `github_webhook` | 2,987 |
| `camlCommand_bridge.cmd_auth_1543` | `command_bridge` | 2,832 |

## Prioritization

- First, trim integration-only hotspots such as `command_bridge`, `daemon`, `http_server`, and channel/provider modules before touching foundational core plumbing.
- In core, config parsing/serialization was the clearest code-size cluster in this snapshot, especially `config_loader` and `runtime_config`.
- `chat_ui_assets` is mostly static data rather than executable code, so it is a packaging/asset-embedding question more than a codegen problem.
