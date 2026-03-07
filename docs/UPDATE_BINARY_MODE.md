# `/update` Binary Mode

`update_clawq` now supports three modes:

- `auto`: use git rebuild when a repo is available, otherwise fall back to binary mode
- `git`: require a repo checkout and run `git pull` + `make build`
- `binary`: require `CLAWQ_UPDATE_BINARY_URL` and replace the current executable directly

Binary mode uses this environment variable:

```bash
CLAWQ_UPDATE_BINARY_URL=https://example.invalid/path/to/clawq
```

In binary mode, clawq:

1. downloads to `<current-executable>.download`
2. runs `chmod 755` on the downloaded file
3. moves it over the current executable
4. sends `SIGUSR1` to trigger the normal graceful restart path

Current guidance:

- Use `auto` as the default tool behavior.
- Use `git` mode for developer checkouts.
- Use `binary` mode for packaged installs only when a trusted binary URL is configured.
