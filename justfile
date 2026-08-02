# Build release-speed, extract debug symbols, strip binary, ship both
bi:
    ./scripts/build-install.sh

# Build the project (dev)
build:
    make build

# Quick test (skips Slow-tagged tests)
test:
    make test

# Run all tests
test-all:
    make test-all

# Format code
fmt:
    make fmt

# Check formatting
fmt-check:
    make fmt-check

# Run CLI help
run:
    make run

# ── Build-box (remote SSH) support ─────────────────────────────────────
# Configure by copying .env.example to .env and setting:
#   CLAWQ_BUILD_BOX_HOST=x-alpha (or your SSH host alias)
#   CLAWQ_BUILD_BOX_REMOTE_DIR=~/src/clawq (remote source path)
# When CLAWQ_BUILD_BOX_HOST is set, build/test commands rsync to the
# remote box and execute there. Otherwise, everything runs locally.

# Load build-box config from .env if present
export CLAWQ_BUILD_BOX_HOST := `echo $$CLAWQ_BUILD_BOX_HOST`
export CLAWQ_BUILD_BOX_REMOTE_DIR := `echo $$CLAWQ_BUILD_BOX_REMOTE_DIR`

# Internal: rsync local source to build box
_build-box-sync:
    @if [ -n "$CLAWQ_BUILD_BOX_HOST" ]; then \
        echo "Syncing to $CLAWQ_BUILD_BOX_HOST:$${CLAWQ_BUILD_BOX_REMOTE_DIR:-~/src/clawq}..."; \
        rsync -az --delete \
            --exclude '_build/' --exclude '.git/' --exclude '.worktrees/' \
            --exclude '_build_opt_*/' --exclude 'node_modules/' \
            ./ "$CLAWQ_BUILD_BOX_HOST:$${CLAWQ_BUILD_BOX_REMOTE_DIR:-~/src/clawq}/"; \
    else \
        echo "CLAWQ_BUILD_BOX_HOST not set, skipping sync."; \
    fi

# Build on remote box (or locally if not configured)
build-remote: _build-box-sync
    @if [ -n "$CLAWQ_BUILD_BOX_HOST" ]; then \
        echo "Building on $CLAWQ_BUILD_BOX_HOST..."; \
        ssh "$CLAWQ_BUILD_BOX_HOST" \
            "cd $${CLAWQ_BUILD_BOX_REMOTE_DIR:-~/src/clawq} && make build"; \
    else \
        echo "CLAWQ_BUILD_BOX_HOST not set, building locally."; \
        make build; \
    fi

# Test on remote box (or locally if not configured)
test-remote: _build-box-sync
    @if [ -n "$CLAWQ_BUILD_BOX_HOST" ]; then \
        echo "Testing on $CLAWQ_BUILD_BOX_HOST..."; \
        ssh "$CLAWQ_BUILD_BOX_HOST" \
            "cd $${CLAWQ_BUILD_BOX_REMOTE_DIR:-~/src/clawq} && make test"; \
    else \
        echo "CLAWQ_BUILD_BOX_HOST not set, testing locally."; \
        make test; \
    fi

# Build optimized release on remote box
build-opt-remote: _build-box-sync
    @if [ -n "$CLAWQ_BUILD_BOX_HOST" ]; then \
        echo "Building optimized on $CLAWQ_BUILD_BOX_HOST..."; \
        ssh "$CLAWQ_BUILD_BOX_HOST" \
            "cd $${CLAWQ_BUILD_BOX_REMOTE_DIR:-~/src/clawq} && make build-opt-speed-stripped"; \
        echo "Syncing artifacts back..."; \
        rsync -az \
            "$CLAWQ_BUILD_BOX_HOST:$${CLAWQ_BUILD_BOX_REMOTE_DIR:-~/src/clawq}/_build_opt_speed/src/main_stripped.exe" \
            ./_build_opt_speed/src/main_stripped.exe; \
    else \
        echo "CLAWQ_BUILD_BOX_HOST not set, building locally."; \
        make build-opt-speed-stripped; \
    fi
