SHELL_SWITCH ?= clawq-5.1
SHELL := opam exec --switch=$(SHELL_SWITCH) -- /usr/bin/env bash
.SHELLFLAGS := -c

.PHONY: bootstrap build restart build-restart build-minimal build-wasm build-opt build-opt-all build-opt-speed build-opt-size build-opt-minimal build-opt-stripped build-opt-stripped-all build-opt-speed-stripped build-opt-size-stripped binary-size-report binary-size-check dependency-audit native-size-report packaging-report flambda-experiment extract extract-check coq-verify coq-check run phase2 benchmark benchmark-quick test test-all fmt fmt-check ui ui-dev ui-check clean release docker-build docker-run verify-report coverage coverage-summary coverage-switch-setup embed-ui update-fv fv-all

LOCAL_DUNE_BUILD_DIRS := _build _build_opt_speed _build_opt_size _build_opt_min

define check_dune_lock
	@./scripts/clean_stale_dune_locks.sh $(LOCAL_DUNE_BUILD_DIRS)
endef

OPT ?= speed
DIST_DIR := dist
CLAWQ_BIN ?= ./_build/default/src/main.exe
SPEED_EXE := _build_opt_speed/default/src/main.exe
SIZE_EXE := _build_opt_size/default/src/main.exe
MIN_EXE := _build/default/src/main_min.exe
BINARY_SIZE_REPORT := $(DIST_DIR)/binary-size-report.tsv
BINARY_SIZE_THRESHOLDS := ci/binary-size-thresholds.tsv
FLAMBDA_BASE_SWITCH ?= clawq-5.1
FLAMBDA_SWITCH ?= clawq-5.1-flambda
FLAMBDA_COMPILER ?= ocaml-variants.5.1.1+options

bootstrap:
	./scripts/bootstrap_coq.sh

build:
	$(call check_dune_lock,_build)
	dune build

restart:
	$(CLAWQ_BIN) service signal-restart

build-restart:
	$(MAKE) build
	$(MAKE) restart

build-minimal:
	$(call check_dune_lock,_build)
	@CLAWQ_BUILD_MINIMAL=true dune build src/main_min.exe
	@exe="$(MIN_EXE)"; \
		size_kb=$$((($$(stat -c%s "$$exe") + 1023) / 1024)); \
		echo "$$exe $$size_kb KB"

build-opt:
	@if [ "$(OPT)" = "speed" ]; then \
		$(MAKE) --no-print-directory build-opt-speed; \
	elif [ "$(OPT)" = "size" ]; then \
		$(MAKE) --no-print-directory build-opt-size; \
	else \
		echo "Unknown OPT='$(OPT)'. Use OPT=speed or OPT=size."; \
		exit 1; \
	fi

build-opt-all: build-opt-speed build-opt-size

build-opt-minimal:
	$(call check_dune_lock,_build_opt_min)
	@DUNE_BUILD_DIR=_build_opt_min CLAWQ_BUILD_MINIMAL=true dune build --profile=release-size src/main_min.exe
	@exe="_build_opt_min/default/src/main_min.exe"; \
		size_kb=$$((($$(stat -c%s "$$exe") + 1023) / 1024)); \
		echo "$$exe $$size_kb KB"

build-opt-stripped:
	@if [ "$(OPT)" = "speed" ]; then \
		$(MAKE) --no-print-directory build-opt-speed-stripped; \
	elif [ "$(OPT)" = "size" ]; then \
		$(MAKE) --no-print-directory build-opt-size-stripped; \
	else \
		echo "Unknown OPT='$(OPT)'. Use OPT=speed or OPT=size."; \
		exit 1; \
	fi

build-opt-stripped-all: build-opt-speed-stripped build-opt-size-stripped

build-opt-speed:
	$(call check_dune_lock,_build_opt_speed)
	@DUNE_BUILD_DIR=_build_opt_speed dune build --profile=release-speed src/main.exe
	@exe="$(SPEED_EXE)"; \
		size_kb=$$((($$(stat -c%s "$$exe") + 1023) / 1024)); \
		echo "$$exe $$size_kb KB"

build-opt-size:
	$(call check_dune_lock,_build_opt_size)
	@DUNE_BUILD_DIR=_build_opt_size dune build --profile=release-size src/main.exe
	@exe="$(SIZE_EXE)"; \
		size_kb=$$((($$(stat -c%s "$$exe") + 1023) / 1024)); \
		echo "$$exe $$size_kb KB"

build-opt-speed-stripped: build-opt-speed
	@mkdir -p "$(DIST_DIR)"
	@out="$(DIST_DIR)/clawq-speed"; \
		cp "$(SPEED_EXE)" "$$out"; \
		chmod u+w "$$out"; \
		strip "$$out"; \
		size_kb=$$((($$(stat -c%s "$$out") + 1023) / 1024)); \
		echo "$$out $$size_kb KB"

build-opt-size-stripped: build-opt-size
	@mkdir -p "$(DIST_DIR)"
	@out="$(DIST_DIR)/clawq-size"; \
		cp "$(SIZE_EXE)" "$$out"; \
		chmod u+w "$$out"; \
		strip "$$out"; \
		size_kb=$$((($$(stat -c%s "$$out") + 1023) / 1024)); \
		echo "$$out $$size_kb KB"

binary-size-report: build-opt-speed build-opt-size
	@mkdir -p "$(DIST_DIR)"
	@./scripts/report_binary_sizes.sh \
		--output "$(BINARY_SIZE_REPORT)" \
		--thresholds "$(BINARY_SIZE_THRESHOLDS)"

binary-size-check: build-opt-speed build-opt-size
	@mkdir -p "$(DIST_DIR)"
	@./scripts/report_binary_sizes.sh \
		--check \
		--output "$(BINARY_SIZE_REPORT)" \
		--thresholds "$(BINARY_SIZE_THRESHOLDS)"

dependency-audit:
	@./scripts/report_dependency_weight.sh

native-size-report: build-opt-size
	@./scripts/report_native_symbols.sh

packaging-report: build-opt-size
	@./scripts/report_packaging_options.sh

flambda-experiment:
	@./scripts/run_flambda_experiment.sh \
		--baseline-switch "$(FLAMBDA_BASE_SWITCH)" \
		--flambda-switch "$(FLAMBDA_SWITCH)" \
		--compiler "$(FLAMBDA_COMPILER)"

extract:
	./scripts/extract.sh

run:
	$(call check_dune_lock,_build)
	dune exec src/main.exe -- help

phase2:
	$(call check_dune_lock,_build)
	dune exec src/main.exe -- phase2

benchmark:
	$(call check_dune_lock,_build)
	dune exec src/main.exe -- benchmark

benchmark-quick:
	$(call check_dune_lock,_build)
	dune exec src/main.exe -- benchmark --iterations 1

extract-check:
	@echo "Checking extraction is up to date..."
	@cp src/extracted/clawq_core.ml /tmp/clawq_core.ml.bak
	@cp src/extracted/clawq_core.mli /tmp/clawq_core.mli.bak
	@./scripts/extract.sh
	@diff -q src/extracted/clawq_core.ml /tmp/clawq_core.ml.bak >/dev/null 2>&1 \
		&& diff -q src/extracted/clawq_core.mli /tmp/clawq_core.mli.bak >/dev/null 2>&1 \
		&& echo "Extraction is up to date." \
		|| (echo "ERROR: Extracted code has drifted. Run 'make extract' and commit."; \
			cp /tmp/clawq_core.ml.bak src/extracted/clawq_core.ml; \
			cp /tmp/clawq_core.mli.bak src/extracted/clawq_core.mli; exit 1)

coq-verify:
	@echo "Verifying all Coq theories and proofs..."
	@./scripts/coq_verify.sh
	@echo "All Coq proofs verified successfully."

coq-check: coq-verify extract-check
	@echo "Coq verification and extraction drift check passed."

build-wasm:
	$(call check_dune_lock,_build)
	dune build src/main_wasm_exe.bc
	@echo "WASM bytecode built: _build/default/src/main_wasm_exe.bc"
	@echo "Run with: ocamlrun _build/default/src/main_wasm_exe.bc help"

test:
	$(call check_dune_lock,_build)
	dune exec test/test_main.exe -- test --quick-tests

test-all:
	$(call check_dune_lock,_build)
	dune build src/main.exe
	dune exec test/test_main.exe -- test

COVERAGE_SWITCH := clawq-coverage
COVERAGE_UNSUPPORTED := bisect_ppx is currently incompatible with this repo's OCaml 5.1/Cmdliner 2 toolchain; coverage targets are disabled until a compatible release is available.

coverage-switch-setup: SHELL := /bin/bash
coverage-switch-setup:
	opam switch create $(COVERAGE_SWITCH) 5.1.0 --no-switch || true
	opam install --switch=$(COVERAGE_SWITCH) . --deps-only --with-test -y
	@printf '%s\n' "$(COVERAGE_UNSUPPORTED)" >&2
	@exit 1

coverage: SHELL := opam exec --switch=$(COVERAGE_SWITCH) -- /usr/bin/env bash
coverage:
	@printf '%s\n' "$(COVERAGE_UNSUPPORTED)" >&2
	@exit 1

coverage-summary: SHELL := opam exec --switch=$(COVERAGE_SWITCH) -- /usr/bin/env bash
coverage-summary:
	@printf '%s\n' "$(COVERAGE_UNSUPPORTED)" >&2
	@exit 1

fmt:
	$(call check_dune_lock,_build)
	dune fmt

fmt-check:
	$(call check_dune_lock,_build)
	@tmp_log="$$(mktemp)"; \
	status=0; \
	timeout 30s dune fmt >"$$tmp_log" 2>&1 || status=$$?; \
	head -20 "$$tmp_log"; \
	rm -f "$$tmp_log"; \
	if [ $$status -eq 124 ]; then \
		echo "fmt-check timed out after 30s" >&2; \
	fi; \
	test $$status -eq 0

ui:
	cd ui && bun run build
	./scripts/gen_chat_ui_assets.sh

ui-dev:
	cd ui && bun run dev

ui-check:
	cd ui && bun run build
	./scripts/gen_chat_ui_assets.sh --check

release:
	$(call check_dune_lock,_build)
	dune build --release

docker-build:
	docker build -t clawq:latest .

docker-run:
	docker run -it --rm -p 13451:13451 --name clawq clawq:latest agent

verify-report:
	@./scripts/formal_verification_report.sh

embed-ui: ui

update-fv:
	bash scripts/update_fv_data.sh

fv-all: coq-check update-fv verify-report
	@echo "=== fv-all complete ==="

clean:
	$(call check_dune_lock,_build)
	dune clean
