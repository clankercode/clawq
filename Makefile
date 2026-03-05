SHELL := opam exec --switch=clawq-5.1 -- /usr/bin/env bash
.SHELLFLAGS := -c

.PHONY: bootstrap build build-minimal build-opt build-opt-all build-opt-speed build-opt-size build-opt-minimal build-opt-stripped build-opt-stripped-all build-opt-speed-stripped build-opt-size-stripped extract extract-check run phase2 test fmt fmt-check clean release docker-build docker-run

OPT ?= speed
DIST_DIR := dist
SPEED_EXE := _build_opt_speed/default/src/main.exe
SIZE_EXE := _build_opt_size/default/src/main.exe
MIN_EXE := _build/default/src/main_min.exe

bootstrap:
	./scripts/bootstrap_coq.sh

build:
	dune build

build-minimal:
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
	@DUNE_BUILD_DIR=_build_opt_speed dune build --profile=release-speed src/main.exe
	@exe="$(SPEED_EXE)"; \
		size_kb=$$((($$(stat -c%s "$$exe") + 1023) / 1024)); \
		echo "$$exe $$size_kb KB"

build-opt-size:
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

extract:
	./scripts/extract.sh

run:
	dune exec clawq -- help

phase2:
	dune exec clawq -- phase2

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

test:
	dune runtest

fmt:
	dune fmt

fmt-check:
	dune fmt 2>&1 | head -20; test $${PIPESTATUS[0]} -eq 0

release:
	dune build --release

docker-build:
	docker build -t clawq:latest .

docker-run:
	docker run -it --rm -p 3000:3000 --name clawq clawq:latest agent

clean:
	dune clean
