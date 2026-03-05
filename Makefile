SHELL := opam exec --switch=clawq-5.1 -- /usr/bin/env bash
.SHELLFLAGS := -c

.PHONY: bootstrap build extract extract-check run phase2 test fmt fmt-check clean release docker-build docker-run

bootstrap:
	./scripts/bootstrap_coq.sh

build:
	dune build

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
