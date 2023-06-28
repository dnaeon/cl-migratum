LISP ?= sbcl

test:
	./scripts/run-tests.sh

cli:
	${LISP} --eval '(ql:quickload :cl-migratum.cli)' \
                --eval '(asdf:make :cl-migratum.cli)' \
                --eval '(quit)'

cli-doc: cli
	bin/migratum print-doc > docs/migratum-cli.md

test-postgres:
	docker build -t dnaeon/postgres:latest -f postgres/Dockerfile postgres/

.PHONY: test cli cli-doc
