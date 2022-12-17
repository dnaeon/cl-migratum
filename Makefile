LISP ?= sbcl

test:
	./scripts/run-tests.sh

cli:
	${LISP} --eval '(ql:quickload :cl-migratum.cli)' \
                --eval '(asdf:make :cl-migratum.cli)' \
                --eval '(quit)'

cli-doc: cli
	bin/migratum print-doc > docs/migratum-cli.md

.PHONY: test cli cli-doc
