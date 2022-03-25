LISP ?= sbcl

test:
	./entrypoint.sh

cli:
	${LISP} --eval '(ql:quickload :cl-migratum.cli)' \
                --eval '(asdf:make :cl-migratum.cli)' \
                --eval '(quit)'

.PHONY: test cli

