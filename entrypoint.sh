#!/usr/bin/env sh

set -e

sbcl --eval '(ql:quickload :cl-migratum.test)' \
     --eval '(setf rove:*enable-colors* nil)' \
     --eval '(asdf:test-system :cl-migratum.test)' \
     --eval '(quit)'
