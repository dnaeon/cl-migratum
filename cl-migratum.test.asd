(defpackage :cl-migratum-test-system
  (:use :cl :asdf))
(in-package :cl-migratum-test-system)

(defsystem "cl-migratum.test"
  :name "cl-migratum.test"
  :description "Test suite for cl-migratum"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :homepage "https://github.com/dnaeon/cl-migratum"
  :bug-tracker "https://github.com/dnaeon/cl-migratum"
  :source-control "https://github.com/dnaeon/cl-migratum"
  :long-name "cl-migratum.test"
  :depends-on (:cl-migratum
               :cl-migratum.provider.local-path
               :cl-migratum.driver.sql
               :dbd-sqlite3
               :tmpdir
               :rove)
  :components ((:module "test-migrations"
                :pathname #P"t/migrations/"
                :components ((:static-file "20200421173657-create-table-foo.down.sql")
                             (:static-file "20200421173657-create-table-foo.up.sql")
                             (:static-file "20200421173908-create-table-bar.down.sql")
                             (:static-file "20200421173908-create-table-bar.up.sql")
                             (:static-file "20200421180337-create-table-qux.down.sql")
                             (:static-file "20200421180337-create-table-qux.up.sql")))
               (:module "tests"
                :pathname #P"t/"
                :depends-on ("test-migrations")
                :components ((:file "test-suite"))))
  :perform (test-op (op c) (uiop:symbol-call :rove :run-suite :cl-migratum.test)))
