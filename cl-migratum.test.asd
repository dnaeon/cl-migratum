(defpackage :cl-migratum-test-system
  (:use :cl :asdf))
(in-package :cl-migratum-test-system)

(defsystem "cl-migratum.test"
  :name "cl-migratum.test"
  :description "Test suite for cl-migratum"
  :version "0.3.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :homepage "https://github.com/dnaeon/cl-migratum"
  :bug-tracker "https://github.com/dnaeon/cl-migratum"
  :source-control "https://github.com/dnaeon/cl-migratum"
  :long-name "cl-migratum.test"
  :depends-on (:cl-migratum
               :cl-migratum.provider.local-path
               :cl-migratum.driver.dbi
               :dbd-sqlite3
               :tmpdir
               :rove)
  :components ((:module "test-migrations"
                :pathname #P"t/migrations/"
                :components ((:static-file "20200421173657-create_table_foo.down.sql")
                             (:static-file "20200421173657-create_table_foo.up.sql")
                             (:static-file "20200421173908-create_table_bar.down.sql")
                             (:static-file "20200421173908-create_table_bar.up.sql")
                             (:static-file "20200421180337-create_table_qux.down.sql")
                             (:static-file "20200421180337-create_table_qux.up.sql")
                             (:static-file "20200605144633-multiple_statements.down.sql")
                             (:static-file "20200605144633-multiple_statements.up.sql")))
               (:module "tests"
                :pathname #P"t/"
                :depends-on ("test-migrations")
                :components ((:file "test-suite"))))
  :perform (test-op (op c) (uiop:symbol-call :rove :run-suite :cl-migratum.test)))
