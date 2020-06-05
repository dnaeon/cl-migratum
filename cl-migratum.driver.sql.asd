(defpackage :cl-migratum-driver-sql-system
  (:use :cl :asdf))
(in-package :cl-migratum-driver-sql-system)

(defsystem "cl-migratum.driver.sql"
  :name "cl-migratum.driver.sql"
  :description "cl-migratum driver for driving migrations against SQL databases using CL-DBI"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD-2 Clause"
  :homepage "https://github.com/dnaeon/cl-migratum"
  :bug-tracker "https://github.com/dnaeon/cl-migratum"
  :source-control "https://github.com/dnaeon/cl-migratum"
  :long-name "cl-migratum.driver.sql"
  :depends-on (:cl-migratum
               :cl-dbi
               :cl-ppcre
               :log4cl)
  :components ((:module "driver"
                :pathname #P"src/driver/"
                :components ((:file "sql")))))
