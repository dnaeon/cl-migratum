(defpackage :cl-migratum-driver-dbi-system
  (:use :cl :asdf))
(in-package :cl-migratum-driver-dbi-system)

(defsystem "cl-migratum.driver.dbi"
  :name "cl-migratum.driver.dbi"
  :description "cl-migratum driver for driving migrations against SQL databases using CL-DBI"
  :version "0.3.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD-2 Clause"
  :homepage "https://github.com/dnaeon/cl-migratum"
  :bug-tracker "https://github.com/dnaeon/cl-migratum"
  :source-control "https://github.com/dnaeon/cl-migratum"
  :long-name "cl-migratum.driver.dbi"
  :depends-on (:cl-migratum
               :cl-dbi
               :cl-ppcre
               :log4cl)
  :components ((:module "driver"
                :pathname #P"src/driver/"
                :components ((:file "dbi")))))
