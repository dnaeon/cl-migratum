(defpackage :cl-migratum-system
  (:use :cl :asdf))
(in-package :cl-migratum-system)

(defsystem :cl-migratum
  :name "cl-migratum"
  :description "Database migrations for Common Lisp"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :long-description "Database migrations for Common Lisp"
  :homepage "https://github.com/dnaeon/c-migratum"
  :bug-tracker "https://github.com/dnaeon/c-migratum"
  :source-control "https://github.com/dnaeon/c-migratum"
  :long-name "cl-migratum"
  :depends-on (:cl-dbi
               :local-time
               :cl-ppcre
               :cl-ascii-table
               :log4cl)
  :components ((:module "util"
                :pathname #P"src/"
                :components ((:file "util")))
               (:module "core"
                :pathname #P"src/"
                :depends-on ("util")
                :components ((:file "core")))
               (:module "provider"
                :pathname #P"src/provider/"
                :depends-on ("core")
                :components ((:file "local-path")))
               (:module "driver"
                :pathname #P"src/driver/"
                :depends-on ("core")
                :components ((:file "sql")))
               (:module "client-package"
                :pathname #P"src/"
                :depends-on ("core" "provider" "driver")
                :components ((:file "package")))))
