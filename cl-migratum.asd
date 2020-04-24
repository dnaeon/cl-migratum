(defpackage :cl-migratum-system
  (:use :cl :asdf))
(in-package :cl-migratum-system)

(defsystem "cl-migratum"
  :name "cl-migratum"
  :description "Database schema migration system for Common Lisp"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :long-description #.(uiop:read-file-string
		       (uiop:subpathname *load-pathname* "README.md"))
  :homepage "https://github.com/dnaeon/cl-migratum"
  :bug-tracker "https://github.com/dnaeon/cl-migratum"
  :source-control "https://github.com/dnaeon/cl-migratum"
  :long-name "cl-migratum"
  :depends-on (:local-time
	       :cl-ascii-table
               :log4cl)
  :components ((:module "util"
                :pathname #P"src/"
                :components ((:file "util")))
               (:module "core"
                :pathname #P"src/"
                :depends-on ("util")
                :components ((:file "core")))
               (:module "client-package"
                :pathname #P"src/"
                :depends-on ("core" "util")
                :components ((:file "package")))))
