(defpackage :cl-migratum-provider-local-path-system
  (:use :cl :asdf))
(in-package :cl-migratum-provider-local-path-system)

(defsystem "cl-migratum.provider.local-path"
  :name "cl-migratum.provider.local-path"
  :description "cl-migratum provider for migration resources discovered from local path"
  :version "0.3.3"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD-2 Clause"
  :homepage "https://github.com/dnaeon/cl-migratum"
  :bug-tracker "https://github.com/dnaeon/cl-migratum"
  :source-control "https://github.com/dnaeon/cl-migratum"
  :long-name "cl-migratum.provider.local-path"
  :depends-on (:cl-migratum
               :cl-ppcre
               :log4cl)
  :components ((:module "provider"
                :pathname #P"src/provider/"
                :components ((:file "local-path")))))
