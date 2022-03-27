(defpackage :cl-migratum-driver-mixins-system
  (:use :cl :asdf))
(in-package :cl-migratum-driver-mixins-system)

(defsystem "cl-migratum.driver.mixins"
  :name "cl-migratum.driver.mixins"
  :description "Various mixins for cl-migratum drivers"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD-2 Clause"
  :homepage "https://github.com/dnaeon/cl-migratum"
  :bug-tracker "https://github.com/dnaeon/cl-migratum"
  :source-control "https://github.com/dnaeon/cl-migratum"
  :long-name "cl-migratum.driver.mixins"
  :depends-on (:cl-migratum)
  :components ((:module "mixins"
                :pathname #P"src/driver/mixins/"
                :components ((:file "mixins")))))
