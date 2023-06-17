(defpackage :cl-migratum-driver-postmodern-postgresql-system
  (:use :cl :asdf))
(in-package :cl-migratum-driver-postmodern-postgresql-system)

(defsystem "cl-migratum.driver.postmodern-postgresql"
  :name "cl-migratum.driver.postmodern-postgresql"
  :description "cl-migratum driver for driving migrations against SQL databases using Postmodern."
  :version "0.1.3"
  :author ("Marin Atanasov  <dnaeon@gmail.com>"
           "Kambiz Darabi <darabi@m-creations.net>")
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD-2 Clause"
  :homepage "https://github.com/dnaeon/cl-migratum"
  :bug-tracker "https://github.com/dnaeon/cl-migratum"
  :source-control "https://github.com/dnaeon/cl-migratum"
  :long-name "cl-migratum.driver.postmodern-postgresql"
  :depends-on (:cl-migratum
               :cl-migratum.driver.mixins
               :str
               :hu.dwim.logger
               :postmodern
               :log4cl)
  :components ((:module "driver"
                :pathname #P"src/driver/"
                :components ((:file "postmodern-postgresql")))))
