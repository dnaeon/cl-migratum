(in-package :cl-user)
(defpackage :cl-migratum.driver.sql
  (:use :cl)
  (:nicknames :migratum.driver.sql)
  (:import-from
   :cl-migratum.core
   :migration
   :base-driver
   :driver-name
   :driver-init
   :list-applied
   :list-pending
   :register-migration)
  (:import-from :log4cl)
  (:import-from :cl-dbi)
  (:export
   :sql-driver
   :driver-connection))
(in-package :cl-migratum.driver.sql)

(defparameter *sql-init-schema*
  "
CREATE TABLE IF NOT EXISTS migration (
    id INTEGER PRIMARY KEY,
    description CHARACTER VARYING(255) NOT NULL,
    applied TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);"
  "Schema used by the SQL driver")

(defclass sql-driver (base-driver)
  ((connection
    :initarg :connection
    :accessor driver-connection
    :initform (error "Must specify database connection")))
  (:documentation "Driver for performing SQL migrations"))

(defmethod driver-init ((driver sql-driver) &key)
  (log:info "Initializing ~a driver" (driver-name driver))
  (let* ((connection (driver-connection driver))
         (query (cl-dbi:prepare connection *sql-init-schema*)))
    (cl-dbi:execute query)))
