(in-package :cl-user)
(defpackage :cl-migratum.driver.sql
  (:use :cl)
  (:nicknames :migratum.driver.sql)
  (:import-from
   :cl-migratum.core
   :migration
   :migration-id
   :migration-description
   :base-driver
   :driver-name
   :driver-init
   :driver-provider
   :apply-migration
   :list-applied
   :list-pending
   :register-migration
   :load-migration)
  (:import-from :log)
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
    (cl-dbi:with-transaction connection
      (cl-dbi:execute query))))

(defmethod list-applied ((driver sql-driver) &key)
  (log:debug "Fetching list of applied migrations")
  (let* ((connection (driver-connection driver))
         (query (cl-dbi:prepare connection "SELECT * FROM migration ORDER BY id DESC"))
         (result (cl-dbi:execute query))
         (rows (cl-dbi:fetch-all result)))
    (mapcar (lambda (row)
              (make-instance 'migration
                             :id (getf row :|id|)
                             :description (getf row :|description|)
                             :applied (getf row :|applied|)))
            rows)))

(defmethod register-migration ((driver sql-driver) (migration migration) &key)
  (log:debug "Registering migration as successful: ~a" (migration-id migration))
  (let* ((connection (driver-connection driver))
	 (id (migration-id migration))
	 (description (migration-description migration))
	 (query (cl-dbi:prepare connection "INSERT INTO migration (id, description) VALUES (?, ?)")))
    (cl-dbi:with-transaction connection
      (cl-dbi:execute query (list id description)))))

(defmethod apply-migration ((driver sql-driver) (migration migration) &key)
  (log:info "Applying migration: ~a - ~a" (migration-id migration) (migration-description migration))
  (let* ((connection (driver-connection driver))
	 (content (load-migration migration))
	 (query (cl-dbi:prepare connection (string-trim #(#\Newline) content))))
    (cl-dbi:with-transaction connection
      (cl-dbi:execute query))))
