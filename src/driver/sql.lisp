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
   :apply-up-migration
   :apply-down-migration
   :list-applied
   :list-pending
   :register-migration
   :unregister-migration
   :load-migration-up-script
   :load-migration-down-script)
  (:import-from :log)
  (:import-from :cl-dbi)
  (:export
   :sql-driver
   :sql-driver-connection
   :make-sql-driver
   :sql-driver-apply-migration))
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
    :accessor sql-driver-connection
    :initform (error "Must specify database connection")))
  (:documentation "Driver for performing SQL migrations"))

(defmethod driver-init ((driver sql-driver) &key)
  (log:debug "Initializing ~a driver" (driver-name driver))
  (let* ((connection (sql-driver-connection driver))
         (query (cl-dbi:prepare connection *sql-init-schema*)))
    (cl-dbi:with-transaction connection
      (cl-dbi:execute query))))

(defmethod list-applied ((driver sql-driver) &key)
  (log:debug "Fetching list of applied migrations")
  (let* ((connection (sql-driver-connection driver))
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
  (let* ((connection (sql-driver-connection driver))
         (id (migration-id migration))
         (description (migration-description migration))
         (query (cl-dbi:prepare connection "INSERT INTO migration (id, description) VALUES (?, ?)")))
    (cl-dbi:with-transaction connection
      (cl-dbi:execute query (list id description)))))

(defmethod unregister-migration ((driver sql-driver) (migration migration) &key)
  (log:debug "Unregistering migration: ~a" (migration-id migration))
  (let* ((connection (sql-driver-connection driver))
	 (id (migration-id migration))
	 (query (cl-dbi:prepare connection "DELETE FROM migration WHERE id = ?")))
    (cl-dbi:with-transaction connection
      (cl-dbi:execute query (list id)))))

(defmethod apply-up-migration ((driver sql-driver) (migration migration) &key)
  (log:debug "Applying upgrade migration: ~a - ~a" (migration-id migration) (migration-description migration))
  (sql-driver-apply-migration driver migration #'load-migration-up-script))

(defmethod apply-down-migration ((driver sql-driver) (migration migration) &key)
  (log:debug "Applying downgrade migration: ~a - ~a" (migration-id migration) (migration-description migration))
  (sql-driver-apply-migration driver migration #'load-migration-down-script))

(defun make-sql-driver (provider connection)
  "Creates a driver for performing migrations against a SQL database"
  (make-instance 'sql-driver
                 :name "SQL"
                 :provider provider
                 :connection connection))

(defun sql-driver-apply-migration (driver migration migration-script-loader-fun)
  "Applies the script loaded using the migration script loader function"
  (let* ((connection (sql-driver-connection driver))
	 (content (funcall migration-script-loader-fun migration))
	 (query (cl-dbi:prepare connection (string-trim #(#\Newline) content))))
    (cl-dbi:with-transaction connection
      (cl-dbi:execute query))))
