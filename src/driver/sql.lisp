(in-package :cl-user)
(defpackage :cl-migratum.driver.sql
  (:use :cl)
  (:nicknames :migratum.driver.sql)
  (:import-from
   :cl-migratum
   :base-migration
   :migration-id
   :migration-description
   :migration-load-up-script
   :migration-load-down-script
   :base-driver
   :driver-name
   :driver-provider
   :driver-init
   :driver-initialized
   :driver-list-applied
   :driver-apply-up-migration
   :driver-apply-down-migration
   :driver-register-migration
   :driver-unregister-migration)
  (:import-from :log)
  (:import-from :cl-dbi)
  (:import-from :cl-ppcre)
  (:export
   :sql-driver
   :sql-driver-connection
   :make-sql-driver))
(in-package :cl-migratum.driver.sql)

(defparameter *sql-statement-separator*
  "--;;"
  "Separator to use when splitting a migration into multiple statements")

(defparameter *sql-init-schema*
  "
CREATE TABLE IF NOT EXISTS migration (
    id BIGINT PRIMARY KEY,
    description CHARACTER VARYING(255) NOT NULL,
    applied TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);"
  "Schema used by the SQL driver")

(defclass sql-driver (base-driver)
  ((connection
    :initarg :connection
    :accessor sql-driver-connection
    :initform (error "Must specify database connection")
    :documentation "CL-DBI connection to use"))
  (:documentation "Driver for performing SQL migrations"))

(defmethod driver-init ((driver sql-driver) &key)
  (log:debug "Initializing ~a driver" (driver-name driver))
  (let* ((connection (sql-driver-connection driver))
         (query (cl-dbi:prepare connection *sql-init-schema*)))
    (cl-dbi:with-transaction connection
      (cl-dbi:execute query))
    (setf (driver-initialized driver) t)))

(defmethod driver-list-applied ((driver sql-driver) &key (offset 0) (limit 100))
  (log:debug "Fetching list of applied migrations")
  (let* ((connection (sql-driver-connection driver))
         (query (cl-dbi:prepare connection "SELECT * FROM migration ORDER BY id DESC LIMIT ? OFFSET ?"))
         (result (cl-dbi:execute query (list limit offset)))
         (rows (cl-dbi:fetch-all result)))
    (mapcar (lambda (row)
              (make-instance 'base-migration
                             :id (getf row :|id|)
                             :description (getf row :|description|)
                             :applied (getf row :|applied|)))
            rows)))

(defmethod driver-register-migration ((driver sql-driver) (migration base-migration) &key)
  (log:debug "Registering migration as successful: ~a" (migration-id migration))
  (let* ((connection (sql-driver-connection driver))
         (id (migration-id migration))
         (description (migration-description migration))
         (query (cl-dbi:prepare connection "INSERT INTO migration (id, description) VALUES (?, ?)")))
    (cl-dbi:with-transaction connection
      (cl-dbi:execute query (list id description)))))

(defmethod driver-unregister-migration ((driver sql-driver) (migration base-migration) &key)
  (log:debug "Unregistering migration: ~a" (migration-id migration))
  (let* ((connection (sql-driver-connection driver))
         (id (migration-id migration))
         (query (cl-dbi:prepare connection "DELETE FROM migration WHERE id = ?")))
    (cl-dbi:with-transaction connection
      (cl-dbi:execute query (list id)))))

(defmethod driver-apply-up-migration ((driver sql-driver) (migration base-migration) &key)
  (log:debug "Applying upgrade migration: ~a - ~a" (migration-id migration) (migration-description migration))
  (sql-driver-apply-migration driver migration #'migration-load-up-script))

(defmethod driver-apply-down-migration ((driver sql-driver) (migration base-migration) &key)
  (log:debug "Applying downgrade migration: ~a - ~a" (migration-id migration) (migration-description migration))
  (sql-driver-apply-migration driver migration #'migration-load-down-script))

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
         (statements (cl-ppcre:split *sql-statement-separator* content)))
    (cl-dbi:with-transaction connection
      (dolist (statement statements)
        (let ((stmt (cl-dbi:prepare connection (string-trim #(#\Newline) statement))))
          (cl-dbi:execute stmt))))))
