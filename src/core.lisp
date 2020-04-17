(in-package :cl-user)
(defpackage :cl-migratum.core
  (:use :cl)
  (:nicknames :migratum.core)
  (:import-from :ascii-table)
  (:export
   :migration
   :migration-id
   :migration-description
   :migration-applied
   :migration-load-up-script
   :migration-load-down-script
   :base-provider
   :provider-name
   :provider-list-migrations
   :provider-create-migration
   :provider-find-migration-by-id
   :base-driver
   :driver-name
   :driver-provider
   :driver-init
   :driver-list-applied
   :driver-register-migration
   :driver-unregister-migration
   :driver-apply-up-migration
   :driver-apply-down-migration
   :list-pending
   :latest-migration
   :display-pending
   :display-applied
   :apply-pending
   :contains-applied-migrations-p
   :apply-and-register
   :make-migration-id))
(in-package :cl-migratum.core)

(defclass migration ()
  ((id
    :type integer
    :initarg :id
    :initform (error "Must specify migration id")
    :accessor migration-id)
   (description
    :type string
    :initarg :description
    :initform (error "Must specify migration description")
    :accessor migration-description)
   (applied
    :initarg :applied
    :initform nil
    :accessor migration-applied))
  (:documentation "Base class for migration resources"))

(defgeneric migration-load-up-script (migration &key)
  (:documentation "Returns the contents of the upgrade migration script"))

(defgeneric migration-load-down-script (migration &key)
  (:documentation "Returns the contents of the downgrade migration script"))

(defclass base-provider ()
  ((name
    :type string
    :initarg :name
    :initform (error "Must specify provider name")
    :accessor provider-name))
  (:documentation "Base class for migration providers"))

(defgeneric provider-list-migrations (provider &key)
  (:documentation "Returns the list of migration resources discovered by the provider"))

(defgeneric provider-create-migration (provider &key id description up down)
  (:documentation "Creates a new migration resource using the given provider"))

(defclass base-driver ()
  ((name
    :type string
    :initarg :name
    :initform (error "Must specify driver name")
    :accessor driver-name)
   (provider
    :initarg :provider
    :initform (error "Must specify migrations provider")
    :accessor driver-provider))
  (:documentation "Base class for migration drivers"))

(defgeneric driver-init (driver &key)
  (:documentation "Initializes the driver, e.g. creates required schema"))

(defgeneric driver-list-applied (driver &key)
  (:documentation "Returns a list of the applied migrations in descending order"))

(defgeneric driver-register-migration (driver migration &key)
  (:documentation "Registers a successfully applied migration"))

(defgeneric driver-apply-up-migration (driver migration &key)
  (:documentation "Applies the upgrade migration script using the given driver"))

(defgeneric driver-apply-down-migration (driver migration &key)
  (:documentation "Applies the downgrade migration script using the given driver"))

(defgeneric driver-unregister-migration (driver migration &key)
  (:documentation "Unregisters a previously applied migration"))

(defun latest-migration (driver)
  "Returns the latest applied migration"
  (first (driver-list-applied driver)))

(defun contains-applied-migrations-p (driver)
  "Predicate for testing whether we have any migrations applied"
  (when (latest-migration driver)
    t))

(defun list-pending (driver)
  "Returns the list of migrations that have not been applied yet"
  (let* ((latest-migration (latest-migration driver))
         (latest-migration-id (or (and latest-migration
                                       (migration-id latest-migration))
                                  -1))
         (provider (driver-provider driver))
         (provided-migrations (provider-list-migrations provider)))
    (sort (remove-if-not (lambda (migration)
                           (> (migration-id migration) latest-migration-id))
                         provided-migrations)
          #'<
          :key #'migration-id)))

(defun display-pending (driver)
  "Display the pending migrations in a table"
  (let ((pending (list-pending driver))
        (table (ascii-table:make-table (list "ID" "DESCRIPTION") :header "PENDING MIGRATIONS")))
    (dolist (migration pending)
      (ascii-table:add-row table (list (migration-id migration)
                                       (migration-description migration))))
    (ascii-table:add-separator table)
    (ascii-table:add-row table (list "TOTAL" (length pending)))
    (when pending
      (ascii-table:display table))))

(defun display-applied (driver)
  "Displays the applied migrations in a table"
  (let ((applied (driver-list-applied driver))
        (table (ascii-table:make-table (list "ID" "DESCRIPTION" "APPLIED") :header "APPLIED MIGRATIONS")))
    (dolist (migration applied)
      (ascii-table:add-row table (list (migration-id migration)
                                       (migration-description migration)
                                       (migration-applied migration))))
    (ascii-table:add-separator table)
    (ascii-table:add-row table (list "" "TOTAL" (length applied)))
    (when applied
      (ascii-table:display table))))

(defun apply-and-register (driver migration)
  "Applies the migration and registers it"
  (log:info "Applying migration ~a - ~a"
            (migration-id migration)
            (migration-description migration))
      (driver-apply-up-migration driver migration)
      (driver-register-migration driver migration))

(defun apply-pending (driver)
  "Applies the pending migrations"
  (let ((pending (list-pending driver)))
    (log:info "Found ~a pending migration(s) to be applied" (length pending))
    (dolist (migration pending)
      (apply-and-register driver migration))))

(defun provider-find-migration-by-id (provider id)
  "Returns the migration with the given id from the provider"
  (let ((migrations (provider-list-migrations provider)))
    (find id migrations :key #'migration-id)))

(defun make-migration-id ()
  "Creates a new migration id"
  (let* ((now (local-time:now))
         (year (format nil "~d" (local-time:timestamp-year now)))
         (month (format nil "~2,'0d" (local-time:timestamp-month now)))
         (day (format nil "~2,'0d" (local-time:timestamp-day now)))
         (hour (format nil "~2,'0d" (local-time:timestamp-hour now)))
         (minute (format nil "~2,'0d" (local-time:timestamp-minute now)))
         (sec (format nil "~2,'0d" (local-time:timestamp-second now)))
         (timestamp-id (parse-integer (format nil "~a~a~a~a~a~a" year month day hour minute sec))))
    timestamp-id))

