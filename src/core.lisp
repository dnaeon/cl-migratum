;; Copyright (c) 2020 Marin Atanasov Nikolov <dnaeon@gmail.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer
;;     in this position and unchanged.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage :cl-migratum.core
  (:use :cl)
  (:nicknames :migratum.core)
  (:import-from :ascii-table)
  (:import-from
   :cl-migratum.util
   :take)
  (:export
   :base-migration
   :migration-id
   :migration-description
   :migration-applied
   :migration-load
   :migration-kind
   :base-provider
   :provider-init
   :provider-shutdown
   :provider-initialized
   :provider-name
   :provider-list-migrations
   :provider-create-migration
   :find-migration-by-id
   :base-driver
   :driver-name
   :driver-provider
   :driver-init
   :driver-shutdown
   :driver-initialized
   :driver-list-applied
   :driver-register-migration
   :driver-apply-migration
   :list-pending
   :latest-migration
   :display-pending
   :display-applied
   :apply-pending
   :contains-applied-migrations-p
   :apply-next
   :revert-last))
(in-package :cl-migratum.core)

(defgeneric migration-id (migration)
  (:documentation "Returns the ID of the migration. The ID must be a
  positive integer"))

(defgeneric migration-description (migration)
  (:documentation "Returns a string describing the migration
  resource"))

(defgeneric migration-applied (migration)
  (:documentation "Returns a timestamp describing when the migration
  was applied or NIL if not applied"))

(defgeneric migration-load (direction migration)
  (:documentation "Loads the given migration. Direction is :UP or
  :DOWN which specifies whether to load the upgrade or downgrade
  migration respectively."))

(defgeneric migration-kind (migration)
  (:documentation "Returns the kind of the migration resource as a
  keyword, e.g. :sql, :lisp, etc"))

(defclass base-migration ()
  ((id
    :type integer
    :initarg :id
    :initform (error "Must specify migration id")
    :reader migration-id
    :documentation "Unique migration id")
   (description
    :type string
    :initarg :description
    :initform (error "Must specify migration description")
    :reader migration-description
    :documentation "Description of the migration")
   (applied
    :initarg :applied
    :initform nil
    :reader migration-applied
    :documentation "Timestamp when the migration was applied or NIL if it is not applied")
   (kind
    :initarg :kind
    :initform (error "Must specify migration kind")
    :reader migration-kind
    :documentation "A keyword describing the migration kind"))
  (:documentation "Base class for migration resources"))

(defclass base-provider ()
  ((name
    :type string
    :initarg :name
    :initform (error "Must specify provider name")
    :reader provider-name
    :documentation "Name of the provider")
   (initialized
    :initarg :initialized
    :initform nil
    :accessor provider-initialized
    :documentation "Returns T if provider is initialized, NIL otherwise"))
  (:documentation "Base class for migration providers"))

(defgeneric provider-init (provider &key)
  (:documentation "Initializes the driver, if needed"))

(defgeneric provider-shutdown (provider &key)
  (:documentation "Shutdowns the provider and cleans up any allocated resources"))

(defgeneric provider-list-migrations (provider &key)
  (:documentation "Returns the list of migration resources discovered by the provider"))

(defgeneric provider-create-migration (direction kind provider id description content &key)
  (:documentation "Creates a new migration resource using the given provider"))

(defgeneric find-migration-by-id (provider id)
  (:documentation "Returns the migration with the given id from the provider"))

(defmethod provider-init ((provider base-provider) &key)
  (log:debug "Initializing provider ~a" (provider-name provider))
  (setf (provider-initialized provider) t))

(defmethod provider-shutdown ((provider base-provider) &key)
  (log:debug "Shutting down provider ~a" (provider-name provider))
  (setf (provider-initialized provider) nil))

(defmethod find-migration-by-id ((provider base-provider) id)
  (let ((migrations (provider-list-migrations provider)))
    (find id migrations :key #'migration-id)))

(defclass base-driver ()
  ((name
    :type string
    :initarg :name
    :initform (error "Must specify driver name")
    :reader driver-name
    :documentation "Name of the driver")
   (provider
    :initarg :provider
    :initform (error "Must specify migrations provider")
    :reader driver-provider
    :documentation "Provider used by the driver")
   (initialized
    :initarg :initialized
    :initform nil
    :accessor driver-initialized
    :documentation "Returns T if driver is initialized, NIL otherwise"))
  (:documentation "Base class for migration drivers"))

(defgeneric driver-init (driver &key)
  (:documentation "Initializes the driver, e.g. creates required schema"))

(defgeneric driver-shutdown (driver &key)
  (:documentation "Shutdowns the driver and cleans up any allocated resources"))

(defgeneric driver-list-applied (driver &key offset limit)
  (:documentation "Returns a list of the applied migrations in descending order"))

(defgeneric driver-register-migration (direction driver migration &key)
  (:documentation "Register or unregister a migration depending on the given direction"))

(defgeneric driver-apply-migration (direction kind driver migration &key)
  (:documentation "Applies the migration script using the given driver and direction"))

(defgeneric latest-migration (driver)
  (:documentation "Returns the latest applied migration"))

(defgeneric contains-applied-migrations-p (driver)
  (:documentation "Predicate for testing whether there are any applied migrations"))

(defgeneric list-pending (driver)
  (:documentation "Returns the list of migrations that have not been applied yet"))

(defgeneric display-pending (driver)
  (:documentation "Displays the pending migrations"))

(defgeneric display-applied (driver &key offset limit)
  (:documentation "Displays the applied migrations"))

(defgeneric apply-and-register (driver migration)
  (:documentation "Applies the migration and registers it"))

(defgeneric apply-pending (driver)
  (:documentation "Applies all pending migrations"))

(defgeneric revert-and-unregister (driver migration)
  (:documentation "Reverts and unregisters a given migration."))

(defgeneric apply-next (driver &key count)
  (:documentation "Apply the next COUNT of pending migrations"))

(defgeneric revert-last (driver &key count)
  (:documentation "Reverts the last COUNT applied migrations"))

(defmethod driver-init ((driver base-driver) &key)
  (log:debug "Initializing driver ~a" (driver-name driver))
  (setf (driver-initialized driver) t))

(defmethod driver-shutdown ((driver base-driver) &key)
  (log:debug "Shutting down driver ~a" (driver-name driver))
  (setf (driver-initialized driver) nil))

(defmethod latest-migration ((driver base-driver))
  (first (driver-list-applied driver :offset 0 :limit 1)))

(defmethod contains-applied-migrations-p ((driver base-driver))
  (when (latest-migration driver)
    t))

(defmethod list-pending ((driver base-driver))
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

(defmethod display-pending ((driver base-driver))
  (let ((pending (list-pending driver))
        (table (ascii-table:make-table (list "ID" "DESCRIPTION" "KIND") :header "PENDING MIGRATIONS")))
    (dolist (migration pending)
      (ascii-table:add-row table (list (migration-id migration)
                                       (migration-description migration)
                                       (migration-kind migration))))
    (ascii-table:add-separator table)
    (ascii-table:add-row table (list "" "TOTAL" (length pending)))
    (when pending
      (ascii-table:display table))))

(defmethod display-applied ((driver base-driver) &key (offset 0) (limit 100))
  (let ((applied (driver-list-applied driver :offset offset :limit limit))
        (table (ascii-table:make-table (list "ID" "DESCRIPTION" "APPLIED" "KIND") :header "APPLIED MIGRATIONS")))
    (dolist (migration applied)
      (ascii-table:add-row table (list (migration-id migration)
                                       (migration-description migration)
                                       (migration-applied migration)
                                       (migration-kind migration))))
    (ascii-table:add-separator table)
    (ascii-table:add-row table (list "" "" "TOTAL" (length applied)))
    (when applied
      (ascii-table:display table))))

(defmethod apply-and-register ((driver base-driver) migration)
  (let ((id (migration-id migration))
        (description (migration-description migration))
        (kind (migration-kind migration)))
    (log:info "Applying migration ~A - ~A (~A)" id description kind)
    (driver-apply-migration :up kind driver migration)
    (driver-register-migration :up driver migration)))

(defmethod apply-pending ((driver base-driver))
  (let ((pending (list-pending driver)))
    (log:info "Found ~a pending migration(s) to be applied" (length pending))
    (dolist (migration pending)
      (apply-and-register driver migration))))

(defmethod revert-and-unregister ((driver base-driver) migration)
  "The migration to be reverted is first loaded via the driver
provider, in order to ensure we can load the downgrade script."
  (let* ((id (migration-id migration))
         (description (migration-description migration))
         (kind (migration-kind migration))
         (provider (driver-provider driver))
         (to-revert (find-migration-by-id provider id)))
    (log:info "Reverting migration ~A - ~A (~A)" id description kind)
    (driver-apply-migration :down kind driver to-revert)
    (driver-register-migration :down driver to-revert)))

(defmethod apply-next ((driver base-driver) &key (count 1))
  (let* ((pending (list-pending driver))
         (to-apply (take count pending)))
    (dolist (migration to-apply)
      (apply-and-register driver migration))))

(defmethod revert-last ((driver base-driver) &key (count 1))
  (let* ((applied (driver-list-applied driver))
         (to-revert (take count applied)))
    (dolist (item to-revert)
      (revert-and-unregister driver item))))
