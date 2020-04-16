(in-package :cl-user)
(defpackage :cl-migratum.core
  (:use :cl)
  (:nicknames :migratum.core)
  (:export
   :migration
   :migration-id
   :migration-description
   :migration-applied
   :base-provider
   :provider-name
   :list-migrations
   :load-migration
   :create-migration
   :base-driver
   :driver-name
   :driver-provider
   :driver-init
   :list-applied
   :register-migration
   :apply-migration
   :list-pending
   :latest-migration))
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
    :accessor migration-applied))
  (:documentation "Base class for migration resources"))

(defgeneric load-migration (migration &key)
  (:documentation "Returns the contents of the migration resource"))

(defclass base-provider ()
  ((name
    :type string
    :initarg :name
    :initform (error "Must specify provider name")
    :accessor provider-name))
  (:documentation "Base class for migration providers"))

(defgeneric list-migrations (provider &key)
  (:documentation "Returns the list of migration resources discovered by the provider"))

(defgeneric create-migration (provider &key id description content)
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

(defgeneric list-applied (driver &key)
  (:documentation "Returns a list of the applied migrations in descending order"))

(defgeneric register-migration (driver migration &key)
  (:documentation "Registers a successfully applied migration"))

(defgeneric apply-migration (driver migration &key)
  (:documentation "Applies a single migration using the given driver"))

(defun latest-migration (driver &rest rest)
  "Returns the latest applied migration"
  (first (apply #'list-applied driver rest)))

(defun list-pending (driver &rest rest)
  "Returns the list of migrations that have not been applied yet"
  (let* ((latest-migration (apply #'latest-migration driver rest))
         (latest-migration-id (migration-id latest-migration))
         (provider (driver-provider driver))
         (provided-migrations (list-migrations provider)))
    (sort (remove-if-not (lambda (migration)
                           (> (migration-id migration) latest-migration-id))
                         provided-migrations)
          #'<
          :key (lambda (item)
                 (migration-id item)))))
