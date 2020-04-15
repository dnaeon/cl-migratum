(in-package :cl-user)
(defpackage :cl-migratum.core
  (:use :cl)
  (:nicknames :migratum.core)
  (:export
   :migration
   :base-provider
   :list-migrations
   :load-migration
   :create-migration))
(in-package :cl-migratum.core)

(defclass migration ()
  ((id
    :type integer
    :initarg :id
    :initform (error "Must specify migration id"))
   (description
    :type string
    :initarg :description
    :initform (error "Must specify migration description"))
   (applied
    :initarg :applied))
  (:documentation "Base class for migration resources"))

(defclass base-provider ()
  ((name
    :type string
    :initarg :name
    :initform (error "Must specify provider name")))
  (:documentation "Base class for migration providers"))

(defgeneric list-migrations (provider &key)
  (:documentation "Returns the list of migration resources discovered by the provider"))

(defgeneric load-migration (provider migration &key)
  (:documentation "Returns the contents of the migration resource using the given provider"))

(defgeneric create-migration (provider &optional id description content)
  (:documentation "Creates a new migration resource using the given provider"))
