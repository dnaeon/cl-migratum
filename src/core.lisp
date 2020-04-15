(in-package :cl-user)
(defpackage :cl-migratum.core
  (:use :cl)
  (:nicknames :migratum.core)
  (:export
   :migration))
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
