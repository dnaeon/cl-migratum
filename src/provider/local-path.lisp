(in-package :cl-user)
(defpackage :cl-migratum.provider.local-path
  (:use :cl)
  (:nicknames :migratum.provider.local-path)
  (:import-from :cl-ppcre)
  (:import-from :uiop)
  (:import-from
   :cl-migratum.core
   :base-provider
   :migration)
  (:export
   :local-path-provider
   :local-path-migration))
(in-package :cl-migratum.provider.local-path)

(defconstant +migration-file-regex+
  "(\\d{14})-(.*)\.sql"
  "Regex used to match migration files")

(defclass local-path-migration (migration)
  ((path
    :initarg :path
    :initform (error "Must specify path")))
  (:documentation "Migration resource discovered from a local path"))

(defclass local-path-provider (base-provider)
  ((path
    :initarg :path
    :initform (error "Must specify path")))
  (:documentation "Provider for discovering migrations from a local path"))

(defmethod list-migrations ((provider local-path-provider) &key)
  (with-slots (path) provider
    (let* ((files (uiop:directory-files path))
	   (scanner (cl-ppcre:create-scanner +migration-file-regex+))
	   (result nil))
      (dolist (file files)
	(cl-ppcre:register-groups-bind (id description)
	    (scanner (namestring file))
	  (push (make-instance 'local-path-migration
			       :id (parse-integer id)
			       :description description
			       :applied nil
			       :path file)
		result)))
      result)))
