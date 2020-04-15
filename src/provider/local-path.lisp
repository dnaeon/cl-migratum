(in-package :cl-user)
(defpackage :cl-migratum.provider.local-path
  (:use :cl)
  (:nicknames :migratum.provider.local-path)
  (:import-from :local-time)
  (:import-from :cl-ppcre)
  (:import-from :uiop)
  (:import-from
   :cl-migratum.core
   :base-provider
   :migration
   :list-migrations
   :load-migration
   :create-migration)
  (:export
   :local-path-provider
   :local-path-migration
   :make-local-path-provider))
(in-package :cl-migratum.provider.local-path)

(defconstant *migration-file-regex*
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

(defun make-local-path-provider (path)
  "Creates a local path provider using the given path"
  (make-instance 'local-path-provider
		 :name "local-path"
		 :path path))

(defmethod list-migrations ((provider local-path-provider) &key)
  (with-slots (path) provider
    (let* ((files (uiop:directory-files path))
	   (scanner (cl-ppcre:create-scanner *migration-file-regex*))
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

(defmethod load-migration ((provider local-path-provider) (migration local-path-migration) &key)
  (with-slots (path) migration
    (uiop:read-file-string path)))

(defmethod create-migration ((provider local-path-provider) &key id description content)
  (let* ((provider-path (slot-value provider 'path))
	 (now (local-time:now))
	 (year (format nil "~d" (local-time:timestamp-year now)))
	 (month (format nil "~2,'0d" (local-time:timestamp-month now)))
	 (day (format nil "~2,'0d" (local-time:timestamp-day now)))
	 (hour (format nil "~2,'0d" (local-time:timestamp-hour now)))
	 (minute (format nil "~2,'0d" (local-time:timestamp-minute now)))
	 (sec (format nil "~2,'0d" (local-time:timestamp-second now)))
	 (timestamp-id (format nil "~a~a~a~a~a~a" year month day hour minute sec))
	 (id (or id timestamp-id))
	 (description (or description "new-migration"))
	 (content (or content ""))
	 (file-name (format nil "~a-~a" id description))
	 (file-path (make-pathname :name file-name :type "sql" :directory `(:relative ,(namestring provider-path)))))
    (with-open-file (out file-path :direction :output)
      (write-string content out))
    (make-instance 'local-path-migration
		   :id (parse-integer id)
		   :description description
		   :path file-path
		   :applied nil)))
