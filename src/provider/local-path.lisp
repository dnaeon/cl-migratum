(in-package :cl-user)
(defpackage :cl-migratum.provider.local-path
  (:use :cl)
  (:nicknames :migratum.provider.local-path)
  (:import-from :log)
  (:import-from :local-time)
  (:import-from :cl-ppcre)
  (:import-from :uiop)
  (:import-from
   :cl-migratum.core
   :base-provider
   :migration
   :migration-id
   :list-migrations
   :load-migration
   :create-migration
   :make-migration-id)
  (:export
   :local-path-provider
   :local-path-migration
   :local-path-migration-path
   :local-path-provider-path
   :make-local-path-provider))
(in-package :cl-migratum.provider.local-path)

(defparameter *migration-file-regex*
  "(\\d{14})-(.*)\.sql$"
  "Regex used to match migration files")

(defclass local-path-migration (migration)
  ((path
    :initarg :path
    :initform (error "Must specify path")
    :accessor local-path-migration-path))
  (:documentation "Migration resource discovered from a local path"))

(defmethod load-migration ((migration local-path-migration) &key)
  (log:debug "Loading migration ~a" (migration-id migration))
  (with-slots (path) migration
    (uiop:read-file-string path)))

(defclass local-path-provider (base-provider)
  ((path
    :initarg :path
    :initform (error "Must specify path")
    :accessor local-path-provider-path))
  (:documentation "Provider for discovering migrations from a local path"))

(defun make-local-path-provider (path)
  "Creates a local path provider using the given path"
  (make-instance 'local-path-provider
                 :name "local-path"
                 :path path))

(defmethod list-migrations ((provider local-path-provider) &key)
  (log:debug "Listing migrations from path ~a" (local-path-provider-path provider))
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

(defmethod create-migration ((provider local-path-provider) &key id description content)
  (log:debug "Creating new migration in path ~a" (local-path-provider-path provider))
  (let* ((provider-path (slot-value provider 'path))
         (id (or id (make-migration-id)))
         (description (or description "new-migration"))
         (content (or content ""))
         (file-name (format nil "~a-~a" id description))
         (file-path (make-pathname :name file-name :type "sql" :directory (pathname-directory (truename provider-path)))))
    (log:debug "Creating migration file ~a" file-path)
    (with-open-file (out file-path :direction :output :if-does-not-exist :create)
      (write-string content out))
    (make-instance 'local-path-migration
                   :id id
                   :description description
                   :path file-path
                   :applied nil)))
