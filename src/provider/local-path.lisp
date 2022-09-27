;; Copyright (c) 2020-2022 Marin Atanasov Nikolov <dnaeon@gmail.com>
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
(defpackage :cl-migratum.provider.local-path
  (:use :cl)
  (:nicknames :migratum.provider.local-path)
  (:import-from :log)
  (:import-from :cl-ppcre)
  (:import-from :uiop)
  (:import-from
   :cl-migratum
   :base-provider
   :base-migration
   :migration-id
   :migration-load
   :migration-kind
   :provider-list-migrations
   :provider-create-migration
   :make-migration-id)
  (:export
   :migration-file-p
   :find-migration-files
   :normalize-description
   :local-path-provider
   :local-path-migration
   :get-file-mapping
   :sql-migration
   :lisp-migration
   :migration-up-script-path
   :migration-down-script-path
   :provider-paths
   :provider-scan-pattern
   :provider-file-mappings
   :make-provider
   :touch-migration))
(in-package :cl-migratum.provider.local-path)

(defclass local-path-migration (base-migration)
  ((up-script-path
    :initarg :up-script-path
    :initform (error "Must specify up script path")
    :reader migration-up-script-path
    :documentation "Path to the upgrade script")
   (down-script-path
    :initarg :down-script-path
    :initform (error "Must specify down script path")
    :reader migration-down-script-path
    :documentation "Path to the downgrade script"))
  (:documentation "Base class for migration resources discovered from a local path"))

(defclass sql-migration (local-path-migration)
  ()
  (:default-initargs
   :kind :sql)
  (:documentation "SQL migration resource"))

(defmethod migration-load ((direction (eql :up)) (migration sql-migration))
  "Loads the upgrade SQL migration"
  (let ((id (migration-id migration))
        (path (migration-up-script-path migration)))
    (log:debug "[SQL] Loading  upgrade migration for id ~a from ~a" id path)
    (uiop:read-file-string path)))

(defmethod migration-load ((direction (eql :down)) (migration sql-migration))
  "Loads the downgrade SQL migration"
  (let ((id (migration-id migration))
        (path (migration-down-script-path migration)))
    (log:debug "[SQL] Loading downgrade migration for id ~a from ~a" id path)
    (uiop:read-file-string path)))

(defclass lisp-migration (local-path-migration)
  ()
  (:default-initargs
   :kind :lisp)
  (:documentation "Migration resource driven by a Lisp function"))

(defun %load-lisp-migration (id path)
  "Loads a Lisp migration from a given path"
  (log:debug "[LISP] Loading migration for id ~A from ~A" id path)
  (let* ((spec (uiop:read-file-form path))
         (system-name (getf spec :system))
         (package-name (getf spec :package))
         (handler-name (getf spec :handler)))
    (unless system-name
      (error "No ASDF system specified for migration ~A" id))
    (unless package-name
      (error "No package specified for migration ~A" id))
    (unless handler-name
      (error "No handler specified for migration ~A" id))
    #+quicklisp
    (ql:quickload system-name :verbose nil :silent t)
    #-quicklisp
    (asdf:load-system system-name :verbose nil)
    (unless (find-package package-name)
      (error "Cannot find package ~A for migration ~A" package-name id))
    (let* ((package (find-package package-name))
           (handler-sym (find-symbol (symbol-name handler-name) package)))
      (unless handler-sym
        (error "Handler symbol ~A not found in package ~A for migration ~A" handler-name package id))
      (symbol-function handler-sym))))

(defmethod migration-load ((direction (eql :up)) (migration lisp-migration))
  (%load-lisp-migration (migration-id migration) (migration-up-script-path migration)))

(defmethod migration-load ((direction (eql :down)) (migration lisp-migration))
  (%load-lisp-migration (migration-id migration) (migration-down-script-path migration)))

(defgeneric get-file-mapping (provider extension)
  (:documentation "Returns the mapping associated with the given file EXTENSION"))

(defclass local-path-provider (base-provider)
  ((paths
    :initarg :paths
    :initform (error "Must specify migration resource paths")
    :reader provider-paths
    :documentation "Local paths from which to discover migrations")
   (pattern
    :initarg :pattern
    :initform "(\\d{14})-(.*)\.(up|down)\.(sql|lisp)$"
    :reader provider-scan-pattern
    :documentation "Regex pattern used to scan for migration files")
   (mappings
    :initarg :mappings
    :reader provider-file-mappings
    :initform `((:extension "sql" :kind :sql :class ,'sql-migration)
                (:extension "lisp" :kind :lisp :class ,'lisp-migration))
    :documentation "Mapping between migration resource files and their respective classes"))
  (:documentation "Provider for discovering migrations from a list of local paths"))

(defmethod initialize-instance :after ((provider local-path-provider) &key)
  (unless (listp (provider-paths provider))
    (error "Must specify a list of migration resource paths")))

(defmethod get-file-mapping ((provider local-path-provider) extension)
  (find extension
        (provider-file-mappings provider)
        :key (lambda (item)
               (getf item :extension))
        :test #'string-equal))

(defun make-provider (paths)
  "Creates a local path provider. Migration resources will be
discovered from the given PATHS."
  (make-instance 'local-path-provider
                 :name "local-path"
                 :paths paths
                 :initialized t))

(defun migration-file-p (path scanner)
  "Predicate used for testing whether a path matches a migration file pattern"
  (when (cl-ppcre:scan scanner (namestring path))
    t))

(defun find-migration-files (path scanner)
  "Filters all files that match the given pattern scanner"
  (unless (uiop:directory-exists-p path)
    (error "path does not exist or is not a directory: ~a" path))
  (log:debug "[LP-PROVIDER] Discovering migration files from ~a" path)
  (let ((files (uiop:directory-files path)))
    (remove-if-not (lambda (file)
                     (migration-file-p file scanner))
                   files)))

(defun group-migration-files-by-id (files scanner)
  "Groups migration files by id. Each group consists of the upgrade and downgrade scripts."
  (reduce (lambda (acc file)
            (cl-ppcre:register-groups-bind (id description direction extension)
                (scanner (namestring file))
              (let* ((id (parse-integer id))
                     (group (gethash id acc nil)))
                (setf (gethash id acc)
                      (push (list :id id
                                  :direction direction
                                  :description description
                                  :path file
                                  :extension extension)
                            group))))
            acc)
          files
          :initial-value (make-hash-table :test #'equal)))

(defun find-migration-from-group (item sequence &key (indicator :direction) (test #'equal))
  "Returns the migration spec from a group of migrations by searching for the item and indicator.
One example usage of this function is to find a migration by `direction' (e.g. `up' or `down')
You can use this helper function to find migrations in the groups returned by the
GROUP-MIGRATION-FILES-BY id function."
  (find item
        sequence
        :key (lambda (x)
               (getf x indicator))
        :test test))

(defun normalize-description (description)
  "Normalizes the description of the migration, which will be
   used as part of the migration filename"
  (cl-ppcre:regex-replace-all "\\s|-"
                              (string-trim #(#\Space) description)
                              "_"))

(defmethod provider-list-migrations ((provider local-path-provider) &key)
  (log:debug "Migration paths to be scanned: ~a" (provider-paths provider))
  (let* ((paths (provider-paths provider))
         (scanner (cl-ppcre:create-scanner (provider-scan-pattern provider)))
         (files (mapcan (lambda (path) (find-migration-files path scanner)) paths))
         (groups (group-migration-files-by-id files scanner))
         (result nil))
    (maphash (lambda (k v)
               (let* ((id k)
                      (up-migration (find-migration-from-group "up" v :indicator :direction))
                      (down-migration (find-migration-from-group "down" v :indicator :direction))
                      (up-extension (getf up-migration :extension))
                      (down-extension (getf down-migration :extension))
                      (mapping (get-file-mapping provider up-extension))
                      (migration-class (getf mapping :class)))
                 (unless up-migration
                   (error "Missing upgrade migration for ~A" id))
                 (unless down-migration
                   (error "Missing downgrade migration for ~A" id))
                 (unless (equal up-extension down-extension)
                   (error "Upgrade and downgrade migration for ~A are of different kinds: ~A and ~A"
                          id up-extension down-extension))
                 (unless mapping
                   (error "No file mapping found for migration id ~A of kind ~A" id up-extension))
                 (unless migration-class
                   (error "No mapping class found for migration id ~A of kind ~A" id up-extension))
                 (push (make-instance migration-class
                                      :id id
                                      :description (getf up-migration :description)
                                      :applied nil
                                      :up-script-path (getf up-migration :path)
                                      :down-script-path (getf down-migration :path))
                       result)))
             groups)
    result))

(defun %write-sql-migration-file (id description direction path content)
  (log:debug "[SQL] Creating new migration in ~A" path)
  (with-open-file (out path :direction :output :if-does-not-exist :create)
    (format out "-- id: ~A~%" id)
    (format out "-- direction: ~A~%" direction)
    (format out "-- description: ~A~2%" description)
    (format out "~A~%" content)))

(defmethod provider-create-migration ((direction (eql :up)) (kind (eql :sql))
                                      (provider local-path-provider) (id integer)
                                      (description string) (content string) &key)
  (let* ((description (normalize-description description))
         (provider-path (first (provider-paths provider)))
         (name (format nil "~A-~A.up" id description))
         (file-path (make-pathname :name name :type "sql" :directory (pathname-directory (truename provider-path)))))
    (%write-sql-migration-file id description direction file-path content)
    (make-instance 'sql-migration
                   :id id
                   :description description
                   :applied nil
                   :up-script-path file-path
                   :down-script-path nil)))

(defmethod provider-create-migration ((direction (eql :down)) (kind (eql :sql))
                                      (provider local-path-provider) (id integer)
                                      (description string) (content string) &key)
  (let* ((description (normalize-description description))
         (provider-path (first (provider-paths provider)))
         (name (format nil "~A-~A.down" id description))
         (file-path (make-pathname :name name :type "sql" :directory (pathname-directory (truename provider-path)))))
    (%write-sql-migration-file id description direction file-path content)
    (make-instance 'sql-migration
                   :id id
                   :description description
                   :applied nil
                   :up-script-path nil
                   :down-script-path file-path)))

(defgeneric touch-migration (kind local-path-provider description &key id-generating-function)
  (:documentation "Creates an empty up-and-down pair of migration resources using the given local-path-provider. If KIND is :lisp, for cl-migratum to invoke your lisp migration handler function, please replace :my-system, :my-package, :my-handler-function in the newly created pair of files as your need."))

(defmethod touch-migration ((kind (eql :sql))
                            (local-path-provider local-path-provider)
                            (description string)
                            &key (id-generating-function #'make-migration-id))
  (let ((args (list :sql
                    local-path-provider
                    (funcall id-generating-function)
                    description
                    "")))
    (values (apply #'provider-create-migration (cons :up args))
            (apply #'provider-create-migration (cons :down args)))))

(defmethod touch-migration ((kind (eql :lisp))
                            (local-path-provider local-path-provider)
                            (description string)
                            &key (id-generating-function #'make-migration-id))
  (let ((args (list :lisp
                    local-path-provider
                    (funcall id-generating-function)
                    description
                    (format nil "~%;; TODO: replace :my-system, :my-package, and my-hander-function as you need, so cl-migration can find your migration handler function. ~%~%"
                            '(:system :my-system :package :my-package :handler :my-handler-function)))))
    (values (apply #'provider-create-migration (cons :up args))
            (apply #'provider-create-migration (cons :down args)))))

(defun %write-lisp-migration-file (id description direction path content)
  (log:debug "[LISP] Creating new migration in ~A" path)
  (let ((system-name (getf content :system))
        (package-name (getf content :package))
        (handler-name (getf content :handler)))
    (unless system-name
      (error "Must provide :system for Lisp migrations"))
    (unless package-name
      (error "Must provide :package for Lisp migrations"))
    (unless handler-name
      (error "Must provide :handler for Lisp migrations"))
    (with-open-file (out path :direction :output :if-does-not-exist :create)
      (format out ";; id: ~A~%" id)
      (format out ";; direction: ~A~%" direction)
      (format out ";; description: ~A~2%" description)
      (write content :stream out :case :downcase :readably t)
      (format out "~%"))))

(defmethod provider-create-migration ((direction (eql :up)) (kind (eql :lisp))
                                      (provider local-path-provider) (id integer)
                                      (description string) (content list) &key)
  (unless content
    (error "Must provide a non-empty property list spec"))
  (let* ((description (normalize-description description))
         (provider-path (first (provider-paths provider)))
         (name (format nil "~A-~A.up" id description))
         (file-path (make-pathname :name name :type "lisp" :directory (pathname-directory (truename provider-path)))))
    (%write-lisp-migration-file id description direction file-path content)
    (make-instance 'lisp-migration
                   :id id
                   :description description
                   :applied nil
                   :up-script-path file-path
                   :down-script-path nil)))

(defmethod provider-create-migration ((direction (eql :down)) (kind (eql :lisp))
                                      (provider local-path-provider) (id integer)
                                      (description string) (content list) &key)
  (unless content
    (error "Must provide a non-empty property list spec"))
  (let* ((description (normalize-description description))
         (provider-path (first (provider-paths provider)))
         (name (format nil "~A-~A.down" id description))
         (file-path (make-pathname :name name :type "lisp" :directory (pathname-directory (truename provider-path)))))
    (%write-lisp-migration-file id description direction file-path content)
    (make-instance 'lisp-migration
                   :id id
                   :description description
                   :applied nil
                   :up-script-path nil
                   :down-script-path file-path)))
