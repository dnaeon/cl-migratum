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
   :provider-package
   :provider-list-migrations
   :provider-create-migration
   :make-migration-id)
  (:export
   :*migration-file-regex*
   :migration-file-p
   :find-migration-files
   :normalize-description
   :local-path-provider
   :local-path-migration
   :local-path-migration-up-script-path
   :local-path-migration-down-script-path
   :local-path-provider-paths
   :make-local-path-provider))
(in-package :cl-migratum.provider.local-path)

(defparameter *migration-file-regex*
  "(\\d{14})-(.*)\.(up|down)\.(sql|sexp)$"
  "Regex used to match migration files")

(defclass local-path-migration (base-migration)
  ((up-script-path
    :initarg :up-script-path
    :initform (error "Must specify up script path")
    :accessor local-path-migration-up-script-path
    :documentation "Path to the upgrade SQL script")
   (up-script-type
    :initarg :up-script-type
    :initform (error "Must specify up script type (one of :sql or :sexp)")
    :accessor local-path-migration-up-script-type
    :documentation "The type of the upgrade script (SQL or a function specification)")
   (down-script-path
    :initarg :down-script-path
    :initform (error "Must specify down script path")
    :accessor local-path-migration-down-script-path
    :documentation "Path to the downgrade SQL script")
   (down-script-type
    :initarg :down-script-type
    :initform (error "Must specify down script type (one of :sql or :sexp)")
    :accessor local-path-migration-down-script-type
    :documentation "The type of the downgrade script (SQL or a function specification s-expression)"))
  (:documentation "Migration resource discovered from a local path"))

(defmethod migration-load ((migration local-path-migration) (direction (eql :up)) package &key)
  (let ((id (migration-id migration))
        (path (local-path-migration-up-script-path migration))
        (type (local-path-migration-up-script-type migration)))
    (log:debug "Loading upgrade migration for id ~a from ~a" id path)
    (load-sql-or-function path type package)))

(defmethod migration-load ((migration local-path-migration) (direction (eql :down)) package &key)
  (let ((id (migration-id migration))
        (path (local-path-migration-down-script-path migration))
        (type (local-path-migration-down-script-type migration)))
    (log:debug "Loading downgrade migration for id ~a with type ~a from ~a" id type path)
    (load-sql-or-function path type package)))

(defun load-sql-or-function (path type package)
  (let ((content (uiop:read-file-string path)))
    (ecase type
      (:sql content)
      (:sexp (let ((*read-eval* nil)
                   (*package* (find-package package)))
               (assert (> 1000 (length content)) nil "Functional migration file '~A' exceeds the maximum size of 1000" path)
               (let* ((fn-symbol (getf (read (make-string-input-stream content)) :function))
                      (fn-package (symbol-package fn-symbol)))
                 (when fn-package
                   (assert (eq *package* fn-package) nil "The function ~A is in package ~A and not in the configured package of the provider: ~A" fn-symbol fn-package *package*))
                 (symbol-function fn-symbol)))))))

(defclass local-path-provider (base-provider)
  ((paths
    :initarg :paths
    :initform (error "Must specify migration resource paths")
    :accessor local-path-provider-paths
    :documentation "Local paths from which to discover migrations"))
  (:documentation "Provider for discovering migrations from a list of local paths"))

(defmethod initialize-instance :after ((provider local-path-provider) &key)
  (unless (listp (local-path-provider-paths provider))
    (error "Must specify a list of migration resource paths")))

(defun make-local-path-provider (paths &optional (package :cl-user))
  "Creates a local path provider using the given path"
  (make-instance 'local-path-provider
                 :name "local-path"
                 :paths paths
                 :package package
                 :initialized t))

(defun migration-file-p (path scanner)
  "Predicate used for testing whether a path matches a migration file pattern"
  (when (cl-ppcre:scan scanner (namestring path))
    t))

(defun find-migration-files (path scanner)
  "Filters all files that match the given pattern scanner"
  (unless (uiop:directory-exists-p path)
    (error "path does not exist or is not a directory: ~a" path))
  (log:debug "Discovering migration files from ~a" path)
  (let ((files (uiop:directory-files path)))
    (remove-if-not (lambda (file)
                     (migration-file-p file scanner))
                   files)))

(defun group-migration-files-by-id (files scanner)
  "Groups migration files by id. Each group consists of the upgrade and downgrade scripts."
  (reduce (lambda (acc file)
            (cl-ppcre:register-groups-bind (id description operation type)
                (scanner (namestring file))
              (let* ((id (parse-integer id))
                    (group (gethash id acc nil)))
                (setf (gethash id acc)
                      (push (list :id id :operation operation :description description :path file :type type)
                            group))))
            acc)
          files
          :initial-value (make-hash-table)))

(defun find-migration-from-group (item sequence &key (indicator :operation) (test #'equal))
  "Returns the migration spec from a group of migrations by searching for the item and indicator.
One example usage of this function is to find a migration by operation (e.g. `up` or `down`)
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
  (log:debug "Migration paths to be scanned: ~a" (local-path-provider-paths provider))
  (let* ((paths (local-path-provider-paths provider))
         (scanner (cl-ppcre:create-scanner *migration-file-regex*))
         (files (mapcan (lambda (path) (find-migration-files path scanner)) paths))
         (groups (group-migration-files-by-id files scanner))
         (result nil))
    (maphash (lambda (k v)
               (let ((id k)
                     (up-migration (find-migration-from-group "up" v :indicator :operation))
                     (down-migration (find-migration-from-group "down" v :indicator :operation)))
                 (push (make-instance 'local-path-migration
                                      :id id
                                      :description (getf up-migration :description)
                                      :applied nil
                                      :up-script-path (getf up-migration :path)
                                      :up-script-type (intern (string-upcase (getf up-migration :type)) :keyword)
                                      :down-script-path (getf down-migration :path)
                                      :down-script-type (intern (string-upcase (getf down-migration :type)) :keyword))
                       result)))
             groups)
    result))

(defmethod provider-create-migration ((provider local-path-provider) &key id description up down (type :sql))
  (log:debug "Creating new migration in path ~a" (first (local-path-provider-paths provider)))
  (assert (member type '(:sql :sexp)) nil "Migration type must be either :sql or :sexp, but was ~A" type)
  (let* ((provider-path (first (local-path-provider-paths provider)))
         (id (or id (make-migration-id)))
         (description (normalize-description (or description "new migration")))
         (up-content (or up ""))
         (down-content (or down ""))
         (up-file-name (format nil "~a-~a.up" id description))
         (down-file-name (format nil "~a-~a.down" id description))
         (type-name (string-downcase (symbol-name type)))
         (up-file-path (make-pathname :name up-file-name :type type-name :directory (pathname-directory (truename provider-path))))
         (down-file-path (make-pathname :name down-file-name :type type-name :directory (pathname-directory (truename provider-path))))
         migration)
    (handler-case
        (progn
          (log:debug "Creating UP migration file ~a" up-file-path)
          (with-open-file (out up-file-path :direction :output :if-does-not-exist :create)
            (write-string up-content out))
          (log:debug "Creating DOWN migration file ~a" down-file-path)
          (with-open-file (out down-file-path :direction :output :if-does-not-exist :create)
            (write-string down-content out))
          (setf migration (make-instance 'local-path-migration
                                        :id id
                                        :description description
                                        :applied nil
                                        :up-script-path up-file-path
                                        :up-script-type type
                                        :down-script-path down-file-path
                                        :down-script-type type))
          ;; verify that the content can be loaded
          (migration-load migration :up (provider-package provider))
          (migration-load migration :down (provider-package provider))
          migration)
      (error (e) (progn
                   (uiop:delete-file-if-exists up-file-path)
                   (uiop:delete-file-if-exists down-file-path)
                   (signal e))))))
