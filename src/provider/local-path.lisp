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
   :migration-up-script-path
   :migration-down-script-path
   :provider-paths
   :provider-scan-pattern
   :provider-file-mappings
   :make-provider))
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
    :initform "(\\d{14})-(.*)\.(up|down)\.(sql)$"
    :reader provider-scan-pattern
    :documentation "Regex pattern used to scan for migration files")
   (mappings
    :initarg :mappings
    :reader provider-file-mappings
    :initform `((:extension "sql" :kind :sql :class ,'sql-migration))
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
  (log:debug "[SQL] Creating new migration in ~a" path)
  (with-open-file (out path :direction :output :if-does-not-exist :create)
    (format out "-- id: ~A~%" id)
    (format out "-- direction: ~A~%" direction)
    (format out "-- description: ~A~2%" description)
    (when content
      (format out "~A~%" content))))

(defmethod provider-create-migration ((direction (eql :up)) (kind (eql :sql))
                                      (provider local-path-provider) (id integer)
                                      (description string) &key content)
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
                                      (description string) &key content)
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
