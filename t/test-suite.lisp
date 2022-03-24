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
(defpackage :cl-migratum.test
  (:use :cl :rove)
  (:nicknames :migratum.test)
  (:import-from :tmpdir)
  (:import-from :asdf)
  (:import-from :cl-dbi)
  (:import-from
   :migratum
   :provider-init
   :provider-shutdown
   :provider-name
   :provider-initialized
   :provider-list-migrations
   :provider-create-migration
   :provider-find-migration-by-id
   :migration-id
   :migration-description
   :migration-load
   :driver-name
   :driver-init
   :driver-shutdown
   :driver-initialized
   :driver-list-applied
   :contains-applied-migrations-p
   :latest-migration
   :list-pending
   :apply-pending
   :revert-last
   :apply-next)
  (:import-from
   :migratum.provider.local-path
   :make-local-path-provider
   :local-path-migration-up-script-path
   :local-path-migration-down-script-path
   :local-path-migration-up-script-type
   :local-path-migration-down-script-type)
  (:import-from :migratum.driver.dbi)
  (:import-from :migratum.driver.rdbms-postgresql))
(in-package :cl-migratum.test)

(defparameter *migrations-path*
  (asdf:system-relative-pathname :cl-migratum.test "t/migrations/")
  "Path to the migration files")

(defparameter *tmpdir*
  nil
  "Temp directory used during tests")

(defparameter *sqlite-conn*
  nil
  "CL-DBI connection used during tests")

(defparameter *dbi-driver*
  nil
  "DBI driver used during tests")

(defparameter *rdbms-postgresql-driver*
  nil
  "Driver from library hu.dwim.rdbms for PostgreSQL")

(defparameter *provider*
  nil
  "Local path provider used during tests")

(setup
  (setf *tmpdir* (tmpdir:mkdtemp))
  (setf *sqlite-conn*
        (cl-dbi:connect :sqlite3
                        :database-name (merge-pathnames (make-pathname :name "cl-migratum" :type "db")
                                                        *tmpdir*)))
  (setf *provider* (make-local-path-provider (list *migrations-path*) :cl-migratum.test))
  (setf *dbi-driver*
        (migratum.driver.dbi:make-driver *provider* *sqlite-conn*))
  (setf *rdbms-postgresql-driver*
        (migratum.driver.rdbms-postgresql:make-driver *provider*
                                                      `(:host ,(or (uiop:getenv "PGHOST") "localhost")
                                                        :database ,(or (uiop:getenv "PGDATABASE") "migratum")
                                                        :user-name ,(or (uiop:getenv "PGUSER") "migratum")
                                                        :password ,(or (uiop:getenv "PGPASSWORD") "FvbRd5qdeWHNum9p")))))

(teardown
  (provider-shutdown *provider*)
  (driver-shutdown *dbi-driver*)
  (when *tmpdir*
    (uiop:delete-directory-tree *tmpdir* :validate t)))

(defun create-table-baz (connection-or-database)
  (log:info "Migration function create-table-baz was called with a ~A" connection-or-database)
  (execute-query "create table baz ( col1 varchar(100), col2 bigint)" connection-or-database))

(defun drop-table-baz (connection-or-database)
  (log:info "Migration function drop-table-baz was called with a ~A" connection-or-database)
  (execute-query "drop table baz" connection-or-database))

(defun create-table-corge (connection-or-database)
  (log:info "Migration function create-table-baz was called with a ~A" connection-or-database)
  (execute-query "create table corge ( col1 varchar(100), col2 bigint)" connection-or-database))

(defun drop-table-corge (connection-or-database)
  (log:info "Migration function drop-table-baz was called with a ~A" connection-or-database)
  (execute-query "drop table corge" connection-or-database))

(defun execute-query (query connection-or-database)
  (etypecase connection-or-database
    (dbi.driver:dbi-connection (cl-dbi:execute (cl-dbi:prepare connection-or-database query)))
    (hu.dwim.rdbms:database (hu.dwim.rdbms:execute query))))
