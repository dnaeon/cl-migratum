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
   :find-migration-by-id
   :migration-id
   :migration-kind
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
   :apply-next
   :make-migration-id)
  (:import-from :migratum.provider.local-path)
  (:import-from :migratum.driver.dbi)
  (:import-from :migratum.driver.rdbms-postgresql)
  (:import-from :migratum.driver.postmodern-postgresql))
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

(defparameter *postmodern-postgresql-driver*
  nil
  "Driver from library pomo for PostgreSQL")

(defparameter *provider*
  nil
  "Local path provider used during tests")

(defun getenv (env &optional default)
  "Returns the value of the given env var, if set."
  (or (uiop:getenv env) default))

(defun getenv-int (env &optional default)
  "Returns the value of the given env var as integer, if set."
  (let ((value (uiop:getenv env)))
    (if value
        (parse-integer value :radix 10)
        default)))

(setup
  (setf *tmpdir* (tmpdir:mkdtemp))
  (setf *sqlite-conn*
        (cl-dbi:connect :sqlite3
                        :database-name (merge-pathnames
                                        (make-pathname :name "cl-migratum" :type "db")
                                                        *tmpdir*)))
  (setf *provider* (cl-migratum.provider.local-path:make-provider (list *migrations-path*)))
  (setf *dbi-driver*
        (migratum.driver.dbi:make-driver *provider* *sqlite-conn*))
  (let ((rdbms-auth
          (list :host (getenv "PGHOST" "localhost")
                :port (getenv-int "PGPORT" 5432)
                :database (getenv "RDBMS_DB" "rdbms")
                :user-name (getenv "RDBMS_USER" "rdbms_user")
                :password (getenv "RDMBS_PASS" "rdbms_p4ss")))
        (postmodern-auth
          (list :host (getenv "PGHOST" "localhost")
                :port (getenv-int "PGPORT" 5432)
                :database (getenv "POSTMODERN_DB" "postmodern")
                :user-name (getenv "POSTMODERN_USER" "postmodern_user")
                :password (getenv "POSTMODERN_PASS" "postmodern_p4ss"))))
    (setf *postmodern-postgresql-driver*
          (migratum.driver.postmodern-postgresql:make-driver *provider* postmodern-auth)
          *rdbms-postgresql-driver*
          (migratum.driver.rdbms-postgresql:make-driver *provider* rdbms-auth))))

(teardown
  (provider-shutdown *provider*)
  (driver-shutdown *dbi-driver*)
  (driver-shutdown *postmodern-postgresql-driver*)
  (when *tmpdir*
    (uiop:delete-directory-tree *tmpdir* :validate t)))
