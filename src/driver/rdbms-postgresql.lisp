;; Copyright (c) 2020-2022 Marin Atanasov Nikolov <dnaeon@gmail.com>, Kambiz Darabi <darabi@m-creations.net>
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
(defpackage :cl-migratum.driver.rdbms-postgresql
  (:use :cl)
  (:nicknames :migratum.driver.rdbms-postgresql)
  (:import-from
   :cl-migratum
   :base-migration
   :migration-id
   :migration-kind
   :migration-description
   :migration-load
   :base-driver
   :driver-name
   :driver-provider
   :driver-init
   :driver-shutdown
   :driver-initialized
   :driver-list-applied
   :driver-apply-migration
   :driver-register-migration
   :driver-unregister-migration)
  (:import-from :log)
  (:import-from :hu.dwim.logger
   :+warn+
   :set-log-level)
  (:import-from :hu.dwim.rdbms.postgresql)
  (:import-from :cl-ppcre)
  (:export
   :rdbms-postgresql-driver
   :make-driver))
(in-package :cl-migratum.driver.rdbms-postgresql)

(defparameter *sql-statement-separator*
  "--;;"
  "Separator to use when splitting a migration into multiple statements")

(defparameter *sql-init-schema*
  "
CREATE TABLE IF NOT EXISTS migration (
    id BIGINT PRIMARY KEY,
    kind CHARACTER VARYING(255) NOT NULL,
    description CHARACTER VARYING(4096) NOT NULL,
    applied TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);"
  "Schema used by the SQL driver")

(defclass rdbms-postgresql-driver (base-driver)
  ((database
    :initarg :database
    :accessor database-of
    :initform nil
    :documentation "hu.dwim.rdbms database to use")
   (connection-specification
    :initarg :connection-specification
    :accessor connection-specification-of
    :initform (error "Must specify database connection specification")
    :documentation "Connection specification for hu.dwim.rdbms e.g. (:host \"localhost\" :port 5432 :database \"migratum\" :user-name \"migratum_user\" :password \"changeit\" :use-ssl :yes)
    Note that :host defaults to \"localhost\", :port to 5432, and :use-ssl to :no"))
  (:documentation "Driver for performing SQL migrations with hu.dwim.rdbms.postgresql"))

(defmethod initialize-instance :after ((-self- rdbms-postgresql-driver) &key)
  (with-slots ((connspec connection-specification) (db database)) -self-
    (setf db (make-instance 'hu.dwim.rdbms.postgresql:postgresql :connection-specification connspec))
    (set-log-level 'hu.dwim.rdbms::rdbms +warn+)))

(defmethod driver-init ((driver rdbms-postgresql-driver) &key)
  (log:debug "[RDBMS-PGSQL] Initializing ~A driver" (driver-name driver))
  (let* ((database (database-of driver))
         (query *sql-init-schema*))
    (hu.dwim.rdbms:with-database database
      (hu.dwim.rdbms:with-transaction
        (hu.dwim.rdbms:execute query)))
    (setf (driver-initialized driver) t)))

(defmethod driver-shutdown ((driver rdbms-postgresql-driver) &key)
  (log:debug "[RDBMS-PGSQL] Shutting down ~A driver" (driver-name driver))
  (setf (driver-initialized driver) nil))

(defmethod driver-list-applied ((driver rdbms-postgresql-driver) &key (offset 0) (limit 100))
  (log:debug "[RDBMS-PGSQL] Fetching list of applied migrations")
  (let* ((db (database-of driver))
         (query (format nil "SELECT * FROM migration ORDER BY id DESC LIMIT ~A OFFSET ~A" limit offset))
         (rows (hu.dwim.rdbms:with-database db (hu.dwim.rdbms:with-transaction (hu.dwim.rdbms:execute query :result-type 'list)))))
    (mapcar (lambda (row)
              (make-instance 'base-migration
                             :id (nth 0 row)
                             :kind (intern (nth 1 row) :keyword)
                             :description (nth 2 row)
                             :applied (nth 3 row)))
            rows)))

(defmethod driver-register-migration ((driver rdbms-postgresql-driver) (migration base-migration) &key)
  (log:debug "[RDBMS-PGSQL] Registering migration as successful: ~a" (migration-id migration))
  (let* ((db (database-of driver))
         (id (migration-id migration))
         (description (migration-description migration))
         (kind (string (migration-kind migration)))
         (query (format nil "INSERT INTO migration (id, description, kind) VALUES (~A, '~A', '~A')" id description kind)))
    (hu.dwim.rdbms:with-database db
      (hu.dwim.rdbms:with-transaction
        (hu.dwim.rdbms:execute query)))))

(defmethod driver-unregister-migration ((driver rdbms-postgresql-driver) (migration base-migration) &key)
  (log:debug "[RDBMS-PGSQL] Unregistering migration: ~A" (migration-id migration))
  (let* ((db (database-of driver))
         (id (migration-id migration))
         (query (format nil "DELETE FROM migration WHERE id = ~A" id)))
    (hu.dwim.rdbms:with-database db
      (hu.dwim.rdbms:with-transaction
        (hu.dwim.rdbms:execute query)))))

(defmethod driver-apply-migration ((direction (eql :up)) (kind (eql :sql))
                                   (driver rdbms-postgresql-driver) migration &key)
  (%rdbms-postgresql-driver-apply-migration direction driver migration))

(defmethod driver-apply-migration ((direction (eql :down)) (kind (eql :sql))
                                   (driver rdbms-postgresql-driver) migration &key)
  (%rdbms-postgresql-driver-apply-migration direction driver migration))

(defun make-driver (provider connection-specification)
  "Creates a driver for performing migrations against a SQL database

Arguments:

  - provider: an instance of a provider (e.g. local-path)
  - connection-specification: a connection specification as required by hu.dwim.rdbms e.g. (:host \"localhost\" :port 5432 :database \"migratum\" :user-name \"migratum_user\" :password \"changeit\" :use-ssl :yes)
    Note that :host defaults to \"localhost\", :port to 5432, and :use-ssl to :no"
  (make-instance 'rdbms-postgresql-driver
                 :name "RDBMS-PG"
                 :provider provider
                 :connection-specification connection-specification))

(defun %rdbms-postgresql-driver-apply-migration (direction driver migration)
  "Applies the script loaded using the migration script loader function"
  (let* ((id (migration-id migration))
         (kind (migration-kind migration))
         (description (migration-description migration))
         (db (database-of driver))
         (content (migration-load direction migration))
         (statements (cl-ppcre:split *sql-statement-separator* content)))
    (log:debug "[RDBMS-PGSQL] Applying ~A migration: ~A - ~A (~A)" direction id description kind)
    (hu.dwim.rdbms:with-database db
      (hu.dwim.rdbms:with-transaction
        (dolist (statement statements)
          (let ((stmt (string-trim #(#\Newline) statement)))
            (hu.dwim.rdbms:execute stmt)))))))
