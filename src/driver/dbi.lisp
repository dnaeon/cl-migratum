;; Copyright (c) 2020 Marin Atanasov Nikolov <dnaeon@gmail.com>
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
(defpackage :cl-migratum.driver.dbi
  (:use :cl)
  (:nicknames :migratum.driver.dbi)
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
  (:import-from :cl-dbi)
  (:import-from :cl-ppcre)
  (:export
   :dbi-driver
   :make-driver))
(in-package :cl-migratum.driver.dbi)

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

(defclass dbi-driver (base-driver)
  ((connection
    :initarg :connection
    :accessor dbi-driver-connection
    :initform (error "Must specify database connection")
    :documentation "CL-DBI connection to use"))
  (:documentation "Driver for performing SQL migrations"))

(defmethod driver-init ((driver dbi-driver) &key)
  (log:debug "[CL-DBI] Initializing ~a driver" (driver-name driver))
  (let* ((connection (dbi-driver-connection driver))
         (query (cl-dbi:prepare connection *sql-init-schema*)))
    (cl-dbi:with-transaction connection
      (cl-dbi:execute query))
    (setf (driver-initialized driver) t)))

(defmethod driver-shutdown ((driver dbi-driver) &key)
  (log:debug "[CL-DBI] Shutting down ~a driver" (driver-name driver))
  (let ((connection (dbi-driver-connection driver)))
    (cl-dbi:disconnect connection))
  (setf (driver-initialized driver) nil))

(defmethod driver-list-applied ((driver dbi-driver) &key (offset 0) (limit 100))
  (log:debug "[CL-DBI] Fetching list of applied migrations")
  (let* ((connection (dbi-driver-connection driver))
         (query (cl-dbi:prepare connection "SELECT * FROM migration ORDER BY id DESC LIMIT ? OFFSET ?"))
         (result (cl-dbi:execute query (list limit offset)))
         (rows (cl-dbi:fetch-all result)))
    (mapcar (lambda (row)
              (make-instance 'base-migration
                             :id (getf row :|id|)
                             :kind (intern (getf row :|kind|) :keyword)
                             :description (getf row :|description|)
                             :applied (getf row :|applied|)))
            rows)))

(defmethod driver-register-migration ((driver dbi-driver) migration &key)
  (log:debug "[CL-DBI] Registering migration as successful: ~A" (migration-id migration))
  (let* ((connection (dbi-driver-connection driver))
         (id (migration-id migration))
         (description (migration-description migration))
         (kind (string (migration-kind migration)))
         (query (cl-dbi:prepare connection "INSERT INTO migration (id, description, kind) VALUES (?, ?, ?)")))
    (cl-dbi:with-transaction connection
      (cl-dbi:execute query (list id description kind)))))

(defmethod driver-unregister-migration ((driver dbi-driver) migration &key)
  (log:debug "[CL-DBI] Unregistering migration: ~a" (migration-id migration))
  (let* ((connection (dbi-driver-connection driver))
         (id (migration-id migration))
         (query (cl-dbi:prepare connection "DELETE FROM migration WHERE id = ?")))
    (cl-dbi:with-transaction connection
      (cl-dbi:execute query (list id)))))

(defun %dbi-driver-apply-sql-migration (direction driver migration)
  "Applies a SQL migration in the given direction"
  (let* ((id (migration-id migration))
         (description (migration-description migration))
         (connection (dbi-driver-connection driver))
         (content (migration-load direction migration))
         (statements (cl-ppcre:split *sql-statement-separator* content)))
    (log:debug "[CL-DBI] Applying ~A migration: ~a - ~a" direction id description)
    (cl-dbi:with-transaction connection
      (dolist (statement statements)
        (let ((stmt (cl-dbi:prepare connection (string-trim #(#\Newline) statement))))
          (cl-dbi:execute stmt))))))

(defmethod driver-apply-migration ((direction (eql :up)) (kind (eql :sql))
                                   (driver dbi-driver) migration &key)
  (%dbi-driver-apply-sql-migration direction driver migration))

(defmethod driver-apply-migration ((direction (eql :down)) (kind (eql :sql))
                                   (driver dbi-driver) migration &key)
  (%dbi-driver-apply-sql-migration direction driver migration))

(defun make-driver (provider connection)
  "Creates a driver for performing migrations against a SQL database"
  (make-instance 'dbi-driver
                 :name "DBI"
                 :provider provider
                 :connection connection))

