;; Copyright (c) 2020-2022 Marin Atanasov Nikolov <dnaeon@gmail.com>
;; Copyright (c) 2022 Kambiz Darabi <darabi@m-creations.net>
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
(defpackage :cl-migratum.driver.postmodern-postgresql
  (:use :cl)
  (:nicknames :migratum.driver.postmodern-postgresql)
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
   :driver-register-migration)
  (:export
   :postmodern-postgresql-driver
   :make-driver
   :database-of
   :connection-specification-of))
(in-package :cl-migratum.driver.postmodern-postgresql)

(defparameter *sql-statement-separator*
  "--;;"
  "Separator to use when splitting a migration into multiple statements")

(defparameter *sql-init-schema*
  "
CREATE TABLE IF NOT EXISTS migration (
    id BIGINT PRIMARY KEY,
    kind CHARACTER VARYING(255) NOT NULL,
    description TEXT NOT NULL,
    applied TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);"
  "Schema used by the SQL driver")

(defclass postmodern-postgresql-driver (base-driver migratum.driver.mixins:lisp-driver-mixin)
  ((connection-specification
    :initarg :connection-specification
    :reader connection-specification-of
    :initform (error "Must specify database connection specification")
    :documentation "Connection specification for Postmodern e.g. (:host \"localhost\" :port 5432 :database \"migratum\" :user-name \"migratum_user\" :password \"changeit\" :use-ssl :yes)
    Note that :host defaults to \"localhost\", :port to 5432, and :use-ssl to :no"))
  (:documentation "Driver for performing SQL migrations with Postmodern."))

(defmethod initialize-instance :after ((-self- postmodern-postgresql-driver) &key)
  (destructuring-bind (&key database user-name password (host "localhost")
                         (port 5432) (use-ssl pomo:*default-use-ssl*)
                         (application-name "")
                         use-binary
                       &allow-other-keys)
      (connection-specification-of -self-)
    (pomo:connect-toplevel database user-name password host
                           :port port
                           :use-ssl use-ssl
                           :application-name application-name
                           :use-binary use-binary)))

(defmethod driver-init ((driver postmodern-postgresql-driver) &key)
  (log:debug "[POSTMODERN-PGSQL] Initializing ~A driver" (driver-name driver))
  (pomo:with-transaction ()
      (pomo:query *sql-init-schema*))
  (setf (driver-initialized driver) t))

(defmethod driver-shutdown ((driver postmodern-postgresql-driver) &key)
  (log:debug "[POSTMODERN-PGSQL] Shutting down ~A driver" (driver-name driver))
  (pomo:disconnect-toplevel)
  (setf (driver-initialized driver) nil))

(defmethod driver-list-applied ((driver postmodern-postgresql-driver)
                                &key (offset 0) (limit 100))
  (log:debug "[POSTMODERN-PGSQL] Fetching list of applied migrations")
  (let* ((query (format nil "SELECT * FROM migration ORDER BY id DESC LIMIT ~A OFFSET ~A"
                        limit offset))
         (rows (pomo:query query)))
    (mapcar (lambda (row)
              (destructuring-bind (id kind description applied)
                  row;;might have to clean things 
                (make-instance 'base-migration
                               :id id 
                               :kind (intern kind :keyword)
                               :description description
                               :applied applied)))
            rows)))

(defmethod driver-register-migration ((direction (eql :up))
                                      (driver postmodern-postgresql-driver)
                                      migration &key)
  (log:debug "[POSTMODERN-PGSQL] Registering migration as successful: ~a"
             (migration-id migration))
  (with-accessors ((id migration-id)
                   (description migration-description)
                   (kind migration-kind))
      migration
    (let ((query (format nil "INSERT INTO migration (id, description, kind) ~
                              VALUES (~A, '~A', '~A')" id description kind)))
      (pomo:with-transaction ()
          (pomo:query query)))))

(defmethod driver-register-migration ((direction (eql :down)) (driver postmodern-postgresql-driver)
                                      migration &key)
  (log:debug "[POSTMODERN-PGSQL] Unregistering migration: ~A" (migration-id migration))
    (with-accessors ((id migration-id))
        migration                      
      (let ((query (format nil "DELETE FROM migration WHERE id = ~A" id)))
        (pomo:with-transaction ()
            (pomo:query query)))))

(defmethod driver-apply-migration ((direction (eql :up)) (kind (eql :sql))
                                   (driver postmodern-postgresql-driver) migration &key)
  (%postmodern-postgresql-driver-apply-migration direction driver migration))

(defmethod driver-apply-migration ((direction (eql :down)) (kind (eql :sql))
                                   (driver postmodern-postgresql-driver) migration &key)
  (%postmodern-postgresql-driver-apply-migration direction driver migration))

(defun make-driver (provider connection-specification)
  "Creates a driver for performing migrations against a SQL database

Arguments:

  - provider: an instance of a provider (e.g. local-path)
  - connection-specification: a connection specification as required by hu.dwim.postmodern e.g. (:host \"localhost\" :port 5432 :database \"migratum\" :user-name \"migratum_user\" :password \"changeit\" :use-ssl :yes)
    Note that :host defaults to \"localhost\", :port to 5432, and :use-ssl to :no"
  (make-instance 'postmodern-postgresql-driver
                 :name "POSTMODERN-PG"
                 :provider provider
                 :connection-specification connection-specification))

(defun %postmodern-postgresql-driver-apply-migration (direction driver migration)
  "Applies the script loaded using the migration script loader function"
  (with-accessors ((id migration-id)
                   (description migration-description)
                   (kind migration-kind))
      migration 
    (let* ((content (migration-load direction migration))
           (statements (str:split *sql-statement-separator* content :omit-nulls t)))
      (log:debug "[POSTMODERN-PGSQL] Applying ~A migration: ~A - ~A (~A)"
                 direction id description kind)
      (pomo:with-transaction ()
          (dolist (statement statements)
            (let ((stmt (string-trim #(#\Newline) statement)))
              (pomo:query stmt)))))))
