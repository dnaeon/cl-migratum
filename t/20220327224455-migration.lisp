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
(uiop:define-package :cl-migratum.test.20220327224455
  (:use :cl)
  (:import-from :cl-dbi)
  (:import-from :cl-migratum.driver.dbi)
  (:import-from :cl-migratum.driver.rdbms-postgresql)
  (:export
   :up
   :down))
(in-package :cl-migratum.test.20220327224455)

(defparameter *table-schema*
  (format nil "CREATE TABLE lisp_code_table ( ~
                 id BIGINT PRIMARY KEY, ~
                 name CHARACTER VARYING(255) NOT NULL, ~
                 value BIGINT NOT NULL ~
               )")
  "Schema of the table we are about to create with this migration")

(defun up (driver)
  "Upgrade handler for migration 20220327224455"
  (etypecase driver
    (cl-migratum.driver.dbi:dbi-driver (%dbi-apply-up driver))
    (cl-migratum.driver.rdbms-postgresql:rdbms-postgresql-driver
     (%rdbms-pgsql-apply-up driver))
    (cl-migratum.driver.postmodern-postgresql:postmodern-postgresql-driver
     (%pomo-pgsql-apply-up driver))))

(defun down (driver)
  "Downgrade handler for migration 20220327224455"
  (etypecase driver
    (cl-migratum.driver.dbi:dbi-driver (%dbi-apply-down driver))
    (cl-migratum.driver.rdbms-postgresql:rdbms-postgresql-driver
     (%rdbms-pgsql-apply-down driver))
    (cl-migratum.driver.postmodern-postgresql:postmodern-postgresql-driver
     (%pomo-pgsql-apply-down driver))))

(defun %dbi-apply-up (driver)
  "CL-DBI upgrade handler for migration id 20220327224455"
  (let* ((conn (cl-migratum.driver.dbi:dbi-driver-connection driver))
         (schema *table-schema*)
         (schema-prepared (cl-dbi:prepare conn schema)))
    (cl-dbi:with-transaction conn
      ;; Create the schema and populate it with some data
      (cl-dbi:execute schema-prepared)
      (let* ((query "INSERT INTO lisp_code_table (id, name, value) VALUES (?, ?, ?)")
             (prepared (cl-dbi:prepare conn query)))
        (loop :for i :from 1 :to 42
              :for name = (format nil "name-~A" i)
              :for value = (+ i 42) :do
                (cl-dbi:execute prepared (list i name value)))))))

(defun %dbi-apply-down (driver)
  "CL-DBI downgrade handler for migration id 20220327224455"
  (let* ((conn (cl-migratum.driver.dbi:dbi-driver-connection driver))
         (query "DROP TABLE lisp_code_table")
         (prepared (cl-dbi:prepare conn query)))
    (cl-dbi:with-transaction conn
      (cl-dbi:execute prepared))))

(defun %rdbms-pgsql-apply-up (driver)
  "RMDBS-POSTGRESQL upgrade handler for migration id 20220327224455"
  (let ((db (cl-migratum.driver.rdbms-postgresql:database-of driver))
        (schema *table-schema*)
        (populate-stmt "INSERT INTO lisp_code_table (id, name, value) VALUES (~A, '~A', ~A)"))
    (hu.dwim.rdbms:with-database db
      ;; Create the schema and populate it with some data
      (hu.dwim.rdbms:with-transaction
          (hu.dwim.rdbms:execute schema)
        (loop :for i :from 1 :to 42
              :for name = (format nil "name-~A" i)
              :for value = (+ i 42) :do
                (hu.dwim.rdbms:execute (format nil populate-stmt i name value)))))))

(defun %rdbms-pgsql-apply-down (driver)
  "RMDBS-POSTGRESQL downgrade handler for migration id 20220327224455"
  (let ((db (cl-migratum.driver.rdbms-postgresql:database-of driver))
        (query "DROP TABLE lisp_code_table"))
    (hu.dwim.rdbms:with-database db
      (hu.dwim.rdbms:with-transaction
          (hu.dwim.rdbms:execute query)))))

(defun %pomo-pgsql-apply-up (driver)
  "POMO-POSTGRESQL upgrade handler for migration id 20220327224455"
  (declare (ignore driver))
  (let ((schema *table-schema*)
        (populate-stmt "INSERT INTO lisp_code_table (id, name, value) VALUES (~A, '~A', ~A)"))
    ;; Create the schema and populate it with some data
    (pomo:with-transaction ()
      (pomo:query schema)
      (loop :for i :from 1 :to 42
            :for name = (format nil "name-~A" i)
            :for value = (+ i 42) :do
              (pomo:query (format nil populate-stmt i name value))))))

(defun %pomo-pgsql-apply-down (driver)
  "POMO-POSTGRESQL downgrade handler for migration id 20220327224455"
  (declare (ignore driver))
  (let ((query "DROP TABLE lisp_code_table"))
    (pomo:with-transaction ()
      (pomo:query query))))
