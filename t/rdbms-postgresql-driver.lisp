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

(in-package :cl-migratum.test)

(deftest rdbms-postgresql-driver
  (testing "driver-name"
    (ok (string= "RDBMS-PG" (driver-name *rdbms-postgresql-driver*)) "driver name matches"))

  (testing "driver-initialized"
    (ok (eq nil (driver-initialized *rdbms-postgresql-driver*)) "driver is not yet initialized"))

  (testing "driver-init"
    (ok (eq t (driver-init *rdbms-postgresql-driver*)) "initialize driver")
    (ok (eq t (driver-initialized *rdbms-postgresql-driver*)) "driver is initialized"))

  (testing "driver-list-applied"
    (ng (driver-list-applied *rdbms-postgresql-driver*) "no migrations have been applied yet"))

  (testing "contains-applied-migrations-p"
    (ok (eq nil (contains-applied-migrations-p *rdbms-postgresql-driver*)) "does not contain applied migrations"))

  (testing "list-pending"
    (let ((pending (list-pending *rdbms-postgresql-driver*)))
      (ok (= 5 (length pending)) "number of pending migrations matches")
      (ok (equal (list 20200421173657   ;; SQL
                       20200421173908   ;; SQL
                       20200421180337   ;; SQL
                       20200605144633   ;; SQL
                       20220327224455)  ;; Lisp
                 (mapcar #'migration-id pending))
          "identifiers of pending migrations matches")
      (ok (equal (list :sql :sql :sql :sql :lisp)
                 (mapcar #'migration-kind pending))
          "migration kinds match")))

  (testing "apply-pending"
    (apply-pending *rdbms-postgresql-driver*)
    (let ((applied (driver-list-applied *rdbms-postgresql-driver*)))
      (ok (= 5 (length applied)) "number of applied migrations matches")
      (ok (equal (list  20220327224455   ;; Lisp
                        20200605144633   ;; SQL
                        20200421180337   ;; SQL
                        20200421173908   ;; SQL
                        20200421173657)  ;; SQL
                 (mapcar #'migration-id applied))
          "identifiers of applied migrations matches")
      (ok (equal (list :lisp :sql :sql :sql :sql)
                 (mapcar #'migration-kind applied))
          "migration kinds match")))

  (testing "pagination"
    (ok (= 1 (length (driver-list-applied *rdbms-postgresql-driver* :offset 0 :limit 1)))
        "page with :offset 0 :limit 1")
    (ok (= 1 (length (driver-list-applied *rdbms-postgresql-driver* :offset 1 :limit 1)))
        "page with :offset 1 :limit 1")
    (ok (= 2 (length (driver-list-applied *rdbms-postgresql-driver* :offset 1 :limit 2)))
        "page with :offset 1 :limit 2")

    ;; We don't have that many test migrations, but if we do -- make
    ;; sure to adjust the numbers.
    (ng (driver-list-applied *rdbms-postgresql-driver* :offset 100 :limit 100)
        "page with :offset 100 :limit 100"))

  (testing "latest-migration"
    (ok (= 20220327224455 (migration-id (latest-migration *rdbms-postgresql-driver*)))
        "latest migration id matches"))

  (testing "revert-last"
    (revert-last *rdbms-postgresql-driver* :count 5)
    (ng (contains-applied-migrations-p *rdbms-postgresql-driver*)
        "no migrations present after revert")
    (ng (driver-list-applied *rdbms-postgresql-driver*)
        "no migrations applied after revert"))

  (testing "apply-next"
    (ng (contains-applied-migrations-p *rdbms-postgresql-driver*)
        "no migrations present yet")
    (apply-next *rdbms-postgresql-driver*)
    (ok (= 20200421173657 (migration-id (latest-migration *rdbms-postgresql-driver*)))
        "id matches next applied migration")
    (apply-next *rdbms-postgresql-driver*)
    (ok (= 20200421173908 (migration-id (latest-migration *rdbms-postgresql-driver*)))
        "id matches next applied migration")
    (apply-next *rdbms-postgresql-driver*)
    (ok (= 20200421180337 (migration-id (latest-migration *rdbms-postgresql-driver*)))
        "id matches next applied migration")
    (apply-next *rdbms-postgresql-driver*)
    (ok (= 20200605144633 (migration-id (latest-migration *rdbms-postgresql-driver*)))
        "id matches next applied migration")
    (apply-next *rdbms-postgresql-driver*)
    (ok (= 20220327224455 (migration-id (latest-migration *dbi-driver*)))
        "id matches next applied migration")

    ;; No more pending migrations at this point
    ;; ID did not change, since last migration
    (apply-next *rdbms-postgresql-driver*)
    (ok (= 20220327224455 (migration-id (latest-migration *rdbms-postgresql-driver*)))
        "id of last migration is the same")))
