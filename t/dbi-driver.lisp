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

(in-package :cl-migratum.test)

(deftest dbi-driver
  (testing "driver-name"
    (ok (string= "DBI" (driver-name *dbi-driver*)) "driver name matches"))

  (testing "driver-initialized"
    (ok (eq nil (driver-initialized *dbi-driver*)) "driver is not yet initialized"))

  (testing "driver-init"
    (ok (eq t (driver-init *dbi-driver*)) "initialize driver")
    (ok (eq t (driver-initialized *dbi-driver*)) "driver is initialized"))

  (testing "driver-list-applied"
    (ng (driver-list-applied *dbi-driver*) "no migrations have been applied yet"))

  (testing "contains-applied-migrations-p"
    (ok (eq nil (contains-applied-migrations-p *dbi-driver*)) "does not contain applied migrations"))

  (testing "list-pending"
    (let ((pending (list-pending *dbi-driver*)))
      (ok (= 5 (length pending)) "number of pending migrations matches")
      (ok (equal (list 20200421173657 20200421173908 20200421180000 20200421180337 20200605144633)
                 (mapcar #'migration-id pending))
          "identifiers of pending migrations matches")))

  (testing "apply-pending"
    (apply-pending *dbi-driver*)
    (let ((applied (driver-list-applied *dbi-driver*)))
      (ok (= 5 (length applied)) "number of applied migrations matches")
      (ok (equal (list 20200605144633 20200421180337 20200421180000 20200421173908 20200421173657)
                 (mapcar #'migration-id applied))
          "identifiers of applied migrations matches")))

  (testing "pagination"
    (ok (= 1 (length (driver-list-applied *dbi-driver* :offset 0 :limit 1)))
        "page with :offset 0 :limit 1")
    (ok (= 1 (length (driver-list-applied *dbi-driver* :offset 1 :limit 1)))
        "page with :offset 1 :limit 1")
    (ok (= 2 (length (driver-list-applied *dbi-driver* :offset 1 :limit 2)))
        "page with :offset 1 :limit 2")
    (ng (driver-list-applied *dbi-driver* :offset 100 :limit 100)
        "page with :offset 100 :limit 100"))

  (testing "latest-migration"
    (ok (= 20200605144633 (migration-id (latest-migration *dbi-driver*)))
        "latest migration id matches"))

  (testing "revert-last"
    (revert-last *dbi-driver* :count 5)
    (ng (contains-applied-migrations-p *dbi-driver*)
        "no migrations present after revert")
    (ng (driver-list-applied *dbi-driver*)
        "no migrations applied after revert"))

  (testing "apply-next"
    (ng (contains-applied-migrations-p *dbi-driver*)
        "no migrations present yet")
    (apply-next *dbi-driver*)
    (ok (= 20200421173657 (migration-id (latest-migration *dbi-driver*)))
        "id matches next applied migration")
    (apply-next *dbi-driver*)
    (ok (= 20200421173908 (migration-id (latest-migration *dbi-driver*)))
        "id matches next applied migration")
    (apply-next *dbi-driver*)
    (ok (= 20200421180000 (migration-id (latest-migration *dbi-driver*)))
        "id matches next applied migration")
    (apply-next *dbi-driver*)
    (ok (= 20200421180337 (migration-id (latest-migration *dbi-driver*)))
        "id matches next applied migration")
    (apply-next *dbi-driver*)
    (ok (= 20200605144633 (migration-id (latest-migration *dbi-driver*)))
        "id matches next applied migration")
    (apply-next *dbi-driver*) ;; No more pending migrations at this point
    (ok (= 20200605144633 (migration-id (latest-migration *dbi-driver*)))
        "id of last migration is the same"))

  (testing "helper-functions"
    (let ((conn (migratum.driver.dbi::dbi-driver-connection  *dbi-driver*)))
      (ok (signals (migratum.driver.dbi::apply-sql-migration #'create-table-baz conn))
          "calling apply-sql-migration with a function instead of a string signals an error")
      (ok (signals (migratum.driver.dbi::apply-functional-migration "drop table baz" conn))
          "calling apply-functional-migration with a string instead of a function signals an error"))))
