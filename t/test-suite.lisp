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
   :migration-load-up-script
   :migration-load-down-script
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
   :local-path-migration-down-script-path)
  (:import-from
   :migratum.driver.dbi
   :make-driver))
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

(defparameter *driver*
  nil
  "DBI driver used during tests")

(defparameter *provider*
  nil
  "Local path provider used during tests")

(setup
  (setf *tmpdir* (tmpdir:mkdtemp))
  (setf *sqlite-conn*
        (cl-dbi:connect :sqlite3
                        :database-name (merge-pathnames (make-pathname :name "cl-migratum" :type "db")
                                                        *tmpdir*)))
  (setf *provider* (make-local-path-provider (list *migrations-path*)))
  (setf *driver*
        (make-driver *provider* *sqlite-conn*)))

(teardown
  (provider-shutdown *provider*)
  (driver-shutdown *driver*)
  (when *tmpdir*
    (uiop:delete-directory-tree *tmpdir* :validate t)))

(deftest local-path-provider
  (testing "provider-name"
    (ok (string= "local-path" (provider-name *provider*))
        "provider name matches"))

  (testing "provider-init"
    (ok (provider-init *provider*)
        "initialize provider"))

  (testing "provider-initialized"
    (ok (provider-initialized *provider*)
        "provider is initialized"))

  (testing "provider-list-migrations"
    (let* ((discovered (provider-list-migrations *provider*))
           (migrations (sort discovered #'< :key #'migration-id)))
      (ok (= 4 (length migrations)) "number of migrations matches")
      (ok (equal (list 20200421173657 20200421173908 20200421180337 20200605144633)
                 (mapcar #'migration-id migrations))
          "identifiers of migrations matches")
      (ok (equal (list "create_table_foo" "create_table_bar" "create_table_qux" "multiple_statements")
                 (mapcar #'migration-description migrations))
          "description of migrations matches")))

  (testing "provider-find-migration-by-id"
    (ok (provider-find-migration-by-id *provider* 20200421173657)
        "find existing migration by id")
    (ng (provider-find-migration-by-id *provider* 'no-such-id)
        "find non-existing migration"))

  (testing "provider-create-migration"
    (let* ((migration (provider-create-migration *provider*
                                                 :description "my-new-migration"
                                                 :up "CREATE TABLE cl_migratum_test (id INTEGER PRIMARY KEY);"
                                                 :down "DROP TABLE cl_migratum_test;")))
      (ok (numberp (migration-id migration))
          "migration id is a number")
      (ok (string= "my_new_migration" (migration-description migration))
          "migration description matches")
      (ok (string= "CREATE TABLE cl_migratum_test (id INTEGER PRIMARY KEY);"
                   (migration-load-up-script migration))
          "upgrade script matches")
      (ok (string= "DROP TABLE cl_migratum_test;"
                   (migration-load-down-script migration))
          "downgrade script matches")
      (uiop:delete-file-if-exists (local-path-migration-up-script-path migration))
      (uiop:delete-file-if-exists (local-path-migration-down-script-path migration)))))

(deftest dbi-driver
  (testing "driver-name"
    (ok (string= "DBI" (driver-name *driver*)) "driver name matches"))

  (testing "driver-initialized"
    (ok (eq nil (driver-initialized *driver*)) "driver is not yet initialized"))

  (testing "driver-init"
    (ok (eq t (driver-init *driver*)) "initialize driver")
    (ok (eq t (driver-initialized *driver*)) "driver is initialized"))

  (testing "driver-list-applied"
    (ng (driver-list-applied *driver*) "no migrations have been applied yet"))

  (testing "contains-applied-migrations-p"
    (ok (eq nil (contains-applied-migrations-p *driver*)) "does not contain applied migrations"))

  (testing "list-pending"
    (let ((pending (list-pending *driver*)))
      (ok (= 4 (length pending)) "number of pending migrations matches")
      (ok (equal (list 20200421173657 20200421173908 20200421180337 20200605144633)
                 (mapcar #'migration-id pending))
          "identifiers of pending migrations matches")))

  (testing "apply-pending"
    (apply-pending *driver*)
    (let ((applied (driver-list-applied *driver*)))
      (ok (= 4 (length applied)) "number of applied migrations matches")
      (ok (equal (list 20200605144633 20200421180337 20200421173908 20200421173657)
                 (mapcar #'migration-id applied))
          "identifiers of applied migrations matches")))

  (testing "pagination"
    (ok (= 1 (length (driver-list-applied *driver* :offset 0 :limit 1)))
        "page with :offset 0 :limit 1")
    (ok (= 1 (length (driver-list-applied *driver* :offset 1 :limit 1)))
        "page with :offset 1 :limit 1")
    (ok (= 2 (length (driver-list-applied *driver* :offset 1 :limit 2)))
        "page with :offset 1 :limit 2")
    (ng (driver-list-applied *driver* :offset 100 :limit 100)
        "page with :offset 100 :limit 100"))

  (testing "latest-migration"
    (ok (= 20200605144633 (migration-id (latest-migration *driver*)))
        "latest migration id matches"))

  (testing "revert-last"
    (revert-last *driver* :count 4)
    (ng (contains-applied-migrations-p *driver*)
        "no migrations present after revert")
    (ng (driver-list-applied *driver*)
        "no migrations applied after revert"))

  (testing "apply-next"
    (ng (contains-applied-migrations-p *driver*)
        "no migrations present yet")
    (apply-next *driver*)
    (ok (= 20200421173657 (migration-id (latest-migration *driver*)))
        "id matches next applied migration")
    (apply-next *driver*)
    (ok (= 20200421173908 (migration-id (latest-migration *driver*)))
        "id matches next applied migration")
    (apply-next *driver*)
    (ok (= 20200421180337 (migration-id (latest-migration *driver*)))
        "id matches next applied migration")
    (apply-next *driver*)
    (ok (= 20200605144633 (migration-id (latest-migration *driver*)))
        "id matches next applied migration")
    (apply-next *driver*) ;; No more pending migrations at this point
    (ok (= 20200605144633 (migration-id (latest-migration *driver*)))
        "id of last migration is the same"))) ;; ID did not change, since previous migration
