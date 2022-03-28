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

(defun test-up-handler (driver)
  "Test upgrade handler"
  (declare (ignore driver))
  :performing-upgrade)

(defun test-down-handler (driver)
  "Test downgrade handler"
  (declare (ignore driver))
  :performing-downgrade)

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
      (ok (= 5 (length migrations)) "number of migrations matches")
      (ok (equal (list 20200421173657 20200421173908 20200421180337 20200605144633 20220327224455)
                 (mapcar #'migration-id migrations))
          "identifiers of migrations matches")
      (ok (equal (list :sql :sql :sql :sql :lisp)
                 (mapcar #'migration-kind migrations))
          "migration kinds match")
      (ok (equal (list "create_table_foo" "create_table_bar"
                       "create_table_qux" "multiple_statements"
                       "lisp_code_migration")
                 (mapcar #'migration-description migrations))
          "description of migrations matches")))

  (testing "find-migration-by-id"
    (ok (find-migration-by-id *provider* 20200421173657)
        "find existing migration by id")
    (ng (find-migration-by-id *provider* 'no-such-id)
        "find non-existing migration"))

  (testing "provider-create-migration - :sql kind"
    (let* ((description "my-new-sql-migration")
           (normalized-description "my_new_sql_migration")
           (id (make-migration-id))
           (up (provider-create-migration :up :sql *provider* id description
                                          "CREATE TABLE cl_migratum_test (id INTEGER PRIMARY KEY);"))
           (down (provider-create-migration :down :sql *provider* id description
                                            "DROP TABLE cl_migratum_test;")))
      (ok (and (numberp (migration-id up)) (numberp (migration-id down)))
          "migration id is a number")
      (ok (and (equal :sql (migration-kind up)) (equal :sql (migration-kind down)))
          "migration kinds match")
      (ok (and (string= normalized-description (migration-description up))
               (string= normalized-description (migration-description down)))
          "migration description matches")
      (ok (string= (migration-load :up up)
                   (format nil "-- id: ~A~
                                ~&-- direction: UP~
                                ~&-- description: ~A~
                                ~2&CREATE TABLE cl_migratum_test (id INTEGER PRIMARY KEY);~%"
                           id normalized-description))
          "upgrade script matches")
      (ok (string= (migration-load :down down)
                   (format nil "-- id: ~A~
                                ~&-- direction: DOWN~
                                ~&-- description: ~A~
                                ~2&DROP TABLE cl_migratum_test;~%"
                           id normalized-description))
          "downgrade script matches")
      (uiop:delete-file-if-exists (cl-migratum.provider.local-path:migration-up-script-path up))
      (uiop:delete-file-if-exists (cl-migratum.provider.local-path:migration-down-script-path down))))

  (testing "provider-create-migration - :lisp kind"
    (let* ((description "my-new-lisp-migration")
           (normalized-description "my_new_lisp_migration")
           (id (make-migration-id))
           (up-spec '(:system :cl-migratum.test :package :cl-migratum.test :handler :test-up-handler))
           (down-spec '(:system :cl-migratum.test :package :cl-migratum.test :handler :test-down-handler))
           (up (provider-create-migration :up :lisp *provider* id description up-spec))
           (down (provider-create-migration :down :lisp *provider* id description down-spec)))
      (ok (and (numberp (migration-id up)) (numberp (migration-id down)))
          "migration id is a number")
      (ok (and (equal :lisp (migration-kind up)) (equal :lisp (migration-kind down)))
          "migration kinds match")
      (ok (and (string= normalized-description (migration-description up))
               (string= normalized-description (migration-description down)))
          "migration description matches")
      (ok (functionp (migration-load :up up))
          "migration-load :up returns a function")
      (ok (functionp (migration-load :down down))
          "migration-load :down returns a function")
      (uiop:delete-file-if-exists (cl-migratum.provider.local-path:migration-up-script-path up))
      (uiop:delete-file-if-exists (cl-migratum.provider.local-path:migration-down-script-path down))))

  (testing "provider-create-migration - invalid :lisp spec"
    (let* ((description "my-invalid-lisp-migration")
           (id (make-migration-id)))
      (ok (signals (provider-create-migration :up :lisp *provider* id description nil))
          "Signals on invalid spec - 1")
      (ok (signals (provider-create-migration :up :lisp *provider* id description '(:foo :bar)))
          "Signals on invalid spec - 2")
      (ok (signals (provider-create-migration :up :lisp *provider* id description '(:system :foo)))
          "Signals on invalid spec - 3")
      (ok (signals (provider-create-migration :up :lisp *provider* id description '(:system :foo :package :bar)))
          "Signals on invalid spec - 4"))))
