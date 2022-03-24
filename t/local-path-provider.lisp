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
      (ok (equal (list 20200421173657 20200421173908 20200421180000 20200421180337 20200605144633)
                 (mapcar #'migration-id migrations))
          "identifiers of migrations matches")
      (ok (equal (list "create_table_foo" "create_table_bar" "create_table_baz" "create_table_qux" "multiple_statements")
                 (mapcar #'migration-description migrations))
          "description of migrations matches")))

  (testing "provider-find-migration-by-id"
    (ok (provider-find-migration-by-id *provider* 20200421173657)
        "find existing migration by id")
    (ng (provider-find-migration-by-id *provider* 'no-such-id)
        "find non-existing migration"))

  (testing "provider-create-sql-migration"
    (let* ((migration (provider-create-migration *provider*
                                                 :description "my-new-migration"
                                                 :up "CREATE TABLE cl_migratum_test (id INTEGER PRIMARY KEY);"
                                                 :down "DROP TABLE cl_migratum_test;")))
      (ok (numberp (migration-id migration))
          "migration id is a number")
      (ok (string= "my_new_migration" (migration-description migration))
          "migration description matches")
      (ok (string= "CREATE TABLE cl_migratum_test (id INTEGER PRIMARY KEY);"
                   (migration-load migration :up :cl-migratum.test))
          "upgrade script matches")
      (ok (string= "DROP TABLE cl_migratum_test;"
                   (migration-load migration :down :cl-migratum.test))
          "downgrade script matches")
      (uiop:delete-file-if-exists (local-path-migration-up-script-path migration))
      (uiop:delete-file-if-exists (local-path-migration-down-script-path migration))))

  (testing "provider-create-functional-migration-happy-path"
    (let* ((migration (provider-create-migration *provider*
                                                 :description "my-functional-migration"
                                                 :up "(:function create-table-corge)"
                                                 :down "(:function drop-table-corge)"
                                                 :type :sexp)))
      (ok (numberp (migration-id migration))
          "migration id is a number")
      (ok (string= "my_functional_migration" (migration-description migration))
          "migration description matches")
      (ok (eq :sexp (local-path-migration-up-script-type migration))
          "type slot of functional migration upgrade script is :sexp")
      (ok (eq :sexp (local-path-migration-down-script-type migration))
          "type slot of functional migration downgrade script is :sexp")
      (ok (eq #'create-table-corge (migration-load migration :up :cl-migratum.test))
          "upgrade script matches")
      (ok (eq #'drop-table-corge (migration-load migration :down :cl-migratum.test))
          "downgrade script matches")
      (uiop:delete-file-if-exists (local-path-migration-up-script-path migration))
      (uiop:delete-file-if-exists (local-path-migration-down-script-path migration))))

  (testing "provider-create-functional-migration-errors"
    (ok (signals (provider-create-migration *provider*
                                            :description "my-functional-migration"
                                            :up "(:function create-table-corge)"
                                            :down "(:function drop-table-corge)"
                                            :type :something-else))
        "a migration type other that :sql or :sexp signals an error")
    (ok (signals (provider-create-migration *provider*
                                            :description "my-functional-migration"
                                            :up "(:function cl:print)"
                                            :down "(:function drop-table-corge)"
                                            :type :sexp))
        "upgrade function in the wrong package signals an error")
    (ok (signals (provider-create-migration *provider*
                                            :description "my-functional-migration"
                                            :up "(:function create-table-corge)"
                                            :down "(:function cl:print)"
                                            :type :sexp))
        "downgrade function in the wrong package signals an error")
    (ok (signals (provider-create-migration *provider*
                                            :description "my-functional-migration"
                                            :up (format nil "~{:~a~^ ~}" (loop for i from 1 upto 1000 collect i))
                                            :down "(:function drop-table-corge)"
                                            :type :sexp))
        "long upgrade function s-expression signals an error")
    (ok (signals (provider-create-migration *provider*
                                            :description "my-functional-migration"
                                            :up "(:function create-table-corge)"
                                            :down (format nil "~{:~a~^ ~}" (loop for i from 1 upto 1000 collect i))
                                            :type :sexp))
        "long downgrade function s-expression signals an error")))
