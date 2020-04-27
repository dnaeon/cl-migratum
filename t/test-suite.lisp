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
   :provider-name
   :provider-initialized
   :provider-list-migrations
   :provider-create-migration
   :migration-id
   :migration-description
   :migration-load-up-script
   :migration-load-down-script)
  (:import-from
   :migratum.provider.local-path
   :make-local-path-provider
   :local-path-migration-up-script-path
   :local-path-migration-down-script-path)
  (:import-from
   :migratum.driver.sql
   :make-sql-driver))
(in-package :cl-migratum.test)

(defparameter *tmpdir*
  nil
  "Temp directory used during tests")

(defparameter *sqlite-conn*
  nil
  "CL-DBI connection used during tests")

(defparameter *sql-driver*
  nil
  "SQL driver used during tests")

(defparameter *provider*
  (let* ((test-system (asdf:find-system :cl-migratum.test))
         (test-system-path (slot-value test-system 'asdf/component:absolute-pathname))
         (test-migrations-path (merge-pathnames (make-pathname :directory '(:relative "t" "migrations"))
                                                test-system-path)))
    (make-local-path-provider test-migrations-path))
  "Local path provider used during tests")

(setup
  (setf *tmpdir* (tmpdir:mkdtemp))
  (setf *sqlite-conn*
        (cl-dbi:connect :sqlite3
                        :database-name (merge-pathnames (make-pathname :name "cl-migratum" :type "db")
                                                        *tmpdir*)))
  (setf *sql-driver*
        (make-sql-driver *provider* *sqlite-conn*)))

(teardown
  (when *tmpdir*
    (uiop:delete-directory-tree *tmpdir* :validate t)))

(deftest local-path-provider
  (testing "provider-name"
    (ok (string= "local-path" (provider-name *provider*))))

  (testing "provider-init"
    (ok (eq t (provider-init *provider*))))

  (testing "provider-initialized"
    (ok (eq t (provider-initialized *provider*))))

  (testing "provider-list-migrations"
    (let* ((discovered (provider-list-migrations *provider*))
           (migrations (sort discovered #'< :key #'migration-id)))
      (ok (= 3 (length migrations)))
      (ok (equal (list 20200421173657 20200421173908 20200421180337)
                 (mapcar #'migration-id migrations)))
      (ok (equal (list "create-table-foo" "create-table-bar" "create-table-qux")
                 (mapcar #'migration-description migrations)))))

  (testing "provider-create-migration"
    (let* ((migration (provider-create-migration *provider*
						 :description "my-new-migration"
						 :up "CREATE TABLE cl_migratum_test (id INTEGER PRIMARY KEY);"
						 :down "DROP TABLE cl_migratum_test;")))
      (ok (numberp (migration-id migration)))
      (ok (string= "my-new-migration" (migration-description migration)))
      (ok (string= "CREATE TABLE cl_migratum_test (id INTEGER PRIMARY KEY);"
                   (migration-load-up-script migration)))
      (ok (string= "DROP TABLE cl_migratum_test;"
                   (migration-load-down-script migration)))
      (uiop:delete-file-if-exists (local-path-migration-up-script-path migration))
      (uiop:delete-file-if-exists (local-path-migration-down-script-path migration)))))
