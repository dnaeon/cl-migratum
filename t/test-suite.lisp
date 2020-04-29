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
   :provider-find-migration-by-id
   :migration-id
   :migration-description
   :migration-load-up-script
   :migration-load-down-script
   :driver-name
   :driver-init
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
   :migratum.driver.sql
   :make-sql-driver))
(in-package :cl-migratum.test)

(defparameter *tmpdir*
  nil
  "Temp directory used during tests")

(defparameter *sqlite-conn*
  nil
  "CL-DBI connection used during tests")

(defparameter *driver*
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
  (setf *driver*
        (make-sql-driver *provider* *sqlite-conn*)))

(teardown
  (when *tmpdir*
    (uiop:delete-directory-tree *tmpdir* :validate t)))

(deftest local-path-provider
  (testing "provider-name"
    (ok (string= "local-path" (provider-name *provider*))))

  (testing "provider-init"
    (ok (provider-init *provider*)))

  (testing "provider-initialized"
    (ok (provider-initialized *provider*)))

  (testing "provider-list-migrations"
    (let* ((discovered (provider-list-migrations *provider*))
           (migrations (sort discovered #'< :key #'migration-id)))
      (ok (= 3 (length migrations)))
      (ok (equal (list 20200421173657 20200421173908 20200421180337)
                 (mapcar #'migration-id migrations)))
      (ok (equal (list "create_table_foo" "create_table_bar" "create_table_qux")
                 (mapcar #'migration-description migrations)))))

  (testing "provider-find-migration-by-id"
    (ok (provider-find-migration-by-id *provider* 20200421173657))
    (ng (provider-find-migration-by-id *provider* 'no-such-id)))

  (testing "provider-create-migration"
    (let* ((migration (provider-create-migration *provider*
						 :description "my-new-migration"
						 :up "CREATE TABLE cl_migratum_test (id INTEGER PRIMARY KEY);"
						 :down "DROP TABLE cl_migratum_test;")))
      (ok (numberp (migration-id migration)))
      (ok (string= "my_new_migration" (migration-description migration)))
      (ok (string= "CREATE TABLE cl_migratum_test (id INTEGER PRIMARY KEY);"
                   (migration-load-up-script migration)))
      (ok (string= "DROP TABLE cl_migratum_test;"
                   (migration-load-down-script migration)))
      (uiop:delete-file-if-exists (local-path-migration-up-script-path migration))
      (uiop:delete-file-if-exists (local-path-migration-down-script-path migration)))))

(deftest sql-driver
  (testing "driver-name"
    (ok (string= "SQL" (driver-name *driver*))))

  (testing "driver-initialized"
    (ok (eq nil (driver-initialized *driver*)))) ;; Driver is not yet initialized

  (testing "driver-init"
    (ok (eq t (driver-init *driver*)))
    (ok (eq t (driver-initialized *driver*)))) ;; Driver should be initialized now

  (testing "driver-list-applied"
    (ng (driver-list-applied *driver*))) ;; No migrations applied yet

  (testing "contains-applied-migrations-p"
    (ok (eq nil (contains-applied-migrations-p *driver*))))

  (testing "list-pending"
    (let ((pending (list-pending *driver*)))
      (ok (= 3 (length pending)))
      (ok (equal (list 20200421173657 20200421173908 20200421180337)
		 (mapcar #'migration-id pending)))))

  (testing "apply-pending"
    (apply-pending *driver*)
    (let ((applied (driver-list-applied *driver*)))
      (ok (= 3 (length applied)))
      (ok (equal (list 20200421180337 20200421173908 20200421173657)
		 (mapcar #'migration-id applied)))))

  (testing "pagination"
    (ok (= 1 (length (driver-list-applied *driver* :offset 0 :limit 1))))
    (ok (= 1 (length (driver-list-applied *driver* :offset 1 :limit 1))))
    (ok (= 2 (length (driver-list-applied *driver* :offset 1 :limit 2))))
    (ng (driver-list-applied *driver* :offset 100 :limit 100)))

  (testing "latest-migration"
    (ok (= 20200421180337 (migration-id (latest-migration *driver*)))))

  (testing "revert-last"
    (revert-last *driver* :count 3)
    (ng (contains-applied-migrations-p *driver*))
    (ng (driver-list-applied *driver*)))

  (testing "apply-next"
    (ng (contains-applied-migrations-p *driver*))
    (apply-next *driver*)
    (ok (= 20200421173657 (migration-id (latest-migration *driver*))))
    (apply-next *driver*)
    (ok (= 20200421173908 (migration-id (latest-migration *driver*))))
    (apply-next *driver*)
    (ok (= 20200421180337 (migration-id (latest-migration *driver*))))
    (apply-next *driver*) ;; No more pending migrations at this point
    (ok (= 20200421180337 (migration-id (latest-migration *driver*)))))) ;; ID did not change, since previous migration
