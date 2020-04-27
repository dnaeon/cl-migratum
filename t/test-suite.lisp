(in-package :cl-user)
(defpackage :cl-migratum.test
  (:use :cl :rove)
  (:nicknames :migratum.test)
  (:import-from :tmpdir)
  (:import-from :asdf)
  (:import-from :cl-dbi)
  (:import-from :migratum)
  (:import-from
   :migratum.provider.local-path
   :make-local-path-provider)
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
