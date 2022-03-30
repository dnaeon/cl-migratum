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

(in-package :cl-migratum.cli)

(defmethod get-driver ((kind (eql :rdbms-pgsql)) (command clingon:command))
  (let ((db-host (clingon:getopt command :driver.rdbms-pgsql/db-host))
        (db-user (clingon:getopt command :driver.rdbms-pgsql/db-user))
        (db-pass (clingon:getopt command :driver.rdbms-pgsql/db-pass))
        (db-port (clingon:getopt command :driver.rdbms-pgsql/db-port))
        (db-name (clingon:getopt command :driver.rdbms-pgsql/db-name)))
    (unless db-name
      (error "RDMBS-PGSQL: No database name specified"))
    (unless db-user
      (error "RDBMS-PGSQL: No database username specified"))
    (let* ((provider (get-provider :infer command))
           (spec (list :host db-host
                       :database db-name
                       :user-name db-user
                       :password db-pass
                       :port db-port))
           (driver (cl-migratum.driver.rdbms-postgresql:make-driver provider spec)))
      (cl-migratum:driver-init driver)
      driver)))
