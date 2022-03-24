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

(defmethod get-driver ((kind (eql :dbi)) (command clingon:command))
  (let ((provider (get-provider (clingon:getopt command :provider/kind) command))
        (conn (get-db-connection (clingon:getopt command :driver.dbi/db-kind) command)))
    (cl-migratum.driver.dbi:make-driver provider conn)))

(defmethod get-db-connection ((kind (eql :dbi/sqlite3)) (command clingon:command))
  (let ((db-name (clingon:getopt command :driver.dbi/db-name)))
    (unless db-name
      (error "No database name specified"))
    (cl-dbi:connect :sqlite3 :database-name db-name)))

(defmethod get-db-connection ((kind (eql :dbi/postgres)) (command clingon:command))
  (let ((db-host (clingon:getopt command :driver.dbi/db-host "localhost"))
        (db-name (clingon:getopt command :driver.dbi/db-name))
        (db-user (clingon:getopt command :driver.dbi/db-user))
        (db-pass (clingon:getopt command :driver.dbi/db-pass))
        (db-port (clingon:getopt command :driver.dbi/db-port 5432)))
    (unless db-name
      (error "No database name specified"))
    (unless db-user
      (error "No database username specified"))
    (cl-dbi:connect :postgres
                    :database-name db-name
                    :username db-user
                    :password db-pass
                    :host db-host
                    :port db-port)))

(defmethod get-db-connection ((kind (eql :dbi/mysql)) (command clingon:command))
  (let ((db-host (clingon:getopt command :driver.dbi/db-host "localhost"))
        (db-name (clingon:getopt command :driver.dbi/db-name))
        (db-user (clingon:getopt command :driver.dbi/db-user))
        (db-pass (clingon:getopt command :driver.dbi/db-pass))
        (db-port (clingon:getopt command :driver.dbi/db-port 3306)))
    (unless db-name
      (error "No database name specified"))
    (unless db-user
      (error "No database username specified"))
    (cl-dbi:connect :mysql
                    :database-name db-name
                    :username db-user
                    :password db-pass
                    :host db-host
                    :port db-port)))
