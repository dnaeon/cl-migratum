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

(defun status/handler (cmd)
  "The handler for the `status' command"
  (let* ((driver (get-driver :infer cmd))
         (pending (migratum:list-pending driver))
         (latest-migration (migratum:latest-migration driver)))
    (when pending
      (format t "There are ~A pending migrations~%" (length pending)))
    (when latest-migration
      (format t "Database is at version ~A~%" (migratum:migration-id latest-migration)))))

(defun status/command ()
  "Returns the command for retrieving migration status"
  (clingon:make-command
   :name "status"
   :description "get status info"
   :handler #'status/handler))
