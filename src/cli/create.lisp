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

(defparameter *default-migration-contents*
  '((:kind :sql  :content "-- TODO: Fill in migration details")
    (:kind :lisp :content (:system :my-system :package :my-system.my-package :handler :my-handler)))
  "Default contents for the different migration kinds")

(defun get-migration-content (kind)
  "Returns the mapping between kind and it's default migration contents"
  (find kind
        *default-migration-contents*
        :key (lambda (item) (getf item :kind))))

(defun get-migration-content-or-lose (kind)
  (let ((mapping (get-migration-content kind)))
    (unless mapping
      (error "No migration content found for '~A'" kind))
    (getf mapping :content)))

(defun %local-path/create-migration (cmd)
  "Creates a new :local-path migration"
  (let* ((id (migratum:make-migration-id))
         (description (clingon:getopt cmd :create-cmd/migration-description))
         (kind (clingon:getopt cmd :create-cmd/migration-kind))
         (content (get-migration-content-or-lose kind))
         (provider (get-provider :infer cmd))
         (up (migratum:provider-create-migration :up kind provider id description content))
         (down (migratum:provider-create-migration :down kind provider id description content)))
    (format t "up: ~A~%" (migratum.provider.local-path:migration-up-script-path up))
    (format t "down: ~A~%" (migratum.provider.local-path:migration-down-script-path down))))

(defun create/handler (cmd)
  "The handler for the `create' command"
  (case (clingon:getopt cmd :provider/kind)
    (:local-path (%local-path/create-migration cmd))
    (t (error "Unsupported provider ~A" (clingon:getopt cmd :provider/kind)))))

(defun create/options ()
  "Returns the list of options for the `create' command"
  (list
   (clingon:make-option :enum
                        :description "migration kind"
                        :long-name "kind"
                        :required t
                        :items '(("sql" . :sql)
                                 ("lisp" . :lisp))
                        :key :create-cmd/migration-kind)
   (clingon:make-option :string
                        :description "description of the migration"
                        :long-name "description"
                        :required t
                        :key :create-cmd/migration-description)))

(defun create/command ()
  "Returns the command for creating new migration resources"
  (clingon:make-command
   :name "create"
   :description "create new migration"
   :aliases '("new")
   :options (create/options)
   :handler #'create/handler))
