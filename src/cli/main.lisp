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

(defun top-level/handler (cmd)
  "The top-level handler"
  (clingon:print-usage-and-exit cmd t))

(defun top-level/sub-commands ()
  "Returns the list of top-level sub-commands"
  (list
   (list-pending/command)))

(defun top-level/options ()
  "Returns the list of options for the top-level command"
  (list
   (clingon:make-option :enum
                        :description "log level"
                        :short-name #\l
                        :long-name "log-level"
                        :items '(("info" . :info)
                                 ("debug" . :debug))
                        :initial-value "info"
                        :env-vars '("MIGRATUM_LOG_LEVEL")
                        :key :logging/level)
   (clingon:make-option :enum
                        :description "migration driver to use"
                        :short-name #\d
                        :long-name "driver"
                        :required t
                        :items '(("dbi" . :dbi))
                        :env-vars '("MIGRATUM_DRIVER")
                        :key :driver/kind)
   (clingon:make-option :enum
                        :description "migration resource provider to use"
                        :short-name #\p
                        :long-name "provider"
                        :required t
                        :items '(("local-path" . :local-path))
                        :key :provider/kind
                        :env-vars '("MIGRATUM_PROVIDER"))

   ;; local-path provider specific options
   (clingon:make-option :list/filepath
                        :category "local-path provider options"
                        :description "migration resources path"
                        :long-name "resources"
                        :env-vars '("MIGRATUM_LP_RESOURCES")
                        :key :provider.local-path/resources)

   ;; DBI driver specific options
   (clingon:make-option :enum
                        :category "DBI driver options"
                        :description "database kind"
                        :long-name "dbi-db-kind"
                        :items '(("sqlite3" . :dbi/sqlite3)
                                 ("postgres" . :dbi/postgres)
                                 ("mysql" . :dbi/mysql))
                        :env-vars '("MIGRATUM_DBI_DB_KIND")
                        :key :driver.dbi/db-kind)
   (clingon:make-option :string
                        :category "DBI driver options"
                        :description "database host"
                        :long-name "dbi-db-host"
                        :initial-value "localhost"
                        :env-vars '("MIGRATUM_DBI_DB_HOST")
                        :key :driver.dbi/db-host)
   (clingon:make-option :string
                        :category "DBI driver options"
                        :description "database name"
                        :long-name "dbi-db-name"
                        :env-vars '("MIGRATUM_DBI_DB_NAME")
                        :key :driver.dbi/db-name)
   (clingon:make-option :string
                        :category "DBI driver options"
                        :description "database username"
                        :long-name "dbi-db-username"
                        :env-vars '("MIGRATUM_DBI_USER")
                        :key :driver.dbi/db-user)
   (clingon:make-option :string
                        :category "DBI driver options"
                        :description "database password"
                        :long-name "dbi-db-password"
                        :env-vars '("MIGRATUM_DBI_DB_PASSWORD")
                        :key :driver.dbi/db-pass)
   (clingon:make-option :integer
                        :category "DBI driver options"
                        :description "database port"
                        :long-name "dbi-db-port"
                        :env-vars '("MIGRATUM_DBI_DB_PORT")
                        :key :driver.dbi/db-port)))

(defun top-level/pre-hook (cmd)
  (let ((level (clingon:getopt cmd :logging/level)))
    (log:config level)))

(defun top-level/command ()
  "Returns the top-level command"
  (clingon:make-command
   :name "migratum"
   :version "0.1.0"
   :description "migration tool"
   :authors '("Marin Atanasov Nikolov <dnaeon@gmail.com>")
   :license "BSD 2-Clause"
   :options (top-level/options)
   :sub-commands (top-level/sub-commands)
   :handler #'top-level/handler
   :pre-hook #'top-level/pre-hook))

(defun main ()
  "Main CLI entrypoint"
  (let ((app (top-level/command)))
    (clingon:run app)))
