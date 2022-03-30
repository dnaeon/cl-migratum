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

(defun applied/handler (cmd)
  "The handler for the `applied' command"
  (let ((driver (get-driver :infer cmd))
        (offset (clingon:getopt cmd :applied-cmd/offset))
        (limit (clingon:getopt cmd :applied-cmd/limit)))
    (cl-migratum:display-applied driver :offset offset :limit limit)))

(defun applied/options ()
  "Returns the options for the `applied' command"
  (list
   (clingon:make-option :integer
                        :description "fetch applied migrations at the given offset"
                        :long-name "offset"
                        :initial-value 0
                        :key :applied-cmd/offset)
   (clingon:make-option :integer
                        :description "fetch this number of migrations at max"
                        :long-name "limit"
                        :initial-value 100
                        :key :applied-cmd/limit)))

(defun applied/command ()
  "Returns the command for listing applied migrations"
  (clingon:make-command
   :name "applied"
   :description "list applied migrations"
   :options (applied/options)
   :handler #'applied/handler))
