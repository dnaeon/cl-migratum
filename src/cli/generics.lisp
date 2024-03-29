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

(defgeneric get-provider (kind command)
  (:documentation "Given the context of COMMAND creates a new provider of the given KIND"))

(defgeneric get-driver (kind command)
  (:documentation "Given the context of COMMAND creates a new driver of the given KIND"))

(defgeneric get-db-connection (kind command)
  (:documentation "Given the context of COMMAND creates a new database connection of the given KIND"))

(defmethod get-provider ((kind (eql :infer)) (command clingon:command))
  "Helper method to infer the provider from the command's context"
  (let ((kind (clingon:getopt command :provider/kind)))
    (unless kind
      (error "No provider kind specified"))
    (get-provider kind command)))

(defmethod get-driver ((kind (eql :infer)) (command clingon:command))
  "Helper method to infer the driver from the command's context"
  (let ((kind (clingon:getopt command :driver/kind)))
    (unless kind
      (error "No driver kind specified"))
    (get-driver kind command)))
