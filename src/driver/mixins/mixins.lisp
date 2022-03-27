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

(in-package :cl-user)
(defpackage :cl-migratum.driver.mixins
  (:nicknames :migratum.driver.mixins)
  (:use :cl)
  (:import-from
   :cl-migratum
   :driver-apply-migration
   :migration-id
   :migration-description
   :migration-load)
  (:export
   :lisp-driver-mixin))
(in-package :cl-migratum.driver.mixins)

(defclass lisp-driver-mixin ()
  ()
  (:documentation "Mixin class for drivers, which adds support for :lisp migrations"))

(defun %apply-lisp-migration (direction driver migration)
  (let ((id (migration-id migration))
        (description (migration-description migration))
        (handler (migration-load direction migration)))
    (log:debug "[LISP] Applying ~A migration: ~A - ~A" direction id description)
    (funcall handler driver)))

(defmethod driver-apply-migration ((direction (eql :up)) (kind (eql :lisp))
                                   (driver lisp-driver-mixin) migration &key)
  (%apply-lisp-migration direction driver migration))

(defmethod driver-apply-migration ((direction (eql :down)) (kind (eql :lisp))
                                   (driver lisp-driver-mixin) migration &key)
  (%apply-lisp-migration direction driver migration))
