;; Copyright (c) 2020 Marin Atanasov Nikolov <dnaeon@gmail.com>
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
(defpackage :cl-migratum.util
  (:use :cl)
  (:nicknames :migratum.util)
  (:export
   :take
   :make-migration-id))
(in-package :cl-migratum.util)

(defun take (n sequence)
  "Take N items from the given sequence"
  (if (>= n (length sequence))
      sequence
      (subseq sequence 0 n)))

(defun make-migration-id ()
  "Creates a new migration id"
  (let* ((now (local-time:now))
         (year (format nil "~d" (local-time:timestamp-year now)))
         (month (format nil "~2,'0d" (local-time:timestamp-month now)))
         (day (format nil "~2,'0d" (local-time:timestamp-day now)))
         (hour (format nil "~2,'0d" (local-time:timestamp-hour now)))
         (minute (format nil "~2,'0d" (local-time:timestamp-minute now)))
         (sec (format nil "~2,'0d" (local-time:timestamp-second now)))
         (timestamp-id (parse-integer (format nil "~a~a~a~a~a~a" year month day hour minute sec))))
    timestamp-id))
