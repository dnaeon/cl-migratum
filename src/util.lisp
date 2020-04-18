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
