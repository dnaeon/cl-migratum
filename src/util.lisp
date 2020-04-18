(in-package :cl-user)
(defpackage :cl-migratum.util
  (:use :cl)
  (:nicknames :migratum.util)
  (:export
   :take))
(in-package :cl-migratum.util)

(defun take (n sequence)
  "Take N items from the given sequence"
  (if (>= n (length sequence))
      sequence
      (subseq sequence 0 n)))
