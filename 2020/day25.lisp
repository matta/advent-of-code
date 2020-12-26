(defpackage :day25 (:use #:common-lisp))
(in-package :day25)

(defparameter *example-input* '(5764801 17807724))
(defparameter *input* '(9717666 20089533))

(defun transform (subject-number loop-size)
  (loop with value = 1
        for i from 0 below loop-size
        do (setf value (rem (* value subject-number)
                            20201227))
        finally (return value)))

(defun loop-size (subject-number public-key)
  (loop with value = 1
        for loop-size from 1
        do (setf value (rem (* value subject-number)
                            20201227))
        until (= value public-key)
        finally (return loop-size)))

(defun encryption-key (keys)
  (transform (first keys) (loop-size 7 (second keys))))

(defun test ()
  (assert (= 14897079 (encryption-key *example-input*)))

  ;; This is the answer to Part One.
  (assert (= 19924389 (Encryption-key *input*))))
