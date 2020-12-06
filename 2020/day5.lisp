;;;;
;;;; Constraint: Use only the standard library.
;;;;
;;;; Disclaimer: I'm very new to the language.
;;;;
;;;; Apology: Not enough comments.
;;;;
;;;; For this day I also tried to write this like a real-ish parser,
;;;; instead of using "scripty" techniques like reading strings line by
;;;; line, then splitting them, etc.  This made it longer and more complex
;;;; than a minimal implementation.

(defpackage :day4 (:use #:common-lisp))
(in-package :day4)

(declaim (optimize (debug 3)))

(defun parse-bit (ch)
  "Returns 1 if CH is B or R, 0 if CH is F or L.
This is used by PARSE-SEAT-ID to decode a boarding pass into a seat ID."
  (cond ((member ch '(#\B #\R)) 1)
        ((member ch '(#\F #\L)) 0)
        (t (error "Invalid value ~S passed to PARSE-BIT." ch))))

(defun parse-seat-id (str)
  "Decodes STR as a boarding pass, returning the seat ID."
  (let ((id 0))
    (loop for ch across str
          do (setf id (logior (ash id 1) (parse-bit ch))))
    id))

(defun test ()
  (assert (eql 567 (parse-seat-id "BFFFBBFRRR")))
  (assert (eql 119 (parse-seat-id "FFFBBBFRRR")))
  (assert (eql 820 (parse-seat-id "BBFFBBFRLL"))))
