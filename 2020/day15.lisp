;;
;; Day 15 of Advent of Code 2020, in Common Lisp.  See
;; https://adventofcode.com/2020/day/15 for the problem statement.
;;
;; I'm using only standard Common Lisp functions; no external packages.
;;

(defpackage :day15 (:use #:common-lisp))
(in-package :day15)

(defparameter *example-input* "0,3,6")

(defparameter *input* "9,6,0,10,18,2,1")

(defun split-by-commas (str)
  (let ((length (length str)))
    (loop
      :with end = length
      :for left := 0 :then (1+ right)
      :for right := (or (position #\, str :start left)
                        length)
      :collect (subseq str left right)
      :until (>= right end))))

(defun parse-input (input)
  (mapcar #'parse-integer (split-by-commas input)))

(defun play-game (numbers turns)
  (let (spoken
        prior-turn
        (memory (make-hash-table)))
    (loop for turn from 1 upto turns
          do (format t "~&turn ~s: prior-turn ~s." turn prior-turn)
          do (setf spoken (cond
                            (numbers (pop numbers))
                            (prior-turn (- turn prior-turn 1))
                            (t 0)))
          do (format t "~%turn ~s: spoken ~s.~%" turn spoken)
          do (setf prior-turn (gethash spoken memory nil))
          do (setf (gethash spoken memory) turn))
    spoken))

(defun part-one (input turns)
  (play-game (parse-input input) turns))

(defun test ()
  (assert (eql 0 (part-one *example-input* 10)))
  (assert (eql 436 (part-one *example-input* 2020)))
  (assert (eql 1 (part-one "1,3,2" 2020)))
  (assert (eql 10 (part-one "2,1,3" 2020)))
  (assert (eql 27 (part-one "1,2,3" 2020)))
  (assert (eql 78 (part-one "2,3,1" 2020)))
  (assert (eql 438 (part-one "3,2,1" 2020)))
  (assert (eql 1836 (part-one "3,1,2" 2020)))
  (assert (eql 1238 (Part-one "9,6,0,10,18,2,1" 2020))))
