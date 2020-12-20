;;
;; Day 15 of Advent of Code 2020, in Common Lisp.  This solves Part Two.
;; See https://adventofcode.com/2020/day/15 for the problem statement.
;;
;; I'm using only standard Common Lisp functions; no external packages.
;;

(defpackage :day15b (:use #:common-lisp))
(in-package :day15b)

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
  (do ((memory (make-hash-table))
       (turn 1 (1+ turn))
       spoken
       prior-turn)
      ((> turn turns) spoken)
    (setf spoken (cond
                   (numbers (pop numbers))
                   (prior-turn (- turn prior-turn 1))
                   (t 0)))
    (setf prior-turn (gethash spoken memory nil))
    (setf (gethash spoken memory) turn)))

(defun part-one (input turns)
  (play-game (parse-input input) turns))

(defun test ()
  (let ((turns 30000000))
    (flet ((run (input expected)
             (assert (eql expected (part-one input turns)))))
      (run "9,6,0,10,18,2,1" 3745954)
      (run "0,3,6" 175594)
      (run "1,3,2" 2578)
      (run "2,1,3" 3544142)
      (run "1,2,3" 261214)
      (run "2,3,1" 6895259)
      (run "3,2,1" 18)
      (run "3,1,2" 362))))
