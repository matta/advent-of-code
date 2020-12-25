;;
;; Day 23 of Advent of Code 2020, in Common Lisp.  See
;; https://adventofcode.com/2020/day/23 for the problem statement.
;;
;; I'm using only standard Common Lisp functions; no external packages.
;;

(defpackage :day23 (:use #:common-lisp))
(in-package :day23)

(defparameter *input* "463528179")

(defun parse-input (str)
  (loop for i below (length str)
        collect (parse-integer str :start i :end (1+ i))))

(defun set-next (cups cup next-cup)
  (setf (aref cups cup) next-cup))

(defun get-next (cups cup)
  (aref cups cup))

(defun make-cups (initial-cups &optional (total-cups (length initial-cups)))
  (let* ((max-initial (reduce #'max initial-cups))
         (max-cup (+ max-initial (max 0 (- total-cups
                                           (length initial-cups)))))
         (cups (make-array (1+ max-cup)
                           :initial-element 0
                           :element-type 'fixnum))
         (prev-cup (first initial-cups)))
    (loop for cup in (rest initial-cups)
          do (set-next cups prev-cup cup)
          do (setf prev-cup cup))
    (loop for cup from (1+ max-initial) to max-cup
          do (set-next cups prev-cup cup)
          do (setf prev-cup cup))
    (set-next cups prev-cup (first initial-cups))
    cups))

(defun hold-after-cup (cups cup n)
  (loop with first = (get-next cups cup)
        for i below n
        for last = first then (get-next cups last)
        finally (return (prog1 (cons first last)
                          (set-next cups cup (get-next cups last))))))

(defun heldp (cups held cup)
  (destructuring-bind (first . last) held
    (loop for c fixnum = first then (get-next cups c)
          if (eql c cup)
            do (return t)
          until (eql c last))))

(defun next-destination (cups cup)
  (if (>= (decf cup) 1)
      cup
      (1- (length cups))))

(defun destination-cup (cups current held)
  (flet ((next (cup) (next-destination cups cup)))
    (loop for dest = (next current) then (next dest)
          while (heldp cups held dest)
          finally (return dest))))

(defun insert-cups (cups dest held)
  (destructuring-bind (first . last) held
    (set-next cups last (get-next cups dest))
    (set-next cups dest first)))

(defun cup-sublist (cups head &optional (n 100))
  (loop for i below n
        for cup = head then (get-next cups cup)
        until (or (eql cup (first sublist)) (zerop cup))
        collect cup into sublist
        finally (return sublist)))

(defun game (input total-cups total-moves)
  (assert (eql 9 (length input)))
  (loop with initial-cups = (parse-input input)
        with cups = (make-cups initial-cups total-cups)
        for current = (first initial-cups) then (get-next cups current)
        for i from 0 below total-moves
        for held = (hold-after-cup cups current 3)
        for dest = (destination-cup cups current held)
        do (insert-cups cups dest held)
        finally (return (rest (cup-sublist cups
                                           1
                                           (length initial-cups))))))

(defun game1 (input total-moves)
  (game input 9 total-moves))

(defun game2 (input)
  (let ((result (game input 1000000 10000000)))
    (* (first result) (second result))))

(defun test ()
  (assert (equal '(9 2 6 5 8 3 7 4)
                 (game1 "389125467" 10)))
  (assert (equal '(6 7 3 8 4 5 2 9)
                 (game1 "389125467" 100)))

  ;; This is the answer to Part One.
  (assert (equal '(5 2 9 3 7 8 4 6)
                 (game1 *input* 100)))

  (assert (equal 149245887792 (game2 "389125467")))

  ;; This is the answer to Part Two.
  (assert (equal 8456532414 (game2 *input*))))
