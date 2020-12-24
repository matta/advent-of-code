;;
;; Day 23 of Advent of Code 2020, in Common Lisp.  See
;; https://adventofcode.com/2020/day/23 for the problem statement.
;;
;; I'm using only standard Common Lisp functions; no external packages.
;;

(defpackage :day23 (:use #:common-lisp))
(in-package :day23)

(defparameter *example-input* "389125467")
(defparameter *input* "463528179")

(defun parse-input (str)
  (loop for i below (length str)
        collect (parse-integer str :start i :end (1+ i))))

(defun make-circle (list)
  (setf (cdr (last list)) list))

(defun length-circle (list)
  "Like LENGTH, but works for circular lists."
  (loop for curr on list
        for rest = (rest curr)
        count t into length
        until (eq rest list)
        finally (return length)))

(defun member-circle (item list)
  "Like MEMBER, but works for circular lists."
  (declare (type fixnum item))
  (loop for curr on list
        for rest = (rest curr)
        count t into count
        if (eql item (car curr))
          do (return curr)
        until (eq rest list)))

(defun nremove-hand (list)
  "Removes three elements from LIST starting from LIST's tail.
LIST is destructively modified to splice the gap, and the
three removed elements are returned as a proper list."
  (let* ((hold (cdr list))
         (tail (cddr hold)))
    (setf (cdr list) (cdr tail))
    (setf (cdr tail) nil)
    hold))

(defun nsplice-after (from into)
  "Splice FROM into (CDR INTO).  Both lists are destructively modified."
  (setf (cdr (last from)) (rest into))
  (setf (cdr into) from)
  nil)

(defun next-destination (num min max)
  "Given a cup number NUM, and the MIN and MAX allowed cup numbers,
returns the next cup number."
  (decf num)
  (if (< num min)
      max
      num))

(defun destination-cup (current min max)
  "Returns the next destination cup, given the CURRENT cup and the MIN and MAX
cup numbers.  Assumes that CURRENT is in a circular list."
  (flet ((next (prev)
           (next-destination prev min max)))
    (loop for number = (next (car current)) then (next number)
          for destination = (member-circle number current)
          while (null destination)
          finally (return destination))))

(defun order-after-cup1 (cups)
  (loop for cup in (rest (member-circle 1 cups))
        until (eql cup 1)
        collect cup))

(defun game (cups moves)
  (assert *print-circle*)
  (loop with min = (reduce #'min cups)
        with max = (reduce #'max cups)
        for i from 0 below moves
        for current = (make-circle (copy-list cups)) then (rest current)
        for hold = (nremove-hand current)
        for destination = (destination-cup current min max)
        if (and (zerop (mod i 1000)) (> i 0))
          do (format t "~&iteration ~S of ~S (~S)~%" i moves (* 1.0 (/ i moves)))
        ;; do (format t "~&hold ~S~%" hold)
        ;; do (format t "~&destination ~S~%" destination)
        ;; do (format t "~&(car destination) ~S~%" (car destination))
        ;; do (format t "~&current ~S (before splicing)~%" current)
        do (nsplice-after hold destination)
        ;; do (format t "~&current ~S (after splicing)~%" current)
        ;; do (format t "~&(rest current) ~S (after splicing)~%" (rest current))
        ;; do (format t "~%")
        finally (return current)))

(defun game1 (cups moves)
  (order-after-cup1 (game cups moves)))

;; (defun game2 (cups)
;;   (let* ((one-million 1000000)
;;          (one-million-cups
;;            (append cups
;;                    (loop for i from (1+ (length cups)) to one-million
;;                          collect i)))
;;          (result (game one-million-cups 10000000)))
;;     (* (first result) (second result))))

(defun test ()
  (assert (equal '(9 2 6 5 8 3 7 4)
                 (game1 (parse-input *example-input*) 10)))
  (assert (equal '(6 7 3 8 4 5 2 9)
                 (game1 (parse-input *example-input*) 100)))

  ;; This is the answer to Part One.
  (assert (equal '(5 2 9 3 7 8 4 6)
                 (game1 (parse-input *input*) 100)))

  ;; (assert (equal -1
  ;;                (game2 (parse-input *example-input*))))
  )

