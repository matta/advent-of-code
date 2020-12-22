;;
;; Day 17 of Advent of Code 2020, in Common Lisp.  This solves Part Two.
;; See https://adventofcode.com/2020/day/17 for the problem statement.
;;
;; I'm using only standard Common Lisp functions; no external packages.
;;

(defpackage :day17b (:use #:common-lisp))
(in-package :day17b)

(defparameter *example-input*
  ".#.
..#
###")

(defparameter *input*
  ".###.#.#
####.#.#
#.....#.
####....
#...##.#
########
..#####.
######.#")

(deftype point () '(cons))

(defun make-point (z y x)
  (list z y x))

(defun point-z (point)
  (declare (type point point))
  (first point))

(defun point-y (point)
  (declare (type point point))
  (second point))

(defun point-x (point)
  (declare (type point point))
  (third point))

(defun point-equal (a b)
  (declare (type point a) (type point b))
  (equal a b))

(defun split-sequence (needle haystack &key (start 0) end)
  (loop with needle-length = (length needle)
        for haystack-start = start then (+ pos needle-length)
        for haystack-end = end
        for pos = (search needle haystack
                          :start2 haystack-start
                          :end2 haystack-end)
        collect (subseq haystack haystack-start pos)
        until (null pos)))

(defun set-active (point points)
  (declare (type point point))
  (setf (gethash point points) t))

(defun active-p (point points)
  (declare (type point point))
  (gethash point points nil))

(defun parse-input (input)
  (loop with points = (make-hash-table :test #'equal)
        for line in (split-sequence '(#\Newline) input)
        for y from 0
        append (loop for ch across line
                     for x from 0
                     do (when (eql ch #\#)
                          (set-active (make-point 0 y x) points)))
        finally (return points)))


(defun shift-point (point shift)
  (declare (type point point))
  (mapcar #'(lambda (num) (+ shift num))
          point))

(defun elementwise-min (a b)
  (loop for m in a
        for n in b
        collect (min m n)))

(defun elementwise-max (a b)
  (loop for m in a
        for n in b
        collect (max m n)))

(defun get-bounds (cubes)
  (let (minimum maximum)
    (loop for p being each hash-key of cubes
          do (setf minimum (if (null minimum)
                               p
                               (elementwise-min p minimum)))
          do (setf maximum (if (null maximum)
                               p
                               (elementwise-max p maximum))))
    (list minimum (shift-point maximum 1))))

(defun grow-bounds (bounds)
  (destructuring-bind (start end) bounds
    (list
     (shift-point start -1)
     (shift-point end 1))))

(defun for-each-point (function bounds)
  (destructuring-bind (start end) bounds
    (declare (type point start) (type point end))
    (loop for z from (point-z start) below (point-z end)
          do (loop for y from (point-y start) below (point-y end)
                   do (loop for x from (point-x start) below (point-x end)
                            do (funcall function (make-point z y x)))))))

(defun neighbor-bounds (point)
  (list (shift-point point -1)
        (shift-point point 2)))

(defun count-active-neighbors (point points)
  (declare (type point point))
  (let ((count 0))
    (for-each-point #'(lambda (neighbor)
                       (when (and (not (point-equal neighbor point))
                                  (active-p neighbor points))
                         (incf count)))
                   (neighbor-bounds point))
    count))

(defun transition-to-active-p (point points)
  (let ((count (count-active-neighbors point points)))
    (if (active-p point points)
        (or (eql count 2) (eql count 3))
        (eql count 3))))

(defun format-points (destination points)
  (destructuring-bind (low high) (get-bounds points)
    (loop for z from (point-z low) below (point-z high)
          do (format destination "~&z=~d~%" z)
          do (loop for y from (point-y low) below (point-y high)
                   do (loop for x from (point-x low) below (point-x high)
                            do (format destination "~c"
                                       (if (active-p (make-point z y x) points)
                                           #\#
                                           #\.))
                            finally (format destination "~%"))
                   finally (format destination "~%")))))

(defun cycle1 (points)
  (let ((next-points (make-hash-table :test #'equal)))
    (for-each-point #'(lambda (point)
                        (when (transition-to-active-p point points)
                          (set-active point next-points)))
                    (grow-bounds (get-bounds points)))
    next-points))

(defun cycle (count points)
  (dotimes (unused count points)
    (setf points (cycle1 points))))

(defun test ()
  (assert (equal 112 (hash-table-count
                      (cycle
                       6 (parse-input *example-input*)))))
  (assert (equal 388 (hash-table-count
                      (cycle
                       6 (parse-input *input*))))))
