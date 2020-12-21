;;
;; Day 17 of Advent of Code 2020, in Common Lisp.  This solves Part One.
;; See https://adventofcode.com/2020/day/17 for the problem statement.
;;
;; I'm using only standard Common Lisp functions; no external packages.
;;

(defpackage :day17a (:use #:common-lisp))
(in-package :day17a)

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

(deftype point () '(simple-array fixnum (3)))

(defun make-point (z y x)
  (make-array 3 :element-type 'fixnum :initial-contents (list z y x)))

(defun point-z (point)
  (declare (type point point))
  (aref point 0))

(defun point-y (point)
  (declare (type point point))
  (aref point 1))

(defun point-x (point)
  (declare (type point point))
  (aref point 2))

(defun point-equal (a b)
  (declare (type point a) (type point b))
  (and (eql (aref a 0) (aref b 0))
       (eql (aref a 1) (aref b 1))
       (eql (aref a 2) (aref b 2))))

(defun split-sequence (needle haystack &key (start 0) end)
  (loop with needle-length = (length needle)
        for haystack-start = start then (+ pos needle-length)
        for haystack-end = end
        for pos = (search needle haystack
                          :start2 haystack-start
                          :end2 haystack-end)
        collect (subseq haystack haystack-start pos)
        until (null pos)))

(defun parse-input (input)
  (loop for line in (split-sequence '(#\Newline) input)
        for y from 0
        append (loop for ch across line
                     for x from 0
                     if (eql ch #\#)
                       collect (make-point 0 y x))))

(defun active-p (cube cubes)
  (declare (type point cube))
  (member cube cubes :test #'point-equal))

(defun shift-point (point shift)
  (make-point (+ shift (point-z point))
              (+ shift (point-y point))
              (+ shift (point-x point))))

(defun get-bounds (cubes)
  (loop for c in cubes
        for z = (point-z c)
        for y = (point-y c)
        for x = (point-x c)
        minimize z into min-z fixnum
        minimize y into min-y fixnum
        minimize x into min-x fixnum
        maximize z into max-z fixnum
        maximize y into max-y fixnum
        maximize x into max-x fixnum
        finally (return (list (make-point min-z min-y min-x)
                              (shift-point (make-point
                                                 max-z max-y max-x)
                                                1)))))

(defun grow-bounds (bounds)
  (destructuring-bind (start end) bounds
    (list
     (shift-point start -1)
     (shift-point end 1))))

(defun for-each-cube (function bounds)
  (destructuring-bind (start end) bounds
    (declare (type point start) (type point end))
    (loop for z from (point-z start) below (point-z end)
          do (loop for y from (point-y start) below (point-y end)
                   do (loop for x from (point-x start) below (point-x end)
                            do (funcall function (make-point z y x)))))))

(defun neighbor-bounds (cube)
  (list (shift-point cube -1)
        (shift-point cube 2)))

(defun count-active-neighbors (cube cubes)
  (declare (type point cube))
  (let ((count 0))
    (for-each-cube #'(lambda (neighbor)
                       (when (and (not (point-equal neighbor cube))
                                  (active-p neighbor cubes))
                         (incf count)))
                   (neighbor-bounds cube))
    count))

(defun transition-to-active-p (cube cubes)
  (let ((count (count-active-neighbors cube cubes)))
    (if (active-p cube cubes)
        (or (eql count 2) (eql count 3))
        (eql count 3))))

(defun format-cubes (destination cubes)
  (destructuring-bind (low high) (get-bounds cubes)
    (loop for z from (point-z low) below (point-z high)
          do (format destination "~&z=~d~%" z)
          do (loop for y from (point-y low) below (point-y high)
                   do (loop for x from (point-x low) below (point-x high)
                            do (format destination "~c"
                                       (if (active-p (make-point z y x) cubes)
                                           #\#
                                           #\.))
                            finally (format destination "~%"))
                   finally (format destination "~%")))))

(defun cycle1 (cubes)
  (let ((next-cubes))
    (for-each-cube #'(lambda (cube)
                       (when (transition-to-active-p cube cubes)
                         (push cube next-cubes)))
                   (grow-bounds (get-bounds cubes)))
    next-cubes))

(defun cycle (count cubes)
  (dotimes (unused count cubes)
    (setf cubes (cycle1 cubes))))

(defun test ()
  (assert (equal 112 (length (cycle 6 (parse-input *example-input*)))))
  (assert (equal 388 (length (cycle 6 (parse-input *input*))))))

