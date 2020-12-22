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

(deftype point () '(simple-array fixnum))

(defun make-point (&rest nums)
  (make-array (length nums)
              :element-type 'fixnum
              :initial-contents nums))

(defun point-equal (a b)
  (equalp a b))

(defun make-points-table ()
  ;; Nit: I'd like to pass :test #'point-equal here but the spec won't
  ;; allow it.
  (make-hash-table :test #'equalp))

(defun point-z (point)
  (declare (type point point))
  (aref point 0))

(defun point-y (point)
  (declare (type point point))
  (aref point 1))

(defun point-x (point)
  (declare (type point point))
  (aref point 2))

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
  (setf (gethash (copy-point point) points) t))

(defun active-p (point points)
  (multiple-value-bind (value present-p) (gethash point points nil)
    (declare (ignore value))
    present-p))

(defun parse-input (input)
  (loop with points = (make-points-table)
        for line in (split-sequence '(#\Newline) input)
        for y from 0
        append (loop for ch across line
                     for x from 0
                     do (when (eql ch #\#)
                          (set-active (make-point 0 y x) points)))
        finally (return points)))

(defun copy-point (source)
  (let* ((dimensions (array-dimensions source))
         (dest (make-array dimensions
                           :element-type (array-element-type source))))
    (dotimes (i (length source))
      (setf (aref dest i) (aref source i)))
    dest))

(defun shift-point (point shift)
  (let ((copy (copy-point point)))
    (dotimes (i (length copy))
      (setf (aref copy i) (+ shift (aref copy i))))
    copy))

(defun elementwise-min-max-into (src min max)
  (macrolet ((setif (dest cmp)
               `(when (funcall ,cmp (aref src i) (aref ,dest i))
                  (setf (aref ,dest i) (aref src i)))))
    (dotimes (i (length src))
      (setif min #'<)
      (setif max #'>))))

(defun get-bounds (points)
  (let (minimum maximum)
    (loop for p being each hash-key of points
          do (if (null minimum)
                 (setf minimum (copy-point p)
                       maximum (copy-point p))
                 (elementwise-min-max-into p minimum maximum)))
    (list minimum maximum)))

(defun grow-bounds (bounds)
  (destructuring-bind (start end) bounds
    (list
     (shift-point start -1)
     (shift-point end 1))))

(defun for-each-point (function bounds)
  (destructuring-bind (start end) bounds
    (let ((current (copy-point start))
          (end-index (length start)))
      (labels ((subscripts (index)
                 (if (eql index end-index)
                     (funcall function current)
                     (loop for i from (aref start index) to (aref end index)
                           do (progn
                                (setf (aref current index) i)
                                (subscripts (1+ index)))))))
        (subscripts 0)))))

(defun neighbor-bounds (point)
  (list (shift-point point -1)
        (shift-point point 1)))

(defun count-active-around (center points)
  (let ((count 0))
    (for-each-point #'(lambda (point)
                        (when (active-p point points)
                          (incf count)))
                    (neighbor-bounds center))
    count))

(defun transition-to-active-p (point points)
  (let ((count (count-active-around point points)))
    (if (active-p point points)
        (progn
          (decf count)
          (or (eql count 2) (eql count 3)))
        (eql count 3))))

(defun print-points (destination points)
  (format destination "~&~S~%"
          (loop for point being each hash-key of points
                collect point)))

(defun format-points (destination points)
  (destructuring-bind (low high) (get-bounds points)
    (loop for z from (point-z low) to (point-z high)
          do (format destination "~&z=~d~%" z)
          do (loop for y from (point-y low) to (point-y high)
                   do (loop for x from (point-x low) to (point-x high)
                            do (format destination "~c"
                                       (if (active-p (make-point z y x) points)
                                           #\#
                                           #\.))
                            finally (format destination "~%"))
                   finally (format destination "~%")))))

(defun cycle1 (points)
  (let ((next-points (make-points-table)))
    (for-each-point #'(lambda (point)
                        (when (transition-to-active-p point points)
                          (set-active point next-points)))
                    (grow-bounds (get-bounds points)))
    next-points))

(defun cycle (count points)
  (dotimes (i count points)
    (setf points (cycle1 points))))

(defun test ()
  (assert (equal 112 (hash-table-count
                      (cycle
                       6 (parse-input *example-input*)))))
  (assert (equal 388 (hash-table-count
                      (cycle
                       6 (parse-input *input*))))))
