;;
;; Day 10 of Advent of Code 2020, in Common Lisp.  See
;; https://adventofcode.com/2020/day/10 for the problem statement.
;;
;; I'm using only standard Common Lisp functions; no external packages.
;;

(defpackage :day10 (:use #:common-lisp))
(in-package :day10)

(defparameter *small-example-input* '(16 10 15 5 1 11 7 19 6 12 4))

(defparameter *large-example-input* '(28 33 18 42 31 14 46 20 48 47 24 23
                                      49 45 19 38 39 11 1 32 25 35 8 17 7 9
                                      4 2 34 10 3))

(defparameter *input* '(160 34 123 159 148 93 165 56 179 103 171 44 110 170
                        147 98 25 37 137 71 5 6 121 28 19 134 18 7 66 90 88
                        181 89 41 156 46 8 61 124 9 161 72 13 172 111 59
                        105 51 109 27 152 117 52 68 95 164 116 75 78 180 81
                        47 104 12 133 175 16 149 135 99 112 38 67 53 153 2
                        136 113 17 145 106 31 45 169 146 168 26 36 118 62
                        65 142 130 1 140 84 94 141 122 22 48 102 60 178 127
                        73 74 87 182 35 ))

(defun normalized-ratings (ratings)
  "Returns a copy of RATINGS with the addition of the implied zero rating
of the seat adapter and the phone adapter 3 higher than the max rating
found in RATINGS."
  (let* ((sorted (cons 0 (sort (copy-seq ratings) #'<)))
         (last (last sorted))
         (last-rating (first last)))
    (nconc last (list (+ 3 last-rating)))
    sorted))

(defun jolt-deltas (ratings)
  "Returns a list of joltage deltas given a list of joltage ratings
RATINGS.  The deltas include the implied zero rating of the seat adapter
and the phone adapter with a joltage rating 3 higher than the max found in
RATINGS.  For example, (2 5 6) will return (2 3 1 3)."
  (loop
    :with normalized = (normalized-ratings ratings)
    :for (a b) :on normalized
    :while b
    :for delta = (- b a)
    :do (assert (<= 1 delta 3))
    :collect delta))

(defun jolt-delta-counts (ratings)
  "Returns a CONS whose CAR is the count of ones in RATINGS and whose CDR
is the count of threes."
  (loop
    :with deltas = (jolt-deltas ratings)
    :for delta :in deltas
    :count (eql delta 1) :into count1
    :count (eql delta 3) :into count3
    :finally (return (cons count1 count3))))

(defun jolt-delta-product (ratings)
  "Returns the product of the two numbers returned by JOLT-DELTA-COUNTS.
This computes the answer as required by Part One."
  (let ((counts (jolt-delta-counts ratings)))
    (* (car counts) (cdr counts))))

;;;; Phase Two
;;;;
;;;; I found it difficult to reason through Part Two of this problem to an
;;;; almost embarassing degree.  I am afraid I'll submit this, then look at
;;;; other solutions, and find an extremely efficient one that I didn't
;;;; think of.  Update: Gah, exactly, this problem is not about programming
;;;; but math: https://brilliant.org/wiki/tribonacci-sequence/.  Oh well,
;;;; brute force is still under one millisecond.
;;;;
;;;; We must find all valid subsets given a list of adapters.  We're given
;;;; that the initial subset contains no duplicate adapters (i.e. when
;;;; sorted the delta between adjacent adapters is positive), and that the
;;;; initial set is a valid combination (i.e. when sorted the delta between
;;;; adjacent adapters does not exceed 3).  It follows that all valid
;;;; subsets result from removing adapters.
;;;;
;;;; The algorithm used here is a recursive one over a list of joltage
;;;; deltas.
;;;;
;;;; 1. If we reach the end we've found one possible valid permutation, so
;;;;    return one.
;;;;
;;;; 2. Otherwise,
;;;;
;;;;    a) Recurse on the rest of the deltas.
;;;;
;;;;    b) If the head of the list can be omitted without exceeding the max
;;;;       delta of 3, recurse on that alternate list too.
;;;;
;;;;    c) Return the sum of (a) and (b).
;;;;
;;;; Note: we also memoize results, which keeps the running time under one
;;;; millisecond on my machine when using SBCL.
;;;;
;;;; Note: I experimented with a hybrid iterative/recursive solution but it
;;;; was almost 3 times slower.  These are the subset-iterative-*
;;;; functions.

(defun alt-deltas (deltas)
  "If the first adapter in DELTAS can be skipped, returns a list of deltas
without that adapter.  Otherwise, NIL.

This function is fast, and will cons at most once.

If the sum of the first two adapters is less than or equal to 3, then the
first adapter can be skipped, creating a new alternate subset adapter
subset.  For example, with the adapters in this sequence, denoted by
joltage ratings (5 6 7 10) we can drop 6 without violating the invariant
that the joltage delta can't exceed 3, producing the new subset (5 7 10).
Note that we don't know if we can drop 5 in this example because we don't
know what comes before it.  This function takes joltage DELTAS instead, and
so the above example would be (1 1 3), and the function returns (2 3) in
this case (with 3 sharing the cons across both lists."
  (let* ((a (pop deltas))
         (b (pop deltas)))
    (if (and a b)
        (let ((b2 (+ a b)))
          (if (<= b2 3)
              (cons b2 deltas))))))

(defun subset-count (ratings)
  "Returns the number of valid adapter subsets given their RATINGS."
  (subset-count-2 (jolt-deltas ratings)
                  (make-hash-table :test #'equal)))

(defun subset-count-2 (deltas cache)
  "This function memoizes SUBSET-COUNT-3, which see."
  (or (gethash deltas cache)
      (setf (gethash deltas cache)
            (subset-count-3 deltas cache))))

(defun subset-count-3 (deltas cache)
  "The recursive implementation of SUBSET-COUNT."
  (if (null deltas)
      1                                 ; null is the base case
      (+
       ;; Otherwise we sum the simple recursive case with...
       (subset-count-2 (cdr deltas) cache)
       ;; An alternate subset, if omitting (first deltas) is valid.
       (let ((alt (alt-deltas deltas)))
         (if (not alt)
             0
             (subset-count-2 alt cache))))))

;;;; This iterative implementation works, but is almost three times slower
;;;; than the recursive one when run on SBCL.

(defun subset-count-iterative (ratings)
   (subset-count-2 (jolt-deltas ratings)
                  (make-hash-table :test #'equal)))

(defun subset-count-iterative-2 (deltas cache)
  (or (gethash deltas cache)
      (setf (gethash deltas cache)
            (subset-count-3 deltas cache))))

(defun subset-count-iterative-3 (deltas cache)
  (do* ((count 1)
        (curr deltas (rest curr))
        (alt (alt-deltas curr) (alt-deltas curr)))
       ((null curr) count)
    (when alt
       (incf count (subset-count-2 alt cache)))))

(defun test ()
  (assert (equal '(0 3) (normalized-ratings '())))
  (assert (equal '(0 2 5) (normalized-ratings '(2))))
  (assert (equal '(0 . 1) (jolt-delta-counts '())))
  (assert (equal '(1 . 1) (jolt-delta-counts '(1))))
  (assert (equal '(0 . 2) (jolt-delta-counts '(3))))
  (assert (equal '(7 . 5) (jolt-delta-counts *small-example-input*)))
  (assert (equal '(22 . 10) (jolt-delta-counts *large-example-input*)))
  (assert (equal '(74 . 37) (jolt-delta-counts *input*)))

  ;; This is the answer to Phase One.
  (assert (eql '2738 (jolt-delta-product *input*)))

  ;; Phase Two results from the problem statement.

  ;; Validate that the purely recursive and iterative/recursive
  ;; implementations are semantically equivalent.
  (assert (eql (subset-count *small-example-input*)
               (subset-count-iterative *small-example-input*)))
  (assert (eql (subset-count *large-example-input*)
               (subset-count-iterative *large-example-input*)))
  (assert (eql (subset-count *input*)
               (subset-count-iterative *input*)))

  ;; Validate the results of the recursive implementation.
  (assert (eql 8 (subset-count *small-example-input*)))
  (assert (eql 19208 (subset-count *large-example-input*)))

  ;; This is the answer to Phase Two.
  (assert (eql 74049191673856 (subset-count *input*))))

