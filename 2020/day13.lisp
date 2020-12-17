;;
;; Day 13 of Advent of Code 2020, in Common Lisp.  See
;; https://adventofcode.com/2020/day/13 for the problem statement.
;;
;; I'm using only standard Common Lisp functions; no external packages.
;;

(defpackage :day13 (:use #:common-lisp))
(in-package :day13)

(defparameter *tiny-input*
  "939
7,13
")

(defparameter *example-input*
  "939
7,13,x,x,59,x,31,19
")

(defparameter *input*
  "1008141
17,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,523,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,19,x,x,x,23,x,x,x,x,x,x,x,787,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29
")

(deftype digit () '(member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defun ctoi (d) (- (char-code d) #. (char-code #\0)))

(defun parse-int ()
  (assert (typep (peek-char) 'digit))
  (loop while (typep (peek-char) 'digit)
        with num
        finally (return num)
        do (let ((n (ctoi (read-char))))
             (setf num
                   (if num
                       (+ n (* num 10))
                       n)))))

(defun must-parse-char (ch)
  (assert (eql (read-char) ch)))

(defun consume-char (ch)
  (if (eql (peek-char) ch)
      (read-char)))

(defun must-parse-newline ()
  (must-parse-char #\Newline))

(defun parse-earliest-departure ()
  (let ((number (parse-int)))
    (must-parse-newline)
    number))

(defun parse-bus-ids ()
  (loop :for peek = (peek-char)
        :if (typep peek 'digit)
          :collect (parse-int)
        :else
          :collect (consume-char #\x)
        :while (consume-char #\,)
        :finally
           (must-parse-newline)))

(defstruct schedule earliest-departure bus-ids)

(defun parse-schedule (string)
  (with-input-from-string (*standard-input* string)
    (make-schedule
     :earliest-departure (parse-earliest-departure)
     :bus-ids (parse-bus-ids))))

(defun departure-time (earliest interval)
  (assert (>= earliest 0))
  (* (ceiling (/ earliest interval))
     interval))

  ;; (* (ceiling (/ earliest interval))
  ;;    interval))

(defun bus-to-take (schedule)
  (with-slots (earliest-departure bus-ids) schedule
    (labels ((departure-time-from-interval (departure-interval)
               (departure-time earliest-departure departure-interval))
             (departs-earlier (a b)
               (if (< (second a) (second b))
                   a b))
             (encode (id)
               (list id (departure-time-from-interval id))))
      (destructuring-bind
          (id departure)
          (reduce #'departs-earlier
                  (remove-if-not #'numberp bus-ids)
                  :key #'encode)
        (* id (- departure earliest-departure))))))

(defun busses-from-schedule (schedule)
  (with-slots (bus-ids) schedule
    (loop for offset from 0
          for id in bus-ids
          if (numberp id)
            collect (list id offset))))

(defun departure-time-p (timestamp interval)
  (zerop (rem timestamp interval)))

(defun step-timestamp (timestamp step interval offset)
  (assert (> step 0))
  (loop while (not (departure-time-p (+ timestamp offset)
                                     interval))
        do (incf timestamp step))
  timestamp)

(defun part-two (input)
  ;; Algorithm shamelessly stolen wholesale from
  ;; https://gist.github.com/joshbduncan/65f810fe821c7a3ea81a1f5a444ea81e
  ;; via
  ;; https://www.reddit.com/r/adventofcode/comments/kc4njx/2020_day_13_solutions/gfxo266?utm_source=share&utm_medium=web2x&context=3
  (loop with timestamp = 0
        with step = 1
        for (interval offset) in (busses-from-schedule
                                  (parse-schedule input))
        do (setf timestamp
                 (step-timestamp timestamp
                                 step
                                 interval
                                 offset))
        do (setf step (* step interval))
        finally (return timestamp)))

(defun test ()
  (assert (eql 295
               (bus-to-take
                (parse-schedule *example-input*))))
  (assert (eql 4722
               (bus-to-take
                (parse-schedule *input*))))

  (assert (eql 1068781 (part-two *example-input*)))
  (assert (eql 825305207525452 (part-two *input*))))
