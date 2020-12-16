;;
;; Day 11 of Advent of Code 2020, in Common Lisp.  See
;; https://adventofcode.com/2020/day/11 for the problem statement.
;;
;; I'm using only standard Common Lisp functions; no external packages.
;;

(defpackage :day11 (:use #:common-lisp))
(in-package :day11)

(defparameter *example-input*
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(defparameter *input*
  "LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLL.LLLLLLL.LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLLLL
LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLL.LLLLLL
LLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
LLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
L.L...L...LL.LL.......LL...LL.L...LL..LL..L.......LLLLL.....LL..LLLL.L....L..L...L.LL....LL...L
LLLLL.LLLLL.LLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLL.LL.LLLLLLLLL.LLLL.LLLL.LLLLLLLLLLL
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LL.LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLL.LLLLLL
LLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLL.LL.LLLLLLLLL.LLLL.LLLL.LLLL.L.LLLL
LLLLL.LLLL.LLLL.LLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLL.LLLL.LLLL.LLLLLLLLLLL
LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLL.LLL.LL.LLLLLLLLLLLLLLLLLLL.LLLL.LLLLLL
.L.....L.LL..LLL.L..L...L.LLL.L...L.L.L.L.....L..L.......L.LLL...L.......L.LLLL......L.L.L...LL
LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL..LLLLLL.LLLL.LLLL.L.LL.LLLLLLLLL.LLLLLL
LLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLL
LLLLL.LLLL.LLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LL.LLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLLLL.LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL
.L..LLL.L...LL.....LL......LL...L...LL...L.L..L....L.L.L.LL.L........L....LL......L..LL..LL....
LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLL
LLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLL.LLLL.LLLLLL
LLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLL.LLLLL.LLL..LLLL.LLLLLL.LLLLLLLLLLL.LL.LLLL.LLLLLLLLLLL
LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLL.LLLL..L.LLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLLLLL
LLLLLLLLLL.LLLL.LLLLLL.LLLLLL.LLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLLLL
L.LLL.LLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL
LLLL....L.......L..LLL...........L..L...LL..L.L.LLL...L.....LL..LL..L....L....L..LL..LL.L....L.
LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.L.LLLLLLLLLLLL.LLLLLL
LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLL.LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLL
LLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLLLLL
LLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLL..LLL.LLLL.LLLL.LLLL.LLLLLLLLLLL
LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLL.L.LLLL.LL.L.LL.LLLLLLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
.LL........L.....L.L.L.......LL.L.L.......LLL.........L....LL........L.L..L......L.LL......L..L
LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLLLLLLL.L.LLLLLLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLL
LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.L.LLLL.LLLLLL.LLLLLLLLL.LLLL.LLLLLLLLL.LLLLLL
LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLL..LLLLLL.LLLLLLLLL.LLLLLLLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL.LL.L.LLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLL
.L..L...L........L.......L.L.LL.LLLL...LLL.L.L..L.L....L.................L.L.L.L....L...L...L..
LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL
LLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL.LLLL
LLLLL.LL.LLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLL
LLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL
LLLL...LLL.......LL..L.L.L.L...L........LL..............L.L......L.......L..LL....L....LL...LL.
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLL.L.LLLLLLL.LLLLLLLLL.LLLLLLLLLLL
LLLLL.LLLL.LLLL.LLLLLL..LLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLL.L.LLLL.LLLLLL
LLLLL.LLLL.LLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLL
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL
LLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLLLL.LLLLL.LLLL.L.LL.LLLLLL.LLLLLLLLLLL.LL.LLLLLLLLL.LLL.LL
LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLLLLL
LLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLL.LLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
LL.LL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLLL
LLLL.L...LL.......LL..L.....L.LL..L....L.L..L.......L..L......LLLLL..L.L..L......L...L.L...L.L.
LLLLL.LLL..LLLL.LLLLLL.LLLLL.LLL.LLLLL.L.LLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLL.LLLLLLLL..LLLLLL
LLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLL
LLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL
LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
L.L...............L..L.LLLL...L..L...LLL.......LLL.LL........L..LL..L..L...L.L.L.LL..LLLL.L.LL.
LLLLLLLL.L.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLL
LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLLLL
LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL
LLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLLLLL
L.LLL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLL
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLL
.L..LL.L.L..L...L....L......LLL......L.LL..L....L.LLLL.LL.....L.L.LL.L.....L......L.LL.........
LLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLL.LLLL..LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLL.LLLLLLLLL.LLLLLL
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL
LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL
LLLLL.LLL..LLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLL.LLLLLLLLLLL.LLLLLLLLL..LLL.LLLLLLLLLLLLLLLL
.LLL......L.L.L......L.....LL......L.LLL.LLL..LL...L.L.......L..L.......L....L.....L.......LL..
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLL
LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
LLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLLL.LLLLLL
LLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
LL...L...........L...L..L......LL...........L...L.LL..LL....L....LLL.LLLL....LLL...LL..L..L...L
LLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLL.LLLL.LLLL.LLLLLLLLLLL
LLLLLLLLLL.LLLL.LLLLLL.LLL.LLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLL
.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL
LLLLL.LL.LLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLLL.LLLLLLLLLLL.LLLLLLLL..LLLL.LLL..LLLLLLLLLLL
.......L...LL.L.....L.L.....LLL.L.......L.....LL.......L..LLL.....L.LL.L..........LL...........
LLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLL.LL.LL.LLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLLLL
LLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLL.LLLLLL.LLLLLLLL..LLLLLLLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.LLLL.LLLL.LLLL.LLLLLL
LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL")

(defclass seats ()
  ((grid :initarg :grid)
   (phase :initarg :phase)))

(defmethod print-object ((object seats) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (grid phase) object
      :do (format stream "~&phase: ~a~&" phase)
      (loop :for y :below (array-dimension grid 0)
            :do (format stream "~&")
            :do (loop :for x :below (array-dimension grid 1)
                      :do (format stream "~a" (aref grid y x)))))))

(defun seats-equal (a b)
  (and (equalp (slot-value a 'grid)
               (slot-value b 'grid))
       (eq (slot-value a 'phase)
           (slot-value b 'phase))))

(defun load-lines (str)
  (with-input-from-string (in str)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun make-grid (lines)
  (make-array (list (length lines) (length (first lines)))
              :initial-contents lines))

(defun make-seats (lines phase)
  (make-instance 'seats :grid (make-grid lines)
                        :phase phase))

;; (defun emptyp (y x grid)
;;   (eql #\L (aref grid y x)))

;; (defun occupiedp (y x grid)
;;   (eql #\# (aref grid y x)))

(defun occupied-count (seats)
  (with-slots (grid) seats
    (loop for i from 0 below (array-total-size grid)
          count (eql #\# (row-major-aref grid i)))))

(defun look (y x dy dx adjacent-only grid)
  (loop for y2 = (+ y dy)
        for x2 = (+ x dx)
        for ch = (if (array-in-bounds-p grid y2 x2)
                  (aref grid y2 x2)
                  (return nil))
        do (cond
             ((eql ch #\#) (return t))  ; occupied
             ((or (eql ch #\L)          ; either empty
                  adjacent-only)        ; or adjacent squares only
              (return nil))
             (t (setf y y2)             ; keep looking
                (setf x x2)))))

(defun count-nearby-occupants (y x adjacent-only grid)
  (loop for (dy dx) in '((-1 -1) (-1 0) (-1 1)
                         (0 -1) (0 1)
                         (1 -1) (1 0) (1 1))
        count (look y x dy dx adjacent-only grid)))

(defun step-cell (y x phase grid)
  (let ((state (aref grid y x)))
    (if (eql state #\.)
        #\.
        (let* ((adjacent-only (ecase phase
                                (phase-one t)
                                (phase-two nil)))
               (stand-threshold (ecase phase
                                  (phase-one 4)
                                  (phase-two 5)))
               (nearby (count-nearby-occupants y x adjacent-only grid)))
          (ecase state
            (#\L (if (zerop nearby) #\# #\L))
            (#\# (if (>= nearby stand-threshold) #\L #\#)))))))

(defun step-seats (seats)
  (with-slots (grid phase) seats
    (loop with new-grid = (make-array (array-dimensions grid))
          for y from 0 below (array-dimension grid 0)
          do (loop for x from 0 below (array-dimension grid 1)
                   do (setf (aref new-grid y x)
                            (step-cell y x phase grid)))
          finally (return (make-instance 'seats :grid new-grid
                                                :phase phase)))))

(defun stabilize (seats)
  (do* ((curr seats next)
        (next (step-seats curr) (step-seats curr)))
       ((seats-equal curr next) curr)
    ()))

(defun phase-x (input phase)
  (occupied-count
   (stabilize
    (make-seats (load-lines input) phase))))

(defun phase-one (input)
  (phase-x input 'phase-one))

(defun phase-two (input)
  (phase-x input 'phase-two))

(defun test ()
  (assert (seats-equal (step-seats
                        (make-seats
                         (load-lines *example-input*)
                         'phase-one))
                       (make-seats '("#.##.##.##"
                                     "#######.##"
                                     "#.#.#..#.."
                                     "####.##.##"
                                     "#.##.##.##"
                                     "#.#####.##"
                                     "..#.#....."
                                     "##########"
                                     "#.######.#"
                                     "#.#####.##")
                                   'phase-one)))
    (assert (seats-equal (step-seats
                          (step-seats
                           (make-seats
                            (load-lines *example-input*)
                            'phase-one)))
                         (make-seats '("#.LL.L#.##"
                                       "#LLLLLL.L#"
                                       "L.L.L..L.."
                                       "#LLL.LL.L#"
                                       "#.LL.LL.LL"
                                       "#.LLLL#.##"
                                       "..L.L....."
                                       "#LLLLLLLL#"
                                       "#.LLLLLL.L"
                                       "#.#LLLL.##")
                                     'phase-one)))
    (assert (seats-equal (stabilize
                          (make-seats
                           (load-lines *example-input*)
                           'phase-one))
                         (make-seats '("#.#L.L#.##"
                                       "#LLL#LL.L#"
                                       "L.#.L..#.."
                                       "#L##.##.L#"
                                       "#.#L.LL.LL"
                                       "#.#L#L#.##"
                                       "..L.L....."
                                       "#L#L##L#L#"
                                       "#.LLLLLL.L"
                                       "#.#L#L#.##")
                                     'phase-one)))

  (assert (eql 37 (phase-one *example-input*)))
  (assert (eql 2406 (phase-one *input*)))

  ;; TODO: test phase-two rules.
  (assert (eql 26 (phase-two *example-input*)))
  (assert (eql 2149 (phase-two *input*))))
