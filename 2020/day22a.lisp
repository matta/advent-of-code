;;
;; Day 22 of Advent of Code 2020, in Common Lisp.  This solves Part One.
;; See https://adventofcode.com/2020/day/22 for the problem statement.
;;
;; I'm using only standard Common Lisp functions; no external packages.
;;

(defpackage :day22a (:use #:common-lisp))
(in-package :day22a)

(defparameter *example-input*
  "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10")

(defparameter *example2-input*
  "Player 1:
43
19

Player 2:
2
29
14")

(defparameter *input*
  "Player 1:
44
31
29
48
40
50
33
14
10
30
5
15
41
45
12
4
3
17
36
1
23
34
38
16
18

Player 2:
24
20
11
32
43
9
6
27
35
2
46
21
7
49
26
39
8
19
42
22
47
28
25
13
37")

(defun split-sequence (needle haystack &key (start 0) end)
  (loop with needle-length = (length needle)
        for haystack-start = start then (+ pos needle-length)
        for haystack-end = end
        for pos = (search needle haystack
                          :start2 haystack-start
                          :end2 haystack-end)
        collect (subseq haystack haystack-start pos)
        until (null pos)))

(defun parse-deck (input)
  (destructuring-bind (player &rest cards) (split-sequence '(#\Newline) input)
    (list player
          (mapcar #'parse-integer cards))))

(defun parse-input (input)
  (mapcar #'parse-deck
          (split-sequence '(#\Newline #\Newline) input)))

(defun win-combat (deck)
  (loop for i from 1
        for card in (reverse deck)
        sum (* i card)))

(defun combat (deck-a deck-b)
  (cond
    ((null deck-a) (win-combat deck-b))
    ((null deck-b) (win-combat deck-a))
    (t (let ((card-a (pop deck-a))
             (card-b (pop deck-b)))
         (flet ((next-round (winner-card winner-rest loser-card loser-rest)
                  (combat loser-rest (append winner-rest (list winner-card loser-card)))))
           (if (> card-a card-b)
               (next-round card-a deck-a card-b deck-b)
               (next-round card-b deck-b card-a deck-a)))))))

(defun play (game parsed)
  (destructuring-bind (player-a player-b) parsed
    (funcall game (second player-a) (second player-b))))

(defun win-recursive-combat (player deck)
  (list player (win-combat deck) deck))

(defun recursive-combat (deck-a deck-b
                         &optional (memory (make-hash-table :test #'equal)))
  (let ((hands (list deck-a deck-b)))
    (if (gethash hands memory)
        (win-recursive-combat 'a deck-a)
        (progn
          (setf (gethash hands memory) t)
          (cond
            ((null deck-a) (win-recursive-combat 'b deck-b))
            ((null deck-b) (win-recursive-combat 'a deck-a))
            (t (let ((card-a (pop deck-a))
                     (card-b (pop deck-b)))
                 (if (if (and (>= (length deck-a) card-a)
                              (>= (length deck-b) card-b))
                         (eq 'a (first (recursive-combat
                                        (subseq deck-a 0 card-a)
                                        (subseq deck-b 0 card-b))))
                         (> card-a card-b))
                     (recursive-combat (append deck-a (list card-a card-b))
                                       deck-b
                                       memory)
                     (recursive-combat deck-a
                                       (append deck-b (list card-b card-a))
                                       memory)))))))))

(defun test ()
  (assert (eql 306 (play #'combat (parse-input *example-input*))))
  (assert (eql 33473 (play #'combat (parse-input *input*))))
  (assert (equal '(B 291 (7 5 6 2 4 1 10 8 9 3))
                 (play #'recursive-combat
                       (parse-input *example-input*))))
  (assert (equal '(A 105 (43 19))
                 (play #'recursive-combat
                       (parse-input *example2-input*))))
  (assert (equal '(A 31793 (15 7 36 31 22 2 21 1 49 30 37 24 35 25 5 8 48 27
                            33 19 43 26 14 3 18 13 50 46 40 20 47 39 28 4 38
                            34 23 9 45 12 44 11 42 29 16 10 41 17 32 6))
                 (play #'recursive-combat
                       (parse-input *input*)))))
