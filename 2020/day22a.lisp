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

(defun play-combat (parsed)
  (destructuring-bind (player-a player-b) parsed
    (combat (second player-a) (second player-b))))

(defun test ()
  (assert (eql 306 (play-combat (parse-input *example-input*))))
  (assert (eql 33473 (play-combat (parse-input *input*)))))
