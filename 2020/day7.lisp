;;;;
;;;; Constraint: Use only the standard library.
;;;;
;;;; Disclaimer: I'm very new to the language.
;;;;
;;;; Apology: Not enough comments.
;;;;

(defpackage :day7 (:use #:common-lisp))
(in-package :day7)

(declaim (optimize (debug 3)))

(defun eat-chars (predicate string start end)
  (loop for i = start then (1+ i)
        while (and (< i end)
                   (funcall predicate (aref string i)))
        finally (return i)))

(defun eat-alphanumeric (string start end)
  (eat-chars #'alphanumericp string start end))

(defun eat-whitespace (string start end)
  (flet ((whitespacep (ch)
           (member ch '(#\Space #\Newline))))
    (eat-chars #'whitespacep string start end)))

(defun eat-something (string start end)
  (setf start (eat-whitespace string start end))
  (cond ((equal start end)
         (list 'eof end))
        ((equal (aref string start) #\,)
         (list 'comma (1+ start)))
        ((equal (aref string start) #\.)
         (list 'period (1+ start)))
        (t
         (let ((token-end (eat-alphanumeric string start end)))
           (assert (< start token-end))
           (list (read-from-string string nil 'eof
                                   :start start :end token-end)
                 token-end)))))

(defun tokenize (string)
  (let ((end (length string)))
    (loop for (token next) = (eat-something string 0 end)
            :then (eat-something string next end)
          :while (not (eq token 'eof))
          :collect token)))

;;;; document => ( sentence )*
;;;; sentence => color BAGS CONTAIN bag-list PERIOD
;;;; bag-list => ( no-other-bags | count-with-color-list )
;;;; count-with-color-list => count-with-color ( COMMA count-with-color )* )
;;;; no-other-bags => NO OTHER BAGS
;;;; count-with-color => NUMBER color ( BAG | BAGS )
;;;; color => color-adjective color-name

(define-condition document-parse-error (error)
  ((expected :initarg :expected
             :initform (error "Must supply :expected arg.")
             :reader expected)
   (actual :initarg :actual
           :initform (error "Must supply :actual arg.")
           :reader actual))
  (:report (lambda (condition stream)
             (format stream "Parse error; expected ~S but got ~S."
                     (expected condition)
                     (actual condition)))))

(defun signal-parse-error (expected actual)
  (error 'document-parse-error :expected expected :actual actual))

(defparameter *tokens* nil)

(defun peek-token ()
  (first *tokens*))

(defun pop-token ()
  (pop *tokens*))

(defun parse-document (tokens)
  (let ((*tokens* tokens))
    (loop :while (peek-token)
          :collect (parse-sentence))))

(defun parse-sentence ()
  (let ((color (parse-color)))
    (must-pop-token 'bags)
    (must-pop-token 'contain)
    (let ((bag-list (parse-bag-list)))
      (must-pop-token 'period)
      (list color bag-list))))

(defun color-adjective-p (token)
  (member token '(bright dark faded light muted vibrant)))

(defun color-name-p (token)
  (member token '(orange purple red white yellow)))

(defun must-pop-token (validator &optional description)
  (let ((token (peek-token)))
    (etypecase validator
      (function
       (unless (funcall validator token)
         (signal-parse-error description token)))
      (symbol
       (unless (eq validator token)
         (signal-parse-error (symbol-name validator) token))))
    (pop-token)))

(defun parse-color ()
  (list 'color
        (must-pop-token #'color-adjective-p "a color adjective")
        (must-pop-token #'color-name-p "a color name")))

(defun parse-no-other-bags ()
  (must-pop-token 'no (symbol-name 'no))
  (must-pop-token 'other (symbol-name 'other))
  (must-pop-token 'bags (symbol-name 'bags)))

(defun parse-count-with-color ()
  (let* ((count (must-pop-token #'numberp "a number"))
         (color (parse-color)))
    (must-pop-token #'(lambda (token) (member token '(bag bags)))
                    "either BAG or BAGS")
    (list 'count count color)))

(defun parse-count-with-color-list ()
  (loop :while (numberp (peek-token))
        :collect (parse-count-with-color)
        :do (if (eq 'comma (peek-token))
                (pop-token))))

(defun parse-bag-list ()
  (cond
    ((eq (peek-token) 'no)
     (parse-no-other-bags)
     nil)
    ((typep (peek-token) 'number)
     (parse-count-with-color-list))
    (t
     (signal-parse-error "a number of the symbol NO" (peek-token)))))

(defparameter *tiny-input*
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.")

(defparameter *example-input*
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defun test ()
  ;; (assert (equal '(eof 1) (eat-something "  " 1 1)))
  ;; (assert (equal '(some 4) (eat-something "something" 0 4)))
  ;; (assert (equal '(so 4) (eat-something "  something" 0 4)))
  ;; (assert (equal '(comma 1) (eat-something ",a" 0 3)))
  ;; (assert (equal '(period 1) (eat-something "." 0 1)))

  ;; (assert (equal '(COMMA PERIOD A COMMA B PERIOD A COMMA B PERIOD)
  ;;                (tokenize ",.a,b. a, b.")))

  ;; (assert (equal '(LIGHT RED BAGS CONTAIN 1 BRIGHT WHITE BAG COMMA
  ;;                  2 MUTED YELLOW BAGS PERIOD
  ;;                  DARK ORANGE BAGS CONTAIN 3 BRIGHT WHITE BAGS COMMA
  ;;                  4 MUTED YELLOW BAGS PERIOD)
  ;;                (tokenize
  ;;                     "light red bags contain 1 bright white bag, 
  ;;                      2 muted yellow bags.
  ;;                      dark orange bags contain 3 bright white bags, 
  ;;                      4 muted yellow bags.")))

  (assert (equal '(((COLOR LIGHT RED)
                    ((COUNT 1 (COLOR BRIGHT WHITE))
                     (COUNT 2 (COLOR MUTED YELLOW)))))
                 (parse-document
                  '(LIGHT RED BAGS CONTAIN 1 BRIGHT WHITE BAG COMMA
                    2 MUTED YELLOW BAGS PERIOD))))

  (assert (equal '(((COLOR LIGHT RED)
                    ((COUNT 1 (COLOR BRIGHT WHITE))
                     (COUNT 2 (COLOR MUTED YELLOW))))
                   ((COLOR DARK ORANGE) ((COUNT 3 (COLOR BRIGHT WHITE)))))
                 (parse-document
                  (tokenize
                   "light red bags contain 1 bright white bag,
                    2 muted yellow bags.
                    dark orange bags contain 3 bright white bags."))))

  ;; (assert (match '(1 #\x) '(integer character)))

  ;; (assert (equal '() (split "" #\,)))
  ;; (assert (equal '((0 1)) (split "a" #\,)))
  ;; (assert (equal '() (split "," #\,)))
  ;; (assert (equal '((1 2)) (split ",a" #\,)))
  ;; (assert (equal '((0 1)) (split "a," #\,)))
  ;; (assert (equal '((0 1) (2 3)) (split "a,a" #\,)))
  ;; (assert (equal '((0 1) (5 6)) (split "a,,,,a" #\,)))
  )
