(defpackage :chapter-5
  (:use :cl))

(in-package :chapter-5)

;;; Variables and Side Effects


;; Scope and Local Variables

(defun double (n)
  (* n 2))


;; Simple Closures example. 

(defun make-adder (n)
  #'(lambda (x)
      (+ n x)))

(setf add (make-adder 5))

(funcall add 5)

;; 5.3 SETF ASSIGNS A VALUE TO A VARIABLE:

#|
(setf vowels '(a b c d e)) --> vowels --> > (A B C D E)
(length vowels) --> 5
(rest vowels) --> (B C D E)
|#

;;; Using setf to assign a variable or list globally

(defparameter *x* nil)

(defun long-list ()
  '(a b c d e f g h i j k))

#|
> (setf item (car (long-list)))
A
> item
A
> (long-list)
>(A B C D E F G H I J K)
|#

;;; Using setf inside a function assigning variable locally.

(defun just-a-scratch (p)
  (setf p (+ p 5))
  (list 'result 'is p))

;;; 5.5 THE LET SPECIAL FUNCTION

(defun average (x &optional (point (length x)))
  "Computer the average with error handling."
  (if (zerop point)
      (error "Cannot calculate average of zero elements.")
      (let ((sum (reduce #'+ x))
            (numbers (float point)))
        (format t "The average is : ~A" (/ sum numbers)))))

;;; Manipulating/testing/experimenting let on printing without arguments

(defun print-using-let ()
  (let ((x 2)
        (y 'ADI))
    (prog2
        (format t "~a ~a" x y)
        (list x y))))

;;; Tracing

(defun average (x y)
  (format t"Enter AVERAGE with inputs ~A and ~A~%" x y)
  (let ((x-value x)
        (y-value y))
    (format t " create variable X, with value ~A~%" x-value)
    (format t "     create variable Y, with value ~A~%" y-value)
    (let ((sum (+ x-value y-value)))
      (format t "          Enter LET body~%")
      (format t "              create variable SUM, with value ~A~%" sum)
      (format t "      Result of LET is ~A~%" (list x-value y-value 'average 'is (/ sum 2.0)))
      (/ sum 2.0))))

(defun switch-name (x)
  (let ((star (first x))
        (co-star (third x)))
    (list co-star 'and star))) --->  (switch-billing '(fred ginger adi))


;;; Since the let special operator assigned first element as star and third element as co-star it will evaluate in the body the star and co-star of what inside the list. since '(fred ginger adi)  since adi is the third element it will evaluate first within the body and the lst would be star.

;;; EXERCISE 5.1 Rewrite function POOR-STYLE to create a new local variable Q using
;; LET, instead of using SETF to change P. Call your new function
;; GOOD-STYLE.

(defun good-style (p)
  (let ((sum (+ p 5)))
    (list 'result 'is sum)))

;;; 5.6 THE LET* SPECIAL FUNCTION

(defun price-changed (old new)
  (let* ((diff (- new old))
         (proportion (/ diff old))
         (percentage (* proportion 100.0)))
    (format t "Widgest Changed: ~a%" percentage)))

;;; with bug that returns nil
(defun coin-with-bug ()
  (cond ((< (random 101) 50) 'heads)
        ((> (random 101) 50) 'tails)
        ((t 'edge))))

;;; without a bug
(defun fair-coin ()
  (let ((toss (random 101)))
    (cond ((< toss 50) 'heads)
          ((> toss 50) 'tails)
          (t 'edge))))


;;;EXERCISE
#|
5.2. What is a side effect?
a side effect refers to any observable change that occurs in the system when a function or expression is executed

5.3. What is the difference between a local and global variable?
local- are only accessible within the block where they are defined.
global- are accessible from anywhere within the program, including all functions and blocks.

5.4 Why must SETF be a macro function instead of a regular function?
SETF cannot be an ordinary function because it does not evaluate its first argument. In Lisp, functions generally evaluate all their arguments before executing. However, SETF operates differently. It needs its first argument unevaluated so that it can use it as a place to perform the assignment or modification. 


5.5. Are LET and LET* equivalent when you are only creating one local variable?
yes the special symbol is just for the users to know if that special operator have one-or-more local variable just to have a good read from the user.

|#

;; FUNCTIONS COVERED IN THIS CHAPTER
;; Macro function for assignment: SETF.
;; Special functions for creating local variables: LET, LET*.


;; EXERCISE
;; 5.6. This keyboard exercise is about dice. We will start with a function to throw one die and end up with a program to play craps

;;Be sure to include a documentation string for each function you write.

;;Own code experimentation of whole exercise from a to g just practicing using let

;; This global variable is too see the first and second rolled dices for the function play-dice.
;; as you can see I push the result of my first and second dices on this global variable in able to see whether the dice win-loss is accurate or telling the truth. 

(defparameter *throw-dices-history* '())

;; This global variable is for predicate snake-eyes and boxcars, the *throw-dice-whole-output* will received the evaluated list on throw-dice

;;for example > (ROLLED DICE (1 2) 3) 

(defparameter *throw-lists* '())

;;This predicate will determine the list from global variable if it's equal to '(1 1) and '(6 6)
;;calling these predicates inside the play-dice function 

(defun snake-eyes-p ()
  (equal (caddr *throw-lists*) '(1 1)))

(defun boxcars-p ()
  (equal (caddr *throw-lists*) '(6 6)))


;;; Display elements of rolled dices

(defun show-history ()
  *throw-dices-history*)

;; throwing the dice function

(defun throw-dice ()
  "Function for throwing dices that evaluates in lists"
  (let* ((first (random 6))
         (second (random 6))
         (result (list (+ first 1)
                       (+ second 1)))
         (total (reduce #'+ result)))
    (push result *throw-dices-history*)
    (setf *throw-lists* (list 'rolled 'dice result total)))) ;; --> (throw) --> (ROLLED DICE (1 2) 3)

(defun play-craps-dice ()
  "Function where the cadddr 4th element gets the total of 2 dices to compare if the player wins or loss"
  (let ((total-rolled (cadddr (throw-dice))))
    (format t "Dices: ~a~%Total Rolled:~a~%~a" total-rolled (caddr *throw-lists*) (show-history))
    (cond ((equal total-rolled 7) 'WIN)
          ((equal total-rolled 11) 'WIN)
          ((equal total-rolled 3) 'LOSS)
          ((snake-eyes-p) '(snake-eyes -- you-lose))
          ((boxcars-p) '(boxcars -- you-lose))
          (t 'throw-again))))


;;; Using Hash table

(defparameter *throw-dices-history* (make-hash-table))

(defun snake-eyes-p ()
  (equal (caddr *throw-lists*) '(1 1)))

(defun boxcars-p ()
  (equal (caddr *throw-lists*) '(6 6)))

(defun show-history ()
  (maphash (lambda (key value)
             (format t "Roll ~A: ~A~%" (+ key 1) value))
           *throw-dices-history*))

(defun clear-history (hash-table)
  (clrhash hash-table))

;;; Helper

(defun throw-dice ()
  "Function for throwing dices that evaluates in lists"
  (let* ((first (random 6))
         (second (random 6))
         (result (list (+ first 1)
                       (+ second 1)))
         (total (reduce #'+ result))
         (roll-key (hash-table-count *throw-dices-history*)))  ;; Use the count as a unique key
    (setf (gethash roll-key *throw-dices-history*) (list 'rolled 'dice result total))
    (setf *throw-lists* (list 'rolled 'dice result total))))

;;; Main

(defun play-craps-dice ()
  "Function where the cadddr 4th element gets the total of 2 dices to compare if the player wins or loss"
  (let ((total-rolled (cadddr (throw-dice))))
    (format t "Total Rolled: ~A~%Dices: ~A~% ~A" total-rolled (caddr *throw-lists*) (show-history))
    (cond ((equal total-rolled 7) 'WIN)
          ((equal total-rolled 11) 'WIN)
          ((equal total-rolled 3) 'LOSS)
          ((snake-eyes-p) '(snake-eyes -- you-lose))
          ((boxcars-p) '(boxcars -- you-lose))
          (t 'throw-again))))
#|
CHAPTER-5> (play-craps-dice)
Roll 1: (ROLLED DICE (5 2) 7)
Roll 2: (ROLLED DICE (3 2) 5)
Roll 3: (ROLLED DICE (2 4) 6)
Roll 4: (ROLLED DICE (3 3) 6)
Roll 5: (ROLLED DICE (1 2) 3)
Dices: 3
Total Rolled:(1 2)
NIL
LOSS
; processing (DEFUN PLAY-CRAPS-DICE ...) ; ; ; ; ; ; ;
CHAPTER-5> (play-craps-dice)
Roll 1: (ROLLED DICE (5 2) 7)
Roll 2: (ROLLED DICE (3 2) 5)
Roll 3: (ROLLED DICE (2 4) 6)
Roll 4: (ROLLED DICE (3 3) 6)
Roll 5: (ROLLED DICE (1 2) 3)
Roll 6: (ROLLED DICE (5 4) 9)
Dices: 9
Total Rolled:(5 4)
NIL
THROW-AGAIN
CHAPTER-5> (play-craps-dice)
Roll 1: (ROLLED DICE (5 2) 7)
Roll 2: (ROLLED DICE (3 2) 5)
Roll 3: (ROLLED DICE (2 4) 6)
Roll 4: (ROLLED DICE (3 3) 6)
Roll 5: (ROLLED DICE (1 2) 3)
Roll 6: (ROLLED DICE (5 4) 9)
Roll 7: (ROLLED DICE (5 6) 11)
Dices: 11
Total Rolled:(5 6)
NIL
WIN
|#
;;;FOR DOCUMENTATION
(documentation 'throw-dice 'function)
;; it will return --> "Function for throwing dices that evaluates in lists"
;; Providing documentation strings for functions you write is good
;; programming practice. It also helps other people to use your programs, since
;; online documentation is always available whenever they need assistance.


