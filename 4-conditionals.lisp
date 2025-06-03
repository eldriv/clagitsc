(defpackage :chapter-4
  (:use :cl))

(in-package :chapter-4)

;;; Conditionals

;;; Introduction
#|
Decision making is a fundamental part of computing; all nontrivial programs
make decisions. In this chapter we will study some special decision-making
functions, called conditi      'test-was-true
'test-was-false))

(defun test-t ()
(if t
'test-was-true
'test-was-false))

(defun symbol-p (x)
(if (symbolp x)
(format t "this is a symbol: ~a" x)
(format t "This is not a symbol: ~a" x)))

(defun my-abs (x)
(if (< x 0)
(abs x)
x))

(defun symbol-test (x)
(if (symbolp x)
(list 'yes x 'is 'a 'symbol)
(list 'no x 'is 'not 'a 'symbol)))

;;; Exercises
;;; 4.1 Write a function MAKE-EVEN that makes an odd number even by adding one to it. If the input to MAKE-EVENis already even, it should be returned unchanged.

(defun make-even (x)
"Using Odd predicate in adding 1 to make even"
(if (oddp x)
(+ x 1)
x))

(defun make-even (x)
(cond ((oddp x) (+ x 1))
(t x)))

;; or

(defun make-even (n)
"Using even predicate in adding 1 to make even"
(if (evenp n)
n
(+ n 1)))

;;; 4.2 Write a function FURTHER that makes a positive number larger by adding one to it, and a negative number smaller by subtracting one from it. What does your function do if given the number 0?

(defun further (x)
"Positive larger x will add 1 and Negative smaller x subtract 1"
(if (> x 0)
(+ x 1)
(if (< x 0)
(- x 1)
x)))

(defun further (x)
(cond ((> x 0) (+ x 1))
((< x 0) (- x 1))
(t x)))

;;; Concise way
;; Signum
;; If the number x is greater than zero, signum(x) returns 1.
;; If the number x is less than zero, signum(x) returns −1.
;; If the number x is exactly zero, signum(x) returns 0.

(defun further (x)
(+ x (signum x)))

;;; 4.3  Recall the primitive function NOT:
;; It returns NIL for a true input and
;; T for a false one. Suppose Lisp didn’t have a NOT primitive. Show
;; how to write NOT using just IF and constants (no other functions). Call
;; your function MY-NOT.

(defun my-not (x)
"Return nil for a true input and T for a false one"
(if x
nil
t))

;;; 4.4 Write a function ORDERED that takes two numbers as input and
;; makes a list of them in ascending order. (ORDERED 3 4) should return
;; the list (3 4). (ORDERED 4 3) should also return (3 4), in other words,
;; the first and second inputs should appear in reverse order when the first
;; is greater than the second.

(defun ordered (&rest num1 num2)
"Return a list of 2 arguments in ascending order"
(if (> num1 num2)
(list num2 num1)
(list num1 num2)))


(defun ordered (order &rest numbers)
(cond ((or (eq order 'ascending)
(eq order 'descending))
(sort numbers (if (eq order 'ascending)
#'<
#'>)))
(t
(format t "Uknown order: ~a" order))))

(defun ascending-order (&rest numbers)
(sort numbers #'<))

(defun descending-order (&rest numbers)
(sort numbers #'>))

CHAPTER-4> (ordered 'descending 8 4 2 1 9 5) --> (9 8 5 4 2 1)
CHAPTER-4> (ordered 'ascending 8 4 2 1 9 5)  --> (1 2 4 5 8 9)
CHAPTER-4> (ordered 'looking 8 4 2 1 9 5)    --> Unknown order: LOOKING

;;; 4.5 The conditional Macro

;;; COND is the classic Lisp conditional.
;; Parenthesis errors can play havoc with COND expressions. Most COND clauses begin with exactly two parentheses.
;; The general form of a COND expression will be
;; described in Chapter 5, but a slightly simplified form is:

(COND (test-1 consequent-1)
(test-2 consequent-2)
(test-3 consequent-3)
....
(test-n consequent-n))

;;;Example
(defun compare (x y)
(cond ((equal x y) 'numbers-are-the-same)
((< x y) 'first-is-smaller)
((> x y) 'first-is-bigger)))


;; EXERCISE  4.5.
;; For each of the following calls to COMPARE, write ‘‘1,’’ ‘‘2,’’ or ‘‘3’’
;; to indicate which clause of the COND will have a predicate that
;; evaluates to true.

#|
(compare 9 1) --> 3
(compare (+ 2 2) 5) --> 2 
(compare 6 (* 2 3)) --> 1
|#

;; 4.4 USING T AS A TEST

(defun where-is (x)
(cond ((equal x 'paris) 'france)
((equal x 'milan) 'italy)
((equal x 'manila) 'philippines)
(t 'unknown)))

;;; or using if nested 
(defun where-is-2 (x)
(if (equal x 'paris)
'france
(if (equal x 'milan)
'italy
(if (equal x 'manila)
'philippines
(if t
'unknown)))))

;;; another version of WHERE-IS using AND and OR.This version employs a simple two-level scheme rather than themore complex nesting required for IF.
(defun where-is-3 (x)
(or (and (equal x 'paris) 'france)
(and (equal x 'london) 'england)
(and (equal x 'beijing) 'china)
'unknown))

;; EXERCISE 4.6. Write a version of the absolute value function MY-ABS using COND instead of IF.
(defun my-abs (x)
(cond ((< x 0) (- x))
(t x)))

;; 4.5 TWO MORE EXAMPLES OF COND

(defun emphasize (list)
(cond ((equal (car list) 'good) (cons 'great (cdr list)))
((equal (car list) 'bad) (cons 'awful (cdr list)))))

(defun emphasize2 (x)
(cond ((equal (first x) 'good) (cons 'great (rest x)))
((equal (first x) 'bad) (cons 'awful (rest x)))
(t x)))

;; Write EMPHASIZE3, which is like EMPHASIZE2 but adds the symbol
;; VERY onto the list if it doesn’t know how to emphasize it. For
;; example, EMPHASIZE3 of (LONG DAY) should produce (VERY
;; LONG DAY). What does EMPHASIZE3 of (VERY LONG DAY)
;; produce?

(defun emphasize3 (x)
(cond ((equal (first x) 'good) (cons 'great (rest x)))
((equal (first x) 'bad) (cons 'awful (rest x)))
((equal (first x) 'long) (cons 'long x)))) ;;--> (long long day)

(defun emphasize3 (x)
(cond ((equal (first x) 'good) (cons 'great (rest x)))
((equal (first x) 'bad) (cons 'awful (rest x)))
(t (cons 'long x)))) ;;--> (emphasize3 ’(very long day)) ⇒ (very very long day)

(defun compute (op x y)
"First input returns the sum of the second and third input"
(cond ((equal op 'sum-of) (+ x y))
((equal op 'product-of) (* x y))
((equal op 'quotient-of) (/ x y))
((equal op 'difference-of) (- x y))
(t '(that does not compute))))

;; 4.6 COND AND PARENTHESIS ERRORS

;; 4.7. For each of the following COND expressions, tell whether the
;; parenthesization is correct or incorrect. If incorrect, explain where the
;; error lies.

(cond (symbolp x) ’symbol
(t 'not-a-symbol)) --> incorrect, because it has only 1 parenthesis on the start of expression and no end parenthesis on the 'symbol part. It should be  --> (cond ((symbolp x) 'symbol) (t 'not-a-symbol))

;;; Let's try my answer from 4.7 
(defun symbol-check (x)
(cond ((symbolp x)
(format t "~a is a symbol" x) 'symbol)
(t
(format t "~a is not a symbol" x) 'not-a-symbol)))

;; (symbol-check 'this-is-a-symbol) --> symbol
;; (symbol-check 5) --> not-a-symbol
;; As you cann see it worked! it should have a proper parentheses in able to have a right evaluation.

;; 4.9 Type in the following suspicious function definition:

(defun make-odd (x)
(cond (t x)
((not (oddp x)) (+ x 1))))

;; I rewrite the function because as you can see on the function above, the first expression should be on the last since the rules in classic cond is that it should start on 2 parenthesis if making an argument. 
(defun make-odd (x)
(cond ((evenp x) (+ x 1))
(t (format t "This is already an odd: ~a" x))))

;; What is wrong with this function? Try out the function on the numbers
;; 3, 4, and -2. Rewrite it so it works correctly.

;; 4.10 Write a function CONSTRAIN that takes three inputs called X, MAX,
;; and MIN. If X is less than MIN, it should return MIN; if X is greater
;; than MAX, it should return MAX. Otherwise, since X is between MIN
;; and MAX, it should return X. (CONSTRAIN 3 -50 50) should return 3.
;; (CONSTRAIN 92 -50 50) should return 50. Write one version using
;; COND and another using nested IFs.

(defun constrain (x min max)
(cond ((< x min) min)
((> x max) max)
(t x)))

(defun constrain (x min max)
(if (< x min)
min
(if (> x max)
max
(if t
x))))

;;; 4.11  Write a function FIRSTZERO that takes a list of three numbers as input
;; and returns a word (one of ‘‘first,’’ ‘‘second,’’ ‘‘third,’’ or ‘‘none’’)
;; indicating where the first zero appears in the list.
;; Example:
;; (FIRSTZERO ’(3 0 4)) should return SECOND. What happens if you
;; try to call FIRSTZERO with three separate numbers instead of a list of
;; three numbers, as in (FIRSTZERO 3 0 4)?

(defun firstzero-cond (lst)
"
(firstzero-cond '(1 2 3 5 0))
(firstzero-cond '(1 2 3 0 5))
" (let ((find-zero (position 0 lst :from-end t)))
(when find-zero
(nth find-zero '(FIRST SECOND THIRD FOURTH FIFTH)))))

(defun firstzero-cond (lst)
"Same result on if but conditional"
(cond ((zerop (first lst)) 'first)
((zerop (second lst)) 'second)
((zerop (third lst)) 'third)
(t 'none)))

(defun firstzero-cond (lst)
"Returns an index word where there is any zero in the list"
(cond ((and (zerop (first lst)) (zerop (second lst)) (zerop (third lst))) '(first second third))
((and (zerop (first lst)) (zerop (second lst))) '(first second))
((and (zerop (second lst)) (zerop (third lst))) '(second third))
((and (zerop (first lst)) (zerop (third lst))) '(first third))
((zerop (first lst)) 'first)
((zerop (second lst)) 'second)
((zerop (third lst)) 'third)
(t 'none)))

(defun firstzero-if (a b c)
"Returns first, secon, third if the separate numbers detect zero."
(if (zerop a)
'first
(if (zerop b)
'second
(if (zerop c)
'third
(if t
'none)))))

;; 4.11 Write a function CYCLE that cyclically counts from 1 to 99. CYCLE
;; called with an input of 1 should return 2, with an input of 2 should
;; return 3, with an input of 3 should return 4, and so on. With an input of
;; 99, CYCLE should return 1. That’s the cyclical part. Do not try to
;; solve this with 99 COND clauses!

(defun cycle (num)
(cond ((not (and (<= 1 num 99))))
((= num 99) (format t "~a/99 CYCLE COMPLETE" num) 1)
(t (+ num 1))))

(defun cycle (num)
(cond ((<= 1 num 99)
(cond ((= num 99)
(format t "~a/99 CYCLE COMPLETED~%" num) 1)
(t (+ num 1))))
((>= num 100) (format t "You exceeded the limit, The cap is only 99"))
(t (format t "~a is a negative number, only positive can cyclically counts" num))))

;;; using progn and nested if

(defun cycle (num)
(format t "~a/99" num)
(if (and (integerp num) (<= 1 num 99))
(if (= num 99)
(progn
(format t " CYCLE COMPLETE~%")
1)
(+ num 1))
(format t " Stop!")))

;; 4.13. Write a function HOWCOMPUTE that is the inverse of the COMPUTE
;; function described previously. HOWCOMPUTE takes three numbers
;; as input and figures out what operation would produce the third from
;; the first two. (HOWCOMPUTE 3 4 7) should return SUM-OF.

(defun how-compute (a b c)
"conditional statement"
(cond ((equal (+ a b) c) 'sum-of)
((equal (* a b) c) 'product-of)
(t '(beats-me))))

(defun how-compute-if (a b c)
"if-statement"
(if (and (equal (+ a b) c) (* a b) c)
'sum-of
'product-of)
'(beats-me))

;; 4.7 THE AND AND OR MACROS
;; Suppose we want a predicate for small (no more than two digit) positive odd numbers. We can use AND to express this conjunction of simple conditions:

(defun small-positive-oddp (x)
(and (< x 100)
(> x 0)
(oddp x)))

(defun gtest (x y)
(or (> x y) (zerop x) (zerop y)))

;;; diferrence of or & and
;;; If a clause returns something other than NIL, stop and return that value; otherwise go on to the next clause, or return NIL if none are left.

(or nil nil 'george)  --> george

(or 'george nil 'harry) --> george

(or nil t t) --> t

(and nil t t) --> nil

(and 'george 'harry nil) --> harry

(and 'george nil 'harry) --> nil

;; EXERCISE
;; What results do the following expressions produce?
;; Read the evaluation rules for AND and OR carefully before answering.

;; (and ’fee ’fie ’foe) --> foe
;; (or ’fee ’fie ’foe) --> fee
;; (or nil ’foe nil) --> foe
;; (and ’fee ’fie nil) --> nil
;; (and (equal ’abc ’abc) ’yes) --> yes
;; (or (equal ’abc ’abc) ’yes) --> T

;; EXERCISES
;; 4.15. Write a predicate called GEQ that returns T if its first input is greater
;; than or equal to its second input.

(defun geq-p (x y)
(or (> x y) (equal x y)))

;; 4.16. Write a function that
;; squares a number if it is odd and positive,
;; doubles it if it is odd and negative,
;; and otherwise divides the number by 2.

(defun idk-the-name-i-can-define-this-function (x)
(cond ((and (> x 0) (oddp x) (* x x)))
((and (< x 0) (oddp x) (* x 2)))
(t (/ x 2))))

;; 4.17 Write a predicate that returns T if the first input is either BOY or GIRL
;; and the second input is CHILD, or the first input is either MAN or WOMAN and the second input is ADULT.

;;using equal function

(defun gender-p (person age)
(cond ((or (and (equal person 'BOY) (equal age 'CHILD))
(and (equal person 'GIRL) (equal age 'CHILD))
(and (equal person 'MAN) (equal age 'ADULT))
(and (equal person 'WOMAN) (equal age 'ADULT))))))

(defun gender-p-if (person age)
(if (or (and (equal person 'boy) (equal age 'child))
(and (equal person 'girl) (equal age 'child))
(and (equal person 'MAN) (equal age 'ADULT))
(and (equal person 'WOMAN) (equal age 'ADULT)))
t nil))

;;; using member function
;;; member function is used to check if an element on the list is present. It only takes two arguments.

(defun gender-p (person age)
(cond ((or (and (member person '(BOY GIRL)) (equal age 'CHILD))
(and (member person '(MAN WOMAN)) (equal age 'ADULT))))
(t nil)))


(defun gender-p (person age)
(if (or (and (member person '(boy girl)) (equal age 'CHILD))
(and (member person '(man woman)) (equal age 'adult)))
t nil))

;; 4.18. Write a function to act as referee in the Rock-Scissors-Paper game. In
;; this game, each player picks one of Rock, Scissors, or Paper, and then
;; both players tell what they picked. Rock ‘‘breaks’’ Scissors, so if the
;; first player picks Rock and the second picks Scissors, the first player
;; wins. Scissors ‘‘cuts’’ Paper, and Paper ‘‘covers’’ Rock. If both
;; players pick the same thing, it’s a tie. The function PLAY should take
;; two inputs, each of which is either ROCK, SCISSORS, or PAPER, and
;; return one of the symbols FIRST-WINS, SECOND-WINS, or TIE.
;; Examples: (PLAY ’ROCK ’SCISSORS) should return FIRST-WINS.
;; (PLAY ’PAPER ’SCISSORS) should return SECOND-WINS.

;;; Scissors ‘‘cuts’’ Paper, and Paper ‘‘covers’’ Rock.
;;; If both players pick the same thing, it’s a tie.

;;; conditional statement using equal,or and

(defun play-1 (p1 p2)
(cond ((equal p1 p2) 'tie)
((or (and (equal p1 'rock)
(equal p2 'scissors))
(and (equal p1 'scissors)
(equal p2 'paper))
(and (equal p1 'paper)
(equal p2 'rock)))
'first-wins)
(t 'second-wins)))

;;; if state using equal, or, and

(defun play-2 (p1 p2)
(if (equal p1 p2)
'tie
(if (or (and (equal p1 'rock) (equal p2 'scissors))
(and (equal p1 'scissors) (equal p2 'paper))
(and (equal p1 'paper) (equal p2 'rock)))
'first-wins
'second-wins)))

(defun posnump (x)
(and (numberp x) (plusp x)))

(defun faulty-posnump (x)
(and (plusp x) (numberp x)))


;; NOTE!!!
;; Since IF, COND, and AND/OR are interchangeable conditionals, you may wonder why Lisp has more than one. It’s a matter of convenience.

;; IF is the easiest to use for simple functions like absolute value.
;; AND and OR are good for writing complex predicates like SMALL-POSITIVE-ODDP.
;; COND is easiest to use when there are many tests, as in WHERE-IS and HOW-ALIKE.

;; Choosing the right conditional for the job is part of the art of programming.

;;; EXERCISE
;; 4.19 Show how to write the expression (AND X Y Z W) using COND
;; instead of AND. Then show how to write it using nested IFs instead of
;; AND.

;; conditional statement

(cond ((not 'x))
((not 'y))
((not 'z))
((not 'w))
(t 'w))
;; if nested statement

(if (not 'x)
'w
(if (not 'y)
'w
(if (not 'z)
'w
(if (not 'w)
'w
'w))))
;;using null

(cond ((null 'x))
((null 'y))
((null 'z))
((null 'w))
(t 'w))

;;using or and null

(cond ((or (null 'x)
(null 'y)
(null 'z)
(null 'w))
'w)
(t 'w))

;;using find and null

(cond ((find 'null '(x y z w)) 'w)
(t 'w))

;;; using every and null

(cond ((every #'null '(x y z w)) 'w)
(t 'w))

;;; 4.20. Write a version of the COMPARE function using IF instead of COND.
;;; Also write a version using AND and OR.


(defun compare (x y)
(cond ((equal x y) 'numbers-are-the-same)
((< x y) 'first-is-smaller)
((> x y) 'first-is-bigger)))

(defun compare (x y)
(if (equal x y)
'numbers-are-the-same
(if (< x y)
'first-is-smaller
'first-is-bigger)))

(defun compare (x y)
(or (and (equal x y) 'numbers-are-the-same)
(and (< x y) 'first-is-smaller)
(and (> x y) 'first-is-bigger)))

;;; 4.21. Write versions of the GTEST function using IF and COND


(defun gtest (x y)
(or (> x y) (zerop x) (zerop y)))

(defun gtest-if (x y)
(if (> x y)
t
(if (zerop x)
t
(zerop y))))

(defun gtest-if (x y)
(if (or (> x y) (zerop x) (zerop y))
t
nil))

(defun gtest-cond (x y)
(cond ((> x y))
((zerop x) t)
(t (zerop y))))


(defun gtest-cond (x y)
(cond ((or (> x y) (zerop x) (zerop y))
t)
(t nil)))

;;; Use COND to write a predicate BOILINGP that takes two inputs, TEMP and SCALE,
;;; and returns T if the temperature is above the boiling point of water on the specified scale.
;;; If the scale is FAHRENHEIT, the boiling point is 212 degrees;
;;; if CELSIUS, the  boiling point is 100 degrees. Also write versions using IF and AND/OR instead of COND.

;;; COND


(defun boiling-p (temp scale)
(cond ((and (equal scale 'FAHRENHEIT) (> temp 212)) t)
((and (equal scale 'CELSIUS) (> temp 100)))
(t nil)))

;;; IF


(defun boilingp (temp scale)
(if (equal scale 'FAHRENHEIT)
(> temp 212)
(if (< temp 212)
(equal scale 'CELSIUS))))

;;; AND/OR


(defun boiling-p (temp scale)
(or (and (> temp 212)
(equal scale 'FAHRENHEIT) t)
(and (> temp 100)
(equal scale 'CELSIUS) t)))

;;; SUMMARY
#|
Conditionals allow the computer to make decisions that control its behavior.

IF is a simple conditional; its syntax is (IF condition true-part false-part).

COND, the most general conditional, takes a set of test-and-consequent clauses as input and evaluates the tests one at a time until it finds a true one. It  then returns the value of the consequent of that clause. If none of the tests are true, COND returns NIL.

AND and OR are also conditionals. AND evaluates clauses one at a time until one of them returns NIL, which AND then returns. If all the clauses evaluate to true, AND returns the value of the last one.

OR evaluates clauses until a non-NIL value is found, and returns that value. If all the clauses evaluate to NIL, OR returns NIL. AND and OR aren’t considered predicates because they’re not ordinary functions.


A useful programming trick when writing COND expressions is to place a list of form (T consequent) as the final clause of the COND. Since the test T is always true, the clause serves as a kind of catchall case that will be evaluated when the tests of all the preceding clauses are false.

An important feature of conditionals is their ability to not evaluate all of
their inputs. This lets us prevent errors by protecting a sensitive expression
with predicate expressions that can cause evaluation to stop. Conditionals can  do this because they are either macros or special functions, not ordinary functions.
|#

;;; ADVANCE TOPICS

;;; 4.12 BOOLEANS FUNCTIONS/LOGICAL FUNCTIONS
;;; boolean functions are function shose inputs and outputs are truth values, meaning T or nil.
;;;The term boolean comes from, George Boole, 19th century English mathematician. True or False.

;;;examples from the book
(defun logical-and (x y)
(and x y t))

;;;4.13 TRUTH TABLES


;;;using cond


(defun logical-and (x y)
(cond (x
(cond (y t)))))

;;;using if

(defun logical-and-if (x y)
(if x
(if y t)))

;;;4.14 DEMORGAN's THEOREM

(and x y) = (not (or (not x) (not y)))
(or x y) = (not (and (not x) (not y)))

(defun demorgan-and (x y)
(not (or (not x) (not y))))

(defun demorgan-or (x y)
(not (and (not x) (not y))))

;;;Done at chapter 4

