(defpackage :chapter-3
  (:use :cl))

(in-package :chapter-3)

;;; 3.1-3.4 EVAL NOTATION 
#|
EVAL’s job is to evaluate Lisp expressions to compute their result.
Most expressions consist of a function followed by a set of inputs. If we give EVAL the expression (+ 2 3), for example, it will invoke the built-in function + on the inputs 2 and 3, and + will return 5. We therefore say the expression (+ 2 3) evaluates to 5.

In the box notation version the computation starts on the left and flows rightward. In EVAL notation the inputs to a function are processed left to right
|#

;;; EXERCISE
;; 3.1. What does (NOT (EQUAL 3 (ABS -3))) evaluate to?
NIL

;; 3.2. Write an expression in EVAL notation to add 8 to 12 and divide the result by 2. -
(/ 2 (+ 8 12))


;; 3.3. You can square a number by multiplying it by itself. Write an expression in EVAL notation to add the square of 3 and the square of 4.
(+ (* 3 3) (* 4 4))

;; 3.4. Draw an evaltrace diagram for each of the following expressions. (- 8 2),

;; The answers on my notebook.

;;; 3.5 defining functions in eval notation

(defun avg (x y)
  (/ (+ x y) 2.0))

;; From iterations and block sturctures—Advanced topics
(defun average (&rest x &aux (len (length x)) (point 1.0))
  (/ (reduce #'+ x) len point))

(defun square (n)
  (* n n))

(defun total-cost (quantity price handling-charge)
  (+ (* quantity price) handling-charge))

;;; EXERCISE - 3.5. Write definitions for HALF, CUBE, and ONEMOREP using DEFUN. (The CUBE function should take a number n as input and return n.)
(defun half (n)
  (/ n))

(defun cube (n)
  (* n n n))

(defun one-more-p (x y)
  (equal x (+ y 1)))

(defun pythag (x y)
  (sqrt (+ (* x x) (* y y))))

(defun miles-per-gallon (initial final consumed)
  (/ (- final initial) consumed))

;;; 3.6 VARIABLES

#|
A variable is a place where data is stored.*
The value of a variable is the data it holds
This use of the term ‘‘variable’’ is peculiar to computer programming. In mathematics, a variable is a
notation for an unknown quantity, not a physical place in computer memory. But these two meanings are
not incompatible, since the inputs to a function are in fact unknown quantities at the time the function is
defined.
|#

;;; 3.7 SYMBOLS

;; Evaluation Rule for Symbols: A symbol evaluates to the value of the variable it refers to.

#|
[Using symbols and lists as Data]
(equal kirk spock) --> ERROR
(equal 'kirk 'spock) --> NIL
|#

(defun riddle (x y)
  (list 'why 'is 'a x 'like 'a y))

;;; The problem of misquoting
#|
(list ’a ’b c)   ⇒   Error! C unassigned variable.
(list ’a ’b ’c)  ⇒   (a b c)
(cons ’a (b c))  ⇒   Error! B undefined function.
(cons ’a ’(b c)) ⇒   (a b c)
|#

;;; Different of quoting and without quoting

#|
(list 'buy '(* 27 34) ’bagels)  ⇒ (buy (* 27 34) bagels)
(list 'buy (* 27 34) ’bagels)   ⇒ (buy 918 bagels)
|#

;;; 3.10 WAYS TO MAKE LISTS

;;; EXERCISES 3.9. The following expressions evaluate without any errors. Write down the results.
#|
Three way to make lists:
'(foo bar baz)
(list 'foo bar baz)
(cons 'foo '(bar baz))
|#

;; One advantage of building the list up from individual elements is that some of the elements can be computed rather than specified directly.

(list 33 'squared 'is (* 33 33))
⇒ (33 squared is 1089)

;; If we quote a list, nothing inside it will get evaluated:
'(33 squared is (* 33 33))  ⇒ (33 squared is (* 33 33))

#|
(cons 5 (list 6 7))                                      --> (5 6 7)
(cons 5 ’(list 6 7))                                     --> (5 LIST 6 7)
(+ (length '(1 foo 2 moo))(third '(1 foo 2 moo)))        --> 6
(list 3 'from 9 'gives (- 9 3))                          --> (3 FROM 9 GIVES 6)

(third (the quick brown fox)) --> It should have a quoting symbol for the elements in the expression on (the quick brown fox --> (third '(the quick brown fox))) --> brown

(list 2 and 2 is 4) --> variable inside the expression is unbound which calls the and & is which needs to have a quoting symbol in able to evaluate --> (list 2 'and 2 'is 4) --> (2 AND 2 IS 4)

(+ 1 ’(length (list t t t t))) --> there is an error of The value (LENGTH (LIST T T T T))
is not of type NUMBER because it has a quote, it should be removed in able to evaluate the list "true" to find the length inside of it and it will evaluate --> 5

(cons ’patrick (seymour marvin)) --> the variable marvin is onbound it needs to have a quoting symbol in '(seymour marvin) to evaluate the function "cons". --> (cons 'patrick '(seymour marvin)) --> (PATRICK SEYMOUR MARVIN)

(cons ’patrick (list seymour marvin)) --> the difference between list and quote is that list will evaluate the inside expression since seymour and marvin doesn't define who they are insidee the expression and will evaluates instead it will received an error aside from quote that will prevent the expression from evaluating and it will evaluate like this --> (cons 'patrick)'(seymour marvin)
|#

;;; EXERCISE—Define a predicate called LONGER-THAN that takes two lists as input and returns T if the first list is longer than the second

(defun longer-than (x y)
  "First element is longer than the second element"
  (>= (length x) (length y)))

(defun add-length (lst)
  (let ((length-value (length lst)))
    (cons length-value lst)))

;; Concise solution
(defun add-length (lst)
  (cons (length lst) lst))

(defun add-length (lst)
  (format t "The length of list (~{~A~^ ~}) is ~A" lst (length lst)))

(defun call-up (caller callee)
  (list 'hello callee 'this 'is caller 'calling))

;;;[3.12 MORE ABOUT VARIABLES]
;; How many arguments does this function require? What are the names of the arguments? -->  2 arguments caller and callee
;; What is the result of (CALL-UP ’FRED ’WANDA)? --> (HELLO WANDA THIS IS FRED CALLING)

(defun double (n)
  (* n 2))

(defun quadruple (n)
  (double (double n)))


;; Consider the following function, paying close attention to the quotes:

;; The symbol WORD is used two different ways in this function. What are they? What is the result of (SCRABBLE ’AARDVARK)? What is the result of (SCRABBLE ’WORD)?

The symbol word is used on two different ways one is for argument and one is for a quoted symbol when it evaluate the word from argument in parameter will evaluate as what the users type and following with the list of '(IS A WORD)

(defun scrabble (word)
  (list word 'is 'a 'word))

(defun stooge (larry curly)
  (list larry (list 'moe curly) curly 'larry))

#|
• Numbers are self-evaluating, meaning they evaluate to themselves.
So do T and NIL.

• When evaluating a list, the first element specifies a function to
call, and the remaining elements specify its arguments. The
arguments are evaluated from left to right to derive the inputs that
are passed to the function.

• Symbols appearing anywhere other than the first element of a list
are interpreted as variable references. A symbol evaluates to the
value of the variable it names. Exactly which variable a symbol is
referring to depends on the context in which the symbol appears.
Variables that haven’t been assigned values cause ‘‘unassigned
variable’’ errors when the symbol is evaluated.

• A quoted list or symbol evaluates to itself, without the quote.

EXERCISE: 3.18. Name two advantages of EVAL notation over box notation.
• Programming concepts that are too sophisticated to express in box
notation can be expressed in EVAL notation.

• EVAL notation is easy to type on a computer keyboard box
notation is not.

• From a mathematical standpoint, representing functions as
ordinary lists is an elegant thing to do, because then we can use
exactly the same notation for functions as for data.

• In Lisp, functions are data, and EVAL notation allows us to write
functions that accept other functions as inputs. We’ll explore this
possibility further in chapter 7.

• When you have mastered EVAL notation, you will know most of
what you need to begin conversing in Lisp with a computer.
|#

;; 3.19. Evaluate each of the following lists. If the list causes an error, tell what the error is. Otherwise, write the result of the evaluation.

#|
(cons ’grapes ’(of wrath))   --> (GRAPES OF WRATH)
(list t ’is ’not nil)        --> (T IS NOT NIL)
(first ’(list moose goose))  --> (LIST) but if you want to evaluate the function first you should remove the list inside the list in able to evaluate the first element of the list
(first (list ’moose ’goose)) --> (MOOSE)
(cons ’home (’sweet ’home))  --> (illegal function call) meaning it should not be on quoted form inside the list because it will expect as kind of functions instead remove that and make it '(sweet home) --> (cons 'home '(sweet home))
|#

;; 3.20 Here is a mystery function:
(defun mystery (x)
  (list (second x) (first x)))

;; What result or error is produced by evaluating each of the following?
#|
(mystery ’(dancing bear)) --> (BEAR DANCING)
(mystery ’dancing ’bear) --> ERROR due too two arguments it should only expect 1 and no list in able to invoke
(mystery ’(zowie)) --> (NIL ZOWIE) 
(mystery (list ’first ’second)) --> (SECOND FIRST)
|#

;; 3.21. What is wrong with each of the following function definitions?

;;; The body has all quoted symbol that it should only be (list 'all x 'is y)
(defun speak (x y)
  (list 'all 'x 'is 'y))

;;; The arguments in the function have incorrect way of implementation it should be (x y)
(defun speak (x) (y)
  (list 'all x 'is y))

;;; The arguments in the function have incorrect way of implementation it should be (x y) and the body has incorrect way of quoting it should be (list 'all x 'is y)
(defun speak ((x) (y))
  (list all ’x is ’y))

;;;[3.14 THE READ-EVAL-PRINT LOOP]
;; A computer running Lisp behaves a lot like a pocket calculator. It reads an
;; expression that you type on the keyboard, evaluates it (using EVAL), and
;; prints the result on the screen. Then it prints another prompt and waits for you
;; to type the next expression. This process is called a read-eval-print loop.

#|
The most frequently occurring errors in LISP are parenthetical errors. It
is thus almost imperative to employ some sort of counting or pairing
device to check parentheses every time that a function is changed.
— Elaine Gord, ‘‘Notes on the debugging of LISP programs,’’ 1964.
|#

;;; 3.15 EXERCISES
;; c. Here is an example of the function MYFUN, a strange function of two inputs. (myfun ’alpha ’beta) Write MYFUN. correctly. ⇒ ((ALPHA) BETA)
;;; Different ways on writing myfun function

(defun myfun (x y)
  (list (cons x nil) y))

(defun myfun (x y)
  (cons (list x) (cons y nil)))

(defun myfun (x y)
  (append (list (list x)) (list y)))

(defun my-fun (x y)
  (list (list x) y))

;;; New function learned
;; rplaca cons object => cons
;; rplacd cons object => cons

(defparameter *some-list* (list* 'adi 'wow))

(defun replace-first (x)
  (rplaca *some-list* x))

(defun replace-last (x)
  (rplacd (last *some-list*) x))


;;; Write a predicate FIRSTP that returns T if its first argument (a symbol) is equal to the first element of its second argument (a list). that is, (FIRSTP ’FOO ’(FOO BAR BAZ)) should return T. (FIRSTP ’BOING ’(FOO BAR BAZ)) should return NIL.

(defun firstp (x y)
  (equalp (car y) x))

(defun firstp (x y)
  (equal (car y) x))

;;; e. Write a function MID-ADD1 that adds 1 to the middle element of a three-element list. For example, (MID-ADD1 ’(TAKE 2 COOKIES)) should return the list (TAKE 3 COOKIES). Note: You are not allowed to make MID-ADD1 a function of three inputs. It has to take a single input that is a list of three elements.

(MID-ADD1 '(take 2 cookies))

;;; Just for fun
(defun MID-ADD1-1 (x)
  (rplacd x (cons (+ (cadr x) 1) (cdr (cdr x)))))

;;; Easiest way
(defun MID-ADD1-2 (lst)
  (setf (cadr lst) (+ (cadr lst) 1))
  lst)

(defun mid-add-1 (x)
  (let ((track (+ (cadr x) 1)))
    (list (car x) track (caddr x))))

(defun mid-add-2 (x)
  (let ((track (+ (nth 1 x) 2)))
    (list (nth 0 x) track (nth 2 x))))

(defun mid-add1 (lst)
    (map 'list
           #'(lambda (x)
               (if (numberp x)(+ x 1) x))
           lst))

;;; f. Write a function F-TO-C that converts a temperature from Fahrenheit to Celsius. The formula for doing the conversion is: Celsius temperature = [5×(Fahrenheit temperature - 32)]/9. To go in the opposite direction, the formula is: Fahrenheit temperature = (9/5× Celsius temperature) + 32

;; celsius to fahrenheit
(defun c-to-f (celsius)
  (+ (* (/ 9 5) celsius) 32))

;; fahrenheit to celsius
(defun f-to-c (fahren)
  (* (/ (- fahren 32 )9) 5))


;;; ADVANCE TOPICS

;;; [3.16 Functions of no arguments]
(defun test ()
  (* 85 97))

;;; [3.17 THE QUOTE SPECIAL FUNCTION]

(quote foo)           ⇒ foo
(quote (hello world)) ⇒ (hello world)

'foo ⇒ foo
''foo ⇒ ’foo also written (quote foo)

;;; [3.18 INTERNAL STRUCTURE OF SYMBOLS]
;; We can extract the various components of a symbol using built-in
;; Common Lisp functions like SYMBOL-NAME and SYMBOL-FUNCTION.
;; The following dialog illustrates this; you’ll see something slightly different if
;; you try it on your computer, but the basic idea is the same.
> (symbol-name ’equal)  "EQUAL"
> (symbol-function ’equal) #<Compiled EQUAL function {60463B0}>

;;; [3.19 LAMBDA NOTATION]

;; Lambda notation was created by Alonzo Church, a mathematician at Princeton
;; University. Church wanted a clear, unambiguous way to describe functions,
;; their inputs, and the computations they perform. In lambda notation, a
;; function that adds 3 to a number would be written as shown below; the λ
;; is the Greek letter lambda: λx.(3+x).

(lambda (x) (+ 3 x))

;; A function f(x,y) = 3x+y2 would be written λ(x,y).(3x+y2) in lambda
;; notation. In Lisp it is written

(lambda (x y)
  (+ (* 3 x) (* y y)))

;;; EXERCISE3.23. Write each of the following functions in Church’s lambda notation: DOUBLE, SQUARE, ONEMOREP.
(lambda (x)
  (* 2 x))

(lambda (x)
  (* x x))

(lambda (x y)
  (equal x (+ y 1)))

(defun add (x y)
  (+ x y))

(defun addition (x y)
  (mapcar #'add (list x) (list y)))

;;; [3.20 SCOPE OF VARIABLES]

(defun parent (n)
  (+ n (child 2)))

(defun child (p)
  (* 2 p))

(defun alpha (x)
  (bravo (+ x 2)
	 (charlie x 1)))

(defun bravo (y z)
  (* y z))

(defun charlie (y x)
  (- y x))


;;; [3.21 EVAL AND APPLY]

;;3.25. What do each of the following expressions evaluate to?
(list ’cons t nil) --> (CONS T NIL)
(eval (list ’cons t nil)) --> (T)
(eval (eval (list ’cons t nil))) ---> error
(apply #’cons ’(t nil)) --> error
(eval nil) --> nil
(list ’eval nil) --> (EVAL NIL)
(eval (list ’eval nil)) --> NIL

;;;SUMMARY  FUNCTIONS COVERED IN ADVANCED TOPICS
;; EVAL-related function: APPLY.
;; EVAL (used explicitly).
;; Special function: QUOTE.

;;;DONE IN CHAPTER-3 EXERCISES ARE BEING EXERCISED
