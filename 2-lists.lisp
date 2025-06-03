(defpackage :chapter-2
  (:use :cl))

(in-package :chapter-2)
;;;LISTS

#|
;;;FUNCTIONS COVERED IN THIS CHAPTER
List functions: FIRST, SECOND, THIRD, FOURTH, REST, CAR, CDR, CONS, LIST, LENGTH.
Compositions of CAR and CDR: CADR, CADDR, and so on.
Predicates: LISTP, CONSP, ATOM, NULL.

;;;Examples
'(RED GREEN BLUE)
'(RHON HATING US)
'(I AM NOT SNITCH)
|#

;;Exercise

;; 2.1. Show how the list (TO BE OR NOT TO BE) would be represented in
;; computer memory by drawing its cons cell representation.

;; Cons cells representation

#|
[•][•]----> [•][•]---->[•][•]---->[•][•]---->[•][•]---->[•][•]----> NIL
'           '          '          '          '          '
'           '          '          '          '          '
TO         BE          OR        NOT        TO          BE
|#

;;; 2.3 LISTS OF ONE ELEMENT
'(AARDVARK)

;;; Cons cells representation

#|
[•][•]----> NIL
'
'
AARDVARK
|#

;;; NESTED LISTS
'((ADI) (YOU CAN) (DO IT) (OKAY))

'((BLUE SKY) (GREEN GRASS) (BROWN EARTH))

;; Cons cell representation
#|
[•][•]--------------------->[•][•]----------------------> [•][•]---> NIL
'                           '                             '
'                           '                             '
ADI                         WELL                          PLAYED

[•][•]---->[•][•]---->NIL   [•][•]---->[•][•]----> NIL    [•][•]------>[•][•] ----> NIL 
'          '                '          '                  '            '
'          '                '          '                  '            '
BLUE       SKY              GREEN      GRASS             BROWN        EARTH
|#

;; Can display like this
'((ADI) (YOU CAN) (DO IT)  (OKAY))


;;; EXERCISES


;;2.1 Which of these are well-formed lists? That is, which ones have properly balanced parentheses?

#|
(A B (C) --> F
((A) (B)) --> T
A B)(C D) --> F
(A (B (C)) --> F
(A (B (C))) --> T
(((A) (B)) (C)) --> F
|#
;; 2.3. Draw the cons cell representation of the list (PLEASE (BE MY)
;; VALENTINE).

#|

'(PLEASE (BE MY) VALENTINE)

[•][•]-----[•][•]------------------------- [•][•]---->NIL
'          '                               '
'          '                               '
PLEASE     [•][•] ----> [•][•] ---->NIL    VALENTINE
'            '
'            '
BE           MY
|#

;;; 2.4
'((BROWS ARROWS) (FLOWERS CHOCOLATES))

;;; LENGTH OF LISTS
#|
(A B C D) --> LENGTH 4   
(A (B C) D) --> LENTH 3
KUMQUAT --> Error Not a list
|#

;;; EXERCISE 2.5. How many elements do each of the following lists have?

#|
(OPEN THE POD BAY DOORS HAL)  --> LENGTH 6
((OPEN) (THE POD BAY DOORS) HAL)  --> LENGTH 3
((1 2 3) (4 5 6) (7 8 9) (10 11 12)) --> LENGTH 4
((ONE) FOR ALL (AND (TWO (FOR ME)))) --> LENGTH 4
((Q SPADES) (7 HEARTS)(6 CLUBS) (5 DIAMONDS) (2 DIAMONDS)) ---> LENGTH 5

((PENNSYLVANIA (THE KEYSTONE STATE))(NEW-JERSEY (THE GARDEN STATE))(MASSACHUSETTS (THE BAY STATE)) (FLORIDA (THE SUNSHINE STATE))(NEW-YORK (THE EMPIRE STATE)) (INDIANA (THE HOOSIER STATE))) --> LENGTH 6
|#

;;
()           ;;NIL
(())        ;;(NIL)
((()))      ;;((NIL))
(() ())     ;;(NIL NIL)
(() (()))   ;;(NIL (NIL))

;;; EQUALITY OF LISTS
(defun equal-lists-p (lst lst1)
  (equal lst lst1))

;;; FIRST, SECOND, THIRD, AND REST
(defun first (x)
  (first x))

(defun second (list)
  (second list))

(defun third (lst)
  (third lst))

(defun rest ()
  (rest list))

;;; Trying to apply cdr—rest of the elements in recursive mod
(defun cdr-recursion (lst)
  "In If statement form."
  (if (null lst)
      nil
      (progn
        (print (rest lst))
        (cdr-recursion (rest lst)))))
(defun cdr-recursion (lst)
  "In Conditional form."
  (cond ((null lst) nil)
        ((progn (print (rest lst))
                (cdr-recursion (rest lst))))))



;;; Combination of CAR and CDR
;;; CADR

(cadr '(FEE FIE FOE FUM)) --> FIE
(cdar '((FEE FIE) (FOE FUM))) --> FIE
(caddr '(FEE FIE FOE FUM)) --> FOE

;;; EXERCISE
;; What C...R name does Lisp use for the function that returns the fourth element of a list? How would you pronounce it?
(cadddr '(FEE FIE FOE FUM)) --> FUM


;; 48 Common Lisp: A Gentle Introduction to Symbolic Computation
;; CAR/CDR Pronunciation Guide
;; Function Pronunciation Alternate Name
#| 
CAR kar                         FIRST
CDR cou-der                     REST
CAAR ka-ar
CADR kae-der                    SECOND
CDAR cou-dar
CDDR cou-dih-der

CAAAR ka-a-ar
CAADR ka-ae-der
CADAR ka-dar
CADDR ka-dih-der                THIRD
CDAAR cou-da-ar
CDADR cou-dae-der
CDDAR cou-dih-dar
CDDDR cou-did-dih-der

CADDDR ka-dih-dih-der           FOURTH
|#

;;; CAR and CDR of Nested Lists
((BLUE CUBE) (RED PYRAMID))

;; Exercises:

(car '((BLUE CUBE) (RED PYRAMID))) ---> (BLUE CUBE)
(cdr '((BLUE CUBE) (RED PYRAMID))) ---> ((RED PYRAMID))
(cadr '((BLUE CUBE) (RED PYRAMID))) ---> (RED PYRAMID)
(caar '((BLUE CUBE) (RED PYRAMID))) ---> (BLUE)
(cdar '((BLUE CUBE) (RED PYRAMID))) ---> (CUBE)
(caadr '((BLUE CUBE) (RED PYRAMID))) ---> (RED)
(cadadr '((BLUE CUBE) (RED PYRAMID))) ---> (PYRAMID)

;;; EXERCISES -- Function Result ((A B) (C D) (E F)))

CAR   --> (A B)
CDDR  --> ((E F))
CADR  --> (C D)
CDAR  --> (B)
B     --> CADAR
CDDAR --> NIL
A     --> CAAR
CDADDR --> (F)

;;; Cadaddr is not built-in function in common lisp
;;; to get the F, I tried reading the cells from right to left since it starts on cdr, ((C D) (E F)) then cdr again ((E F)). then car (E F) then cdr. (F). then car F. 
(defun cadaddr (lst)
  "Get the F on nested list --> ((A B) (C D) (E F))."
  (car (cdr (car (cdr (cdr lst))))))

(defun get-c (lst)
  "Get the C on nested list --> ((A B) (C D) (E F))"
  (car (car (cdr lst))))

(defun get-a (lst)
  "Get the A on nested list --> ((A B) (C D) (E F))"
  (car (car lst)))

;; Exercises - 2.17. Fill in the results of the following computations.
(POST NO BILLS) (CAR)       --> (car '(POST NO BILLS))     --> POST
(POST NO BILLS) (CDR)       --> (cdr '(POST NO BILLS))     --> NO BILLS
((POST NO) BILLS) (CAR)     --> (car '((POST NO) BILLS))   --> BILLS
(BILLS) (CDR)               --> (cdr '(bills))             --> NIL 


;;; CONS or construct
(cons 'sink '(or swim)) ---> (SINK OR SWIM)

(defun greet (word)
  (cons word '(PROFESSOR HIGGINS)))

(defun greet (word)
  (cons word '(THERE MISS DOOLITTLE)))

;;; CONS and the Empty List
(cons 'frob nil)
(cons nil nil)

;;; Building Nested Lists With CONS
(cons '(FRED) '(AND GINGER)) ---> ((FRED) AND GINGER)
(cons '(NOW) '(IS THE TIME)) ---> ((NOW) IS THE TIME)
(cons 'the '((manila) times)) ---> (THE (MANILA) TIMES)

;;; CONS Can Build Lists From Scratch
(cons 'baz nil) ---> (BAZ)
(cons 'bar '(baz)) ---> (BAR BAZ)
(cons 'foo '(bar baz))--->(FOO BAR BAZ)

;; EXERCISE - 2.18. Write a function that takes any two inputs and makes a list of them using CONS.
(defun constructor(list list1)
  (cons list list1))

;;; LIST
(defun BLURT (x y)
  (list x 'is 'a  y))

;; Difference of cons and list
#|
(cons 'zort 'nil)   ---> (zort)
(list 'zort 'nil) ---> (zort nil)

(cons 'zort '(vuldermort))   ---> (ZORT VULDERMORT)
(list 'zort '(vuldermort)) ---> (ZORT (VULDERMORT))
|#

;;; Replacing the first element of the list
(defun say-what (y)
  (cons 'what (rest y)))

;;; EXERCISE
;; 2.20. What results are returned by the following?
#|
NIL ---> LIST ---> (NIL)
T NIL  ---> LIST ---> (T NIL)
T NIL  ---> CONS ---> (T)   
(T) NIL ---> CONS ---> ((T))
(IN ONE EAR AND) (OUT THE OTHER) ----> LIST ------> ((IN ONE EAR AND) (OUT THE OTHER))
(IN ONE EAR AND) (OUT THE OTHER) ----> CONS -----> ((IN ONE EAR AND) (OUT THE OTHER)
|#									
;;;EXERCISE - Write a function that takes four inputs and returns a two-element nested list. The first element should be a list of the first two inputs, and the second element a list of the last two inputs.

(defun nested-list (a b c d)
  (list (list a b) (list c d)))



;;; EXERCISE - DUO CONS
;;; Suppose we wanted to make a function called DUO-CONS that added
;; two elements to the front of a list. Remember that the regular CONS
;; function adds only one element to a list. DUO-CONS would be a
;; function of three inputs. For example, if the inputs were the symbol
;; PATRICK, the symbol SEYMOUR, and the list (MARVIN), DUOCONS would return the list (PATRICK SEYMOUR MARVIN). Show how to write the DUO-CONS function.
(defun duo-cons (elem elem1 list)
  (cons elem (cons elem1 list)))


;; EXERCISE TWO-DEEPER
;; TWO-DEEPER is a function that surrounds its input with two levels of
;; parentheses. TWO-DEEPER of MOO is ((MOO)). TWO-DEEPER of
;; (BOW WOW) is (((BOW WOW))). Show how to write TWODEEPER using LIST. Write another version using CONS.
(defun two-deeper (elem)
  (list (cons elem nil)))

;;;or

(defun two-deeper (elem)
  (cons (cons elem nil) nil))

;; 2.24. What built-in Lisp function would extract the symbol NIGHT from the
;; list (((GOOD)) ((NIGHT)))?

;;; it produces the same result.

;; SUMMARY
#|								     
This chapter introduced the most versatile data type in Lisp: lists. Lists have
both a printed and an internal representation. They may contain numbers,
symbols, or other lists as elements.
We can take lists apart using CAR and CDR (‘‘first’’ and ‘‘rest’’) and put
them together with CONS or LIST. The LENGTH function counts the
number of elements in a list, which is the same as its number of top-level cons
cells.
The important points about CAR and CDR are:
• CAR and CDR accept only lists as input.
• FIRST and REST are the same as CAR and CDR.
• SECOND and THIRD are the same as CADR and CADDR.
• Common Lisp provides built-in C...R functions for all
combinations of CAR and CDR up to and including four As and
Ds.
The symbol NIL has several interesting properties:
• NIL is a symbol. It is the only way to say ‘‘no’’ or ‘‘false’’ in
Lisp.
• NIL is a list. It is the empty list; its LENGTH is zero.
• NIL is the only Lisp object that is both a symbol and a list.
• NIL marks the end of a cons cell chain. When lists are printed in
parenthesis notation, the NILs at the end of chains are omitted by
convention.
• NIL and () are interchangeable notations for the same object.
• The CAR and CDR of NIL are defined to be NIL.
|#

;;; ADVANCE TOPICS 2.29 Exercise
;;  Write a function UNARY-ADD1 that increases a unary number by one.
(defun unary-add1 (unary-num)
  (concatenate 'list unary-num '(x)))

;; Write a UNARY-ZEROP predicate.
(defun unary-zerop (unary-num)
  (zerop (length unary-num)))


;; Write a unary-greater-p predicate
(defun unary-greater-p (unary-num unary-num1)
  (> (length unary-num) (length unary-num1)))

;;; EXERCISE- Write an expression involving cascaded calls to CONS to construct the dotted list (A B C . D).
;;; non-list cons constructures
(defun check-cons (a b c d)
  (cons a (cons b (cons c d))))

;;; Draw the dotted list ((A . B) (C . D)) in cons cell notation. Write an expression to construct this list.

(defun nested-cons (a b c d)
  (cons (cons a b) (cons (cons c d) nil)))

(defun nested-cons (a b &optional (cons-pair c d))
  (list (cons a b) cons-pair))

;;;DONE on chapter 2, each exercises are being exercised.
