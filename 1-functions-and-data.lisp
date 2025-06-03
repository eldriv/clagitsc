(defpackage :chapter-1
  (:use :cl))

(in-package :chapter-1)

;;; Functions on numbers

(+ 2 3)
;;+    - Adds two numbers
(- 3 2)
;;-    - SUbtracts the second number for the first
(* 2 3)
;;*    - Multiplies two numbers
(/ 3 6)
;;/    - Divides the first number by the second
(abs -4)
;;ABS  - Absolute value of a number
(sqrt 25)
;;SQRT - Square root of a number

;;; Exercise

(+ 6 7)  ;;; = 13
(* 3 4)  ;;; = 12
(/ 16 8) ;;; = 2
(- 4 3)  ;;; = 1
(abs -3) ;;; = 3

;;;SYMBOLS

;;; Integer --   A sequence of digits ‘‘0’’ through ‘‘9,’’ optionally preceded by a plus or minus sign.
;;; Symbol  --   Any sequence of letters, digits, and permissible special characters that is not a number.

;;; Some simple Predicates

(numberp 2) ;;; T
(numberp /) ;;; NIL

(symbolp CAT ) ;;; T
(symbolp 42)   ;;; NIL

(zerop 45) ;;; NIL
(zerop 0) ;;; T

(oddp 28) ;;; NIL
(oddp 27) ;;; T

(evenp 27) ;;;NIL
(evenp 28) ;;; T

(< 2 3) ;;; T
(> 2 3) ;;; NIL

;;; EQUAL Predicate

(defun cat-mouse-p ()
  (cond
    ((equalp "CAT" "MOUSE")
     (format t "CAT is equal to CAT."))
    (t
     (format t "CAT is not equal to MOUSE."))))

(defun cat-mouse-p (x y)
  (if (equalp x y)
      (format t "CAT is equal to CAT")
      (format t "CAT is not equal to MOUSE")))

(defun cat-mouse-p (x y)
  (cond ((equalp x y)
         (format t "~A is equal to ~A" x y))
        (t
         (format t "~a is not equal to ~a" x y))))

;;(cat-mouse-p "CAT" "CAT")

;;; Defining add-2


(defun add-5 (x)
  (+ x 5))

;;;using decf 
(defun sub-1 (x)
  (decf x))

(defun subtract-plus (x y)
  (list (sub-1 x) (add-5 y)))


(defun subtract-plus (x y z)
  (append (list x) (list (sub-1 x)) (list (add-5 y))))

;;; EXERCISE:


;; Define a sub-2 function that  subtracts two from its input

(defun sub-2 (x)
  (- x 2))

;; 1.5. Show how to write TWOP in terms of ZEROP and SUB2.

(defun two-p (x)
  (zerop (sub-2 x)))

;; 1.6. The HALF function returns a number that is one-half of its input. Show
;; how to define HALF two different ways.

(defun half (x)
  (/ x 2))

(defun half (x)
  (* x 0.5))

;; 1.7. Write a MULTI-DIGIT-P predicate that returns true if its input is
;; greater than 9.

(defun multi-digit-p (x)
  (> x 9))


;;; two-p definition


(defun two-p (n)
  (= n 2))

(defun two-p (n)
  (when (= n 2) t))

(defun is-a-num-p (num)
  (numberp num))

(defun two-p (x)
  (let* ((result (- x 2))
         (zero-p-result (zerop result)))
    (format t "Result: ~a~%Zero-p result :~a" result zero-p-result)
    result))

(defun two-p-negative (x)
  (let* ((result (- x 2))
         (result-zero (if (zerop result)
                          t
                          (if (< result 0)
                              (format t "Negative Result!")))))
    (format t "~%Result: ~a~%Result-zero: ~a" result result-zero)))

;;;ONE-MORE-P

(defun one-more-p (x y)
  (equal x (+ y 1)))

;; Write a predicate TWOMOREP that returns T if its first input is exactly two more than its second input. Use the ADD2 function in your definition of TWOMOREP.

(defun add2 (x)
  (+ x 2))

(defun two-more-p (x y)
  "First input is exactly two more than its second input."
  (equal x (add2 y)))

;; Using SUB2

(defun sub-2 (x)
  (- x 2))

(defun two-more-p (x y)
  "Second input is exactly two more than its first input."
  (equal x (sub-2 y)))

;; Write a MORE-THAN-HALF-P predicate that returns T if its first input is more than half of its second input.

(defun half (x)
  (/ x 2.0))

(defun more-than-half-p (x y)
  (> x (half y)))

;;; NEGATING A PREDICATE

(defun color (color1 color2)
  (not (equal color1 color2)))

(defun not-one-p (x)
  (not (= x 1)))

(defun not-plus-p (x)
  (when (<= x 0) t))

(defun not-plus-p (x)
  (and (<= x 0) t))

;;; To implement the logical exclusive or (XOR) operation.
(defun xor-p (x y)
  (if (or (and x (not y)) (and y (not x)))
      t
      nil))

;; In this chapter we covered two types of data: numbers and symbols.

;; We also learned several built-in functions that operate on them.
;; Predicates are a special class of functions that use T and NIL to answer
;; questions about their inputs. The symbol NIL means false, and the symbol T
;; means true. Actually, anything other than NIL is treated as true in Lisp.
;; A function must have a definition before we can use it. We can make new
;; functions by putting old ones together in various ways. A particularly useful
;; combination, used quite often in programming, is to feed the output of a
;; predicate through the NOT function to derive its opposite, as the NOT EQUAL predicate was derived from EQUAL

;;; DONE on chapter 1, each exercises are being exercised.
