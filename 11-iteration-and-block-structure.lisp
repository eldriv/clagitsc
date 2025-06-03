(defpackage :chapter-11
  (:use :cl))

(in-package :chapter-11)

;;; 11.1 INTRODUCTION


;; Iteration and Block Structure

;; The word ‘‘iterate’’ means to repeat, or to do something over and over.
;; but iteration (also known as ‘‘looping’’) is the simplest repetitive control structure. Virtually all
;; programming languages include some way to write iterative expressions.

;; Iteration in Lisp is more sophisticated than in most other languages. Lisp
;; provides powerful iteration constructs called DO and DO*, as well as simple
;; ones called DOTIMES and DOLIST.

;; 11.2 DOTIMES AND DOLIST

;; Template:

(dotimes (index-var n [result-form])
  body)

(dolist (index-var list [result-form])
  body)

;; The simplest iterative forms are DOTIMES and DOLIST. Both are macro
;; functions, meaning they don’t evaluate all their arguments. They have the
;; same syntax:

;;;Template
(defun func ()
  (dotimes (i value)
    (body)))

(defun dotimes-example ()
  (dotimes (i 10)
    (format t "~a~%" (+ i 1))))


(defun divisible-by-3 ()
  (format t "~%Divisible by 3")
  (dotimes (i 30)
    (let ((num (+ i 3)))
      (if (zerop (mod num 3))
          (format t "~%~d" num)))))

(defun divisible-by-2 ()
  (format t "~%Divisible by 2")
  (dotimes (i 20)
    (let ((num (+ i 2)))
      (if (zerop (mod num 2))
          (format t "~%~d" num)))))

(defun divisible-by-5 ()
  (format t "~%Divisible by 5")
  (dotimes (i 50)
    (let ((num (+ i 5)))
      (if (zerop (mod num 5))
          (format t "~%~d" num)))))

(defun dotimes-1-10 ()
  (dotimes (i 10 )
    (format t " ~A" (+ i 1))))


(defun dolist-example ()
  (dolist (i '(red blue green) 'colors)
    (format t "~%This is ~s." i)))

(defun countdown-4 (n)
  (dotimes (i n)
    (format t "~a~%" (- n i))))

;; 11.3 EXITING THE BODY OF A LOOP


;; The RETURN function can be used to exit the body of an iteration form
;; immediately, without looping any further. RETURN takes one input: the
;; value to return as the result of the iteration form. When RETURN is used to
;; force an exit from an iteration form, the result-form expression, if any, is
;; ignored.

;; Here is an iterative function called FIND-FIRST-ODD that returns the first
;; odd number in a list. It uses DOLIST to loop through the elements of the list,
;; and RETURN to exit the loop as soon as an odd number is found. If the list
;; contains no odd numbers, then when the loop is finished, DOLIST will return
;; NIL. An interesting point about FIND-FIRST-ODD is that the body of the
;; loop contains two forms instead of one. Loop bodies may contain any number
;; of forms.

;; Template:

(defun func (args)
  (dolist (i args)
    (body)
    (return i)))

;;using dolist and when, return

(defun find-first-odd (numbers)
  (dolist (i numbers)
    (when (oddp i)
      (format t "~%[Odd number found]")
      (return i))
    (unless (evenp i))
    (format t "~&Testing ~S..[Not Odd]~&" i)))

;;using dolist and if, return

(defun check-all-odd (list-of-numbers)
  (dolist (e list-of-numbers t)
    (format t "~&Checking ~S..." e)
    (if (not (oddp e))
        (return nil))))

;;using recursive approach : conditional augmentation test

(defun check-all-odd (x)
  (cond ((null x) t)
        (t (format t "~&Checking ~S..."
                   (first x))
           (unless (evenp (first x))
             (check-all-odd (rest x))))))

;;;Exercise:
;; 11.1. Write an iterative version of the MEMBER function, called IT-
;; MEMBER. It should return T if its first input appears in its second
;; input; it need not return a sublist of its second input.

(defun it-member (item x)
  (dolist (i x)
    (when (equal item i) (return t))))

;; 11.4 COMPARING RECURSIVE AND ITERATIVE SEARCH


;;;recursion


(defun rec-ffo (x)
  "Recursively find first odd number in a list."
  (cond ((null x) nil)
        ((oddp (first x)) (first x))
        (t (rec-ffo (rest x)))))

;;;iteration


(defun it-ffo (list-of-numbers)
  "Iteratively find first odd number in a list."
  (dolist (e list-of-numbers)
    (if (oddp e) (return e))))


;; First, the termination test is implicit:


;; In the recursive version we have to write a COND clause to explicitly
;; check for this.

;; DOLIST always stops when it gets to the end of
;; the list.


;; Second,

;; The iterative version the variable E names successive elements of the list, which is most convenient.

;; In the recursive version, X names successive RESTs of the list, so we have to remember to
;; write (FIRST X) to refer to the elements themselves, and we have to explicitly
;; compute (REST X) with each recursive call.

;;11.5 BUILDING UP RESULTS WITH ASSIGNMENT


;; Let’s start by using DOTIMES to compute the factorial function.

;; First we create an auxiliary variable PROD with initial value one. We will repetitively update this value in the body of the DOTIMES. and then return the final value of PROD as the result of the DOTIMES.

;;;assignment using dotimes


(defun it-fact (n)
  (let ((prod 1))
    (dotimes (i n prod)
      (setf prod (* prod (+ i 1))))))


(defun it-intersection (x y)
  (let ((result-set nil))
    (dolist (element x result-set)
      (when (member element y)
        (push element result-set)))))

;; EXERCISES


;; 11.4. Write an iterative version of LENGTH, called IT-LENGTH.
(defun it-length (x)
  (let ((number 0))      ; Define a local variable `number` and initialize it to 0
    (dolist (e x number) ; Iterate over each element `e` in list `x`, using `number` as the return value
      (incf number))))   ; Inside the loop, increment `number` by 1 for each element `e`
;; 11.5. Write an iterative version of NTH, called IT-NTH.

(defun it-nth (n x)
  (let ((current x)
        (result nil))
    (dotimes (i (+ 1 n) result)
      (setf result (car current))
      (setf current (cdr current)))))


;; 11.6 COMPARING DOLIST WITH APPLICATIVE FUNCTION AND RECURSION using square

;;;;applicative
(defun app-square-list (lst)
  (mapcar #'(lambda (n) (* n n))
          lst))

;;;recursion
(defun rec-square-list (x)
  (cond ((null x) nil)  
        (t (cons (* (first x) (first x))
                 (rec-square-list (rest x))))))
;;;iteration
(defun it-square-list (lst)
  (let ((result nil))
    (dolist (i lst (reverse result))
      (push (* i i) result))))

;;; 11.8 ADVANTAGES OF IMPLICIT ASSIGNMENT

;; DO has several advantages over DOTIMES and DOLIST. It can step the
;; index variables any way you like, so it can count down instead of up, for
;; example. DO can also bind multiple variables. This makes it easy to build up
;; a result in the variable list of the DO; there is no need for a surrounding LET
;; and an explicit SETF, as with the simpler iteration forms DOTIMES and
;; DOLIST.

;;Here is a version of the factorial function written with DO.

(defun fact (n)
  (do ((i n (- i 1))
       (result 1 (* result i)))
      ((zerop i) result)))

(defun do-intersection (x y)
  (do ((i x (rest i))
       (result nil))
      ((null i) result)
    (when (member (first i) y)
      (push (first i) result))))

;; Example
;; Suppose I have (do-intersection '(1 2 3 4) '(2 3 5)):

;; Iterations:
;; (first i) is 1. Not in y, so skip.
;; (first i) is 2. Found in y, so result becomes (2).
;; (first i) is 3. Found in y, so result becomes (3 2).
;; (first i) is 4. Not in y, so skip.

;; Finally, loop terminates because i becomes null.
;; Returns (3 2) as result.

(defun find-matching-letters (x y)
  (do ((i x (rest i))
       (j y (rest j)))
      ((or (null i) (null j)) nil)
    (if (equal (first i)
               (first j))
        (return (first i)))))

(defun find-matching-letters (x y)
  (let ((matches '()))
    (do ((i x (rest i))
         (j y (rest j)))
        ((or (null i) (null j)) (reverse matches))
      (when (equal (first i) (first j))
        (push (first i) matches)))))


;;; WHILE MACRO using DO
;; Using values ensures that the while loop only performs its side effects and does not influence the function's intended return value.
(defmacro while (test &body body)
  `(do ()
    ((not ,test)
     ;;Since WHILE is only used for side-effects, it should behave as such. You are explicitly telling DO not to return any value,
     (values))
     ,@body))

(defun using-while (x)
  (let ((cleared nil))
    (while (and x (not (string= (car x) "stop")))
           (push (car x) cleared)
           (pop x))
    (nreverse cleared)))

> (using-while '("apple" "banana" "lime" "stop" "oranges"))
("apple" "banana" "lime")

;; In this example, it doesn't matter because the last form is by NREVERSE. However, if the last form is WHILE, it makes a difference since the value returned by the function using WHILE may be inadvertently used, which is NIL, in this case.

;; How do we know that it's NIL?
(progn) => NIL

;;;The do* macro

;; Here is FIND-FIRST-ODD written with DO. It follows the usual convention:
;; A variable i is stepped through successive RESTs of the input. Within the
;; body, we write (FIRST i) to refer to elements of the input.

(defun find-first-odd (lst)
  (do ((i lst (rest i))) ;;assigning lst to i and to the rest of elements of i
      ((null i) nil) ;; termination: if no elements in i return nil
    (if (oddp (first i)) 
        (return (first i)))))

;; The DO* macro has the same syntax as DO, but it creates and updates the
;; variables sequentially like LET*, rather than all at once like LET. One
;; advantage of DO* in a function like FIND-FIRST-ODD is that it allows us to
;; define a second index variable to hold the successive elements of a list, while
;; the first index variable holds the successive cdrs:

(defun ffo-with-do* (lst)
  (do* ((i lst (rest i))
        (e (first i) (first i)))
       ((null i) nil)
    (if (oddp e) (return e))))

;; EXERCISES
;; 11.11. Rewrite the following function to use DO* instead of DOLIST.
(defun find-largest (list-of-numbers)
  (let ((largest (first list-of-numbers)))
    (dolist (element (rest list-of-numbers) largest)
      (when (> element largest)
        (setf largest element)))))

(defun find-largest-do* (lst)
  (do* ((largest (first lst))
        (i (rest lst) (rest i))
        (element (first i) (first i)))
       ((null i) largest)
    (when (> element largest)
      (setf largest element))))

;; 11.12. Rewrite the following function to use DO instead of DOTIMES.
(defun power-of-2 (n) ;2 to the Nth power.
  (let ((result 1))
    (dotimes (i n result)
      (incf result result))))

(defun power-of-2 (n)
  (let ((result 1))
    (do ((i 0 (+ i 1)))      
        ((>= i n) result)     
      (setf result (* result 2))))) 

(defun power-of-2 (n)
  (do ((result 1 (+ result result))
       (i 0 (+ i 1)))
      ((equal i n) result)))

;;;11.10 Infinite loops with do

(defun read-a-number ()
  (do ((answer nil))
      (nil)
    (format t "~&Please type a number: ")
    (setf answer (read))
    (if (numberp answer)
        (return answer))
    (format t
            "~&Sorry, ~S is not a number. Try again." answer)))

(defun read-a-number ()
  (loop
    (format t "~&Please type a number: ")
    (let ((answer (read)))
      (if (numberp answer)
          (return answer)
          (format t "~&Sorry, ~S is not a number. Try again." answer)))))

;;;11.11 IMPLICIT BLOCKS


;; In Common Lisp function bodies are contained in implicit blocks, and the
;; function name also serves as the block name. A block is a sequence of
;; expressions that can be exited at any point via the RETURN-FROM special
;; function. In the following example the body of FIND-FIRST-ODD is a block
;; named FIND-FIRST-ODD. The arguments to RETURN-FROM are a block
;; name and a result expression; the block name is not evaluated, so it should not
;; be quoted.

(defun find-first-odd (x)
  (format t "~&Searching for an odd number...")
  (dolist (element x)
    (when (evenp element)
      (format t "~&Found ~S." element)
      (return-from find-first-odd element)))
  (format t "~&None Found."))


;; In this example we used RETURN-FROM to exit the body of FIND-
;; FIRST-ODD, not just the body of the DOLIST. RETURN-FROM returns
;; from the closest enclosing block with the specified name. The bodies of
;; looping forms such as DOTIMES, DOLIST, DO, and DO* are enclosed in
;; implicit blocks named NIL. The expression (RETURN x) is actually just an
;; abbreviation for (RETURN-FROM NIL x). So in the body of FIND-FIRST-
;; ODD, the RETURN-FROM is nested inside a block named NIL, which is in
;; turn contained in a block named FIND-FIRST-ODD.


;; Here’s an example where RETURN-FROM is needed, that does not
;; involve iteration. The function SQUARE-LIST uses MAPCAR to square a
;; list of numbers. However, if any of the elements turns out not to be a number,
;; SQUARE-LIST returns the symbol NOPE instead of getting an error. The
;; RETURN-FROM inside the lambda expression exits not only the lambda
;; expression, but also the MAPCAR, and the body of SQUARE-LIST itself.

(defun square-list (x)
  (mapcar #'(lambda (e)
              (if (numberp e)
                  (* e e)
                  (return-from square-list 'nope)))
          x))

;;;Advanced Topics

;; 11.12 PROG1, PROG2, AND PROGN


;; PROG1, PROG2, and PROGN are three very simple functions. They all take
;; an arbitrary number of expressions as input and evaluate the expressions one
;; at a time. PROG1 returns the value of the first expression; PROG2 returns the
;; value of the second. PROGN returns the value of the last expression.

(prog1
    (setf x 'foo)
  (setf x 'bar)
  (setf x 'baz)
  (format t "~&X is ~S" x))

(prog2
    (setf x 'foo)
    (setf x 'bar)
  (setf x 'baz)
  (format t "~&X is ~S" x))


(progn
  (setf x 'foo)
  (setf x 'bar)
  (setf x 'baz)
  (format t "~&X is ~S" x))

;; These forms are used infrequently today. They were important in earlier
;; versions of Lisp, in which the body of a function could contain at most one
;; expression and a COND clause could contain at most one consequent.
;; One place where PROGN is still useful is in the true-part and false-part of
;; an IF. If you want to evaluate several expressions in the true-part or false-part,
;; you must group them together using something like PROGN, BLOCK, or a
;; LET.

(prog1
    (first x)
  (setf x (rest x)))

(let ((old-top (first x)))
  (setf x (rest x))
  old-top)


;;Today, the second is generally considered easier to read and understand.

;; 11.14 REST ARGUMENTS

;; The variable following an &REST lambda-list keyword will be bound to a list
;; of the remaining arguments to a function. This allows the function to accept
;; an unlimited number of arguments, as + and FORMAT do. Here’s a function
;; that takes an unlimited number of arguments and returns their average:

(defun average (&rest n)
  (/ (reduce #'+ n)
     (length n)
     1.0))

;; 11.15 KEYWORD ARGUMENTS


;; By using keywords, we avoid having to memorize an
;; order for these optional arguments; all we have to remember are their names.
;; You can create your own functions that accept keyword arguments by using
;; the &KEY lambda-list keyword. As with &OPTIONAL, default values can be
;; supplied if desired. Here is a function MAKE-SUNDAE that accepts up to six
;; keyword arguments:

(defun make-sundae (name &key (size 'regular)
                           (ice-cream 'vanilla)
                           (syrup 'hot-fudge)
                           nuts
                           cherries
                           whipped-cream)
  (list 'Sundae (list 'for name) (list 'size size)
        (list ice-cream 'with syrup 'syrup)
        (list 'toppings '=
              (remove nil
                      (list (and nuts 'nuts)
                            (and cherries 'cherries)
                            (and whipped-cream
                                 'whipped-cream))))))

> (make-sundae 'adi) (SUNDAE (FOR ADI) (SIZE REGULAR) (VANILLA WITH HOT-FUDGE SYRUP)
                             (TOPPINGS = NIL))

> (make-sundae 'adi
               :syrup 'strawberry
               :nuts t
               :cherries t) (SUNDAE (FOR ADI) (SIZE REGULAR) (VANILLA WITH STRAWBERRY SYRUP)
                                    (TOPPINGS = (NUTS CHERRIES)))


(defun make-music (title &key
                           (publish '2024)
                           (artist 'Adi)
                           opm
                           kundiman
                           english
                           p-pop)
  (list 'Music 'made 'by
        (list artist (list 'published 'on publish)
              (list 'title ' = title)
              (list 'type '=
                    (remove nil
                            (list (and opm 'Opm)
                                  (and kundiman 'Kundiman)
                                  (and english 'English)
                                  (and p-pop 'P-pop)))))))

> (make-music 'buwan 
              :opm t)

(MUSIC MADE BY (ADI (PUBLISHED ON 2024) (TITLE = BUWAN) (TYPE = (OPM))))

;; 11.16 AUXILIARY VARIABLES

;; The &AUX lambda-list keyword is used to define auxiliary local variables.
;; You can specify just the variable name, in which case the variable is created
;; with an initial value of NIL, or you can use a list of form (var expression). In
;; the latter case expression is evaluated, and the result serves as the initial value
;; for the variable. Here is an example of the use of an auxiliary variable LEN to
;; hold the length of a list:

(defun average (&rest n
                &aux
                  (len (length n))
                  (point 1.0))
  (/ (reduce #'+ n) len point))

;; SUMMARY
;; DOLIST and DOTIMES are the simplest iteration forms.
;; DO and DO* are more powerful because they can step several variables at once using arbitrary
;; update expressions and termination tests. But for simple problems like
;; searching the elements of a list, DOLIST is more concise.

;; All the iteration forms make implicit assignments to their index variables.
;; This is the cleanest type of assignment to use; you never actually have to write
;; a SETF because the loop does the assignment for you. Sometimes, though, it
;; is better to build up the result using explicit assignment in the loop body. This
;; is especially true when we are using conditional assignment, as in the IT-
;; INTERSECTION function.

;; Function names serve as implicit block names. We can therefore use
;; RETURN-FROM to exit a function from anywhere in its body.

;;; FUNCTIONS COVERED IN THIS CHAPTER
;; Iteration macros: DOTIMES, DOLIST, DO, DO*.
;; Special functions for block structure: BLOCK, RETURN-FROM.

;; Ordinary function for exiting a block named NIL: RETURN.

;;; FUNCTIONS COVERED IN ADVANCED TOPICS
;; PROG1, PROG2, PROGN.
;; Lambda-list keywords: &OPTIONAL, &REST, &KEY, &AUX.

;; Lisp Toolkit: TIME
;; The TIME macro function tells you how long it took to evaluate an
;; expression. It may also tell you how much memory was used during the
;; evaluation, and other useful things. The exact details of what TIME measures
;; and how the information is displayed are implementation dependent. TIME is
;; useful for gauging the efficiency of programs, for example, to compare two
;; solutions to a problem to see which is faster, or to see how much slower a
;; function runs when given a larger input. Here is an example:

(defun addup (n)
  "Adds up the first N integers"
  (do ((i 0 (+ i 1))
       (sum 0 (+ sum i)))
      ((> i n) sum)))

> (time (addup 1000))

;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000006 seconds of total run time (0.000005 user, 0.000001 system)
;;   100.00% CPU
;;   6,655 processor cycles
;;   0 bytes consed


;; As you can see, when the input to ADDUP was increased from 1000 to
;; 10,000, the user run time and total bytes consed also increased by a factor of
;; ten. But the number of page faults increased by a factor of just 2.6.

;;;Additional materials (LOOP)

(loop :repeat 10
      :collect (print "Hello World!"))

(loop :for i :from 0 :to 10
      :collect i)

(loop :for i :in '(0 1 2 3 4 5 6 7 8 9)
      :collect (+ i 1))


(loop :for i :in '(0 1 2 3 4 5 6 7 8 9)
      :for j :in '(1 2 3 4 5 6 7 8 9 10)
      :do (format t "~a+~a " i j)
      :collect (+ i j))

(loop :for i :in '(0 1 2 3 4 5 6 7 8 9)
      :for j :in '(1 2 3 4 5 6 7 8 9 10)
      :do (format t "~a+~a " i j)
      :maximize (+ i j))


(loop :for i :in '(0 1 2 3 4 5 6 7 8 9)
      :for j :in '(1 2 3 4 5 6 7 8 9 10)
      :do (format t "~a+~a " i j)
      :minimize (+ i j))


(loop :for i :across #(0 1 2 3 4 5 6 7 8 9 10)
      :for j :in     '(6 9 5 1 3 5 4 0 7 4 10)
      :when (= i j)
        :collect (cons i j))

(loop :for i :across #(0 1 2 3 4 5 6 7 8 9 10)
      :for j :in     '(6 9 5 1 3 5 4 0 7 4 10)
      :when (= i j)
        :collect (cons i j)
      :when (< i j)
        :collect (cons i j))

(loop :for i :across #(0 1 2 3 4 5 6 7 8 9 10)
      :for j :in     '(6 9 5 1 3 5 4 0 7 4 10)
      :unless (= i j)
        :collect (cons i j))

(defun divisible-by-2 ()
  (loop :for i :from 2 :by 2
        :while (<= i 20)
        :do (format t "~d " i);;if want to print with aesthetic format
        :collect i))  ;;if want to print with list

(defun divisible-by-3 ()
  (loop :for i :from 3 :by 3
        :while (<= i 30)
        :do (format t "~d " i)
        :collect i))

(defun divisible-by-5 ()
  (loop :for i :from 5 :by 5
        :while (<= i 50)
        :do (format t "~d " i)
        :collect i))

(defun read-a-number ()
  (loop
    :for answer := nil
    :do (progn
          (format t "~&Please type a number: ")
          (setf answer (read)))
    :while (not (numberp answer))
    :do (format t "~&Sorry, ~S is not a number. Try again.~%" answer)
    :finally (return answer)))
