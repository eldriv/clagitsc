(defpackage :chapter-10
  (:use :cl))

(in-package :chapter-10)

;;; FUNCTIONS COVERED: SETF, INCF, DECF, PUSH, POP


;; We saw in Chapter 5 that the SETF macro changes the value of a variable;
;; this  is called assignment.

;; 10.2 UPDATING A GLOBAL VARIABLE


;; Suppose we are operating a lemonade stand, and we want to keep track of how
;; many glasses have been sold so far. We keep the number of glasses sold in a
;; global variable, *TOTAL-GLASSES*, which we will initialize to zero this
;; way:

;; The lemonade stand is a
;; typical example of updating a variable.

(setf *total-glasses* 0)

(defparameter *total-glasses* 0)

(defun sell (n)
  (setf *total-glasses*  (+ *total-glasses* n))
  (format t "~&We have ~S Glasses so far today." *total-glasses*))

;;;10.3 STEREOTYPICAL UPDATING METHODS


;;10.3.1 The INCF and DECF Macros
;; Instead of incrementing a numeric variable by writing, say, (SETF A (+ A 5)),
;; you can write (INCF A 5). INCF and DECF are special assignment macros
;; for incrementing and decrementing variables. If the increment/decrement
;; value is omitted, it defaults to one.

;; > (setf a 2)
;; = 2
;; > (incf a 10)
;; = 12
;; > (decf a)
;; = 11

;; EXERCISE


;; 10.2. Rewrite the lemonade stand SELL function to use INCF instead of
;; SETF.

(defun sell (n)
  (incf *total-glasses* n)
  (format t "~&We have ~S Glasses so far today." *total-glasses*))

;; 10.3.2 The PUSH and POP Macros
;;Let’s try using PUSH to build a stack of dishes:
(defparameter *mystack* nil)

;; > (push 'dish1 *mystack*)
;; = (DISH1)
;; > (push 'dish2 *mystack*)
;; = (DISH2 DISH1)
;; > (push 'dish3 *mystack*)
;; = (DISH3 DISH2 DISH1)

;; > (pop *mystack*)
;; = DISH3

;; > *mystack*
;; = (DISH2 DISH1)

(defun dishes (x)
  (push x *mystack*)
  *mystack*)

(defparameter *friends* nil)

(defun meet (person)
  (cond ((equal person (first *friends*)) 'we-just-met)
        ((member person *friends*) 'we-know-each-other)
        (t (push person *friends*) 'pleased-to-meet-you)))

;; EXERCISES

;; 10.3. Modify the MEET function to keep a count of how many people have
;; been met more than once. Store this count in a global variable.

(defparameter *met-count* 0)
(defparameter *friends* nil)

(defun see-friends-list ()
  (if (equal *friends* nil)
      (format t "You don't have any friends yet! Meet some people!")
      (format t "Your list of friends: ~{~%~A~^ ~}" *friends*)) )

(defun meet-1 (person)
  (cond ((equal person (first *friends*)) (incf *met-count*) 'We-just-met)
        ((member person *friends*) (incf *met-count*) 'We-know-each-other)
        (t (push person *friends*) 'pleased-to-meet-you)))

;;removed friend from the list
(defun forget (person)
  (cond ((member person *friends*) (setf *friends* (remove person *friends*)) (format t "Removed:~a ~%Your friend list: ~{~%~a~^ ~}" person *friends*))
        (t (list 'This 'friend 'is 'not 'on 'your 'friend 'list))))


;; CL-USER> (meet-1 'adi)
;; PLEASED-TO-MEET-YOU
;; CL-USER> (meet-1 'buknoy)
;; PLEASED-TO-MEET-YOU
;; CL-USER> (see-friends-list)
;; Your list of friends: 
;; BUKNOY 
;; ADI
;; NIL
;; CL-USER> (forget 'adi)
;; Removed:ADI 
;; Your friend list: 
;; BUKNOY
;; NIL
;; CL-USER> (forget 'buknoy)
;; Removed:BUKNOY 
;; Your friend list: 
;; NIL
;; CL-USER> (see-friends-list)
;; You don't have any friends yet! Meet some people!
;; NIL

;;;10.3.3 Updating Local Variables


;; Assignment should not be used indiscriminately. For example, it is usually
;; considered inelegant to change the value of a local variable; one should just
;; bind a new local variable with LET instead. (There are exceptions, of course.)
;; It is even less elegant to modify a variable that appears in a function’s
;; argument list; doing this makes the function hard to understand. Consider the
;; following code written in very bad style:

(defun bad-style (n)
  (format t "~&N is ~S." n)
  (decf n 2)
  (format t "~&Now N is ~S." n)
  (decf n 2)
  (format t "~&Now N is ~S." n)
  (list ’result ’is (* n (- n 1))))

;; (bad-style 9)
;; N is 9.
;; Now N is 7.
;; Now N is 5.
;; (RESULT IS 20)

;; This code can be cleaned up by introducing some extra variables and replacing
;; the DECF expressions with a LET* form. When all assignments have been
;; eliminated, we are assured that the value of a variable will never change once
;; it is created. Programs written in this assignment-free style are easy to
;; understand, and very elegant.

(defun good-style (n)
  (let* ((p (- n 2))
         (q (decf p 2)))
    (format t "~&N is ~S." n)
    (format t "~&P is ~S." p)
    (format t "~&Q is ~S." q)
    (list 'result 'is (* q (- q 1)))))

;; > (good-style 9)
;; N is 9.
;; P is 7.
;; Q is 5.
;; (RESULT IS 20)

;; There are some occasions when it is more convenient to assign to a local
;; variable instead of LET-binding it. The following is an example. Note that
;; each variable is bound to NIL initially, and then is assigned a new value just
;; once. This form of ‘‘disciplined’’ assignment is not bad style; it is quite
;; different from the assignment occurring in the BAD-STYLE function.

(defun get-name ()
  (let ((last-name nil)
        (first-name nil)
        (middle-name nil)
        (title nil))
    (format t "~&Last name: ")
    (setf last-name (read))
    (format t "~&First name: ")
    (setf first-name (read))
    (format t "~&Middle name or initial: ")
    (setf middle-name (read))
    (format t "~&Preferred title: ")
    (setf title (read))
    (format t "Title:~a~%First Name:~a~%Middle-name:~a~%Last-name:~a" title first-name middle-name last-name)))

;; CHAPTER-10> (get-name)
;; Last name? Villareal
;; First name? Adi
;; Middle name or initial? A.
;; Preferred title? MABUHAY
;; Title:MABUHAY
;; First Name:ADI
;; Middle-name:A.
;; Last-name:VILLAREAL
;; NIL

;;;10.4 WHEN AND UNLESS

;;;WHEN and UNLESS are conditional forms that are useful when you need to evaluate more than one expression when a test is true. Their syntax is:

(WHEN test
  body)
(UNLESS test
  body)

;;WHEN first evaluates the test form. If the result is NIL, WHEN just
;;returns NIL. If the result is non-NIL, WHEN evaluates the forms in its body
;;and returns the value of the last one. UNLESS is similar, except it evaluates
;;the forms in its body only if the test is false. For both of these conditionals,
;;when the forms in the body are evaluated, the value of the last one is returned.
;;Forms prior to the last one are only useful for side effects, such as i/o or
;;assignment.

(defun picky-multiply (x y)
  "Computes X times Y.
Input X must be odd; Y must be even."
  (unless (oddp x)
    (incf x)
    (format t
            "~&Changing X to ~S to make it odd." x))
  (when (oddp y)
    (decf y)
    (format t
            "~&Changing Y to ~S to make it even." y))
  (* x y))

;; > (picky-multiply 4 6)
;; Changing X to 5 to make it odd.
;; 30
;; > (picky-multiply 2 9)
;; Changing X to 3 to make it odd.
;; Changing Y to 8 to make it even.

(defun validate-and-multiply (x y)
  "Computes the product of X and Y after validating conditions:
  - X must be positive.
  - Y must be negative."
  (unless (> x 0)
    (setq x (abs x))
    (format t "~&Adjusted X to ~S because it must be positive." x))
  (when (>= y 0)
    (setq y (- y))
    (format t "~&Adjusted Y to ~S because it must be negative." y))
  (* x y))


;; > (validate-and-multiply 3 -4)
;; Adjusted Y to -4 because it must be negative.
;; -12

;; > (validate-and-multiply -2 5)
;; Adjusted X to 2 because it must be positive.
;; Adjusted Y to -5 because it must be negative.
;; -10

;; 10.5 GENERALIZED VARIABLES

;; (setf x ’(jack benny was 39 for many years))

;; (setf (sixth x) ’several)
;; > x
;; (JACK BENNY WAS 39 FOR SEVERAL YEARS)

;; > (decf (fourth x) 2)
;; 37
;; > x
;; (JACK BENNY WAS 37 FOR SEVERAL YEARS)

;;; Currently at pg. 344 (Advance topics)

;; 10.7 DO-IT-YOURSELF LIST SURGERY


;; You can use SETF on generalized variables to manipulate pointers directly.
;; For example, suppose we want to turn a chain of three cons cells into a chain
;; of two cons cells by ‘‘snipping out’’ the middle cell. In other words, we want
;; to change the cdr of the first cell so it points directly to the third cell. Here’s
;; how to do it:

(defun snip (x)
  (setf (cdr x) (cdr (cdr x))))

(setf a '(no down payment))

> (setf b (cdr a))
(DOWN PAYMENT)

> (snip a)
(PAYMENT)

> a
(NO PAYMENT)

> b
(DOWN PAYMENT)

;; we can use SETF to create the following circular structure.

> (setf circ (list ’foo))
(FOO)
> (setf (cdr circ) circ)
(FOO FOO FOO FOO ...)

;; 10.8 DESTRUCTIVE OPERATIONS ON LISTS


;; Destructive list operations are those that change the contents of a cons cell.
;; These operations are ‘‘dangerous’’ because they can create circular structures
;; that may be hard to print, and because their effect on shared structures may be
;; hard to predict. But destructive functions are also powerful and efficient tools.
;; By convention, most of them have names that begin with N. (Like the
;; CAR/CDR convention and the ‘‘F’’ convention, this arose essentially by
;; accident but remains by virtue of brevity and usefulness.)



;; NCONC (pronounced ‘‘en-konk,’’ derived from ‘‘concatenate’’) is a
;; destructive version of APPEND. While APPEND creates a new list for its
;; result, NCONC physically changes the last cons cell of its first input to point
;; to its second input.

Example:

(setf x '(a b c))

> (A B C)

(setf y ’(d e f))

> (D E F)

(append x y)    ;Doesn’t change X or Y, but result shares structure with Y.

> (A B C D E F)

x
> (A B C)       ;X is unchanged.


> (nconc x y)

(A B C D E F)          ;NCONC alters the list (A B C).
> x

(A B C D E F)          ;X’s value has changed.

> y
(D E F)                 ;Y’s has not.

;; 10.8.2 NSUBST


;; NSUBST is a destructive version of SUBST. It modifies a list by changing
;; the pointers in the cars of some cells.

> (setf tree ’(i say (e i (e i) o)))
(I SAY (E I (E I) O))

> (nsubst ’a ’e tree)
(I SAY (A I (A I) O))

> tree
(I SAY (A I (A I) O))

> (nsubst ’cheery ’(a i) tree :test #’equal)
(I SAY (A I CHEERY O))

;; In the last example, since we were searching the tree for the list (A I), we
;; had to tell NSUBST to use EQUAL as the equality test. The default test,
;; EQL, would not have worked.

;; 10.10 SETQ AND SET


;; In earlier Lisp dialects, where SETF and generalized variables were not
;; available, the assignment function was called SETQ. The SETQ special
;; function is still around today. Its syntax is the same as the SETF macro, and it
;; can be used to assign values to ordinary (but not generalized) variables.

;; > (setq x ’(slings and arrows))
;; (SLINGS AND ARROWS)

;; If you read older Lisp books you will notice that assignment is done with
;; SETQ rather than SETF. Modern Common Lisp programmers use SETF for
;; all forms of assignment, whether they are storing into an ordinary variable
;; such as X or a generalized variable such as (SECOND X). SETQ is today
;; considered archaic. Internally, though, most Lisp implementations turn a
;; SETF into a SETQ if the assignment is to an ordinary variable, so you may see
;; some references to SETQ in debugger output.

(setf duck ’donald)               ;;  The global DUCK.

(defun test1 (duck)                ;; A local DUCK.
  (list duck                     
        (symbol-value 'duck)))

(test1 ’huey) ⇒ (huey donald)

(defun test2 (duck)              ;; Another Local Duck.           
  (set 'duck 'daffy)             ;; Changed the global duck
  (list duck
        (symbol-value 'duck)))

;; > (test2 ’huey) ---> (huey daffy)
;; > duck --> daffy
