(defpackage :chapter-8
  (:use :cl))

(in-package :chapter-8)

;;;RECURSION


;; Recursion is one of the most fundamental and beautiful ideas in computer
;; science. A function is said to be ‘‘recursive’’ if it calls itself. Recursive
;; control structure is the main topic of this chapter, but we will also take a look
;; at recursive data structures in the Advanced Topics section. The insight
;; necessary to recognize the recursive nature of many problems takes a bit of
;; practice to develop, but once you ‘‘get it,’’ you’ll be amazed at the interesting
;; things you can do with just a three- or four-line recursive function.

;; We will use a combination of three techniques to illustrate what recursion
;; is all about:
1. Dragon stories,
2. program traces, and
3. recursion templates.
4. Dragon stories are the most controversial technique
;; Students enjoy them and find  them helpful, but
;; computer science professors aren’t always as appreciative.
;; If you don’t like dragons, you may skip Sections 8.2, 8.4, 8.6, and 8.9. The
;; intervening sections will still make sense; they just won’t be as much fun.

;;; EXAMPLES of TAIL-RECURSION:

(defun add-all-list (lst)
  (cond
    ((null lst) 0) ; ; Base case: if the list is empty, return 0
    ((numberp (car lst))  ;; If the first element is a number
     (+ (car lst) (add-all-list (cdr lst))))  ; Add the first element to the sum of the rest of the list
    (t (error "List contains non-numeric elements"))))  

;; (add-all-list '(1 2 3 4))

(defun square-all-elements (x)
  (cond ((null x) nil) ;; Base case: if the list is empty, return nil
        ((numberp (car x)) ;;If the first elements is a number
         (cons (* (car x) (car x)) ;create a new list to square the first element
               (square-all-elements (cdr x)))) ;;square the rest of the list
        (t (format t "~a ~a" x 'Is-not-a-number))))

;; (square-all-elements '(1 2 3 4 5 6))

;;; RECURSION EXAMPLE WITH LABELS

(defun factorial (x)
  (labels ((fact (n)
             (cond ((zerop n) 1)
                   (t (* n (factorial (-)))))))
    (fact x)))

(defun add-all-list (x)
  (labels ((sum-up (n)
             (cond ((null n) 0)
                   ((numberp (car n))
                    (+ (car n) (add-all-list (cdr n))))
                   (t (format t "List contains non-numberic elements")))))
    (sum-up x)))

(defun square-all-elements (x)
  (labels ((square-it (n)
             (cond ((null n) nil)
                   ((numberp (car n))
                    (cons (* (car n) (car n)) (square-it (cdr n))))
                   (t (format t "There is non-numeric on the list")))))
    (square-it x)))

;;; 8.3 A FUNCTION TO SEARCH FOR ODD NUMBERS


(defun anyoddp (x)
  (cond ((null x) nil) 
        ((oddp (first x)))
        (t (anyoddp (rest x)))))

(defun anyoddp (x)
  (labels ((anyoddp-rec (y)
             (cond ((null y) nil)
                   ((oddp (first y)) t)
                   (t (anyoddp-rec (rest y))))))
    (anyoddp-rec x)))

(trace anyoddp)

;;If the list of numbers is empty, ANYODDP should return NIL,
;;If the list is not empty, we go to the second COND clause and test the first element.
;;If the first element is odd, there is no need to look any further; ANYODDP can stop and return T
;; When the first element is even, ANYODDP must call itself on the rest of the list to keep looking for odd elements. That is the recursive part of the definition.


;; EXERCISES


;; 8.1. Use a trace to show how ANYODDP would handle the list (3142 5798
;; 6550 8914). Which COND clause is never true in this case?
;; CHAPTER-8> (anyoddp '(3142 5798
;; 6550 8914))
;;   0: (CHAPTER-8::ANYODDP (3142 5798 6550 8914))
;;     1: (CHAPTER-8::ANYODDP (5798 6550 8914))
;;       2: (CHAPTER-8::ANYODDP (6550 8914))
;;         3: (CHAPTER-8::ANYODDP (8914))
;;           4: (CHAPTER-8::ANYODDP NIL)   ;;;second clause is never true because there is no any odd on the list.
;;           4: ANYODDP returned NIL
;;         3: ANYODDP returned NIL
;;       2: ANYODDP returned NIL
;;     1: ANYODDP returned NIL
;;   0: ANYODDP returned NIL
;; NIL

;; 8.2. Show how to write ANYODDP using IF instead of COND.

(defun anyoddp-if (x)
  (if (null x)
      nil
      (if (oddp (first x))
          t
          (anyoddp-if (rest x)))))

(defun anyoddp (x)
  (if (null x)
      nil
      (or (oddp (first x))
          (anyoddp (rest x)))))

;; 8.5 A LISP VERSION OF THE FACTORIAL FUNCTION

(defun factorial (x)
  "First clause check if x is zero if T, it returns 1"
  (cond ((zerop x) 1) 
        (t (* x (factorial (- x 1))))))

;; second clause
;; For factorial(5), the function proceeds as follows:

;; (factorial 5) evaluates to (* 5 (factorial 4)).
;; (factorial 4) evaluates to (* 4 (factorial 3)).
;; (factorial 3) evaluates to (* 3 (factorial 2)).
;; (factorial 2) evaluates to (* 2 (factorial 1)).
;; (factorial 1) evaluates to (* 1 (factorial 0)).
;; (factorial 0) evaluates to 1.
;; So, factorial(5) = 5 * 4 * 3 * 2 * 1 = 120.

(defun factorial (x)
  (if (zerop x)
      1
      (* x (factorial (- x 1)))))

;; 8.7 A RECURSIVE FUNCTION FOR COUNTING SLICES OF BREAD
;; If we represent a slice of bread by a symbol, then a loaf can be represented as
;; a list of symbols. The problem of finding how many slices a loaf contains is
;; thus the problem of finding how many elements a list contains. This is of
;; course what LENGTH does, but if we didn’t have LENGTH, we could still
;; count the slices recursively.

(defun count-slices (bread-slice)
  (cond ((null bread-slice) 0)
        (t (+ 1 (count-slices (rest bread-slice))))))

;;TRACE
CHAPTER-8> (trace count-slices)
(COUNT-SLICES)

CHAPTER-8> (count-slices '())
0: (CHAPTER-8::COUNT-SLICES NIL)
0: COUNT-SLICES returned 0
0

CHAPTER-8> (count-slices '(x x x))
0: (CHAPTER-8::COUNT-SLICES (CHAPTER-8::X CHAPTER-8::X CHAPTER-8::X))
1: (CHAPTER-8::COUNT-SLICES (CHAPTER-8::X CHAPTER-8::X))
2: (CHAPTER-8::COUNT-SLICES (CHAPTER-8::X))
3: (CHAPTER-8::COUNT-SLICES NIL)
3: COUNT-SLICES returned 0
2: COUNT-SLICES returned 1
1: COUNT-SLICES returned 2
0: COUNT-SLICES returned 3
3

CHAPTER-8> (count-slices '(x))
0: (CHAPTER-8::COUNT-SLICES (CHAPTER-8::X))
1: (CHAPTER-8::COUNT-SLICES NIL)
1: COUNT-SLICES returned 0
0: COUNT-SLICES returned 1
1

;; 8.8 THE THREE RULES OF RECURSION
;; 1. Know when to stop.
;; 2. Decide how to take one step.
;; 3. Break the journey down into that step plus a smaller journey.

;; ‘‘know when to stop,’’


;; warns us that any recursive function must check to see if the journey has been completed before recursing further. Usually this is done in the first COND clause. In ANYODDP the first clause
;; checks if the input is the empty list, and if so the function stops and returns
;; NIL, since the empty list doesn’t contain any numbers.

;; In COUNT-SLICES the first COND clause checks
;; for NIL, ‘‘the empty loaf.’’ COUNT-SLICES returns zero if NIL is the input.
;; Again, this is based on the realization that the empty loaf contains no slices, so
;; we do not have to recurse any further.

;; The second rule, ‘‘decide how to take one step,’’


;; asks us to break off from the problem one tiny piece that we instantly know how to solve. In
;; ANYODDP we check whether the FIRST of a list is an odd number; if so we
;; return T. In the factorial function we perform a single multiplication,
;; multiplying the input N by factorial of N− 1. In COUNT-SLICES the step is
;; the + function: For each slice we cut off the loaf, we add one to whatever the
;; length of the resulting loaf turned out to be.

;; The third rule, ‘‘break the journey down into that step plus a smaller  journey,’’


;; means find a way for the function to call itself recursively on the
;; slightly smaller problem that results from breaking a tiny piece off. The
;; ANYODDP function calls itself on the REST of the list, a shorter list than the
;; original, to see if there are any odd numbers there. The factorial function
;; recursively computes factorial of N-1, a slightly simpler problem than
;; factorial of N, and then uses the result to get factorial of N. In COUNT-
;; SLICES we use a recursive call to count the number of slices in the REST of a
;; loaf, and then add one to the result to get the size of the whole loaf.

;; FIRST RECURSION EXERCISE


;; We are going to write a function called LAUGH that takes a number as
;; input and returns a list of that many HAs. (LAUGH 3) should return
;; the list (HA HA HA). (LAUGH 0) should return a list with no HAs in
;; it, or, as the dragon might put it, ‘‘the empty laugh.’’

(defun laugh-recursion (x)
  (cond ((<= x 0) nil)
        (t (cons 'ha (laugh-recursion (1- x))))))


(defun laugh-recursion (n)
  (if (<= n 0)
      nil
      (cons 'ha (laugh-recursion (1- n)))))

;;SECOND RECURSION EXERCISE


;; In this exercise we are going to write a function ADD-UP to add up all
;; the numbers in a list. (ADD-UP ’(2 3 7)) should return 12. You
;; already know how to solve this problem applicatively with REDUCE;
;; now you’ll learn to solve it recursively. Before writing ADD-UP we
;; must answer three questions posed by our three rules of recursion.

(defun add-up (x)
  (cond ((null x) 0)
        (t (+ (first x) (add-up (rest x))))))

;;or

(defun add-up (x)
  (if (null x)
      0
      (+ (car x) (add-up (cdr x)))))

;; 1. If the list is empty ('()), the sum is 0 because there are no numbers to add.
;; 2. I think of reducing the problem size by splitting the list into a smaller part and the first element.
;; 3. To find the sum of a list, we can add the first element of the list to the sum of the rest of the list.

;; Write ALLODDP, a recursive function that returns T if all the numbers
;; in a list are odd.


;; Steps and Considerations:
;; Base Case: Determine when to stop recursing.
;; Recursive Case: Define how to break down the problem and recursively solve it.
;; Edge Cases: Handle scenarios like an empty list.

(defun alloddp (x)
  (cond ((null x) t)                 ; Base case: If the list x is empty, return true.
        ((oddp (first x))            ; If the first element of x is odd,
         (alloddp (rest x)))         ; recursively check the rest of the list.
        (t nil)))                    ; If the first element is not odd, return false.

(defun alloddp (x)
  (if (null x)
      t
      (if (oddp (first x))
          (alloddp (rest x))
          nil)))

;; Overall Logic: The function alloddp recursively checks each element of the list:
;; If all elements (after the first) are odd, it eventually returns t.
;; If any element (after the first) is not odd, it returns nil.

;; Write a recursive version of MEMBER. Call it REC-MEMBER so you
;; don’t redefine the built-in MEMBER function.


(defun rec-member (item list)
  (cond ((null list) nil)               ; Base case: If the list is empty, return nil.
        ((eql item (first list)) list)  ; If ITEM is equal to the first element of LIST, return the list starting from that element.
        (t (rec-member item (rest list))))) ; Otherwise, recursively search in the rest of the list.

;; Write a recursive version of NTH and Assoc. Call it REC-NTH and rec-assoc.

(defun rec-nth (n lst)
  (cond ((zerop n) (car lst)) ; Base case: return the first element when n is zero
        (t (rec-nth (decf n) (cdr lst))))) ; Recursive case: decrement n and move to the next element in the list

(defun rec-assoc (key alist)
  (cond ((null alist) nil) ; If the list is empty, return nil (not found)
        ((eql key (caar alist)) (car alist)) ; If key matches the car of the first element, return the element
        (t (rec-assoc key (cdr alist))))) ; Otherwise, recursively search in the rest of the list

;; 8.10 INFINITE RECURSION IN LISP

;; Lisp functions can be made to recurse infinitely by ignoring the dragon’s first
;; rule of recursion, which is to know when to stop. Here is the Lisp
;; implementation of Martin’s algorithm:

(defun fib (n)
  (+ (fib (- n 1))
     (fib (- n 2))))

(defun c (n)
  (cond ((equal n 1) t)
        ((evenp n) (c (/ n 2)))
        (t (c (+ (* 3 n) 1)))))

;;; EXERCISE
;; The missing part of Martin’s Fibonacci algorithm is the rule for Fib(1)
;; and Fib(0).  write a correct version of the FIB function. (FIB 4) should
;; return five. (FIB 5) should return eight.

(defun fib (n)
  (cond ((= n 0) 1)                     
        ((= n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

;; 8.14. Write the very shortest infinite recursion function you can.
(defun countdown (n)
  (let ((count-dec (1- n)))
    (format t "~a~%" n)
    (if (zerop n)
        nil
        (countdown count-dec))))

(defun countdown (n)
  (format t "~%~A" n)
  (cond ((zerop n) nil)
        (t (countdown (1- n)))))

;;8.11 RECURSION TEMPLATES


;; Most recursive Lisp functions fall into a few standard forms. These are
;; described by recursion templates, which capture the essence of the form in a
;; fill-in-the-blanks pattern. You can create new functions by choosing a
;; template and filling in the blanks. Also, once you’ve mastered them, you can
;; use the templates to analyze existing functions to see which pattern they fit.

;; The first template we’ll study is double-test tail recursion, which is shown in
;; Figure 8-1.

;;‘‘Double-test’’ indicates that the recursive function has two end
;; tests; if either is true, the corresponding end value is returned instead of
;; proceeding with the recursion. When both end tests are false, we end up at the
;; last COND clause, where the function reduces the input somehow and then
;; calls itself recursively. This template is said to be tail-recursive because the
;; action part of the last COND clause does not do any work after the recursive
;; call. Whatever result the recursive call produces, that is what the COND
;; returns, so that is what each parent call returns. ANYODDP is an example of
;; a tail-recursive function.

;; Template:

(defun func (X)
  (cond (end-test-1 end-value-1)
        (end-test-2 end-value-2)
        (T (func reduced-x))))

(defun anyoddp (x) ;;function definition
  (cond ((null x) nil) ;;end test-1: null x end-value-1: nil
        ((oddp (first x)) t)  ;;end test-2: (oddp (first x)) end value-2: T
        (t (anyoddp (rest x))))) ;;reduced-x: (rest x)


(defvar *lists* '(1 2 3 4 5 6))

(defun check-if-any-odd (x)
  (labels ((check (x)
             (cond ((null x) nil)
                   ((oddp (first x)) t)
                   (t (check (rest x))))))
    (check x)))

;; EXERCISES
;; 8.16. What would happen if we switched the first and second COND clauses
;; in ANYODDP?

;;switching the first and second COND clauses in the ANYODDP function would change its behavior to prioritize checking for an odd first element before checking if the list is empty.

;; 8.17 Use double-test tail recursion to write FIND-FIRST-ODD, a function
;; that returns the first odd number in a list, or NIL if there are none. Start
;; by copying the recursion template values for ANYODDP; only a small
;; change is necessary to derive FIND-FIRST-ODD.

(defun find-first-odd (x)
  (cond ((null x) nil)
        ((oddp (car x)) (car x))
        (t (find-first-odd(rest x)))))

;;; 8.11.2 Single-Test Tail Recursion

;; Template:

(defun func (X)
  (cond (end-test end-value)
        (T (func reduced-x))))

;; A simpler but less frequently used template is single-test tail recursion, which
;; is shown in Figure 8-2. Suppose we want to find the first atom in a list, where
;; the list may be nested arbitrarily deeply. We can do this by taking successive
;; FIRSTs of the list until we reach an atom. The function FIND-FIRST-ATOM
;; does this:

(defun find-first-atom (x)
  (conxod ((atom x) x)
          (t (find-first-atom (first x)))))

;; (find-first-atom ’(ooh ah eee)) --> ooh

;; 8.18. Use single-test tail recursion to write LAST-ELEMENT, a function that
;; returns the last element of a list. LAST-ELEMENT should recursively
;; travel down the list until it reaches the last cons cell (a cell whose cdr is
;; an atom); then it should return the car of this cell.


(defun last-element (x)
  (cond ((null (cdr x)) (car x))  ; If the cdr of x is null, return the car of x (last element)
        (t (last-element (cdr x))))) ; Otherwise, recursively call last-element on the cdr of x

;; Notes:
;; This implementation uses single-test tail recursion, meaning it uses a single conditional test
;;((null (cdr x))) to determine whether to return the current element ((car x)) or to continue the recursion.
;; It handles lists of varying lengths, including empty lists ('()).

;; The function assumes that the input x is a proper list (a series of cons cells ending with nil).
;; By following this approach, LAST-ELEMENT effectively returns the last element of any given list using tail recursion, ensuring efficient use of stack space.

;;;8.11.3 Augmenting Recursion

;; Augmenting recursive functions like COUNT-SLICES build up their result
;; bit-by-bit. We call this process augmentation. Instead of dividing the
;; problem into an initial step plus a smaller journey, they divide it into a smaller
;; journey plus a final step. The final step consists of choosing an augmentation
;; value and applying it to the result of the previous recursive call. In COUNT-
;; SLICES, for example, we built up the result by first making a recursive call
;; and then adding one to the result. A template for single-test augmenting
;; recursion is shown in Figure 8-3.
;; No augmentation of the result is permitted in tail-recursive functions.
;; Therefore, the value returned by a tail-recursive function is always equal to
;; one of the end-values in the function definition; it isn’t built up bit-by-bit as
;; each recursive call returns. Compare ANYODDP, which always returns T or
;; NIL; it never augments its result.

;;;Template:
(defun func (X)
  (cond (end-test end-value)
        (T (aug-val aug-function
                    (func reduced-x)))))

(defun count-slices (x)
  (cond ((null x) 0)
        (t (+ 1 (count-slices (rest x))))))

;; EXERCISES
;; 8.20. Of the three templates we’ve seen so far, which one describes FACT,
;; the factorial function? Write down the values of the various template
;; components for FACT

;; FACT uses single-test augmenting recursion.

;; 8.21. Write a recursive function ADD-NUMS that adds up the numbers N,
;; N− 1, N− 2, and so on, down to 0, and returns the result. For example,
;; (ADD-NUMS 5) should compute 5+4+3+2+1+0, which is 15.

(defun add-nums (x)
  (cond ((= x 0) 0)
        (t (+ x (add-nums (1- x))))))

;; 8.22. Write a recursive function ALL-EQUAL that returns T if the first
;; element of a list is equal to the second, the second is equal to the third,
;; the third is equal to the fourth, and so on. (ALL-EQUAL ’(I I I I))
;; should return T. (ALL-EQUAL ’(I I E I)) should return NIL. ALL-
;; EQUAL should return T for lists with less than two elements. Does this
;; problem require augmentation? Which template will you use to solve
;; it?

(defun all-equal (lst)
  (cond ((or (null lst) (null (cdr lst))) t) ; Base case: if list has less than two elements, return true
        ((equal (car lst) (cadr lst))         ; Check if current element equals next element
         (all-equal (cdr lst)))               ; Recursively check the rest of the list
        (t nil)))                             ; If elements are not equal, return false

;; 8.12 VARIATIONS ON THE BASIC TEMPLATES

;; The templates we’ve learned so far have many uses. Certain ways of using
;; them are especially common in Lisp programming, and deserve special
;; mention. In this section we’ll cover four variations on the basic templates.

;;; 8.12.1 List-Consing Recursion


;; List-consing recursion is used very frequently in Lisp. It is a special case of
;; augmenting recursion where the augmentation function is CONS. As each
;; recursive call returns, we create one new cons cell. Thus, the depth of the
;; recursion is equal to the length of the resulting cons cell chain, plus one
;; (because the last call returns NIL instead of a cons). The LAUGH function
;; you wrote in the first recursion exercise is an example of list-consing
;; recursion. See Figure 8-4 for the template.


;; Template:
(defun func (N)
  (cond (end-test NIL)
        (T (CONS new-element
                 (func reduced-n)))))

;; 8.24. Write COUNT-DOWN, a function that counts down from n using list-
;; consing recursion. (COUNT-DOWN 5) should produce the list (5 4 3 2
;; 1).
(defun countdown (n)
  (cond ((<= n 0) nil)
        (t (cons n (countdown (- n 1))))))

;; 8.26. Suppose we wanted to modify COUNT-DOWN so that the list it
;; constructs ends in zero. For example, (COUNT-DOWN 5) would
;; produce (5 4 3 2 1 0). Show two ways this can be done.

(defun countdown (n)
  (cond ((<= n 0) '(0))
        (t (cons n (countdown (- n 1))))))


(defun countdown (n)
  (cond ((< n 0) nil)
        (t (cons n (countdown (- n 1))))))

;; Write SQUARE-LIST, a recursive function that takes a list of numbers
;; as input and returns a list of their squares. (SQUARE-LIST ’(3 4 5 6))
;; should return (9 16 25 36).

(defun square-list (lst)
  (if (null lst) '()                        
      (cons (* (car lst) (car lst))  
            (square-list (cdr lst))))) 


(Defun square-list (x)
  (cond ((null x) nil)
        (t (cons (* (car x) (car x))
                 (square-list (rest x))))))

;;; 8.12.2 Simultaneous Recursion on Several Variables


;; Simultaneous recursion on multiple variables is a straightforward extension to
;; any recursion template. Instead of having only one input, the function has
;; several, and one or more of them is ‘‘reduced’’ with each recursive call.

;;Template:
;; (defun func (N X)
;;   (cond (end-test end-value)
;; 	(T (func reduced-n reduced-x))))


(defun my-nth (n x)
  (cond ((zerop n) (first x))
        (t (my-nth (- n 1) (rest x)))))

;; EXERCISES
;; 8.28. The expressions (MY-NTH 5 ’(A B C)) and (MY-NTH 1000 ’(A B C))
;; both run off the end of the list. and hence produce a NIL result. Yet the
;; second expression takes quite a bit longer to execute than the first.
;; Modify MY-NTH so that the recursion stops as soon the function runs
;; off the end of the list.

(defun my-nth (n x)
  (cond ((or (zerop n) (null x)) nil)   ; Base case: if n is zero or x is null, return nil
        (t (my-nth (- n 1) (rest x))))) ; decrement n and move to the rest of x

;;; 8.12.3 Conditional Augmentation


;; In some list-processing problems we want to skip certain elements of the list
;; and use only the remaining ones to build up the result. This is known as
;; conditional augmentation. For example, in EXTRACT-SYMBOLS, defined
;; on the facing page, only elements that are symbols will be included in the
;; result.

;;Template:
(defun func (X)
  (cond (end-test end-value)
        (aug-test (aug-fun aug-val
                           (func reduced-x))
                  (T (func reduced-x)))))


(defun extract-symbols (x)
  (cond ((null x) nil)
        ((symbolp (first x))
         (cons (first x) (extract-symbols (rest x))))
        (t (extract-symbols (rest x)))))

;;  EXERCISES
;; 8.32. Write the function SUM-NUMERIC-ELEMENTS, which adds up all
;; the numbers in a list and ignores the non-numbers. (SUM-NUMERIC-
;; ELEMENTS ’(3 BEARS 3 BOWLS AND 1 GIRL)) should return
;; seven

(defun sum-numeric-elements (x)
  (cond ((null x) 0)
        ((numberp (first x))
         (+ (first x)
            (sum-numeric-elements (rest x))))
        (t (sum-numeric-elements (rest x)))))


(defun mid-add-1 (x)
  "Add 1 into numeric elements"
  (cond ((null x) nil)
        ((numberp (car x))(cons (+ 1 (car x)) (mid-add-1 (cdr x))))
        ((listp (car x))(cons (mid-add-1 (car x)) (mid-add-1 (cdr x))))
        (t (cons (car x)(mid-add-1 (cdr x))))))


;; 8.33. Write MY-REMOVE, a recursive version of the REMOVE function.
(defun my-remove (e x)
  (cond ((null x) nil)
        ((equal e (first x))
         (my-remove e (rest x)))
        (t (cons e (my-remove e (rest x))))))


(defun my-intersection (x y)
  (cond ((null x) nil)
        ((member (first x) y) (cons (first x) (my-intersection (rest x) y)))
        (t (my-intersection (rest x) y))))


;; The function COUNT-ODD counts the number of odd elements in a list
;; of numbers; for example, (COUNT-ODD ’(4 5 6 7 8)) should return
;; two.

;; Show how to write COUNT-ODD using conditional
;; augmentation. Then write another version of COUNT-ODD using the
;; regular augmenting recursion template. (To do this you will need to
;; write a conditional expression for the augmentation value.)

(defun count-odd (x)
  (cond ((null x) 0)
        ((oddp (first x)) ; checks if the first element is odd
         (+ 1 (count-odd (rest x)))) ; Add 1 and recurse on the rest of the list
        (t (count-odd (rest x))))) ; Otherwise, just recurse on the rest of the list

(defun count-odd (x)
  (cond ((null x) 0)
        (t (+ (if
               (oddp (first x)) 1 0) ; Conditionally add 1 if the first element is odd
              (count-odd (rest x)))))) ; Recursively count odd elements in the rest of the list


;;; 8.12.4 Multiple Recursion


;; A function is multiple recursive if it makes more than one recursive call with
;; each invocation. (Don’t confuse simultaneous with multiple recursion. The
;; former technique just reduces several variables simultaneously; it does not
;; involve multiple recursive calls with each invocation.)

;; Template:
(defun func (n)
  (cond (end-test-1 end-value-1)
        (end-test-2 end-value-2)
        (T (combiner (func first-reduced-n)
                     (func second-reduced-n)))))


(defun fib (n)
  (cond ((equal n 0) 1)
        ((equal n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

;;; 8.13 TREES AND CAR/CDR RECURSION

;; Sometimes we want to process all the elements of a nested list, not just the
;; top-level elements. If the list is irregularly shaped, such as (((GOLDILOCKS
;; . AND)) (THE . 3) BEARS), this might appear difficult. When we write our
;; function, we won’t know how long or how deeply nested its inputs will be.

;; CAR/CDR Recursion
;; (A Special Case of Multiple Recursion)
;; Template:
(defun func (X)
  (cond (end-test-1 end-value-1)
        (end-test-2 end-value-2)
        (T (combiner (func (CAR X))
                     (func (CDR X))))))

(defun find-number (x)
  (cond ((numberp x) x)
        ((atom x) nil)
        (t (or (find-number (car x))
               (find-number (cdr x))))))

(defun find-all-number (x)
  (cond ((numberp x) (list x))
        ((atom x) nil)
        ((null x) nil)
        (t (append (find-all-number (car x))
                   (find-all-number (cdr x))))))


;; Besides tree searching, another common use for CAR/CDR recursion is to
;; build trees by using CONS as the combiner. For example, here is a function
;; that takes a tree as input and returns a new tree in which every non-NIL atom
;; has been replaced by the symbol Q.
(defun atoms-to-q (x)
  (cond ((null x) nil)
        ((atom x) ’q)
        (t (cons (atoms-to-q (car x))
                 (atoms-to-q (cdr x))))))

;; (atoms-to-q ’(a . b))
;; (Q . Q)
;; (atoms-to-q ’(hark (harold the angel) sings))
;; (Q (Q Q Q) Q)

;; EXERCISE;
;; 8.39. Write a function COUNT-ATOMS that returns the number of atoms in
;; a tree. (COUNT-ATOMS ’(A (B) C)) should return five, since in
;; addition to A, B, and C there are two NILs in the tree.

(defun count-atoms (x)
  (cond ((atom x) 1)
        (t (+ (count-atoms (car x))
              (count-atoms (cdr x))))))

;;; 8.14 USING HELPING FUNCTIONS


;; For some problems it is useful to structure the solution as a helping function
;; plus a recursive function. The recursive function does most of the work. The
;; helping function is the one that you call from top level; it performs some
;; special service either at the beginning or the end of the recursion. For
;; example, suppose we want to write a function COUNT-UP that counts from
;; one up to n:

;; (count-up 5)⇒(1 2 3 4 5)
;; (count-up 0)⇒nil

(defun count-up (n)
  (count-up-recursively 1 n))

(defun count-up-recursively (cnt n)
  (cond ((> cnt n) nil)
        (t (cons cnt (count-up-recursively (+ cnt 1) n)))))

(trace count-up count-up-recursively)

;;; 8.15 RECURSION IN ART AND LITERATURE

;; Recursion can be found not only in computer programs, but also in stories and
;; in paintings. The classic One Thousand and One Arabian Nights contains
;; stories within stories within stories, giving it a recursive flavor.

;; The Cat in the Hat Comes Back. One of these is shown in Figure 8-9. The nesting of cats
;; within hats is like the nesting of contexts when a recursive function calls itself.
;; In the story, each cat’s taking off his hat plays the role of a recursive function
;; call. Little cat B has his hat on at this point, but the recursion eventually gets
;; all the way to Z, and terminates with an explosion. (If this story has any
;; moral, it would appear to be, ‘‘Know when to stop!’’)

;; Some of the most imaginative representations of recursion and self-
;; referentiality in art are the works of the Dutch artist M. C. Escher, whose
;; lithograph ‘‘Drawing Hands’’ appears in Figure 8-10. Douglas Hofstadter
;; discusses the role of recursion in music, art, and mathematics in his book
;; Godel, Escher, Bach: An Eternal Golden Braid. The dragon stories in this
;; chapter were inspired by characters in Hofstadter’s book.

;;;SUMMARY

;; Recursion is a very powerful control structure, and one of the most important
;; ideas in computer science. A function is said to be ‘‘recursive’’ if it calls
;; itself. To write a recursive function, we must solve three problems posed by
;; the Dragon’s three rules of recursion:
;; 1. Know when to stop.
;; 2. Decide how to take one step.
;; 3. Break the journey down into that step plus a smaller journey.

;; We’ve seen a number of recursion templates in this chapter. Recursion
;; templates capture the essence of certain stereotypical recursive solutions.
;; They can be used for writing new functions, or for analyzing existing
;; functions. The templates we’ve seen so far are:

1. Double-test tail recursion.
2. Single-test tail recursion.
3. Single-test augmenting recursion.
4. List-consing recursion.
5. Simultaneous recursion on several variables.
6. Conditional augmentation.
7. Multiple recursive calls.
8. Trees and CAR/CDR recursion.


;;;Excluded: Adv. Topics. will delved into the future. 
