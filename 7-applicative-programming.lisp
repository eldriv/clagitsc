(defpackage :chapter-7
  (:use :cl))

(in-package :chapter-7)

;;; FUNCTIONS COVERED IN THis CHAPTER:
;; Funcall, mapcar, Find-if, remove-if, remove-if-not, every, reduce, apply

;; Applicative programming is based on the idea that functions are data.
;; Just like symbols and lists are data, so one should be able to pass functions as inputs to other functions, and also return functions as values

;;7.2 FUNCALL

;; FUNCALL calls a function on some inputs
;; The #’ (or ‘‘sharp quote’’) notation is the correct way to quote a function in Common Lisp.

;; CONS function on the inputs A and B like this:

(funcall #'cons 'a 'b) ⇒ (a . b)

;; SBCL:

> (setf fn #’cons)  ;; #<Compiled-function CONS {6041410}>
> fn ;; #<Compiled-function CONS {6041410}>
> (type-of fn) ;; COMPILED-FUNCTION
> (funcall fn ’c ’d) ;;(C . D)

;; 7.3 THE MAPCAR OPERATOR


;;; MAPCAR is the most frequently used applicative operator.

(defun square (n)
  (* n n))

(defun mapcar-test ()
  (mapcar #'square '(1 2 3 4 5 6)))

(mapcar #'(lambda (x) (* x x)) '(1 2 3 4 5 6))

(mapcar #'(lambda (x) (* x x)) '(1 2 3 4 5 6))

;; 7.4 MANIPULATING TABLES WITH MAPCAR

;; Given a function TRANSLATE, defined using ASSOC, we can translate a
;; string of English digits into a string of French ones:

(defun translate (x)
  (second (assoc x *words*))) ;; (mapcar #'translate '(three one four one five))

> (mapcar #'translate ’(three one four one five))
(TROIS UN QUATRE UN CINQ)

;;; Maplist

(maplist #'append '(1 2 3 4) '(1 2) '(1 2 3))
> ((1 2 3 4 1 2 3) (2 3 4 2 3) (3 4 3))

;;; Mapc

(setq dummy nil)
(mapc #'(lambda (&rest x) (setq dummy (append dummy x)))
      '(1 2 3 4)
      '(a b c d e)
      '(x y z))
> (1 A X 2 B Y 3 C Z)

;;; Mapcan

(mapcan #'(lambda (x y) (if (null x) nil (list x y)))
        '(nil nil nil d e)
        '(1 2 3 4 5 6))
> (D 4 E 5 F 6)

;;; EXERCISE


;; 7.1. Write an ADD1 function that adds one to its input. Then write an
;; expression to add one to each element of the list (1 3 5 7 9).

(defun add-1 (n)
  (incf n))

(defun mapcar-test ()
  (mapcar #'add-1 '(1 3 5 7 8 9)))

;; 7.2. Let the global variable DAILY-PLANET contain the following table:
;; Use MAPCAR on this table to extract a list of
;; social security numbers

(defun extract (x)
  (third (assoc x planet))) -- > (mapcar #'extract '(olsen kent lane white))

;;(|123-76-4535| |089-52-6787| |951-26-1438| |355-16-7439|)

;; 7.3 Write an expression to apply the ZEROP predicate to each element of
;; the list (2 0 3 4 0 -5 -6). The answer you get should be a list of Ts and
;; NILs.

> (mapcar #'zerop '(2 0 3 4 0 -5 -6))

;; 7.4. Suppose we want to solve a problem similar to the preceding one, but
;; instead of testing whether an element is zero, we want to test whether it
;; is greater than five. We can’t use > directly for this because > is a
;; function of two inputs; MAPCAR will only give it one input. Show
;; how first writing a one-input function called GREATER-THAN-FIVE-
;; P would help.

(defun greater-than-five (n)
  (if (> n 5)
      '++
      '--))

> (mapcar #'greater-than-five '(7 4 3 9 10 4 3 0))

;; LAMBDA EXPRESSIONS

;; There are two ways to specify the function to be used by an applicative
;; operator. The first way is to define the function with DEFUN and then specify
;; it by #’name, The second way is to pass the function definition directly.

(lambda (n)
  (* n n))

;;Don’t forget to quote it!

> (mapcar #'(lambda (n) (* n n)) '(1 4 9 16 25)) --> (1 16 81 256 625)
> (mapcar #'(lambda (n) (* n 10)) '(1 2 3 4 5)) -->  (10 20 30 40 50)
> (mapcar #'(lambda (x) (list 'hi 'there x)) '(joe fred wanda)) --> ((HI THERE JOE) (HI THERE FRED) (HI THERE WANDA))

;;NOTE:
;; Lambda expressions look similar to DEFUNs, except that the function
;; name is missing and the word LAMBDA appears in place of DEFUN. But
;; lambda expressions are actually unnamed functions. LAMBDA is not a macro
;; or special function that has to be evaluated, like DEFUN. Rather it is a marker
;; that says ‘‘this list represents a function.’’


;;;EXERCISES


;;;7.5. Write a lambda expression to subtract seven from a number..

(lambda (n)
  (- n 7))

;;to use this lambda expression you can write, ((lambda (n) (- n 7)) 25) == 18

;; 7.6. Write a lambda expression that returns T if its input is T or NIL, but
;; NIL for any other input.

(lambda (x)
  (or (null x)
      (equal x t)))

;; 7.7. Write a function that takes a list such as (UP DOWN UP UP) and
;; "flips" each element, returning (DOWN UP DOWN DOWN). Your function should include a lambda expression that knows how to flip an
;; individual element, plus an applicative operator to do this to every  element of the list.

(defun flips (x)
  (cond ((equal x 'down) 'up)
        (t 'down)))

(mapcar #'flips x)

;;;7.6 THE FIND-IF OPERATOR


;;;FIND-IF is another applicative operator. If you give FIND-IF a predicate and
;; a list as input, it will find the first element of the list for which the predicate
;; returns true (any non-NIL value). FIND-IF returns that element.

;;Example:
> (find-if #’oddp ’(2 4 6 7 8 9)) -->  7

;;using lambda on find-if applicative operator, using the function lambda as it creates a predicate the find-if applicative operator find which element is greater than three and get the first element of that. so it will evaluate the 4. 
> (find-if #’(lambda (x) (> x 3))
           ’(2 4 6 7 8 9))

;;If no elements satisfy the predicate, FIND-IF returns NIL.
> (find-if #’(lambda (x) (> x 10))
           ’(2 4 6 7 8 9))

;;;7.7 WRITING ASSOC WITH FIND-IF


;; ASSOC searches for a table entry with a specified key. We can write a simple
;; version of ASSOC that uses FIND-IF to search the table.

(defparameter *words* '((one un) (two deux) (three trois) (four quatre) (five cinq)))

(defun my-assoc (key table)
  (find-if #'(lambda (entry)
               (equal key (first entry)))
           tableo))

> (my-assoc 'one *words*) --> (ONE UN)

;;; EXERCISES
;; 7.8. Write a function that takes two inputs, X and K, and returns the first
;; number in the list X that is roughly equal to K. Let’s say that ‘‘roughly
;; equal’’ means no less than K − 10 and no more than K + 10.

(defun rough-equal (x y)
  (and (not (< x (- y 10)))
       (not (> x (+ y 10)))))

(defun exe (x k)
  (find-if #'(lambda (z) (rough-equal z k))
           x))

;; 7.9. Write a function FIND-NESTED that returns the first element of a list
;; that is itself a non-NIL list.

;;using find-if and cons-p
(defun find-nested (x)
  (find-if #'consp x))

;;using lambda and find-if
(defun find-nested (x)
  (find-if #'(lambda (x)
               (and (consp x) (not (atom x))))
           x))

;;;using cond
(defun find-nested (x)
  (cond ((null x) nil)             
        ((consp (car x)) (car x))  
        (t (find-nested (cdr x)))))

;; MINI KEYBOARD EXERCISE
;; 7.10. In this exercise we will write a program to transpose a song from one
;; key to another. In order to manipulate notes more efficiently, we will
;; translate them into numbers. Here is the correspondence between notes
;; and numbers for a one-octave scale:

;; a. Write a table to represent this information. Store it in a global
;; variable called NOTE-TABLE.
(defparameter *note-table* '((c . 1)
                             (c sharp . 2)
                             (d . 3)
                             (d-sharp . 4)
                             (e . 5)
                             (f . 6)
                             (f-sharp . 7)
                             (g . 8)
                             (g-sharp . 9)
                             (a . 10)
                             (a-sharp . 11)
                             (b . 12)))

;; ;; Write a function called NUMBERS that takes a list of notes as input
;; and returns the corresponding list of numbers. (NUMBERS ’(E D C
;; D E E E)) should return (5 3 1 3 5 5 5). This list represents the first
;; seven notes of ‘‘Mary Had a Little Lamb.’’

(defun numbers (x)
  (mapcar #'(lambda (z) (cdr (assoc z *note-table*)))
          x))

;; c. Write a function called NOTES that takes a list of numbers as input
;; and returns the corresponding list of notes. (NOTES ’(5 3 1 3 5 5
;; 5)) should return (E D C D E E E). Hint: Since NOTE-TABLE is
;; keyed by note, ASSOC can’t look up numbers in it; neither can
;; RASSOC, since the elements are lists, not dotted pairs. Write your
;; own table-searching function to search NOTE-TABLE by number
;; instead of by note.


(defun notes (x)1
  (mapcar #'(lambda (z) (car (rassoc z *note-table*)))
          x))

;; e. To transpose a piece of music up by n half steps, we begin by adding
;; the value n to each note in the piece. Write a function called RAISE
;; that takes a number n and a list of numbers as input and raises each
;; number in the list by the value n. (RAISE 5 ’(5 3 1 3 5 5 5)) should
;; return (10 8 6 8 10 10 10), which is ‘‘Mary Had a Little Lamb’’
;; transposed five half steps from the key of C to the key of F.


(defun add-n (n)
  (lambda (z) (+ z n)))

(defun raise (n x)
  (mapcar (add-n n) x))

;;or

(defun raise (r x)
  (mapcar #'(lambda (e) (+ e r))
          x))

;; f. Sometimes when we raise the value of a note, we may raise it right
;; into the next octave. For instance, if we raise the triad C-E-G
;; represented by the list (1 5 8) into the key of F by adding five to
;; each note, we get (6 10 13), or F-A-C. Here the C note, represented
;; by the number 13, is an octave above the regular C, represented by
;; 1. Write a function called NORMALIZE that takes a list of numbers
;; as input and ‘‘normalizes’’ them to make them be between 1 and 12.
;; A number greater than 12 should have 12 subtracted from it; a
;; number less than 1 should have 12 added to it. (NORMALIZE ’(6
;; 10 13)) should return (6 10 1).

(defun normalize (x)
  (mapcar #’(lambda (n)
              (cond ((< n 1) (+ n 12))
                    ((> n 12) (- n 12))
                    (t e)))
            x))


;; g. Write a function TRANSPOSE that takes a number n and a song as
;; input, and returns the song transposed by n half steps.
;; (TRANSPOSE 5 ’(E D C D E E E)) should return (A G F G A A A).
;; Your solution should assume the availability of the NUMBERS,
;; NOTES, RAISE, and NORMALIZE functions. Try transposing
;; ‘‘Mary Had a Little Lamb’’ up by 11 half steps. What happens if
;; you transpose it by 12 half steps? How about − 1 half steps?

(defun transpose (x y)
  (notes (normalize (raise x (numbers y)))))


;; 7.8 REMOVE-IF AND REMOVE-IF-NOT


;; REMOVE-IF is another applicative operator that takes a predicate as input.
;; REMOVE-IF removes all the items from a list that satisfy the predicate, and
;; returns a list of what’s left.
> (remove-if #'numberp '(2 for 1 sale)) --> (FOR SALE)
> (remove-if #’oddp '(1 2 3 4 5 6 7)) --> (2 4 6)

;;;REMOVE-IF example
(defun plus-p (x)
  (remove-if #'(lambda (y) (not (plusp y)))
             x))

;; The REMOVE-IF-NOT operator is used more frequently than REMOVE-
;; IF. It works just like REMOVE-IF except it automatically inverts the sense of
;; the predicate. This means the only items that will be removed are those for
;; which the predicate returns NIL. So REMOVE-IF-NOT returns a list of all
;; the items that satisfy the predicate. Thus, if we choose PLUSP as the
;; predicate, REMOVE-IF-NOT will find all the positive numbers in a list.
;;REMOVE-IF-NOT example
> (remove-if-not #’plusp ’(2 0 -4 6 -8 10))
--> (2 6 10)

> (remove-if-not #’oddp ’(2 0 -4 6 -8 10))
--> NIL
> (remove-if-not #’(lambda (x) (> x 3)) ’(2 4 6 8 4 2 1))
--> (4 6 8 4)


;; Here is a function, COUNT-ZEROS, that counts how many zeros appear in
;; a list of numbers. It does this by taking the subset of the list elements that are
;; zero, and then taking the length of the result.
;; (remove-if-not #’zerop ’(34 0 0 95 0)) ⇒ (0 0 0)

(defun count-zeros (x)
  (length (remove-if-not #'zerop x)))

(count-zeros ’(34 0 0 95 0))⇒3
(count-zeros ’(1 0 63 0 38))⇒2
(count-zeros ’(0 0 0 0 0))⇒5
(count-zeros ’(1 2 3 4 5))⇒0


(defun count-ones (x)
  (length (remove-if #'zerop x)))

(count-ones '(1 0 1 0 1 0 1 0 1)) -> 5
(count-ones '(1 0 1 0 1 0 1 0 1 1 1 1 1 1)) -> 10


;; EXERCISES

;; 7.11. Write a function to pick out those numbers in a list that are greater than
;; one and less than five.

(defun greater-than-one (x)
  (remove-if-not #'(lambda (z) (and (> z 1) (< z 5)))
                 x))

;; 7.12. Write a function that counts how many times the word ‘‘the’’ appears
;; in a sentence.

(defun the-counter (x)
  (length (remove-if-not #'(lambda (z) (equal 'the z))
                         x)))

(defun the-counter (x)
  (count 'the x))

;; 7.13. Write a function that picks from a list of lists those of exactly length
;; two.

(defun pick (x)
  (remove-if-not #'(lambda (z) (equal (length z) 2))
                 x))

> (setq list-of-lists '((1 2) (3 4 5) (6) ("a" "b") ("c" "d") ("e" "f" "g")))
> (pick list-of-lists) ---> ((1 2) ("a" "b") ("c" "d"))

;; 7.14. Here is a version of SET-DIFFERENCE written with REMOVE-IF:

(defun my-setdiff (x y)
  (remove-if #’(lambda (e) (member e y))
               x))

;; Show how the INTERSECTION and UNION functions can be written, using REMOVE-IF or REMOVE-IF-NOT.
(defun my-intersection (x y)
  (remove-if-not #'(lambda (e) (member e y))
                 x))

(defun my-union (x y)
  (append x (remove-if #'(lambda (e) (member e x))
                       y)))

;; MINI KEYBOARD EXERCISE

;; 7.15. In this keyboard exercise we will manipulate playing cards with
;; applicative operators. A card will be represented by a list of form (rank
;; suit), for example, (ACE SPADES) or (2 CLUBS). A hand will be
;; represented by a list of cards.


;; a. Write the functions RANK and SUIT that return the rank and suit of
;; a card, respectively. (RANK ’(2 CLUBS)) should return 2, and
;; (SUIT ’(2 CLUBS)) should return CLUBS.

(defun rank-card (rank)
  (first rank))

(defun suit-card (suit)
  (last suit))

;; b. Set the global variable MY-HAND to the following hand of cards:
;; ((3 hearts)
;; (5 clubs)
;; (2 diamonds)
;; (4 diamonds)
;; (ace spades))

(defparameter *my-hand* '((3 hearts)
                          (5 clubs)
                          (2 diamonds)
                          (4 diamonds)
                          (ace spades)))

(defun count-suit (suit hand)
  (count-if (lambda (card)
              (eq (second card) suit))
            hand))

;; c. Set the global variable COLORS to the following table:
;; ((clubs black)
;; (diamonds red)
;; (hearts red)
;; (spades black))

(defparameter *colors* '((clubs black)
                         (diamonds red)
                         (hearts red)
                         (spades black)))

(defun color-of (cards)
  (second (assoc (suit-card cards) *colors*)))

;; d. Write a function FIRST-RED that returns the first card of a hand
;; that is of a red suit, or NIL if none are.
(defun first-red (hands)
  (find-if #'(lambda (card)
               (equal (color-of card) 'red))
           hands))

;; e. Write a function BLACK-CARDS that returns a list of all the black
;; cards in a hand.

(defun black-cards (hands)
  (remove-if-not #'(lambda (card)
                     (equal (color-of card) 'black))
                 hands))

;; f. Write a function WHAT-RANKS that takes two inputs, a suit and a
;; hand, and returns the ranks of all cards belonging to that suit.
;; (WHAT-RANKS ’DIAMONDS MY-HAND) should return the list
;; (2 4). (WHAT-RANKS ’SPADES MY-HAND) should return the list (ACE). Hint: First extract all the cards of the specified suit,then use another operator to get the ranks of those cards.

(defun what-ranks (suit hands)
  (mapcar #'rank-card
          (remove-if-not #'(lambda (card)
                             (equal (suit-card card) suit))
                         hands)))

;; 7.9 THE REDUCE OPERATOR

;; REDUCE is an applicative operator that reduces the elements of a list into a
;; single result. REDUCE takes a function and a list as input, but unlike the
;; other operators we’ve seen, REDUCE must be given a function that accepts
;; two inputs. Example: To add up a list of numbers with REDUCE, we use +
;; as the reducing function.

(reduce #’+ ’(1 2 3)) ---> 6 
(reduce #’+ ’(10 9 8 7 6)) ---> 40
(reduce #’+ ’(5)) ⇒ 5
(reduce #’+ nil) ⇒ 0

;; Similarly, to multiply a bunch of numbers together, we use * as the
reducing function:
(reduce #’* ’(2 4 5)) --> 40
(reduce #’* ’(3 4 0 7)) --> 0
(reduce #’* ’(8)) --> 8


;; EVERY takes a predicate and a list as input. It returns T if there is no element
;; that causes the predicate to return false. Examples:
> (every #’numberp ’(1 2 3 4 5)) --> T
> (every #’numberp ’(1 2 A B C 5))  --> NIL



;;;REDUCE
(defun how-reduce-works (x)
  (let ((return-reduce (reduce #'+ x)))
    (format t "Result: ~a" return-reduce)))
;;;EVERY

(defun greater-than-threshold-p (number threshold)
  (> number threshold))

(defun greater-than ()
  (let ((numbers '(6 7 8 9 10)))
    (if (every #'(lambda (x)
                   (greater-than-threshold-p x 5))
               numbers)
        (format t "Every number is greater than 5.~%")
        (format t "Not every number is greater than 5.~%"))))

(defun greater-than (x)
  (let ((numbers x))
    (if (every #'(lambda (y)
                   (greater-than-threshold-p x y))
               numbers)
        (format t "Every number is greater than 5. ~%")
        (Format t "Not"))))
;;; SUMMARY
;; Applicative operators are functions that apply other functions to data
;; structures. There are many possible applicative operators, only a few of which
;; are built in to Lisp. Advanced Lisp programmers make up their own operators
;; whenever they need new ones.

;; FUNCALL -- calls a function on some inputs (funcall #’cons 'a 'b) ⇒ (a . b)

;; MAPCAR -- applies a function to every element of a list and returns a list of
;; the results.

;; FIND-IF -- searches a list and returns the first element that satisfies a
;; predicate.

;; REMOVE-IF -- removes all the elements of a list that satisfy a
;; predicate, so the list it returns contains only those elements that fail to satisfy
;; it.

;; REMOVE-IF-NOT -- is used more frequently than REMOVE-IF. It returns
;; all the elements that do satisfy the predicate, having removed those that don’t
;; satisfy it.

;; EVERY -- returns T only if every element of a list satisfies a
;; predicate.

;; REDUCE -- uses a reducing function to reduce a list to a single value.

;; EXERCISES
;; 7.19. Write a function ALL-ODD that returns T if every element of a list of
;; numbers is odd.

(defun all-odd (x)
  (every #'oddp x))

;; 7.20. Write a function NONE-ODD that returns T if every element of a list of
;; numbers is not odd.

(defun none-odd (x)
  (every #'evenp x))

;; 7.21. Write a function NOT-ALL-ODD that returns T if not every element of
;; a list of numbers is odd.

(defun not-all-odd (x)
  (find-if #'evenp x))

;; 7.22. Write a function NOT-NONE-ODD that returns T if it is not the case
;; that a list of numbers contains no odd elements.

(defun not-none-odd (x)
  (find-if #'oddp x))

;; 7.23. Are all four of the above functions distinct from one another, or are
;; some of them the same? Can you think of better names for the last
;; two?

;;All for are distinct. Better names for the last two is find-even and find-odd.

;; 7.26. Show how to write FIND-IF given REMOVE-IF-NOT.

(defun other-find-if (x)
  (remove-if-not #'oddp x))

;;or

(defun my-find-if (func x)
  (car (remove-if-not func x)))


;; 7.27. Show how to write EVERY given REMOVE-IF.

(defun my-every (func x)
  (null (remove-if func x)))

;;pg. 227/587

;; 7 Advanced Topics



;;7.11 OPERATING ON MULTIPLE LISTS

;; example

> (mapcar #’(lambda (x y) (list x ’gets y))
          ’(fred wilma george diane)
          ’(job1 job2 job3 job4))

> (mapcar #’+ ’(1 2 3 4 5) ’(60 70 80 90 100)) -->  (61 72 83 94 105)

> (mapcar #’+ ’(1 2 3) ’(10 20 30 40 50)) --> (11 22 33)

;;;EXERCISE;
;; 7.30. Recall the English–French dictionary we stored in the global variable
;; WORDS earlier in the chapter. Given this dictionary plus the list or
;; corresponding Spanish words (UNO DOS TRES QUATRO CINCO),

;; write an expression to return a trilingual dictionary. The first entry of
;; the dictionary should be (ONE UN UNO).

(defparameter *words* '((one un) (two deux) (three trois) (four quatre) (five cinq)))

(defun english-french ()
  (mapcar #'(lambda (x y) (append x (list y)))
          *words*
          '(uno dos tres quatro cinco)))


;; The result returned by FUNCTION is always some kind of function object.
;; These objects are a form of data, just like symbols and lists. For example, we
;; can store them in variables. We can also call them, using FUNCALL or
;; APPLY. (APPLY was discussed in Advanced Topics section 3.21.)

;;7.12 THE FUNCTION SPECIAL FUNCTION


;; Just as ’ is shorthand for the QUOTE special function
;; #’ is shorthand for the
;; FUNCTION special function. Writing #’CONS is therefore equivalent to writing (FUNCTION CONS).

> (setf g #’(lambda (x) (* x 10))) ---> #<Lexical-closure 41653824>
> (funcall g 12) --->  120

> (setf k #'(lambda (x) (cons x 'ADI)))
> (apply k '(adi)) --> (ADI . ADI)


;; 7.13 KEYWORD ARGUMENTS TO APPLICATIVE OPERATORS


;; Some applicative operators, such as FIND-IF, REMOVE-IF, REMOVE-IF-
;; NOT, and REDUCE, accept optional keyword arguments. For example, the
;; :FROM-END keyword, if given a non-NIL value, causes the list to be
;; processed from right to left.

> (find-if #’oddp ’(2 3 4 5 6))             --> 3 Find the first odd number.
> (find-if #’oddp ’(2 3 4 5 6) :from-end t) --> 5 Find the last odd number.


;; The :FROM-END keyword is particularly interesting with REDUCE; it
;; causes elements to be reduced from right to left instead of the usual left to
;; right.

> (reduce #’cons ’(a b c d e))            ----> ((((A . B) . C) . D) . E)

> (reduce #’cons ’(a b c d e) :from-end t) ---> (A B C D . E)

;; 7.15 WRITING AN APPLICATIVE OPERATOR


;; Using FUNCALL, we can write our own applicative operator that takes a
;; function as input. Our operator will be called INALIENABLE-RIGHTS. It
;; applies its input to a particular list, drawn from the American Declaration of
;; Independence.

(defun inalienable-rights (fn)
  (funcall fn
           ’(life liberty and the pursuit of happiness)))

;; > (inalienable-rights #’length)  --> 7
;; > (inalienable-rights #’reverse) --> (HAPPINESS OF PURSUIT THE AND LIBERTY LIFE)
;; > (inalienable-rights #’first)   -->  LIFE
;; > (inalienable-rights #’rest)    --> (LIBERTY AND THE PURSUIT OF HAPPINESS)

(defun inalianable-left (foo bar)
  (funcall foo bar '(3 3 3 3 3)))


;; > (inalianable-left #'every #'oddp) --> T
;; > (inalianable-left #'every #'evenp) --> NIL

;; 7.16 FUNCTIONS THAT MAKE FUNCTIONS

;; It is possible to write a function whose value is another function. Suppose we
;; want to make a function that returns true if its input is greater than a certain
;; number N. We can make this function by constructing a lambda expression
;; that refers to N, and returning that lambda expression:

(defun make-greater-than-predicate (n)
  #'(lambda (x) (> x n)))

;; The value returned by MAKE-GREATER-THAN-PREDICATE will be a
;; lexical closure. We can store this value away somewhere, or pass it as an
;; argument to FUNCALL or any applicative operator.


> (setf pred (make-greater-than-predicate 3)) ---> #<Lexical-closure 7315225>

(funcall pred 2) --> nil

(funcall pred 5) --> t

(find-if pred '(2 3 4 5 6 7 8 9)) --> 4

(remove-if pred '(2 3 4 5 6 7 8 9)) --> '(2 3)

(remove-if-not pred '(2 3 4 5 6 7 8 9)) --> '(4 5 6 7 8 9)
