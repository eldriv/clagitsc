
#|This chapter presents more list-manipulation functions, and shows how lists
are used to implement such other data structures as sets, tables, and trees. |#

(defpackage :chapter-6
  (:use :cl)
  (:export
   :*words*))

(in-package :chapter-6)

;;6.2 PARENTHESIS NOTATION VS. CONS CELL NOTATION


(cons 'w '(x y z))
⇒ (w x y z)

(defun add-to-end (x e)
  "Adds element E to the end of list X."
  (append x (list e))) ⇒ (add-to-end '(a b c) 'd) ⇒ (A B C D)

(defun add-to-end (x e)
  (concatenate 'list x (list e)))


;;6.4 COMPARING CONS, LIST, AND APPEND

#|
• CONS creates one new cons cell. It is often used to add an element to the front of a list.

• LIST makes new lists by accepting an arbitrary number of inputs and building a chain of cons cells ending in NIL. The car of each cell points to the corresponding input.

• APPEND appends lists together by copying its first input and  making the cdr of the last cell of the copy point to the secondinput. It is an error for the first input to APPEND to be a non-list.
|#

;;; Example 
#|
(cons 'rice '(and beans)) -->   (RICE AND BEANS)
(list 'rice '(and beans)) -->   (RICE (AND BEANS))
(append 'rice '(and beans)) --> Error: RICE is not a list.
|#

;;; Next, let’s see what happens when both inputs are lists:
#|
(cons '(here today) '(gone tomorrow))      --> ((HERE TODAY) GONE TOMORROW)
(list '(here today) '(gone tomorrow))      --> ((HERE TODAY) (GONE TOMORROW))
(append '(here today) '(gone tomorrow))    -->  (HERE TODAY GONE TOMORROW)
|#

;;; First input is list and second input is symbol
#|
(cons '(eat at) 'joes) -->   ((EAT AT) . JOES)
(list '(eat at) 'joes) -->   ((EAT AT) JOES)
(append '(eat at) 'joes) --> (EAT AT . JOES)
|#

;;6.5 MORE FUNCTIONS ON LISTS

;;; FUNCTIONS COVERED: REVERSE, NTH, NTHCDR, LAST, and REMOVE.

;;; 6.5.1—REVERSE

(defun reverse-example ()
  (reverse '(A B C D E)))

(defun reverse-character ()
  "Reverse character in first element on each lists that will form a new list "
  (let ((lists '((a b c) (d e f) (g h i))))
    (apply #'mapcar #'list (reverse lists)))) --> ((G D A) (H E B) (I F C))

;;; REVERSE can use to add an element to the end of a list.

(defun add-to-end-2(x y)
  (reverse (cons y (reverse x)))) -->  (add-to-end-2 '(A B C) 'D) --> (A B C D)

;; 6.5.2 NTH and NTHCDR

(nthcdr 0 '(a b c)) ⇒ (a b c)
(nthcdr 1 '(a b c)) ⇒ (b c)

(nthcdr 2 '(a b c)) ⇒ (c)
(nthcdr 3 '(a b c)) ⇒ nil

(defun nth-1 (n x)
  (car (nthcdr n x)))

(nth 0 '(a b c)) ⇒ a
(nth 1 '(a b c)) ⇒ b
(nth 2 '(a b c)) ⇒ c
(nth 3 '(a b c)) ⇒ nil
(car (nthcdr 0 '(a b c))) --> A

;;; EXERCISES


;; 6.1. Why is (NTH 4 '(A B C)) equal to NIL? --> Because the elements on the list are only 2, since 0, 1, 2 index there is no nth-4

;; 6.2. What is the value of (NTH 3 '(A B C . D)), and why? --> D

;; 6.5.3 LAST

;; LAST returns the last cons cell of a list, in other words, the cell whose car is
;; the list’s last element. By definition, the cdr of this cell is an atom; otherwise
;; it wouldn’t be the last cell of the list. If the list is empty, LAST just returns NIL

(last '(all is forgiven)) ⇒ (forgiven)

(last nil)  ⇒ nil

(last '(a b c . d))  ⇒ (c . d)

(last 'nevermore)  ⇒ Error! NEVERMORE is not a list.

;; EXERCISES


;; 6.3. What is the value of (LAST '(ROSEBUD)) ? -->  (ROSEBUD)

;; 6.4. What is the value of (LAST '((A B C))), and why? --> (C) because it returns the last cons cell of a list, because it is on the last list's element.


;;6.5.4 REMOVE


;;REMOVE removes an item from a list.

(remove 'a '(b a n a n a)) ⇒ (b n n)

(remove 1 '(3 1 4 1 5 9))  ⇒ (3 4 5 9)

;; The following table should help you remember which functions copy their
;; input and which do not. APPEND, REVERSE, and REMOVE return a new
;; cons cell chain that is not contained in their input, so they must copy their
;; input to produce the new chain. Functions such as NTHCDR, NTH, and
;; LAST return a pointer to some component of their input.

(defparameter *rows* '(APPEND REVERSE NTHCDR NTH LAST REMOVE))
(defparameter *column* '(yes-except-last-input yes no no no yes-only-second-input))

(defun table-function ()
  "JUST FOR FUN, JUST PRINTING THE TABLE IN THE BOOK"
  (format t "----------------------------------~%FUNCTION   |  COPIES ITS INPUT~%----------------------------------~&")
  (let ((x *rows*)
        (y *column*))
    (loop :for row :in x
          :for col :in y
          :do (format t "~10a | ~10a~%" row col)))
  (format t"----------------------------------"))

;;EXERCISES


;; 6.5. Write an expression to set the global variable LINE to the list (ROSES
;; ARE RED). Then write down what each of the following expressions
;; evaluates to:

(defvar *LINE* '(ROSES ARE RED))

#|
(reverse line)     ---> (RED ARE ROSES)
(first (last line))---> (RED)
(nth 1 line)       ---> (ARE)
(reverse (reverse line)) ---> (ROSES ARE RED)
(append line (list (first line))) --> (ROSES ARE RED ROSES)
(append (last line) line) --> (RED ROSES ARE RED)
(list (first line) (last line)) --> (ROSES '(RED))
(cons (last line) line) --> ((RED) ROSES ARE RED) 
(remove ’are line) --> (ROSES RED)
(append line ’(violets are blue)) --> (ROSES ARE RED VIOLETS ARE BLUE)
|#

;; 6.6 Use the LAST function to write a function called LAST-ELEMENT that returns the last element of a list instead of the last cons cell. Write another version of LAST-ELEMENT using REVERSE instead of LAST. Write another version using NTH and LENGTH.

(defun last-element (lst)
  (car (last lst)))

(defun last-but-reverse (x)
  (car (reverse x)))

(defun last-element (x)
  (and (nth (- (length x) 1)
            x)))

;; 6.7 Use REVERSE to write a NEXT-TO-LAST function that returns the
;; next-to-last element of a list. Write another version using NTH.

(defun next-to-last (x)
  (second (reverse x)))

(defun next-to-last-nth (x)
  (if (cdr x)
      (nth (- (length x) 2)
           x)))

;; 6.8 Write a function MY-BUTLAST that returns a list with the last element
;; removed. (MY-BUTLAST ’(ROSES ARE RED)) should return the list
;; (ROSES ARE). (MY-BUTLAST ’(G A G A)) should return (G A G))

(defun my-butlast (x)
  (reverse (rest (reverse x))))

;; 6.9 A palindrome is a sequence that reads the same forwards and
;; backwards. The list (A B C D C B A) is a palindrome; (A B C A B C)
;; is not. Write a function PALINDROMEP that returns T if its input is a
;; palindrome.

(defun palindrome-p (x)
  (and (equal (reverse x)
              x)))

;;  7.0 Write a function MAKE-PALINDROME that makes a palindrome out
;; of a list, for example, given (YOU AND ME) as input it should return
;; (YOU AND ME ME AND YOU).

(defun make-palindrome (lists)
  (append lists (reverse lists)))

;; 6.6 LISTS AS SETS


;;; 6.6.1 MEMBER

;; The MEMBER predicate checks whether an item is a member of a list. If the
;; item is found in the list
(defun beforep (x y l)
  "Returns true if X appears before Y in L"
  (member y (member x l)))

;;USAGE;:

(before-p 'not 'whom '(ask not for whom the bell tolls)) --> (WHOM THE BELL TOLLS) returns T

;; 6.6.2 INTERSECTION

;; The INTERSECTION function takes the intersection of two sets and returns a
;; list of items appearing in both sets. The exact order in which elements appear
;; in the result is undefined; it may differ from one Lisp implementation to
;; another. Order isn't important for sets anyway; only the elements themselves
;; matter.

(intersection '(fred john mary)
              '(sue mary fred)) --> (FRED MARY)


;;; EXERCISES REGARDING THE CONTAINS-ARTICLE-P
;;; Suppose we instead want a predicate CONTAINS-ART;; ICLE-P that  returns a true value if a sentence contains any article, such as ‘‘the,’’ ‘‘a,’’ or ‘‘an.’’ Write a version of this predicate using INTERSECTION. Write another version using MEMBER and OR. Could you solve this problem with AND instead of OR?

(defun contains-the-p (sent)
  (member 'the sent))

(defun contains-article-p (sent)
  (intersection sent '(the a and)))

(defun contains-article-p1 (sent)
  (or (member 'the sent)
      (member 'a sent)
      (member 'an sent)))

(defun contains-article-p2 (sent)
  (not (and (not (member 'the sent))
            (not (member 'a sent))
            (not (member 'an sent)))))

;; 6.6.3 UNION


;; The UNION function returns the union of two sets, in other words, a list of
;; items that appear in either set. If an item appears in both sets, it will still
;; appear only once in the result. The exact order in of items in the result is
;; undefined (and unimportant) for sets.

#|
> (union '(finger hand arm) --->
'(toe finger foot leg)) --> (FINGER HAND ARM TOE FOOT LEG)
|#

;; EXERCISES

;; Write a function ADD-VOWELS that takes a set of letters as input and
;; adds the vowels (A E I O U) to the set. For example, calling ADD-
;; VOWELS on the set (X A E Z) should produce the set (X A E Z I O
;; U), except that the exact order of the elements in the result is
;; unimportant.

(defun add-vowels (letters)
  (let* ((vowels '(A E I O U))
         (union-letters (union (reverse vowels) letters)))
    (reverse (reverse union-letters))))

(defun add-vowels (letters)
  (let ((vowels '(A E I O U)))
    (loop :for vowel :in vowels
          :unless (member vowel letters)
            :do (setf letters (append letters (list vowel))))
    letters))


;; 6.6.4 SET-DIFFERENCE

;; The SET-DIFFERENCE function performs set subtraction. It returns what is
;; left of the first set when the elements in the second set have been removed.
;; Again, the order of elements in the result is undefined.

> (set-difference '(alpha bravo charlie delta)
                  '(bravo charlie)) ----> (ALPHA DELTA)



;; 6.6.5 SUBSETP


;; The SUBSETP predicate returns T if one set is contained in another, in other
;; words, if every element of the first set is an element of the second set.

(subsetp '(a i) '(a e i o u)) ⇒t
(subsetp '(a x) '(a e i o u)) ⇒nil

;; EXERCISE
;; 6.21. If set x is a subset of set y, then subtracting y from x should leave the
;; empty set. Write MY-SUBSETP, a version of the SUBSETP predicate
;; that returns T if its first input is a subset of its second input.

(defun my-subsetp (x y)
  (null (set-difference x y)))

(defun my-subsetp (x y)
  (loop :for i :in x
        :always (member i y)))

(defun my-subsetp (x y)
  (let ((is-subset t))
    (dolist (i x)
      (unless (member i y)
        (setq is-subset nil)))
    is-subset))



;; 6.7 PROGRAMMING WITH SETS

;; To write functions to figure out whether a word is a male or
;; female first name. We will use only a few instances of each type of name to
;; keep the example brief.

(defun titledp (name)
  (member (first name)
          '(mr ms miss mrs)))

(setf male-first-names
      '(john kim richard fred george))

(setf female-first-names
      '(jane mary wanda barbara kim))

(defparameter *male-first-names* '(john kim rom adi mcwhyne))
(defparameter *female-first-names* '(jane mary wanda barbara kimmy))


(defun malep (name)
  (and (member name *male-first-names*)
       (not (member name *female-first-names*))))

(defun femalep (name)
  (and (member name *female-first-names*)
       (not (member name *male-first-names*))))


(defun give-title (name)
  "Returns a name with an appropriate title on
the front."
  (cond ((titledp name) name)
        ((malep (first name)) (cons 'mr name))
        ((femalep (first name)) (cons 'ms name))
        (t (append '(ms or mr) name))))

;;; Assoc and rassoc
;; The ASSOC function looks up an entry in a table, given its key.


(defparameter *words* '((one un) (two deux) (three trois) (four quatre) (five cinq)))

(defparameter *produce* '((apple . fruit)
                          (celery . veggie)
                          (banana . fruit-yellow)
                          (lettuce . veggie)))

(defparameter *sounds* '((cow . moo)
                         (pig . oink)
                         (cat . meow)
                         (dog . woof)
                         (bird . tweet)))

(defun assoc-example (x y)
  (caar (cons (assoc y *words*) (member 'cinq x)))) -->  FIVE

(defun assoc-example (x)
  (cdr (assoc x *produce*)))

;; RASSOC is like ASSOC, except it looks at the cdr of each element of the
;; table instead of the car.

(defun rassoc-example (x)
  (rassoc x *produce*))

;;6.9 PROGRAMMING WITH TABLES


(defparameter *objects*
  '((object1 A nice word can be presented into wisdom and sentences i don't know what I am talking about)
    (object2 small red dull metal cube)
    (object3 red small dull plastic cube)
    (object4 small dull blue metal cube)
    (object5 small shiny red four-sided pyramid)
    (object6 large shiny green sphere)))


(defun description (x)
  (rest (assoc x *objects*)))

(defparameter *objects*
  '(:object1 "Description of object 1"
    :object2 "Description of object 2"
    :object3 "Description of object 3"))

(defun description (x)
  (let ((y (format nil "~a" x)))  
    (getf *objects* x y)))

;; The technical term for this is set exclusive or. There is a built-in
;; Common Lisp function to compute it.

(defun differences (x y)
  (set-exclusive-or (description x)
                    (description y)))



;; 6.30. Make a table called BOOKS of five books and their authors. The first
;; entry might be (WAR-AND-PEACE LEO-TOLSTOY).


(defparameter *BOOKS* '((WAR-AND-PEACE LEO-TOLSTOY)
                        (ON-LISP       PAUL-GRAHAMS)
                        (CLAGITSC      DAVID)
                        (MR-WAS        PETE-HAUTMAN)
                        (GOT           GEORGE)))


;; 6.31. Write the function WHO-WROTE that takes the name of a book as  input and returns the book’s author.
(defun who-wrote (x)
  (second (assoc x (reverse *BOOKS*))))


;; 6.32. Suppose we do (SETF BOOKS (REVERSE BOOKS)), which reverses
;; the order in which the five books appear in the table. What will the
;; WHO-WROTE function do now?

;; > still like that nothing happened.


;; 6.33 Suppose we wanted a WHAT-WROTE function that took an author’s
;; name as input and returned the title of one of his or her books. Could
;; we create such a function using ASSOC and the current table? If not,
;; how would the table have to be different?

;; > No, I could create WHAT-WROTE by using RASSOC, by changing the table into dotted pair.


(defparameter *atlas* '(((pennsylvania (pittsburgh johnstown))
                         (new-jersey (newark princeton trenton))
                         (ohio (columbus)))))


(defun assoc-key (x)
  (rest (assoc x *atlas*)))



;;; COMPUTER HACKERUS NERDUS
;; a. What type of data structure would be useful for representing the
;; connection between a state and its successor? Write such a data
;; structure for the five-state cycle given above, and store it in a global
;; variable called NERD-STATES.
(defparameter *cylic* '((SLEEPING . EATING)
                        (EATING    . WAITING)
                        (WAITING . PROGRAMMING)
                        (PROGRAMMING . DEBUGGING)
                        (DEBUGGING . SLEEPING)))

;; b. Write a function NERDUS that takes the name of a state as input
;; and uses the data structure you designed to determine the next state
;; the creature will be in. (NERDUS ’SLEEPING) should return
;; EATING, for example. (NERDUS ’DEBUGGING) should return
;; SLEEPING.

(defun nerdus (state)
  (cdr (assoc state *cylic*)))

;; c. What is the result of (NERDUS ’PLAYING-GUITAR)?

;; > mil

;; d. When Nerdus Americanis ingests too many stimulants (caffeine
;; overdose), it stops sleeping.
;; After finishing Debugging, it
;; immediately goes on to state Eating. Write a function SLEEPLESS-
;; NERD that works just like NERDUS except it never sleeps. Your
;; function should refer to the global variable NERD-STATES, as
;; NERDUS does.

(defun sleepless-nerd (x)
  (let ((state (nerdus x)))
    (cond ((equal state 'sleeping) (nerdus state))
          (t state))))

;; e.
(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))

;; 6.36. Write a function to swap the first and last elements of any list. (SWAP-
;; FIRST-LAST ’(YOU CANT BUY LOVE)) should return (LOVE
;; CANT BUY YOU).

(SWAP-FIRST-LAST ’(YOU CANT BUY LOVE)) --> LOVE CANT BUY YOU

(defun swap-first-last (x)
  (let* ((reverse-a (reverse (rest x)))  ;;LOVE BUY CANT
         (reverse-b (reverse (rest reverse-a))) ;; CANT BUY
         (first-element (list (first x))) ;;LOVE
         (last-element (last x))) ;;YOU
    (append last-element reverse-b 
            first-element))) ;;YOU CANT BUY LOVE

;; 6.37. ROTATE-LEFT and ROTATE-RIGHT are functions that rotate the
;; elements of a list. (ROTATE-LEFT ’(A B C D E)) returns (B C D E
;; A), whereas ROTATE-RIGHT returns (E A B C D). Write these
;; functions.

;;'(A B C D E) returns (B C D E A)

(defun rotate-left (lists)
  (let ((n -1))
    (rotatef (nth (incf n) lists)
             (nth (incf n) lists)
             (nth (incf n) lists)
             (nth (incf n) lists)
             (nth (incf n) lists))
    lists))

;;(A B C D E), whereas ROTATE-RIGHT returns (E A B C D)

(defun rotate-right (lists)
  (let ((n 5))
    (rotatef (nth (decf n) lists)
             (nth (decf n) lists)
             (nth (decf n) lists)
             (nth (decf n) lists)
             (nth (decf n) lists))
    lists))

;; 6.40. Show how to transform the list (A B C D) into a table so that the
;; ASSOC function using the table gives the same result as MEMBER
;; using the list.

(defparameter *tables* '((1 2 3 4)
                         (2 3 4)
                         (3 4)
                         (4)))

;;; FUNCTIONS COVERED IN THIS CHAPTER

;; List functions: APPEND, REVERSE, NTH, NTHCDR, LAST, REMOVE.

;; Set functions: UNION, INTERSECTION, SET-DIFFERENCE, SET-
;; EXCLUSIVE-OR, MEMBER, SUBSETP, REMOVE-DUPLICATES.

;; Table functions: ASSOC, RASSOC.

;;;ADVANCED TOPICS


;; Trees are nested lists. All the functions covered so far operate on the top level
;; of a list; they do not look at any more of the structure than that. Lisp also
;; includes a few functions that operate on the entire list structure. Two of these
;; are SUBST and SUBLIS.

;;6.10.1 SUBST

;;;EXAMPLE

> (subst 'fred 'bill
         '(bill jones sent me an itemized
           bill for the tires))

RESULT:
(FRED JONES SENT ME AN ITEMIZED FRED FOR THE TIRES)

;;If the symbol being sought doesn’t appear at all in the list, SUBST returns the original list unchanged.

> (subst 'bill 'fred '(keep off the grass)) --> (KEEP OFF THE GRASS)
> (subst 'on 'off '(keep off the grass))    --> (KEEP ON THE GRASS)

;;6.10.2 SUBLIS


(sublis '((roses . violets) (red . blue)) '(roses are red)) --> VIOLETS ARE BLUE


(setf dotted-words
      ’((one   . un)
        (two   . deux)
        (three . trois)
        (four  . quatre)
        (five  . cinq)))

> (sublis dotted-words ’(three one four one five))  ---> (TROIS UN QUATRE UN CINQ)

;;EXERCISE:

(defun royal-we (lists)
  (subst 'we 'i lists)) --> (IF WE LEARN LISP WE WILL BE PLEASED)

;; 6.11 EFFICIENCY OF LIST OPERATIONS


;;;more efficient function

(defun add-to-end-1 (x y)
  (append x (list y)))

;;;it performs garbage collection 

(defun add-to-end-2 (x y)
  (reverse (cons y (reverse x))))

;; 6.12 SHARED STRUCTURE


> (setf x '(a b c))           -->          (A B C)
> (setf y (cons 'd (cdr x)))  -->          (D B C)


;; The value of X is (A B C) and the value of Y is (D B C). The lists share
;; the same cons cell structure for (B C), as the following indicates. The sharing
;; comes about because we built Y from (CDR X). If we had simply said (SETF
;; Y ’(D B C)), no structure would be shared with X.

;; 6.13 EQUALITY OF OBJECTS


;; The EQ function is faster than the EQUAL function because EQ only has
;; to compare an address against another address, whereas EQUAL has to first
;; test if its inputs are lists, and if so it must compare each element of one against
;; the corresponding element of the other. Due to its greater efficiency,
;; programmers often use EQ instead of EQUAL when symbols are being
;; compared. They don’t usually use EQ on lists, unless they want to tell
;; whether two cons cells are the same.

;;; NOTES

#|
• EQ - for identity comparison (pointer equality).

is the fastest equality test: It compares addresses. Experts use
it to compare symbols quickly, and to test whether two cons cells
are physically the same object. It should not be used to compare
numbers.

• EQL - for identity comparison with some allowances for numbers and characters.

is like EQ except it can safely compare numbers of the same
type, such as two integers or two floating point numbers. It is the
default equality test in Common Lisp.

• EQUAL - for structural comparison of objects

is the predicate beginners should use. It compares lists
element by element; otherwise it works like EQL.

• EQUALP 
is more liberal than EQUAL: It ignores case distinctions
in strings, among other things.

• = 
is the most efficient way to compare numbers, and the only way
to compare numbers of disparate types, such as 3 and 3.0. It only
accepts numbers.
|#


;; KEYWORD ARGUMENTS


(setf text '(b a n a n a - p a n d a))


(remove 'a text) --> (B N N - P N D)Remove all As.

> (remove 'a text :count 3) --> (B N N - P A N D A)Remove 3 As.

;; Remove also accepts a :FROM-END keyword. If its value is non-NIL,
;; then REMOVE starts from the end of the list instead of from the beginning.
;; So, to remove the last two As in the list, we could write:

> (remove 'a text :count 2 :from-end t) --> (B A N A N A - P N D)


(setf cards '((3 clubs) (5 diamonds) (ace spades)))

(defparameter *cards* '((3 clubs) (5 diamonds) (ace spades)))

> (member '(5 diamonds) cards :test #'equal) --> ((5 DIAMONDS) (ACE SPADES))

(defun member-using-test (x)
  (member x *cards* :test #'equal))

(defun remove-using-test (x)
  (setf *cards* (remove x *cards* :test #'equal)))

;; Other functions that accept a :TEST keyword are UNION,
;; INTERSECTION, SET-DIFFERENCE, ASSOC, RASSOC, SUBST, and
;; SUBLIS. To find out which keywords a function accepts, use the online
;; documentation.

;; FUNCTIONS COVERED IN ADVANCED TOPICS

;; Tree functions: SUBST, SUBLIS.

;; Additional equality functions: EQ, EQL, EQUALP, =.

;; Keyword predicate: KEYWORDP.


;;;DONE 212/212 chapter 6. 
