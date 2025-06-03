(defpackage :chapter-12
  (:use :cl))

(in-package :chapter-12)

;;; Structures and The Type System

;; Common Lisp includes many built-in datatypes, which together form a type
;; system. The types we’ve covered so far are numbers (of several varieties),
;; symbols, conses, strings, function objects, and stream objects. These are the
;; basic datatypes, but there are quite a few more.

;; The Common Lisp type system has two important properties. First, types
;; are visible: They are described by Lisp data structures (symbols or lists), and
;; there are built-in functions for testing the type of an object and for returning a
;; type description of an object. Second, the type system is extensible:
;; Programmers can create new types at any time.

;; 12.2 TYPEP AND TYPE-OF


;; The TYPEP predicate returns true if an object is of the specified type. Type
;; specifiers may be complex expressions, but we will only deal with simple
;; cases here

(typep 3 ’number) ⇒ t
(typep 3 ’integer) ⇒ t

(typep ’foo ’symbol) ⇒ t
(typep 3 ’float) ⇒ nil

;; The TYPE-OF function returns a type specifier for an object. Since objects
;; can be of more than one type
(type-of ’aardvark)  ⇒ symbol
(type-of 3.5) ⇒ short-float
(type-of ’(bat breath)) ⇒ cons
(type-of "Phooey") ⇒ (simple-string 6)

;;; Why cons instead of symbols


;; Representation in Lisp: In Lisp, symbols are internally represented using a structure called a CONS cell. This cell structure is a fundamental building block that also serves to create lists.

;; Type Representation: The type-of function in Lisp often returns the lowest-level type that encompasses the object. In the case of symbols like + and -, the type-of function returns CONS because the underlying implementation of symbols involves using CONS cells.

;; Historical Reasons: Lisp symbols are not just simple identifiers; they are objects with properties. They are implemented using CONS cells to store additional information such as the symbol's value, function definitions, and metadata.
(type-of -) -> cons
(type-of +) -> cons

;;; 12.3 DEFINING STRUCTURES


;; Structures are programmer-defined Lisp objects with an arbitrary number of
;; named components. Structure types automatically become part of the Lisp
;; type hierarchy. The DEFSTRUCT macro defines new structures and specifies
;; the names and default values of their components. For example, we can define
;; a structure called STARSHIP like this:

(defstruct (starship
            (:name nil)
            (:speed 0)
            (:condition 'green)
            (:shields 'down)))

;; This DEFSTRUCT form defines a new type of object called a STARSHIP
;; whose components are called NAME, SPEED, CONDITION, and SHIELDS.
;; STARSHIP becomes part of the system type hierarchy and can be referenced
;; by such functions as TYPEP and TYPE-OF.

> (setf s1 (make-starship))
#S(STARSHIP NAME NIL
            SPEED 0
            CONDITION GREEN
            SHIELDS DOWN)

> (setf s2 ’#s(starship speed (warp 3)
                        condition red
                        shields up))

;; #S(STARSHIP NAME NIL
;; 	    SPEED (WARP 3)
;; 	    CONDITION RED
;; 	    SHIELDS UP)


;; The #S notation is the standard way to display structures in Common Lisp.
;; The list following the #S contains the type of the structure followed by an
;; alternating sequence of component names and values. Do not be misled by the
;; use of parentheses in #S notation: Structures are not lists. Ordinary list
;; operations like CAR and CDR will not work on structures.
> s1
#s(starship name nil
            speed 0
            condition green
            shields down)

;; Although new instances are usually created by calling the constructor
;; function MAKE-STARSHIP, it is also possible to type in STARSHIP objects
;; directly to the read-eval-print loop, using #S notation. Notice that the
;; structure must be quoted to prevent its evaluation.
> (setf s2 '#s(starship speed (warp 3)
                        condition red
                        shields up))

;; 12.4 TYPE PREDICATES FOR STRUCTURES

;; Another side effect of DEFSTRUCT is that it creates a type predicate for the
;; structure based on the structure name. In this case the predicate is called
;; STARSHIP-P.

(starship-p s2)  -> t
(starship-p 'foo) -> nil


;; Since the type name STARSHIP is fully integrated into the type system, it can be use with TYPEP and will be returned by TYPE-OF

(typep s1 'starship) --> T
(type-of s2) --> STARSHIP

;; 12.5 ACCESSING AND MODIFYING STRUCTURES

;; When a new structure is defined, DEFSTRUCT creates accessor functions for each of it components.
;; For example, it creates a STARSHIP-SPEED accessor for retrieving the SPEED comonponent of a starship

(starship-speed s2) --> (warp 3)
(starship-condition s2) --> red
(starship-shields s2) --> up

;; These accessor functions can also serve as place descriptions to SETF and
;; the other generalized assignment operators.

(setf (starship-name s1) "Adiboomspace")

CHAPTER-12> s1
#S(STARSHIP :NAME "Adiboomspace" :SPEED 0 :CONDITION GREEN :SHIELDS DOWN)

(incf (starship-speed s1))

CHAPTER-12> s1
#S(STARSHIP :NAME "Adiboomspace" :SPEED 1 :CONDITION GREEN :SHIELDS DOWN)

;; Using these accessor functions, we can easily write our own functions to
;; manipulate structures in interesting ways. For example, the ALERT function
;; below causes a starship to raise its shields, and in addition raises the condition
;; level to be at least YELLOW.

(defun alert (x)
  (setf (starship-shields x) 'up)
  (if (equal (starship-condition x) 'green)
      (setf (starship-condition x) 'yellow))
  'shields-raised)

;;;An experienced Lisp programmer would prefer to use a more descriptive
;; name than X for the argument to ALERT. Since ALERT expects its argument
;; to be a starship, why not use that name in the argument list? The result would
;; look like this:

(defun alert (starship)
  (setf (starship-shields starship) 'up)
  (if (equal (starship-condition starship) 'green)
      (setf (starship-condition starship) 'yellow))
  'shields-raised)

;; 12.6 KEYWORD ARGUMENTS TO CONSTRUCTOR FUNCTIONS

;; When a new structure instance is created, we aren’t required to use the default
;; values for the components. We can specify different values by supplying them
;; as keyword arguments in the call to the constructor. (See Advanced Topics
;; section 6.14 for an explanation of keywords and keyword arguments.) Here’s
;; an example using the MAKE-STARSHIP constructor:

(setf s3 (make-starship :name "Reliant"
                        :shields 'damaged))


;; 12.7 CHANGING STRUCTURE DEFINITIONS
(defstruct starship-1
  (captain nil)
  (name nil)
  (shields 'down)
  (condition 'green)
  (speed 0))

;; SUMMARY


;; Common Lisp contains many built-in datatypes; only the basic ones are
;; discussed in this book. The Common Lisp type system is both visible and
;; extensible. Users can extend the type system by defining new structure types.
;; DEFSTRUCT defines structure types. The structure definition includes the
;; names of all the components, and optionally specifies default values for them.
;; If no default is given for a component, NIL is used. DEFSTRUCT also
;; automatically defines a constructor function for the type (such as MAKE-
;; STARSHIP) and a type predicate (such as STARSHIP-P).

;; FUNCTIONS COVERED IN THIS CHAPTER
;; Structure-defining macro: DEFSTRUCT.
;; Type system functions: TYPEP and TYPE-OF.

;;;  DESCRIBE is particularly useful for displaying structures. In most
;; implementations of Common Lisp, DESCRIBE shows the fields of the
;; structure in a more readable format than the #S notation Lisp uses by default.

;; CHAPTER-12> (describe s2)
;; #S(STARSHIP :NAME NIL :SPEED (WARP 3) :CONDITION RED :SHIELDS UP)
;;   [structure-object]

;; Slots with :INSTANCE allocation:
;;   NAME                           = NIL
;;   SPEED                          = (WARP 3)
;;   CONDITION                      = RED
;;   SHIELDS                        = UP
;; ; No value
;; CHAPTER-12> (describe s1)
;; #S(STARSHIP :NAME "Adiboomspace" :SPEED 1 :CONDITION YELLOW :SHIELDS UP)
;;   [structure-object]

;; Slots with :INSTANCE allocation:
;;   NAME                           = "Adiboomspace"
;;   SPEED                          = 1
;;   CONDITION                      = YELLOW
;;   SHIELDS                        = UP
;; ; No value
;; CHAPTER-12> (describe s3)
;; #S(STARSHIP-1..
;;   [structure-object]

;; Slots with :INSTANCE allocation:
;;   CAPTAIN                        = "Benson"
;;   NAME                           = "Reliant"
;;   SHIELDS                        = DAMAGED
;;   CONDITION                      = GREEN
;;   SPEED                          = 0


;; ;;(inspect s3)
;; ;;;INSPECT may let you
;; ;; inspect the components of an object by pointing to them with the mouse
;; The object is a STRUCTURE-OBJECT of type STARSHIP-1.
;; 0. CAPTAIN: "Benson"
;; 1. NAME: "Reliant"
;; 2. SHIELDS: DAMAGED
;; 3. CONDITION: GREEN
;; 4. SPEED: 0

;;;Types (Experimentations)

;; values have type, variables do not have types, because variables are objects
;; In simpler terms, the type system in Common Lisp helps programmers specify what kind of data they expect to work with, which can prevent errors and make programs clearer and safer.

(defvar *test-variable*)

(setf *test-variable* 1234)
(setf *test-variable* "Hello, World!")


(defun add-two-numbers (a b)
  (check-type a number)
  (check-type b number)
  (format t "Result: ~a" (+ a b)))

(add-two-numbers 5 "this is string")
;; ERROR: The value of B is "this is string", which is not of type NUMBER.
;;    [Condition of type SIMPLE-TYPE-ERROR]

(defun should-be-string-number (a b)
  (check-type a number)
  (check-type b string)
  (format t "Result: ~a and a ~d " b (+ a 5)))

(add-two-numbers 5 5)
;; The value of B is 5, which is not of type STRING.
;;    [Condition of type SIMPLE-TYPE-ERROR]

(subtypep 'compiled-function 'function)
>
T
T
(subtypep 'integer 'rational)
>T
T
(subtypep 'integer 'string) =>  false, true

;; 12 - Advanced Topics


;; It is often convenient to invent specialized notations for printing structures.
;; For example, we may not want to see all the fields of a starship object
;; whenever it is printed; we may be satisfied to just see the name. The
;; convention for printing abbreviated structure descriptions in Common Lisp is
;; to make up a notation beginning with ‘‘#<’’ and ending with ‘‘>’’ that
;; includes the type of the structure plus whatever identifying information is
;; desired. For example, we might choose to print starships this way:

#<STARSHIP Enterprise>

;; The first step in customizing the way starship objects print is to write our
;; own print function. It must take three inputs: the object being printed, the
;; stream on which to print it, and a number (called depth) that Common Lisp
;; uses to limit the depth of nesting when printing complex structures. We will
;; ignore the depth argument in this book, but our function must still accept three
;; arguments to work correctly. Here it is:

(defstruct (starship-1
            (:print-function print-starship))
  (:captain nil)
  (:name 'Reliant)
  (:shields 'down)
  (:condition 'green)
  (:speed 0))


(defun print-starship (starship stream depth)
  (format stream "#<STARSHIP ~a>"
          (starship-1-name starship)))

>s1
#<STARSHIP Reliant>

>(starship-1-shields s1)
DOWN

>(starship-1-name s1)
"Reliant"

>(format t "~&This is ~S leaving orbit." s1)
;; This is #<STARSHIP Reliant> leaving orbit.
;; NIL

;; 12.9 EQUALITY OF STRUCTURES


;; The EQUAL function does not treat two distinct structures as equal even if
;; they have the same components. For example:

(defstruct starship
  (captain nil)
  (name nil)
  (shields 'down)
  (condition 'green)
  (speed 0))

(setf s2 (make-starship))
(setf s3 (make-starship))

(equal s2 s3) --> NIL
(equal s3 s3) --> T

;; However, the EQUALP function will treat two structures as equal if they
;; are of the same type and all their components are equal.

> (equalp s5 s6)   --- > T

> (equalp s5 ’#s(starship name nil
                          speed 0
                          condition green
                          shields down))
----> T


;; EQUALP also differs from EQUAL in ignoring case distinctions when
;; comparing characters.

(equal "enterprise" "Enterprise")⇒nil
(equalp "enterprise" "Enterprise")⇒t

;; 12.10 INHERITANCE FROM OTHER STRUCTURES


;; Structure types can be organized into a hierarchy using the :INCLUDE option
;; to DEFSTRUCT. For example, we could define a structure type SHIP whose
;; components are NAME, CAPTAIN, and CREW-SIZE. Then we could define
;; STARSHIP as a type of SHIP with additional components WEAPONS and
;; SHIELDS, and SUPPLY-SHIP as a type of SHIP with an additional
;; component called CARGO.

(defstruct ship
  (name nil)
  (captain nil)
  (crew-size nil))

(defstruct (starship-3 (:include ship))
  (weapons nil)
  (shields nil))

(defstruct (supply-ship (:include ship))
  (cargo nil))

;; The fields of a STARSHIP structure include all the components of SHIP.
;; Thus, when we make a starship, its first three components will be NAME,
;; CAPTAIN, and CREW-SIZE. The same holds for supply ships.

;; CL-USER> s3
;; #S(STARSHIP :CAPTAIN NIL :NAME NIL :SHIELDS DOWN :CONDITION GREEN :SPEED 0)
;; CL-USER> s5
;; #S(SUPPLY-SHIP :NAME NIL :CAPTAIN "Rommel Martinez" :CREW-SIZE NIL :CARGO NIL)
;; CL-USER> s4
;; #S(STARSHIP-3
;;    :NAME NIL
;;    :CAPTAIN "Adrian Villareal"
;;    :CREW-SIZE NIL
;;    :WEAPONS NIL
;;    :SHIELDS NIL)
;; (setf s4 (make-starship-3 :captain "Adrian Villareal"))
;; (setf s5 (make-supply-ship :captain "Rommel Martinez"))
;; CL-USER> (ship-p s4)
;; T
;; CL-USER> (ship-p s5)
;; T
;; CL-USER> (starship-p s4)
;; NIL
;; CL-USER> (starship-p s5)
;; NIL
;; CL-USER> (starship-p s3)
;; T
;; CL-USER> (ship-captain s4)
;; "Adrian Villareal"
;; CL-USER> (ship-captain s5)
;; "Rommel Martinez"
