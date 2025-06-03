(defpackage :chapter-13
  (:use :cl))

(in-package :chapter-13)

;;;; Arrays, Hash Tables, And Property Lists

;;; 13.1 Introduction


;; This chapter briefly covers three distinct datatypes: arrays, hash tables, and property lists.
;; Arrays are used very frequently in other programming languages, but not so often in Lisp
;; The reason is that most languages have such an impoverished set of datatypes that arrays must be used for many applications where lists, structures, or hash tables would be preferable.

;; Property lists are the oldest of the three datatypes discussed in this chapter;
;; they were part of the original Lisp dialect, Lisp 1.5. In modern Lisp programming
;; they have largely been replaced by hash tables, but they’re still worth understanding.

;;; 13.2 Creating an Array


;; An Array is contiguous block of storage whose elements are named by numeric subscripts
;; In this book we will consider only one-dimensional  arrays, which are called vectors. (It’s only a minor step from vectors to
;; matrices and higher dimensional arrays; see your reference manual for details.)
;; The components of a vector of length n are numbered zero through n− 1.

;; Let’s create our first vector and store it in the variable MY-VEC:

REPL> (setf my-vec '#(tuning violin 440 a))

REPL> my-vec
#(tuning violin 440 a)

REPL> (car my-vec)
Error: #(tuning violin 440 a) is not a list.

;;; NOTE:
;; Basic list operations such as CAR and CDR do not work on arrays, since arrays are not cons cells.
;; Because storage in arrays is contiguous, we can access each element of an array as fast as any
;; other element. With lists, we have to follow a chain of pointers to get from one cons cell to the
;; next, so depending on the length of the list, it can take much, much longer to access the last
;; element than the first. Efficient access is the prime advantage arrays have over lists. Another
;; advantage is that in most implementations, an array uses only half as much memory as a list of
;; equal length. But lists also have some advantages over arrays. Lists of arbitrary length are easily
;; built up element by element, either recursively or iteratively. It is not as easy to grow an array
;; one element at a time.* Another advantage of lists is that they can share structure in ways that
;; are impossible for arrays, but we won’t get into the details of that in this book

;;; 13.3 Printing Arrays


;; To be able to see the elements of an array, we must set the global variable
;; *PRINT-ARRAY* to T. This assures that vectors will be printed in the same
;; #(thing1 thing2...) notation we use to type them in. If *PRINT-ARRAY* is
;; NIL, vectors and arrays will print in a more concise implementation-dependent
;; form using #< > notation, in which their individual elements are suppressed.

REPL> (setf *print-array* nil)
NIL

REPL> my-vec
#<(SIMPLE-VECTOR 4) {1002617EBF}>

REPL> (setf *print-array* t)
#(TUNING VIOLIN 440 A)

;;; 13.4 ACCESSING AND MODIFYING ARRAY ELEMENTS


;; The vector we stored in MY-VEC has four elements, numbered zero, one, two,
;; and three. The AREF function is used to access the elements of an array by
;; number, just as NTH is used to access the elements of lists.

(aref my-vec 1)

CHAPTER-13> 
(aref my-vec 1)
VIOLIN

CHAPTER-13> 
(aref my-vec 0)
TUNING

CHAPTER-13> 
(aref my-vec 2)
440

CHAPTER-13> 
(aref my-vec 3)
A

(defparameter *my-array* (make-array 4 :initial-contents '(1 2 3 4)))

(defun accessing-element ()
  (aref *my-array* 3))

(defparameter *my-array1* (make-array 5 :initial-contents '(nil nil nil nil nil)))

(defun store-element ()
  (setf (aref *my-array1* 0) 'foo)
  (setf (aref *my-array1* 1) 32)
  (setf (aref *my-array1* 2) 'bar)
  (setf (aref *my-array1* 3) 69)
  (setf (aref *my-array1* 4) 'baz))

;;; 13.5 CREATING ARRAYS WITH MAKE-ARRAY


;; The Lisp function MAKE-ARRAY creates and returns a new array. The
;; length of the array is specified by the first argument. The initial contents of
;; the array are undefined. Some Common Lisp implementations initialize array
;; elements to zero; others use NIL. To be safe, you should not rely on array
;; elements having any particular initial value unless you have specified one
;; explicitly.

;; MAKE-ARRAY accepts several keyword arguments.

;; The :INITIAL-ELEMENT keyword specifies one initial value to use for all the elements of
;; the array.

> (make-array 5 :initial-element 1)
#(1 1 1 1 1)

;; The :INITIAL-CONTENTS keyword specifies a list of values for
;; initializing the respective elements of an array. The list must be exactly as
;; long as the array.

> (make-array 5 :initial-contents ’(a e i o u))
#(A E I O U)

;; If you do not use one of these keywords when calling MAKE-ARRAY, the
;; initial contents of the array will be unpredictable.

;;; 13.6 STRINGS AS VECTORS

;; Strings are actually a special type of vector. Thus, such functions as
;; LENGTH, REVERSE, and AREF, which work on vectors, also work on
;; strings.

(length "Cockatoo") --> 8

(reverse "Cockatoo") --> "ootakcoC"

(aref "Cockatoo" 3) --> #\k

;; The elements of a string are called character objects. For example, #\k
;; denotes the character object known as lowercase ‘‘k.’’ Characters are yet
;; another datatype, distinct from symbols and numbers. Character objects do
;; not need to be quoted because they evaluate to themselves, just as numbers do.

REPL>  (type-of #\k) --> STANDARD-CHAR

(setf pet "Cockatoo")

(setf (aref pet 5) #\p)

REPL > pet
"Cockapoo"

REPL > (setf (aref pet 6) ’cute)
Error: CUTE is not of type CHARACTER.

;;;13.7 HASH TABLES


#|
A hash table offers essentially the same functionality as an association list.
You supply a key, which may be any sort of object, and Lisp gives you back
the item associated with that key. The advantage of hash tables is that they are
implemented using special hashing algorithms that allow Lisp to look things
up much faster than it can look them up in an association list. Hashing is fast
in part because hash tables are implemented using vectors rather than cons cell
chains.

Association lists still have some advantages over hash tables. They are
easier to create and manipulate because they are ordinary list structures. Hash
tables use implementation-dependent representations that are not directly
visible to the user. So if you want utter simplicity, use an association list. If
you’re willing to trade some simplicity for efficiency, use a hash table.

Hash tables cannot be typed in from the keyboard the way vectors can.
They can only be created by the MAKE-HASH-TABLE function. In the
default kind of hash table, EQL is used to compare the keys of items that are
stored. It is also possible to create hash tables that use EQ or EQUAL. Hash
table objects are printed in an implementation-dependent manner that usually
does not show you the elements. The following example is typical:
|#

REPL > (setf h (make-hash-table))

REPL > (type-of h) --> HASH-TABLE


;; The GETHASH function looks up a key in a hash table. The key can be
;; any sort of object. GETHASH is understood as a place specification by SETF,
;; so it can also be used to store into the hash table.

(setf (gethash 'adi h)
      '(attorney (16 maple drive)))


(setf (gethash 'adrian h)
      '(physician (23 cedar court)))


(setf (gethash 'sion h)
      '(ml-gamer (15 maple drive)))


CHAPTER-13> (gethash 'adi h)

(ATTORNEY (16 MAPLE DRIVE))
T

CHAPTER-13> (gethash 'adrian h)

(PHYSICIAN (23 CEDAR COURT))
T

CHAPTER-13> (gethash 'sion h)
(ML-GAMER (15 MAPLE DRIVE))
T

CHAPTER-13> (gethash 'nice h)
NIL
NIL

(setf a (make-hash-table))

(setf (gethash 'adi a)
      '(attorney (16 maple drive)))

CHAPTER-13> (equal (gethash 'adi h) (gethash 'adi a)) -- > T

#|
GETHASH returns two values instead of one. The first value is the item
associated with the key, or NIL if the key was not found in the hash table. The
second value is T if the key was found in the hash table, or NIL if it was not
found. The reason for this second value is to distinguish a key that appears in
the table with an associated item of NIL from a key that does not appear at all.
You can safely ignore the second return value; we will not make use of
multiple return values in this book.

DESCRIBE will tell you useful things about a hash table, such as the
number of buckets it has. A bucket is a group of entries. The more buckets
there are, the fewer entries will be assigned to the same bucket, so retrievals
will be faster. But the price of this speed is an increase in the amount of
memory the hash table uses. INSPECT can be used to look at the entries of a
hash table.
|#

;; REPL > (describe h)
;; #<HASH-TABLE :TEST EQL :COUNT 3 {10020A9953}>
;;   [hash-table]
;; Occupancy: 0.4
;; Rehash-threshold: 1.0
;; Rehash-size: 1.5
;; Size: 7
;; Synchronized: no

;;; 13.8 PROPERTY LISTS


#|
In Lisp, every symbol has a property list. Property lists provide basically the
same facilities as association lists and hash tables: You can store a value in a
property list under a given key (called an indicator), and later look things up
in the property list by supplying the indicator. Property lists are organized as
lists of alternating indicators and values, like this:

Property lists are very old; they were part of the original Lisp 1.5. They
are included here for the sake of completeness; for most applications it is
better to use an association list or hash table. Many Lisp implementations use
the property lists of symbols for their own purposes. For example, if you look
on the property list of CONS or COND you may see some system-specific
information. Users are free to put their own properties on the property list, but
it is a very bad idea to tamper with the properties your Lisp puts there.
|#

REPL> (setf (get 'fred 'sex) 'male)

REPL> (setf (get 'fred 'age) 23)

REPL> (setf (get 'fred 'siblings) '(george wanda))

REPL> (describe 'fred)

[]CHAPTER-13::FRED
[symbol]

Symbol-plist:
SIBLINGS -> (GEORGE WANDA)
AGE -> 23
SEX -> MALE

(get 'fred 'age)
(get 'fred 'sex)
(get 'fred 'siblings)


#|
As you can see, when a symbol does not have the specified property, GET
normally returns NIL. However, GET also accepts a third argument that it
will return instead of NIL if it can’t find the property it was asked to look up.
This is one way to distinguish a symbol having a property FOO with value
NIL from a symbol that does not have a FOO property at all. For example, we
may know that Mabel is an only child (her SIBLINGS property is NIL), but
Clara’s siblings may not be recorded.
|#

(setf (get ’mabel ’siblings) nil)
(get 'mabel 'siblings 'unknown) --> NIL
(get 'clara 'siblings 'unknown) --> UNKNOWN

;; The value of a property can be changed at any time. Suppose FRED has a birthday:
(incf (get ’fred ’age)) --> 24
(get ’fred ’age) --> 24

;; The SYMBOL-PLIST function returns a symbol’s property list.

(symbol-plist 'fred) --> (SIBLINGS (GEORGE WANDA) AGE 25 SEX MALE)

;; We can remove a property entirely using a function called REMPROP.

REPL> (remprop 'fred 'age)

CHAPTER-13> (remprop 'fred 'age)
(AGE 24 SIBLINGS (GEORGE WANDA) SEX MALE)

CHAPTER-13> (symbol-plist 'fred)
(SIBLINGS (GEORGE WANDA) SEX MALE)

CHAPTER-13> (get 'fred 'age)
NIL

;; to get it back the age

CHAPTER-13> (setf (get 'fred 'age) 23)
23

CHAPTER-13> (get 'fred 'age)
23

CHAPTER-13> (symbol-plist 'fred)
(AGE 23 SIBLINGS (GEORGE WANDA) SEX MALE)

CHAPTER-13> (setf (get 'fred 'age) 23)
23

CHAPTER-13> (get 'fred 'age)
23

CHAPTER-13> (incf (get 'fred 'age))
24

;; 13.9 PROGRAMMING WITH PROPERTY LISTS

;; Suppose we are building a database about the characters in a story, and one of
;; the facts we want to record is meetings between the characters. We can store a
;; list of names under the HAS-MET property of each individual. A name
;; should not appear on the list more than once, in other words, the list should be
;; a set. The easiest way to do this is to write a function called ADDPROP to
;; add an element to a set stored under a property name. Here is the definition of
;; ADDPROP:

(defun addprop (sym elem prop)
  (pushnew elem (get sym prop)))

(defun record-meeting (x y)
  (addprop x y 'has-met)
  (addprop y x 'has-met)
  t)

;;;;;experiment
;; REPL> (addproperty 'Alice 'Bob "Met for coffee at 10 AM")
(defun addproperty (person person1 prop)
  (pushnew person1 (get person 'meetings))
  (pushnew person (get person1 'meetings))
  (pushnew prop (get person1 'meeting-details))
  (pushnew prop (get person 'meeting-details))
  t)

(defun get-meetings-person (person)
  (get person 'meetings))

(defun get-meeting-details (person)
  "Retrieve the list of meeting details for PERSON."
  (get person 'meeting-details))


(defun print-all-details (person)
  (format t "~a has met ~a"  person (get-meetings-person person))
  (format t "~%~a" (get-meeting-details person)))

;; FUNCTIONS COVERED IN THIS CHAPTER

;; Array functions: MAKE-ARRAY, AREF.

;; Printer switch: *PRINT-ARRAY*.

;; Hash table functions: MAKE-HASH-TABLE, GETHASH.

;; Property list functions: GET, SYMBOL-PLIST, REMPROP.

;; Lisp systems tend to use a lot of memory. When they run out, they try to get
;; more. There are several ways Lisp might get more memory. First, it can try to
;; reclaim any previously allocated storage that is no longer in use, such as cons
;; cells to which nothing points anymore. This process is called garbage
;; collection. Some Lisps garbage collect continuously, but most have to stop
;; what they’re doing, garbage collect, and then resume. The pause for a garbage
;; collection is usually only a few seconds, but if your Lisp is garbage collecting
;; frequently, these pauses can be annoying.

;; Although all Lisp implementations include a garbage collector, it is not
;; part of the Common Lisp standard, so there is no standard way to modify a
;; garbage collector’s parameters or otherwise interact with it. In many
;; implementations, though, there is a built-in function called GC that causes
;; Lisp to garbage collect immediately. It usually prints some sort of informative
;; message afterwards.

> (gc)
Garbage collection complete.
Approximately 303,008 bytes have been reclaimed.
NIL

;;; Advance topics

;; 13.10 PROPERTY LIST CELLS


;; Recall that a symbol is composed of five pointers. So far we’ve seen three of
;; them: the symbol name, the value cell, and the function cell. The property list
;; cell is another of these components. Every symbol has a property list,
;; although it may be NIL. In contrast, not every symbol has a function
;; definition in its function cell, or a value in its value cell.
;; Suppose we establish a property list for the symbol CAT-IN-HAT. The
;; SYMBOL-PLIST function can be used to access the property list we have
;; created.

REPL> (setf (get 'cat-in-hat 'bowtie) 'red)
REPL> (setf (get 'cat-in-hat 'tail) 'long)

REPL> (symbol-plist 'cat-in-hat) --> (TAIL LONG BOWTIE RED)

;; The structure of the symbol CAT-IN-HAT now looks like this:

---------------
-name       •----> "CAT-IN-HAT"
---------------            
-valuecell    -
---------------             
-function cell- 
---------------          
-plist      •----> [•][•]-->[•][•]-->[•][•]-->[•][•]--> NIL 
---------------   TAIL     LONG    BOWTIE     RED  
-???????      -
---------------

;; SETF understands SYMBOL-PLIST as a place name, so it is possible to
;; give a symbol a new property list using SETF. Replacing the contents of a
;; symbol’s property list cell is dangerous, though, because it could wipe out
;; important properties that Lisp itself had stored on the property list.
;; One reason property lists are today considered archaic is that they are
;; global data structures: A symbol has only one property list, and it is accessible
;; everywhere. If we use hash tables to store our information, we can keep
;; several of them around at the same time, representing different sets of facts.
;; Each hash table is independent, so changes made to one will not affect the
;; others.

;; 13.11 MORE ON SEQUENCES


;; The COERCE function can be used to convert a sequence from one type to
;; another. If we coerce a string to a list, we can see the individual character
;; objects. Conversely, we can use COERCE to turn a list of characters into a
;; string.

REPL> (coerce "Adi" 'list) --> (#\A #\d #\i)
REPL> (coerce '(#\A #\d #\i) 'string) --> "Adi"

(defun translate-list-to-string (lst)
  (coerce lst 'string))


(defun translate-string-to-list (string)
  (coerce string 'list))

(defun translate-list-to-vector (lst)
  (coerce lst 'vector))


;; Yet another way to make a string is to make a vector with MAKE-
;; ARRAY, using the :ELEMENT-TYPE keyword to specify that this vector
;; holds only objects of type STRING-CHAR. (STRING-CHAR is a subtype of
;; CHARACTER.) Vectors of STRING-CHARs are strings.
(defparameter *another-way* (make-array 3 :element-type 'character
                                          :initial-contents '(#\A #\d #\i)))

;;; 'list specifies that the result of the map function should be a list.
(defun how-map-works ()
  (map 'list #'+ '(1 2 3 4 5) '#(10 20 30 40 50)))


(defun how-map-works-1 ()
  (map 'list #'list '(a b c) '#(1 2 3) "x y z"))


;; If MAP is given NIL as a first argument, it returns NIL instead of
;; constructing a sequence from the results of the mapping. This is useful if you
;; want to apply a function to every element of a sequence only for its side effect.
CHAPTER-13> (map nil #'print "a b")

#\a 
#\  
#\b 
NIL
CHAPTER-13> (map 'list #'print "a b")

#\a 
#\  
#\b 
(#\a #\  #\b)

;; FUNCTIONS COVERED IN ADVANCED TOPICS
;; Sequence functions: MAP, COERCE.
