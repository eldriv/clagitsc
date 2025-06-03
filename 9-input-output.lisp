(defpackage :chapter-9
  (:use :cl))

(in-package :chapter-9)

;;; Input/Output


;; 9.1 INTRODUCTION


;; Input/output, or ‘‘i/o,’’ is the way a computer communicates with the world.
;; Lisp’s read-eval-print loop provides a simple kind of i/o, since it reads
;; expressions from the keyboard and prints the results on the display.
;; Sometimes we want to do more.

;; You can make your program print any message you like.
;; Historically, input/output has been one of the areas of greatest
;; disagreement among Lisp systems. Even today there is no standard window
;; system interface, for example, and no standard way to control a mouse or
;; produce graphic designs.

;; 9.2 CHARACTER STRINGS


;; Character strings (strings for short)
;; are a type of sequence; they are similar in some ways to lists, and are a
;; subtype of vectors
;; Strings evaluate to themselves, as numbers do.

"strings are things"

;;9.3 THE FORMAT FUNCTION


;; The FORMAT function normally returns NIL, but as a side effect it causes
;; things to be written on the display or to a file.

;; format nil --> but as a side effect it causes things to be written on the display or to a file.
;; format t   --> when we want to write to the display format control string -->  writes the string, without the quotes, and then returns NIL.

;; Examples:
(format t "Hello mom!") ---> Hello mom! NIL

;; The format control string can also contain special formatting directives,
;; which begin with a tilde, ‘‘~,’’ character. For example, the ~% directive
;; causes FORMAT to move to a new line. Two ~% directives right next to each
;; other result in a blank line in the output.

(format t "Time flies~%like an arrow.")
Result:
Time flies
like an arrow.
NIL

(format t "Fruit flies~%~%like bananas.")
Result:
Fruit flies
vlike bananas.
NIL

(defun mary ()
  (format t "~&Mary had a little bat.")
  (format t "~&Its wings were long and brown.")
  (format t "~&And everywhere that Mary went")
  (format t "~&The bat went, upside-down."))

;; Another important formatting directive is ~S, which inserts the printed
;; representation of a Lisp object into the message that FORMAT prints. (The S
;; stands for ‘‘S-expression,’’ or ‘‘symbolic expression,’’ a somewhat archaic
;; term for a Lisp object.)

(format t "From ~S to ~S in ~S minutes!" ’boston ’(new york) 55)
Result:
From BOSTON to (NEW YORK) in 55 minutes!
NIL

;; Here is another example. The function SQUARE-TALK takes a number
;; as input and tells you the square of that number. It does not return the square;
;; it returns NIL because that is the result returned by FORMAT.

(defun square-talk (n)
  (format t "Square of ~S is ~S" n (* n n)))

(defun sub-talk (x y)
  (format t "Differece of ~S and ~s is ~s" x y (- x y )))

;; ~S includes the quotation marks,
;; ~A directive prints an object without using escape characters.

(defun test (x)
  (format t "~&With escape characters: ~S" x)
  (format t "~&Without escape characters: ~A" x))

;;;EXERCISES

;; 9.1. Write a function to print the following saying on the display: ‘‘There
;; are old pilots, and there are bold pilots, but there are no old bold
;; pilots.’’ Your function should break up the quotation into several lines.

(defun saying ()
  (format t "~&There are old pilots,~%")
  (format t "The are bold pilots,~%")
  (format t "But there no old bold pilots."))

;; 9.2 Write a recursive function DRAW-LINE that draws a line of a specified
;; length by doing (FORMAT T "*") the correct number of times.
;; (DRAW-LINE 10) should produce

(defun draw-line (x)
  (cond ((zerop x) (format t "~%"))
        (t (format t "*")
           (draw-line (- x 1)))))

;; 9.3. Write a recursive function DRAW-BOX that calls DRAW-LINE
;; repeatedly to draw a box of specified dimensions. (DRAW-BOX 10 4)
;; should produce

(defun draw-box (w h)
  (cond ((zerop h) nil)
        (t (draw-line w)
           (draw-box w (- h 1)))))

;;;loop macro

(defun draw-line ()
  (let ((width-number 20))
    (loop :repeat width-number
          :do (format t "*"))
    (format t "~%")))

(defun draw-box ()
  (let ((height-number 10))
    (loop :repeat height-number
          :do (draw-line))))

(defun display-box ()
  (draw-box)
  (format t "This is 10x20 size of box~% "))

;;;using dotimes

(defun draw-line ()
  (let ((width-number 20))
    (dotimes (i width-number)
      (format t "*"))
    (format t "~%")))

(defun draw-box ()
  (let ((height-number 10))
    (dotimes (i height-number)
      (draw-line))))

(defun display-box ()
  (draw-box)
  (format t "This is 10x20 size of box~%"))

;;;using recursion

(defun draw-line ()
  (let ((width-number 20))
    (labels ((draw-line-recur (width)
               (cond ((zerop width) (format t "~%")) 
                     (t (format t "*")               
                        (draw-line-recur (- width 1))))))
      (draw-line-recur width-number))))

(defun draw-box ()
  (let ((height-number 10))
    (labels ((draw-height-recur (height)
               (cond ((zerop height) nil)
                     (t (draw-line)
                        (draw-height-recur (- height 1))))))
      (draw-height-recur height-number))))

(defun display-box ()
  (format t "~a ~&This is 10x20 size of box " (draw-box)))

;; 9.5. Part of any tic-tac-toe playing program is a function to display the
;; board. Write a function PRINT-BOARD that takes a list of nine
;; elements as input. Each element will be an X, an O, or NIL. PRINT-
;; BOARD should display the corresponding board. For example,
;; (PRINT-BOARD ’(X O O NIL X NIL O NIL X)) should print:

(defun print-board (board)
  (labels ((print-row (row)
             (format t "~a | ~a | ~a~%" (or (nth 0 row) " ") (or (nth 1 row) " ") (or (nth 2 row) " "))))
    (print-row (subseq board 0 3))
    (format t "-----------~%")
    (print-row (subseq board 3 6))
    (format t "-----------~%")
    (print-row (subseq board 6 9))))

;;9.4 THE READ FUNCTION


;; READ is a function that reads one Lisp object (a number, symbol, list, or
;; whatever) from the keyboard and returns that object as its value. The object
;; does not have to be quoted because it will not be evaluated. By placing calls
;; to READ inside a function, we can make the computer read data from the
;; keyboard under program control. Here are some examples. User type-in in
;; response to READ is underlined.

(defun my-square ()
  (loop
    (format t "Please type in a number: ")
    (let ((x (read)))
      (if (numberp x)
          (progn
            (format t "The number ~S squared is ~S.~%" x (* x x))
            (return))
          (format t "Input again.~%")))))

(defun my-square ()
  (format t "Please type in a number: ")
  (let ((x (read)))
    (if (not (numberp x))
        (format t "input again")
        (format t "The number ~S squared is ~S.~%" x (* x x)))))

(defun sqrt-advisor ()
  (loop (format t "~&Number: ")
        (let ((n (parse-integer (read-line) :junk-allowed t)))
          (when (not n) (return))
          (format t "~&The square root of ~D is ~D.~%" n (sqrt n)))))


;; 9.5 THE YES-OR-NO-P FUNCTION

;; The YES-OR-NO-P function takes a format control string as input and asks
;; the user a yes or no question. The user must respond by typing ‘‘yes,’’ in
;; which case the function returns T, or ‘‘no,’’ in which case it returns NIL.

(defun riddle ()
  (if (yes-or-no-p ;;There is also a shorter form of this function, called Y-OR-N-
       "Do you seek Zen Enlightenment? ")
      (format t "Then don't")
      (format t "You found it.")))


;;9.6 READING FILES WITH WITH-OPEN-FILE


;; The WITH-OPEN-FILE macro provides a convenient way to read data from a
;; file. Its syntax is:

Template:
(WITH-OPEN-FILE (var pathname)
  body)

;;example
(defun on-new-window (body)
  ;; Load HTML content from file
  (let ((html-file "/home/michael-adrian-villareal/quicklisp/local-projects/cl-journey/content.html")
        (css-file "/home/michael-adrian-villareal/quicklisp/local-projects/cl-journey/styles.css"))
    (with-open-file (html-stream html-file)
      (let ((html-content (make-string (file-length html-stream))))
        (read-sequence html-content html-stream)
        (with-open-file (css-stream css-file)
          (let ((css-content (make-string (file-length css-stream))))
            (read-sequence css-content css-stream)
            ;; Use the panel-box-layout to center horizontally
            ;; and vertically our div on the screen
            (let* ((layout (create-panel-box-layout body)))
              (center-children (center-panel layout))
              (create-div (center-panel layout) :content
                          (format nil "<style>~A</style>~A" css-content html-content)))))))))

;;9.7 WRITING FILES WITH WITH-OPEN-FILE


;; We can also use WITH-OPEN-FILE to open files for output by passing it the
;; special keyword argument :DIRECTION :OUTPUT. The stream that WITH-
;; OPEN-FILE creates can then be used in place of the usual T as a first
;; argument to FORMAT.

;; If we write data to a file using just the ~S directive, we are assured of being
;; able to read it back in again. It is of course possible to write arbitrary
;; messages to a file, containing strange punctuation, unbalanced parentheses, or
;; what have you, but we would not be able to read the file back into Lisp using
;; READ. Such a file might still be useful, though, because it could be read by
;; people. If necessary it could be read by Lisp a character at a time, using
;; techniques not covered here.

(defun save-tree-data (x y z)
  (with-open-file (stream "/home/michael-adrian-villareal/Desktop/notes1.txt"
                          :direction :output)
    (format stream "~S~%" x)
    (format stream "~S" y)
    (format stream " ~S" z)))

;;usage
> (save-tree-data
   "The West Ridge"
   ’((45 redwood) (22 oak) (43 maple))
   110)


> (save-tree-data '1. 'Just 'Example)

;; SUMMARY


;; The FORMAT function takes two or more arguments. The first argument
;; should be T to print on the display; the second must be a format control string.
;; The remaining arguments are used to fill in information required by ~S
;; directives in the format control string. The ~% directive causes FORMAT to
;; begin a new line; the ~& directive begins a new line only if not already at the
;; beginning of a new line.

;; The READ function reads one Lisp object from the terminal and returns
;; that object. The object does not have to be quoted because it will not be
;; evaluated. YES-OR-NO-P and Y-OR-N-P print questions (using a format
;; control string) and then return T or NIL depending on the answer the user
;; gives.

;; WITH-OPEN-FILE opens a file for either input or output, and binds a local
;; variable to a stream object that represents the connection to that file. This
;; stream object can be passed to READ or FORMAT to do file i/o.

;; FUNCTIONS COVERED IN THIS CHAPTER
;; String predicate: STRINGP.
;; Input/output functions: FORMAT, READ, YES-OR-NO-P, Y-OR-N-P.
;; Macro for simple file i/o: WITH-OPEN-FILE.


;;;ADVANCED TOPICS

;;9.8 PARAMETERS TO FORMAT DIRECTIVES


;; Some format directives accept prefix parameters that further specify their
;; behavior. Prefix parameters appear between the ~ and the directive. For
;; example, the ~S directive accepts a width parameter. By using an explicit
;; width, like ~10S, we can produce columnar output.

(defparameter *glee-club* '((john smith) (barbara wilson) (mustapha ali)))

(defun print-one-name (name)
  (format t "~&~10S ~S" (second name)(first name)))

(defun print-all-names (x)
  (mapcar #'print-one-name x)
  'done)

> (print-all-names *glee-club*)

SMITH      JOHN
WILSON     BARBARA
ALI        MUSTAPHA

;;9.9 ADDITIONAL FORMAT DIRECTIVES

;; The ~D directive prints an integer in decimal notation (that is, base 10).
;; The ~F directive prints floating point numbers in a fixed-format notation that always includes a decimal point.

(defun sevenths (x)
  (mapcar #'(lambda (numerator)
              (format t "~%~4,2F / 7 is ~7,4F" numerator (/ numerator 7.0)))
          x)
  'done)

> (sevenths '(1 3/2 2 2.5 3))
1.00 / 7 is 0.14286
1.50 / 7 is 0.21429
2.00 / 7 is 0.28571
2.50 / 7 is 0.35714
3.00 / 7 is 0.42857
DONE

;;9.10 THE LISP 1.5 OUTPUT PRIMITIVES

;; The primitive i/o functions TERPRI, PRIN1, PRINC, and PRINT were
;; defined in Lisp 1.5 (the ancestor of all modern Lisp systems)

;; TERPRI stands for terminate print. It moves the cursor to a new line.
;; PRIN1 and PRINC take a Lisp object as input and print it on the terminal. PRIN1
;; prints the object with whatever escape characters are necessary to assure that it
;; can be read back in with READ; PRINC prints the object without escape
;; characters.

> (setf a "Wherefore art thou, Romeo?")
"Wherefore art thou, Romeo?"
> (prin1 a)
"Wherefore art thou, Romeo?"
"Wherefore art thou, Romeo?"
> (princ a)
Wherefore art thou, Romeo?
"Wherefore art thou, Romeo?"

(defun my-print (x)
  (terpri)
  (prin1 x)
  (princ " A B C")
  x)

(mapcar #’my-print ’(0 1 2 3 4))

RESULT:
0
1
2
3
4
(0 1 2 3 4)

;; TERPRI, PRIN1, and PRINC accept an optional stream argument just like
;; READ; this allows them to be used for file i/o.

;; FUNCTIONS COVERED IN ADVANCED TOPICS
;; Lisp 1.5 output primitives: TERPRI, PRIN1, PRINC, PRINT.

;; 9.11 HANDLING END-OF-FILE CONDITIONS


;; Sometimes it’s necessary to read a file without knowing in advance how many
;; objects it contains. When your program gets to the end of the file, the next
;; READ will generate an end-of-file error, and you’ll end up in the debugger.
;; It is possible to tell READ to return a special value, called an eof indicator,
;; instead of generating an error on end of file.

;; Here is an example of a program that reads an arbitrary file of Lisp objects,
;; tells how many objects were read, and returns a list of them. It uses the cons
;; cell ($EOF$) as its special end-of-file value, but any freshly generated cons
;; cell will do, since only the cell’s address is important, not its contents.

(defun read-my-file ()
  (with-open-file (stream "/home/michael-adrian-villareal/Downloads/Valmiz.txt")
    (let ((contents (read-all-objects stream (list '$eof$))))
      (format t "~&Read ~S objects from the file."
              (length contents))
      contents)))

(defun read-all-objects (stream eof-indicator)
  (let ((result (read stream nil eof-indicator)))
    (if (eq result eof-indicator)
        nil
        (cons result (read-all-objects stream result)))))

;;; Without EOF condition
;; This function opens the file specified (Valmiz.txt in my case) and reads all lines from it using read-all-lines and prints each line to the standard output.

(defun read-my-file ()
  (with-open-file (stream "/home/michael-adrian-villareal/Downloads/Valmiz.txt")
    (let ((contents (read-all-lines stream)))
      (format t "~&Read ~d lines from the file.~%" (length contents)) ;;print the number of lines read from the file 
      (mapc #'(lambda (line) (format t "~A~%" line)) contents)))) ;;maps over each line in contents local variable

;;;another example of reading .txt file 

(defun read-my-file ()
  (with-open-file (stream "/home/michael-adrian-villareal/Downloads/Valmiz.txt")
    (let ((contents (read-all-lines stream)))
      (format t "~&Read ~d lines from the file.~%" (length contents))
      ;; Print all lines in one go using format ~{ directive
      (format t "~{~A~}" contents))))

;;; This helper function reads lines from the provided stream (stream) until the end of file (EOF).
;;; It collects each line into a list (contents) which is returned to read-my-file.

(defun read-all-lines (stream)
  (loop for line = (read-line stream nil) ;; This loop reads a line from stream using read-line.
        until (null line) ;;The loop continues until line is nil, which happens when read-line encounters the end of the stream.
        collect line)) ;; Each non-null line read by read-line is collected into a list.

;; Suppose our sample file contains the following lines:
;; 35 cat (moose
;; meat) 98.6 "Frozen yogurt"
;; 3.14159

> (read-my-file)
Read 6 objects from the file.
(35 CAT (MOOSE MEAT) 98.6 "Frozen yogurt" 3.14159)

;;DONE CHAPTER-9
;;Will proceed next chapter soon. 
