#lang eopl

#|-------------------------------------------------------------------------------
 | Name: Anthony Curcio-Petraccoro 
 | Pledge: I pledge my honor that I have abided by the Stevens Honor System. 
 |-------------------------------------------------------------------------------|#

#| Some reminders:
 |   Each progamming assignment must be completed individually.
 |   If your code does not compile, you will receive a 0.
 |   Before submitting your code, try test cases on every function
 |     to make sure that they run.
 |   If one of your functions is breaking the whole program due to an error,
 |     you are better off commenting out the body of that function
 |     than submitting code that doesn't run.
 |   Do not modify or delete any function headers.
 |#


#|-------------------------------------------------------------------------------|
 |                          Lab 2: Recursion (30 PTS)                            |
 |-------------------------------------------------------------------------------|#

#| This lab serves as an introduction to recursive programming in Racket.
 |
 | Some reminders:
 |   There is no partial credit on each individual function.
 |     If a function passes all test cases, you get full credit for that function.
 |     If it doesn't pass all test cases, you get no credit for that function.
 |   The test cases provided to you may not be comprehensive,
 |     and will rarely match the cases we use to grade you.
 |   Try every function before submitting your code. Even if your code compiles,
 |     a function may break when you run it.
 |   Unless otherwise specified, the output of your function must match
 |     the test cases precisely to be correct. Extra parentheses are incorrect,
 |     values out of order are incorrect, etc.
 |#

#|-------------------------------------------------------------------------------|
 |                       Part 1: Nested Lists (10 PTS)                           |
 |-------------------------------------------------------------------------------|#


#| Implement "rotate" to accept an xy-coordinate c
 |   and return a list of the four rotations of c around the origin.
 | These rotations should be in the order 0°, 90° clockwise, 180°, 270° clockwise.
 | As a refresher, the 90° clockwise rotation of
 |   an arbitrary coordinate (x,y) is (y,-x).
 | Consider using subdefinitions to avoid repeatedly accessing the components of c.
 |
 | Examples:
 |   (rotate '(0 1))  -> ((0 1) (1 0) (0 -1) (-1 0))
 |   (rotate '(2 3))  -> ((2 3) (3 -2) (-2 -3) (-3 2))
 |   (rotate '(-5 7)) -> ((-5 7) (7 5) (5 -7) (-7 -5))
 |#

;; Type signature: (rotate (int int)) -> ((int int) (int int) (int int) (int int))
;; 2 PTS
(define (rotate c)
  (list (list (car c) (car (cdr c))) (list (car (cdr c)) (* (car c) -1)) (list (* (car c) -1) (* (car (cdr c)) -1)) (list (* (car (cdr c)) -1) (car c)))
)

#| For the remaining functions in this lab, we'll be working with
 |   an informally defined data structure called a "student".
 | A student is represented by a nested list with the following structure:
 |#

(define student
  '((id-number degree)
    (last-name first-name)
    (birth-day birth-month birth-year)
    (class-year ((major) (minor)) gpa)
    ((number street apt) (city state zip))
    (course1 course1 ... coursen)))


;; Here are two example students for testing your functions:

(define stu1
  '((12345 "Bachelor of Science")
    ("Dongle" "Jonathy")
    (29 "February" 1999)
    (2021 (("Computer Science") ("Math")) 3.75)
    ((5 "Bubble Street" 16) ("Hoboken" "NJ" "07030"))
    ("CS-334" "CS-385" "MA-331" "BT-353")))

(define stu2
  '((10101010101 "Bachelor of Science")
    ("Sprimpling" "Sir Ardlinton")
    (1 "December" 1852)
    (1874 (("Engineering") ("Literature")) 4.000001)
    ((999 "Road Street" 11) ("Old Town Place" "MA" "00001"))
    ("MA-121" "CAL-103" "PE-200" "CH-115" "E-101")))




;; Here's an example function which returns the entire birthday of a student:

(define (get-birthday student)
  (car (cdr (cdr student))))

#| Since the birthday is the third element in the student list, we can access it
 |   by dropping the first two elements (by using cdr twice),
 |   then using car to get the first remaining element.
 |
 | Racket has extra functions for shorthands of nesting car and cdr,
 |   so the body of get-birthday could equivalently be written as (caddr student).
 | A shorthand function exists for every permutation of up to 4 car's and cdr's. Here they all are:
 |    https://docs.racket-lang.org/reference/pairs.html#%28part._.Pair_.Accessor_.Shorthands%29
 |#




;; Now implement the following functions for accessing parts of the student datatype:


#| "get-id-number" returns the id-number field.
 | Examples:
 |   (get-id-number stu1) -> 12345
 |   (get-id-number stu2) -> 10101010101
 |#

;; Type signature: (get-id-number student) -> id-number
;; 1 PTS
(define (get-id-number s)
  (car (car s))
)


#| "get-address" returns the fields which make up the address.
 | Examples:
 |   (get-address stu1) -> ((5 "Bubble Street" 16) ("Hoboken" "NJ" "07030"))
 |   (get-address stu2) -> ((999 "Road Street" 11) ("Old Town Place" "MA" "00001"))
 |#

;; Type signature: (get-address student) -> ((number street apt) (city state zip))
;; 1 PTS
(define (get-address s)
  (car (cdr ( cdr (cdr (cdr s)))))
)


#| "get-gpa" returns the gpa field.
 | Examples:
 |   (get-gpa stu1) -> 3.75
 |   (get-gpa stu2) -> 4.000001
 |#

;; Type signature: (get-gpa student) -> gpa
;; 1 PTS
(define (get-gpa s)
  (car (cdr (cdr (car (cdr (cdr (cdr s)))))))
)


#| "get-state" returns the state field.
 | Consider taking advantage of another function you already wrote
 |   for implementing this one!
 | Examples:
 |   (get-state stu1) -> "NJ"
 |   (get-state stu2) -> "MA"
 |#

;; Type signature: (get-state student) -> state
;; 2 PTS
(define (get-state s)
  (car (cdr (car (cdr (car (cdr (cdr (cdr (cdr s)))))))))
)


#| Now implement "combine courses", which accepts two students
 |   and returns a list of the first student's courses
 |   followed by the second student's courses.
 | Consider cleaning this function up with a subdefinition
 |   or external helper function.
 | Examples:
 |   (combine-courses stu1 stu2)
 |     -> ("CS-334" "CS-385" "MA-331" "BT-353" "MA-121" "CAL-103" "PE-200" "CH-115" "E-101")
 |#

;; Type signature: (combine-courses student student) -> string-list
;; 3 PTS
(define (combine-courses s1 s2)
  (append (car (cdr (cdr (cdr (cdr (cdr s1)))))) (car (cdr (cdr (cdr (cdr (cdr s2))))))) 
)


#|-------------------------------------------------------------------------------|
 |                        Part 2: Conditionals (2 PTS)                           |
 |-------------------------------------------------------------------------------|#

#| Like in other languages, the main way to implement a conditional branch
 |   is with an "if-then-else" statement.
 | In Racket this is done with the function (if condition then else),
 |   where "condition" is an expression which evaluates to a boolean.
 | For example, (if (= 1 2) (+ 3 4) (+ 5 6)) will evaluate to 11.
 |
 | "if" functions can be nested to emulate the "else if/elif" of other languages.
 |
 | You may want to look into Racket's "cond" function for some parts of this assignment.
 | cond allows for multiple conditions in a row, effectively condensing
 |   a bunch of nested if statements into one statement.
 |#




#| Implement "piecewise" to accept an integer x
 |   and return 2*x^2 + 1 if x is positive,
 |   and x + 1 otherwise.
 | Examples:
 |   (piecewise -5) -> -4
 |   (piecewise -2) -> -1
 |   (piecewise 0)  -> 1
 |   (piecewise 3)  -> 19
 |   (piecewise 6)  -> 73
 |#

;; Type signature: (piecewise int) -> int
;; 1 PTS
(define (piecewise x)
  (if (> x 0) (+ (* 2 (* x x)) 1) (+ x 1))
)


#| Implement "sign" to accept an integer x
 |   and return "positive" if x is positive,
 |   "negative" if x is negative,
 |   and "zero" if x is zero.
 | Examples:
 |   (sign 5)  -> "positive"
 |   (sign 0)  -> "zero"
 |   (sign -2) -> "negative"
 |#

;; Type signature: (sign int) -> string
;; 1 PTS
(define (sign x)
  (if (= x 0) "zero" (if (< x 0) "negative" "positive")) 
)




#|-------------------------------------------------------------------------------|
 |               Part 3: Arithmetic Recursion (9 PTS) (+2 EC PTS)                |
 |-------------------------------------------------------------------------------|#

#| Here, we'll implement some arithmetic functions with recursion.
 | For each function, consider the base case of the recursion
 |   and use an "if" or "cond" statement to catch the base case.
 | In Racket, to call a function recursively you can simply call the function
 |   from inside of itself.
 | Below is an example function which summates the whole numbers up to n:
 |#

;; Type signature: (example-sum int) -> int
(define (example-sum n)
  (if (<= n 0)
      0
      (+ n (example-sum (- n 1)))))

#| If you try to run a recursive function when the base case or recursive call
 |   isn't properly implemented, it may run indefinitely until it runs out of memory.
 | This can freeze up DrRacket, which is annoying but has no permanent consequence.
 | Consider using DrRacket's debug mode if you want to slowly step
 |   through a function to prevent it from crashing.
 |#

;; Run at your own risk! (Nothing actually bad will happen; DrRacket will just seize up)
;; Type signature: (example-bad int) -> an unhappy IDE
(define (example-bad n)
  (if (<= n 0)
      0
      (+ n (example-bad (+ n 1)))))


#| Implement "sum-pow" to accept positive integers p and n
 |   and return X, where X equals the sum of integers to the power p from 1 to n.
 | For example:
 |   If p = 2 and n = 5, then X = 5^2 + 4^2 + 3^2 + 2^2 + 1^2 = 55.
 |   If p = 4 and n = 3, then X = 3^4 + 2^4 + 1^4 = 98.
 | Recommended: use Racket's "expt" function for exponentiation.
 |
 | Examples:
 |   (sum-pow 1 6)  -> 21
 |   (sum-pow 2 15) -> 1240
 |   (sum-pow 5 8)  -> 61776
 |   (sum-pow 10 7) -> 353815700
 |#

;; Type signature: (sum-pow positive-int positive-int) -> int
;; 3 PTS
(define (sum-pow p n)
  (if (<= n 0) 0 (+ (expt n p) (sum-pow p (- n 1))))
)

#| Implement "subfact" to accept a positive integer n
 |   and return the subfactorial of n (denoted !n).
 | Whereas n! equals the number of permutations of n items,
 |   !n equals the number of derangements of n items.
 | A derangement is a permutation of items where no item is in its initial position.
 | For example, given 5 items in the order [1,2,3,4,5],
 |   [2,4,5,1,3] is a derangement but [5;2;3;1;4] is not.
 |
 | The recursive function S, where S(n) = !n, is defined as follows:
 | S(n) =
 |   0,                if n = 1.
 |   1,                if n = 2.
 |   n * S(n - 1) + 1, if n > 2 and n is even.
 |   n * S(n - 1) - 1, if n > 2 and n is odd.
 |
 | Examples:
 |   (subfact 1)  -> 0
 |   (subfact 3)  -> 2
 |   (subfact 7)  -> 1854
 |   (subfact 15) -> 481066515734
 |
 | Try inputting some larger numbers to see one of the advantages of Racket:
 |   it can compute HUGE numbers really fast!
 |#

;; Type signature: (subfact positive-int) -> int
;; 3 PTS
(define (subfact n)
  (if (= n 1) 0 (if (= n 2) 1 (if (= (remainder n 2) 0) (+ (* n (subfact(- n 1))) 1) (- (* n (subfact(- n 1))) 1))))
)


#| Implement "gen-fib", which will compute a generalized version of
 |   the Fibonacci sequence where the two starting values
 |   of the sequence are parameters.
 | The familiar Fibonacci recursive function F is usually defined as follows:
 |   F(n) = 0,                   if n = 0.
 |   F(n) = 1,                   if n = 1.
 |   F(n) = F(n - 1) + F(n - 2), if n > 1.
 | We will generalize the sequence with the following recursive function, G:
 |   G(a,b,n) = a,                           if n = 0.
 |   G(a,b,n) = b,                           if n = 1.
 |   G(a,b,n) = G(a,b,n - 1) + G(a,b,n - 2), if n > 1.
 |
 | (gen-fib a b n) should return G(a,b,n).
 |
 | There are a few different ways to implement gen-fib.
 | One of which is to directly implement the recursive function described above,
 |   which you may do, however it will run slowly for large inputs.
 |
 | If you implement gen-fib such that it returns the correct answer for small inputs,
 |   you will receive the full 3 points for this function.
 | If you implement gen-fib such that it quickly returns the correct answer for small and large inputs,
 |   you will receive an additional 2 points of extra credit on this assignment.
 |
 | Examples for 3 points:
 |   (gen-fib 5 6 0)   -> 5
 |   (gen-fib 5 6 1)   -> 6
 |   (gen-fib 5 6 2)   -> 11
 |   (gen-fib 0 1 8)   -> 21
 |   (gen-fib 0 1 15)  -> 610
 |   (gen-fib 2 1 10)  -> 123
 |   (gen-fib 8 -13 5) -> -41
 |   (gen-fib 0 0 7) -> 0
 |
 | Additional examples for +2 points (each should compute in a second or less):
 |   (gen-fib 0 1 39)    -> 63245986
 |   (gen-fib 2 1 52)    -> 73681302247
 |   (gen-fib 5 6 70)    -> 1730700096559780
 |   (gen-fib 15 -15 80) -> -134165914856871960
 |#

;; Type signature: (gen-fib int int nonnegative-int) -> int
;; 3 PTS (+2 potential PTS)
(define (gen-fib a b n)
  (if (= n 0) a (if (= n 1) b (+ (gen-fib a b (- n 1)) (gen-fib a b (- n 2))))) 
)




#|-------------------------------------------------------------------------------|
 |                        Part 4: List Recursion (9 PTS)                         |
 |-------------------------------------------------------------------------------|#

#| Recursion is an essential tool for working with lists in Racket.
 | Here, we'll implement some fundamental recursive functions which operate over lists.
 | Instead of a numerical base case, some of these functions
 |   will have a base case concerning the list, such as the list being empty.
 | For example, below is a recursive function which accepts a nonempty list
 |   and returns the list's last element.
 |#

;; Type signature: (example-list nonempty-list) -> element from list
(define (example-last L)
  (if (null? (cdr L))
      (car L)
      (example-last (cdr L))))




#| Implement "nth" to accept a list L and integer n
 |   and return the element of L at index n.
 | The first element of L is at index 0, the second is at index 1, etc.
 | You may assume that L is not empty, and that 0 ≤ n < length of L. 
 |
 | Examples:
 |   (nth 3 '(Sandeep Franklin Isabella Jared Suzy Toby)) -> Jared
 |   (nth 5 '("zero" "one" "two" "three" "four" "five")) -> "five"
 |   (nth 1 '((a b) (c d) (e f))) -> (c d)
 |#

;; Type signature: (nth int list) -> element from list
;; 2 PTS
(define (nth n L)
  (if (> n 0) (nth (- n 1) (cdr L)) (car L))
)


#| Implement "product" to accept a list of integers
 |   and returns the result of multiplying said numbers together.
 | Mathematical convention is that the "empty product" - the product of no numbers - is 1.
 | The empty list is a valid input into the function.
 |
 | Examples:
 |   (product '())                     -> 1
 |   (product '(1 3 5 4))              -> 60
 |   (product '(100 -50 6789 4183457)) -> -142007447865000
 |   (product '(6 1 7 245 0 983 -143)) -> 0
 |#

;; Type signature: (product int-list) -> int
;; 3 PTS
(define (product lst)
  (if (null? lst) 1 (* (car lst) (product (cdr lst))))
)


#| Implement "filter" to accept a predicate P (a function which returns a boolean) and a list L,
 |   and return a list of all elements in L which satisfy P.
 | In other words, "filter" keeps each element e in L for which P(e) returns true,
 |   and throws out each element e for which P(e) returns false.
 | The resultant list must maintain the original order of the elements in L.
 | You may assume that every element of L is a valid input for P.
 | Even though you can't know what specific function P will be,
 |   you can still invoke it the same as any other function with (P e).
 |
 | Examples (some use lambda functions, check 'em out!):
 |   (filter zero? '(1 0 2 34 56 1 0))
 |     -> (0 0)
 |   (filter even? '(0 1 2 3 4 5 6 7 8 9))
 |     -> (0 2 4 6 8)
 |   (filter number? '(shave and 1 haircut 2 bits))
 |     -> (1 2)
 |   (filter (lambda (s) (= 3 (string-length s))) '("hello" "string" "one" "a" "two" "a b"))
 |     -> ("one" "two" "a b")
 |   (filter (lambda (pair) (car pair)) '((#t 1) (#t 2) (#f 3) (#f 4) (#t 5)))
 |     -> ((#t 1) (#t 2) (#t 5))
 |#

;; Type signature: (filter predicate list) -> list
;; 4 PTS
(define (filter P L)
  (cond ((null? L) L) ((P (car L)) (append (list (car L)) (filter P (cdr L)))) (else (filter P (cdr L)))))
