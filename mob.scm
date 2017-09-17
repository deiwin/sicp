(load "testing.rkt")

; Exercise 2.17: Define a procedure last-pair that returns the list that
; contains only the last element of a given (nonempty) list

(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))

(and (equal? (last-pair (list 1 2 3 4)) (cons 4 '()))
     (equal? (last-pair (list 5 6 7 8)) (cons 8 '()))
     (equal? (last-pair (list 23 72 149 34)) (list 34)))

; Exercise 2.18: Define a procedure reverse that takes a list as argument and
; returns a list of the same elements in reverse order

(define (reverse l)
  (define (iter acc rest)
    (if (null? rest)
      acc
      (iter (cons (car rest) acc) (cdr rest))))
  (iter '() l))

(equal? (reverse (list 1 2 3 4)) (list 4 3 2 1))

; Use this notation to write a procedure same-parity that takes one or more
; integers and returns a list of all the arguments that have the same even-odd
; parity as the first argument.

(define (filter f l)
  (cond ((null? l) l)
        ((f (car l)) (cons (car l) (filter f (cdr l))))
        (else (filter f (cdr l)))))

(define (same-parity first . rest)
  (cons first
        (filter (lambda (x) (= (modulo x 2) (modulo first 2)))
                rest)))

(and (equal? (same-parity 1 2 3 4 5 6 7) '(1 3 5 7))
     (equal? (same-parity 2 3 4 5 6 7) '(2 4 6)))

; Exercise 2.21: The procedure square-list takes a list of numbers as argument
; and returns a list of the squares of those numbers.

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

(equal? (square-list (list 1 2 3 4)) '(1 4 9 16))

; Exercise 1.3: Define a procedure that takes three numbers as arguments and
; returns the sum of the squares of the two larger numbers.
(define (sum-two-largest-squares a b c)
  (- (+ (square a) (square b) (square c))
     (square (min a b c))))

(and (equal? (sum-two-largest-squares 1 2 3) 13)
     (equal? (sum-two-largest-squares 1 1 -5) 2))

; Exercise 1.7: Design a square-root procedure that uses this kind of end test.
; Does this work better for small and large numbers?

(define (sqrt-iter guess x step)
  (if (good-enough? guess (improve guess x))
      (values guess step)
      (sqrt-iter (improve guess x) x (+ step 1))))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess next-guess)
  (< (abs (- (/ guess next-guess) 1)) 0.000001))
(define (sqrt x)
  (sqrt-iter 1.0 x 0))

; Exercise 1.8: Newton’s method for cube roots is based on the fact that if y
; is an approximation to the cube root of x , then a better approximation is
; given by the value x / y 2 + 2 y 3 .  Use this formula to implement a
; cube-root procedure analogous to the square-root procedure. (In 1.3.4 we will
; see how to implement Newton’s method in general as an abstraction of these
; square-root and cube-root procedures.)

(define (cbrt x)
  (define (cbrt-iter guess x step)
    (if (good-enough? guess (improve-by-newton guess x))
        (values guess step)
      (cbrt-iter (improve-by-newton guess x) x (+ step 1))))
  (define (improve-by-newton guess x)
    (/ (+ (* guess 2) (/ x (square guess))) 3))
  (define (good-enough? guess next-guess)
    (< (abs (- (/ guess next-guess) 1)) 0.000001))
  (cbrt-iter 1.0 x 0))

; Surprise Exercise: How many different ways can we make change of $1.00,
; given half-dollars, quarters, dimes, nickels, and pennies? More
; generally, can we write a procedure to compute the number of ways
; to change any given amount of money?
(define (coin-change-for amount coins)
  (cond ((= amount 0) 1)
    ((< amount 0) 0)
    ((null? coins) 0)
    (else
     (+
      (coin-change-for amount (cdr coins))
      (coin-change-for (- amount (car coins)) coins)))
    )
  )

(define eesti-coins '(1 2 5 10 20 50 100 200))
(define us-coins '(1 5 10 25 50))
(define bad-coinz '(5 8 14))
(and (= (coin-change-for 0 eesti-coins) 1)
     (= (coin-change-for 3 eesti-coins) 2)
     (= (coin-change-for 12 bad-coinz) 0))

; Exercise 1.11: A function f is defined by the rule that f ( n ) = n if n < 3
; and f ( n ) = f ( n − 1 ) + 2 f ( n − 2 ) + 3 f ( n − 3 ) if n ≥ 3 . Write a
; procedure that computes f by means of a recursive process. Write a procedure
; that computes f by means of an iterative process.

(define (f-rec n)
  (if (< n 3)
    n
    (+ (f-rec (- n 1))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (iter step a b c)
    (if (= step n)
      a
      (iter (+ step 1)
            b
            c
            (+ (* 3 a)
               (* 2 b)
               c))))
  (iter 0 0 1 2))

(and (= (f-rec 1) 1)
     (= (f-rec 2) 2)
     (= (f-rec 3) 4)
     (= (f-rec 5) 25)
     (= (f-iter 1) 1)
     (= (f-iter 2) 2)
     (= (f-iter 3) 4)
     (= (f-iter 5) 25))

; Exercise 1.12 Write a procedure that computes elements of
; Pascal’s triangle by means of a recursive process.
(define (pascal n)
  (define (new-row prev old-row)
    (if (null? old-row)
        '(1)
        (cons
          (+ prev (car old-row))
          (new-row (car old-row) (cdr old-row)))))
  (if (= n 0)
      '(1)
      (new-row 0 (pascal (- n 1)))))

(assert "generates rows of the pascal diagram"
        (and (equal? (pascal 0) '(1))
             (equal? (pascal 1) '(1 1))
             (equal? (pascal 2) '(1 2 1))
             (equal? (pascal 3) '(1 3 3 1))))
