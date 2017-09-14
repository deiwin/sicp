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
