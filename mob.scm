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
      (coin-change-for (- amount (car coins)) coins)))))
; N = amount
; M = nr of coins
; O(2^(N+M))

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

; Exercise 1.15: The sine of an angle (specified in radians) can be computed by making use of the approximation sin ⁡ x ≈ x if x is sufficiently small, and the trigonometric identity
; sin ⁡ x = 3 sin ⁡ x 3 − 4 sin 3 ⁡ x 3
; to reduce the size of the argument of sin. (For purposes of this exercise an angle is considered “sufficiently small” if its magnitude is not greater than 0.1 radians.) These ideas are incorporated in the following procedures:


(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(sine 1000)
; O(log(a))
; O(log(a))

(log 10000 3)

; Exercise 1.16: Design a procedure that evolves an iterative exponentiation
; process that uses successive squaring and uses a logarithmic number of steps,
; as does fast-expt. (Hint: Using the observation that ( b n / 2 ) 2 = ( b 2 ) n / 2 ,)
; keep, along with the exponent n and the base b , an additional state variable a ,
; and define the state transformation in such a way that the product a b n is unchanged
; from state to state. At the beginning of the process a is taken to be 1, and the
; answer is given by the value of a at the end of the process. In general, the technique
; of defining an invariant quantity that remains unchanged from state to state is a powerful
; way to think about the design of iterative algorithms.

(define (fast-expt b n)
  (cond ((= n 0)
         1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))
  (iter b n 1))

(assert "Is same as fast-expt"
  (and (= (fast-expt 2 2) (fast-expt-iter 2 2))
       (= (fast-expt 3 3) (fast-expt-iter 3 3))
       (= (fast-expt 2 7) (fast-expt-iter 2 7))))

; Exercise 1.17: The exponentiation algorithms in this section are
; based on performing exponentiation by means of repeated multiplication. In a
; similar way, one can perform integer multiplication by means of repeated
; addition. The following multiplication procedure (in which it is assumed that)
; our language can only add, not multiply is analogous to the expt procedure:

;(define (* a b)
;  (if (= b 0)
;      0
;      (+ a (* a (- b 1)))))

; This algorithm takes a number of steps that is linear in b. Now suppose we
; include, together with addition, operations double, which doubles an integer,
; and halve, which divides an (even) integer by 2. Using these, design a
; multiplication procedure analogous to fast-expt that uses a logarithmic number
; of steps.

(define (double a) (+ a a))

(define (fast-* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-* a (/ b 2))))
        (else
          (+ a (fast-* a (- b 1))))))

(assert "It multiplies"
  (and (= (fast-* 2 2) 4)
       (= (fast-* 5 6) 30)))

;Exercise 1.18: Using the results of Exercise 1.16 and Exercise 1.17, devise a
;procedure that generates an iterative process for multiplying two integers in
;terms of adding, doubling, and halving and uses a logarithmic number of steps.

(define (fast-*-iter a b)
  (define (iter a b acc)
    (cond ((= b 0) acc)
          ((even? b) (iter (double a) (/ b 2) acc))
          (else (iter a (- b 1) (+ acc a)))))

  (iter a b 0))

(assert "Is same as fast-*"
  (and (= (fast-* 2 2) (fast-*-iter 2 2))
       (= (fast-* 3 3) (fast-*-iter 3 3))
       (= (fast-* 2 7) (fast-*-iter 2 7))))


;  Exercise 1.19: There is a clever algorithm for computing the Fibonacci numbers)
;in a logarithmic number of steps. Recall the transformation of the state
;variables a and b in the fib-iter process of 1.2.2: a ← a + b and b ← a . Call
;this transformation T , and observe that applying T over and over again n times,
;starting with 1 and 0, produces the pair Fib ( n + 1 ) and Fib ( n ) . In other
;words, the Fibonacci numbers are produced by applying T n , the n th power of
;the transformation T , starting with the pair (1, 0). Now consider T to be the
;special case of p = 0 and q = 1 in a family of transformations T p q , where T p
;q transforms the pair ( a , b ) according to a ← b q + a q + a p and b ← b p + a
;q . Show that if we apply such a transformation T p q twice, the effect is the
;same as using a single transformation T p ′ q ′ of the same form, and compute p ′
;and q ′ in terms of p and q . This gives us an explicit way to square these
;transformations, and thus we can compute T n using successive squaring, as in the fast-expt procedure. Put this all together to complete the following procedure, which runs in a logarithmic number of steps:41

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0)
         b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)) ; p'
                   (+ (* 2 p q) (square q)) ; q'
                   (/ count 2)))
        (else
         (fib-iter (+ (* b q)
                      (* a q)
                      (* a p))
                   (+ (* b p)
                      (* a q))
                   p
                   q
                   (- count 1)))))

(assert (and
         (= (fib 0) 0)
         (= (fib 1) 1)
         (= (fib 2) 1)
         (= (fib 3) 2)
         (= (fib 6) 8)
         (= (fib 1200000) (+ (fib 1199999) (fib 1199998)))))
; 227 steps

(define (prime? x)
  (define (iter n)
    (cond ((= n 1) #t)
          ((= (remainder x n) 0) #f)
          (else (iter (- n 1)))))
  (iter (integer-sqrt x)))
