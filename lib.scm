(define (square x) (* x x))

(define (cube x) (* x x x))

(define (average x y)
  (/ (+ x y) 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (inc x) (+ 1 x))
(define (id x) x)

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

