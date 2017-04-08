(define (square x) (* x x))

(define (cube x) (* x x x))

(define (average x y)
  (/ (+ x y) 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (inc x) (+ 1 x))
(define (id x) x)
(define (always n) (lambda (i) n))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess n)
    (printf "guess ~a: ~a\n" n guess)
    (let (( next (f guess)))
      (if (close-enough? guess next)
        next
        (try next (+ n 1)))))
  (try first-guess 1))
