(load "lib.scm")

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? previous-guess guess)
  (< (/ (abs (- previous-guess guess)) guess) 0.001))

(define (sqrt-iter previous-guess guess x)
  (if (good-enough? previous-guess guess)
      guess
      (sqrt-iter guess (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 0.0 1.0 x))
