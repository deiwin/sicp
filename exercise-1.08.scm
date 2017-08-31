(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (good-enough? previous-guess guess)
  (< (/ (abs (- previous-guess guess)) guess) 0.001))

(define (cubert-iter previous-guess guess x)
  (if (good-enough? previous-guess guess)
      guess
      (cubert-iter guess (improve guess x) x)))

(define (cubert x)
  (cubert-iter 0.0 1.0 x))
