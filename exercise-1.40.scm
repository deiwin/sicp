(load "lib.scm")

; given
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) (( deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

