(load "lib.scm")

; 1.41
(define (double f)
  (lambda (x)
    (f (f x))))

; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

; 1.43
(define (repeated f n)
  ; Have the guarding check be (< n 2) instead of (= n 1), so that repeating
  ; e.g. 2.9 times would be the same as repeating 2 times. Without anything
  ; breaking.
  (if (< n 2)
    f
    (compose f (repeated f (- n 1)))))

; 1.44
(define (smooth f)
  (lambda (x)
    (average3 (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-fold-smooth n)
  (repeated smooth n))

; example usage
(((n-fold-smooth 9) square) 3); 9.000000000599998

; 1.45
(define (nth-root-fixed-point-f n x)
  (lambda (y)
    (/ x (expt y (- n 1)))))
(define (nth-root-with-damping root-n damp-count x)
  (fixed-point ((repeated damp damp-count) (nth-root-fixed-point-f root-n x)) 1.0))

; nth-root-with-damping was used to determine (with safeguards in the
; fixed-point function) that it needs to dampened about floor(log2(n)) times.

(define (nth-root root-n x)
  (nth-root-with-damping root-n (log-with-base 2 root-n) x))
