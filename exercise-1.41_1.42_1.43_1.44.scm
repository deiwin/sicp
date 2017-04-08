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
  (if (= n 1)
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
