(define (square x) (* x x))

(define (cube x) (* x x x))

(define (log-with-base a x)
  (/ (log x) (log a)))

(define (average x y)
  (/ (+ x y) 2))
(define (average3 x y z)
  (/ (+ x y z) 3))

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
      (cond ((close-enough? guess next) next)
            ((>= n 1000)
             (printf "likely does not converge")
             next)
            (else (try next (+ n 1)))
        )))
  (try first-guess 1))

(define dx 0.00001)

(define (damp f)
  (lambda (x)
    (/ (+ x (f x)) 2)))

(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

(define (prime? x)
  (define (iter n)
    (cond ((= n 1) #t)
          ((= (remainder x n) 0) #f)
          (else (iter (- n 1)))))
  (iter (integer-sqrt x)))

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low
          (enumerate-interval
            (+ low 1)
            high))))

(define (repeat value n)
  (map (always value)
       (enumerate-interval 1 n)))
