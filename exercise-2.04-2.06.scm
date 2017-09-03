(load "lib.scm")

; 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

; 2.5
(define (cons x y)
  (*
    (expt 2 x)
    (expt 3 y)))

(define (car z)
  (define (twos z)
    (if (= (remainder z 3) 0)
      (twos (/ z 3))
      z))
  (log-with-base 2 (twos z)))

(define (cdr z)
  (define (threes z)
    (if (= (remainder z 2) 0)
      (threes (/ z 2))
      z))
  (log-with-base 3 (threes z)))

; 2.6
(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define three
  (lambda (f) (lambda (x) (f (f (f x))))))

(define five
  (lambda (f) (lambda (x) (f (f (f (f (f x))))))))

(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define (eval n) ((n inc) 0))
