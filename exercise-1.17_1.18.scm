(define (halve n) (/ n 2))
(define (dbl n) (+ n n)) ; "double" seems to be reserved

(define (fast-mul a b)
  (cond ((= b 1) a)
        ((even? b) (fast-mul (dbl a) (halve b)))
        (else (+ a (fast-mul a (- b 1)))))
  )

(define (iter-fast-mul a b)
  (define (iter-fast-mul-iter x a b)
    (cond ((= b 0) x)
          ((even? b) (iter-fast-mul-iter x (dbl a) (halve b)))
          (else (iter-fast-mul-iter (+ x a) a (- b 1))))
    )
  (iter-fast-mul-iter 0 a b))
