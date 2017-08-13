(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))

(define (reverse l)
  (define (reverse-iter result remaining)
    (if (null? remaining)
      result
      (reverse-iter (cons (car remaining) result) (cdr remaining))))
  (reverse-iter '() l))

(define (filter f l)
  (cond ((null? l) l)
        ((f (car l)) (cons (car l) (filter f (cdr l))))
        (else (filter f (cdr l)))))

(define (same-parity x . rest)
  (cons x (filter (lambda (y) (= (remainder x 2) (remainder y 2))) rest)))
