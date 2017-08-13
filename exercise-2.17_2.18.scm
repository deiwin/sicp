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
