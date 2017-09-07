; Exercise 2.17: Define a procedure last-pair that returns the list that
; contains only the last element of a given (nonempty) list

(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))

(and (equal? (last-pair (list 1 2 3 4)) (cons 4 '()))
     (equal? (last-pair (list 5 6 7 8)) (cons 8 '())))
