(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1 (intersection-set
                        (cdr set1)
                        (cdr set2))))
            ((< x1 x2) (intersection-set
                         (cdr set1)
                         set2))
            ((< x2 x1) (intersection-set
                         set1
                         (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (rest1 (cdr set1))
                    (x2 (car set2))
                    (rest2 (cdr set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set rest1 rest2)))
                      ((< x1 x2)
                       (cons x1 (union-set rest1 set2)))
                      (else
                        (cons x2 (union-set set1 rest2))))))))

(and (equal? (adjoin-set 3 '(1 2 3))
             '(1 2 3))
     (equal? (intersection-set '(1 2 3) '(2 3 4))
             '(2 3))
     (equal? (union-set '(1 2 3) '(2 3 4))
             '(1 2 3 4)))
