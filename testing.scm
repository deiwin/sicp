; (assert (and (equal? '(1) '(2))
;              (= 1 1)))

(define (combine-failure a b)
  (list 'failed (append (cadr a)
                        (cadr b))))
(define (failed? x)
  (and (pair? x)
       (equal? (car x) 'failed)))

(define (assert-and . args)
  (if (null? args)
    #t
    (let ((head (car args))
          (rest (apply assert-and (cdr args))))
      (cond ((failed? head) (if (failed? rest)
                              (combine-failure head rest)
                              head))
            (head rest)
            (else
              (error "macro wrong, got #f instead of (list 'failed (list))"))))))

(define (assert-or . args)
  (define (or-combine head rest)
    (cond ((not rest) head)
          ((failed? rest) (combine-failure head rest))
          (else #t)))
  (if (null? args)
    #f
    (let ((head (car args))
          (rest (apply assert-or (cdr args))))
      (cond ((failed? head) (or-combine head rest))
            (head #t)
            (else
              (error "macro wrong, got #f instead of (list 'failed (list))"))))))

(define (assert-equal? a b)
  (if (equal? a b)
    #t
    (let ((message
            (string-append
              "Expected "
              (pretty-format a)
              " to equal? "
              (pretty-format b))))
      (list 'failed (list message)))))
(define (assert= a b)
  (let ((message (string-append
                   "Expected "
                   (pretty-format a)
                   " to = "
                   (pretty-format b))))
    (if (= a b)
      #t
      (list 'failed (list message)))))

(assert-or (assert-equal? '(1) '(2))
           (assert= 1 2))
