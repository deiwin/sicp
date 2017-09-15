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
              (pretty-format a)
              " does not equal? "
              (pretty-format b))))
      (list 'failed (list message)))))
(define (assert= a b)
  (let ((message (string-append
                   (pretty-format a)
                   " does not = "
                   (pretty-format b))))
    (if (= a b)
      #t
      (list 'failed (list message)))))

; (string-append
;   "Assertion error! "
;   (string-join (cadr (assert-or (assert-equal? '(1) '(2))
;                           (assert= 1 2)))
;                " and "))


(define-syntax assert
  (lambda (stx)
    (define (convert exprsn)
      (cond ((not pair?) exprsn)
            ((equal? (car exprsn) 'equal?) (cons 'assert-equal? (cdr exprsn)))
            ((equal? (car exprsn) '=) (cons 'assert= (cdr exprsn)))
            ((equal? (car exprsn) 'or) (cons 'assert-or (map convert (cdr exprsn))))
            ((equal? (car exprsn) 'and) (cons 'assert-and (map convert (cdr exprsn))))))
    (datum->syntax stx
                   (let ((content (cadr (syntax->datum stx))))
                     (let ((converted-content (convert content)))
                       (list 'let
                             (list (list 'result converted-content))
                             (list 'if
                                   (list 'equal? #t 'result)
                                   #t
                                   (list 'string-append
                                         "Assertion error! "
                                         (list 'string-join
                                               (list 'cadr 'result)
                                               " and ")))))))))

(assert (or (equal? '(1) '(2))
            (= 1 2)))
