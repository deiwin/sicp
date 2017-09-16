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

(define-syntax assert
  (lambda (stx)
    (define (convert exprsn)
      (cond ((not (pair? exprsn)) exprsn)
            ((equal? (car exprsn) 'equal?) (cons 'assert-equal? (cdr exprsn)))
            ((equal? (car exprsn) '=) (cons 'assert= (cdr exprsn)))
            ((equal? (car exprsn) 'or) (cons 'assert-or (map convert (cdr exprsn))))
            ((equal? (car exprsn) 'and) (cons 'assert-and (map convert (cdr exprsn))))
            (else (error "unexpected expression" exprsn))))
    (define (color-red exprsn)
      `(string-append
         "\033[31m"
         (string-join (string-split ,exprsn "\n")
                      "\033[0m\n\033[31m")
         "\033[0m"))
    (datum->syntax stx
                   (let ((exprsn-to-assert (cadr (syntax->datum stx))))
                     `(let ((result ,(convert exprsn-to-assert)))
                        (if (equal? #t result)
                          #t
                          (error ,(color-red '(string-append
                                                "Assertion error!\n  * "
                                                (string-join (cadr result)
                                                             "\n  * "))))))))))

(assert (or (equal? '(1) '(2))
            (= 1 2)))
