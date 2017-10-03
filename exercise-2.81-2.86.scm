(load "testing.rkt")
(load "generics.scm")

(define (exp x y)
  (apply-generic 'exp x y))

(put 'exp
     '(scheme-number scheme-number)
     (lambda (x y)
       (attach-tag 'scheme-number (expt x y))))

(assert "fails to call scheme-number exponentiation with complex numbers"
        (equal? 'failed
                (with-handlers ((exn:fail? (lambda (exn) 'failed)))
                               (exp
                                 (attach-tag 'complex-number 1)
                                 (attach-tag 'complex-number 2)))))

(define complex-list (curry apply-generic 'complex-list))
(put 'complex-list
     '(complex-number complex-number complex-number)
     (lambda (x y z) (list x y z)))

(define (coerce-to-complex x)
  (attach-tag 'complex-number (contents x)))
(put-coercion 'integer 'complex-number coerce-to-complex)
(put-coercion 'rational-number 'complex-number coerce-to-complex)

(assert "can coerce functions with more than 2 arguments"
        (equal? (list (attach-tag 'complex-number 1)
                      (attach-tag 'complex-number 2)
                      (attach-tag 'complex-number 3))
                (complex-list (attach-tag 'integer 1)
                              (attach-tag 'rational-number 2)
                              (attach-tag 'complex-number 3))))
