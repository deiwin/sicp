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
