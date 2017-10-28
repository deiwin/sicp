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

(put-coercion 'integer 'complex-number
              (curry attach-tag 'complex-number))
(put-coercion 'rational-number 'complex-number
              (curry attach-tag 'complex-number))

(assert "can coerce functions with more than 2 arguments"
        (equal? (list (attach-tag 'complex-number 1)
                      (attach-tag 'complex-number 2)
                      (attach-tag 'complex-number 3))
                (complex-list (attach-tag 'integer 1)
                              (attach-tag 'rational-number 2)
                              (attach-tag 'complex-number 3))))

(define (raise x)
  (apply-generic 'raise x))
(put 'raise '(integer)
     (lambda (x) (attach-tag 'rational-number x)))
(put 'raise '(rational-number)
     (lambda (x) (attach-tag 'real-number x)))
(put 'raise '(real-number)
     (lambda (x) (attach-tag 'complex-number x)))

(assert "raises numbers to higher level in type tower"
        (and (equal? (attach-tag 'rational-number 1)
                     (raise (attach-tag 'integer 1)))
             (equal? (attach-tag 'real-number 2)
                     (raise (attach-tag 'rational-number 2)))
             (equal? (attach-tag 'complex-number 3)
                     (raise (attach-tag 'real-number 3)))))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (fail (lambda () (error
                            "No method for these types"
                            (list op type-tags)))))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (letrec ((can-coerce-args-to
                   (lambda (to-type)
                     (andmap (lambda (from-type)
                               (or (equal? from-type to-type)
                                   (get-coercion from-type to-type)))
                             type-tags)))
                 (coerce-args-to
                   (lambda (to-type)
                     (map (lambda (arg)
                            (let ((from-type (type-tag arg)))
                              (if (equal? to-type from-type)
                                arg
                                ((get-coercion from-type to-type) (contents arg)))))
                          args)))
                 (all-args-of-type
                   (lambda (ref-type)
                     (andmap (lambda (type)
                               (equal? ref-type type))
                             type-tags)))
                 (coerce-iter
                   (lambda (types-to-try)
                     (if (null? types-to-try)
                       (fail)
                       (let* ((type-to-try (car types-to-try))
                              (rest (cdr types-to-try))
                              (proc-for-type (get op (map (const type-to-try) type-tags))))
                         (if (and (can-coerce-args-to type-to-try)
                                  (not (all-args-of-type type-to-try))
                                  proc-for-type)
                           (apply proc-for-type
                                  (coerce-args-to type-to-try))
                           (coerce-iter rest)))))))
          (coerce-iter type-tags))))))
