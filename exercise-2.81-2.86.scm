(load "testing.rkt")
(load "generics.scm")

(define (create-integer x)
  (attach-tag 'integer x))
(define (create-rational-number num denom)
  (attach-tag 'rational-number (list num denom)))
(define (create-complex-number real imaginary)
  (attach-tag 'complex-number (list real imaginary)))

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
     (lambda (x) (create-rational-number (create-integer x) (create-integer 1))))
(put 'raise '(rational-number)
     (lambda (x) (create-complex-number (attach-tag 'rational-number x) (raise (create-integer 0)))))

(assert "raises numbers to higher level in type tower"
        (and (equal? (create-rational-number (create-integer 2) (create-integer 1))
                     (raise (create-integer 2)))
             (equal? (create-complex-number (create-rational-number (create-integer 3) (create-integer 1))
                                            (create-rational-number (create-integer 0) (create-integer 1)))
                     (raise (create-rational-number (create-integer 3) (create-integer 1))))))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (fail (lambda () (error
                            "No method for these types"
                            (list op type-tags)))))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (letrec ((try-raising
                   (lambda (arg to-type)
                     (if (equal? (type-tag arg) to-type)
                       arg
                       (let ((raise (get 'raise (list (type-tag arg)))))
                         (if raise
                           (try-raising (raise (contents arg)) to-type)
                           arg)))))
                 (all-of-type
                   (lambda (args ref-type)
                     (andmap (lambda (arg)
                               (equal? ref-type (type-tag arg)))
                             args)))
                 (coerce-iter
                   (lambda (types-to-try)
                     (if (null? types-to-try)
                       (fail)
                       (let* ((type-to-try (car types-to-try))
                              (rest (cdr types-to-try))
                              (proc-for-type (get op (map (const type-to-try) type-tags))))
                         (if (and (not (all-of-type args type-to-try))
                                  proc-for-type)
                           (let ((raised-args
                                   (map (lambda (arg) (try-raising arg type-to-try)) args)))
                             (if (all-of-type raised-args type-to-try)
                               (apply proc-for-type
                                    (coerce-args-to type-to-try))
                               (coerce-iter rest)))
                           (coerce-iter rest)))))))
          (coerce-iter type-tags))))))

(assert "raises all arguments to the highest type"
        (equal? (list (attach-tag 'complex-number 1)
                      (attach-tag 'complex-number 2)
                      (attach-tag 'complex-number 3))
                (complex-list (attach-tag 'integer 1)
                              (attach-tag 'rational-number 2)
                              (attach-tag 'complex-number 3))))

(define (project x)
  (apply-generic 'project x))
(put 'project '(rational-number)
     (lambda (x) (create-integer (round (/ (contents (car x))
                                           (contents (cadr x)))))))
(put 'project '(complex-number)
     (lambda (x) (car x)))

(assert "can project to lower types"
        (and (equal? (create-integer 1)
                     (project (create-rational-number (create-integer 1) (create-integer 1))))
             (equal? (create-integer 2)
                     (project (create-rational-number (create-integer 9) (create-integer 4))))
             (equal? (create-rational-number (create-integer 1) (create-integer 2))
                     (project (create-complex-number (create-rational-number (create-integer 1) (create-integer 2))
                                                     (create-rational-number (create-integer 3) (create-integer 4)))))))
