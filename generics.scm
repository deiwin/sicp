(load "testing.rkt")

(define (get-table op type table)
  (cond ((null? table) #f)
        ((and (equal? op (caar table))
              (equal? type (cadar table)))
         (caddar table))
        (else (get-table op type (cdr table)))))
(define (put-table op type val table)
  (cons (list op type val) table))
(define empty-table '())

(assert "get returns false if no val found for provided operation and type"
        (equal? #f (get-table 'op 'type empty-table)))

(assert "gets the value put into the table"
        (let* ((table (put-table 'op 'type 'val empty-table)))
          (equal? 'val (get-table 'op 'type table))))

(assert "gets the last value that was put into the table"
        (let* ((first-table (put-table 'op 'type 'first-val empty-table))
               (table (put-table 'op 'type 'last-val first-table)))
          (equal? 'last-val (get-table 'op 'type table))))

(define global-table empty-table)
(define (get op type) (get-table op type global-table))
(define (put op type val)
  (set! global-table (put-table op type val global-table)))

(define global-coerion-table empty-table)
(define (get-coercion from-type to-type)
  (get-table from-type to-type global-coerion-table))
(define (put-coercion from-type to-type coercion)
  (set! global-coerion-table
    (put-table from-type to-type coercion global-coerion-table)))

(define (attach-tag type-tag contents)
  (if (or (and (string? contents)
               (equal? 'string type-tag))
          (and (number? contents)
               (equal? 'number type-tag)))
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((string? datum) 'string)
        ((number? datum) 'number)
        (else (error "Bad tagged datum:
                     TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((or (string? datum)
             (number? datum))
         datum)
        (else (error "Bad tagged datum:
                     CONTENTS" datum))))

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

(define (proxy-to-type type op)
  (lambda args
    (apply (get op type)
           args)))
