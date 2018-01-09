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


(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((same-variable? a1 a2)
         (make-product 2 a2))
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 1) base)
        ((=number? exponent 0) 1)
        (else (list '** base exponent))))

(put 'deriv '+ (lambda (operands var)
                 (make-sum (deriv (car operands) var)
                           (deriv (cadr operands) var))))

(put 'deriv '* (lambda (operands var)
                 (make-sum
                   (make-product
                    (car operands)
                    (deriv (cadr operands) var))
                   (make-product
                    (deriv (car operands) var)
                    (cadr operands)))))

(put 'deriv '** (lambda (operands var)
                  (make-product
                    (make-product
                      (cadr operands)
                      (make-exponentiation
                        (car operands)
                        (make-sum (cadr operands) -1)))
                    (deriv (car operands) var))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
           1
           0))
        (else ((get 'deriv (operator exp))
               (operands exp)
               var))))

(assert (and (equal? '(+ (* x y) (* y (+ x 3))) (deriv '(* (* x y) (+ x 3)) 'x))
             (equal? '(* 2 x) (deriv '(** x 2) 'x))
             (equal? 2 (deriv (deriv '(** x 2) 'x) 'x))
             (equal? 1 (deriv '(** x 1) 'x))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types:
             APPLY-GENERIC"
            (list op type-tags))))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum:
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum:
              CONTENTS" datum)))

(define (get-record name file)
  (apply-generic 'get-record (attach-tag 'string name) file))
(define (get-salary record)
  (apply-generic 'get-salary record))

(define (make-division-a-file records)
  ((get 'make-division-a-file 'division-a-file) records))

(define (make-division-a-record name salary address)
  ((get 'make-division-a-record 'division-a-record) name salary address))

(define (install-division-a)
  (define (make-file records) records)
  (define (make-record name salary address) (list name salary address))
  (define record-name car)
  (define record-salary cadr)
  (define (get-record name file)
    (cond ((null? file) #f)
          ((string=? (record-name (car file)) name) (car file))
          (else (get-record name (cdr file)))))

  (put 'get-salary '(division-a-record) record-salary)
  (put 'get-record '(string division-a-file)
       (lambda (name file)
         (let ((result (get-record name file)))
           (if (equal? #f result)
             #f
             (attach-tag 'division-a-record result)))))
  (put 'make-division-a-file 'division-a-file
       (lambda (records)
         (attach-tag 'division-a-file (make-file (map contents records)))))
  (put 'make-division-a-record 'division-a-record
       (lambda (name salary address)
         (attach-tag 'division-a-record (make-record name salary address))))

  'done)

(install-division-a)
(assert (and (equal? #f (get-record "Ostap" (make-division-a-file '())))
             (let ((ostap (make-division-a-record "Ostap" 1337 "Tartu")))
               (and (equal? ostap (get-record "Ostap" (make-division-a-file (list ostap))))
                    (equal? 1337 (get-salary ostap))))))
