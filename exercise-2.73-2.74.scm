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

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
           1
           0))
        (else ((get (operator exp) 'deriv)
               (operands exp)
               var))))

(define (install-deriv-ops)
  ;; internal procedures
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
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
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((and (number? base) (number? exponent))
           (expt base exponent))
          (else (list '** base exponent))))
  (define (deriv+ operands var)
    (let ((addend (car operands))
          (augend (cadr operands)))
      (make-sum (deriv addend var)
                (deriv augend var))))
  (define (deriv* operands var)
    (let ((multiplier (car operands))
          (multiplicand (cadr operands)))
      (make-sum
        (make-product
          multiplier
          (deriv multiplicand var))
        (make-product
          (deriv multiplier var)
          multiplicand))))
  (define (deriv** operands var)
    (let ((base (car operands))
          (exponent (cadr operands)))
      (make-product
        exponent
        (make-product
          (make-exponentiation
            base
            (make-sum exponent -1))
          (deriv base var)))))
  ;; interface to the rest of the system
  (put '+ 'deriv deriv+)
  (put '* 'deriv deriv*)
  (put '** 'deriv deriv**)
  'done)

(assert "differentiates by dispatching on type"
        (begin
          (install-deriv-ops)
          (and
            (equal? '(+ (* x y) (* y (+ x 3)))
                    (deriv '(* (* x y) (+ x 3)) 'x))
            (equal? '(+ (* (* x y) (* 3 (** x 2))) (* y (** x 3)))
                    (deriv '(* (* x y) (** x 3)) 'x)))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((string? datum) 'string)
        (else (error "Bad tagged datum:
                     TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((string? datum) datum)
        (else (error "Bad tagged datum:
                     CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types:
          APPLY-GENERIC"
          (list op type-tags))))))

(define (proxy-to-type type op)
  (lambda args
    (apply (get op type)
           args)))

(define get-record (curry apply-generic 'get-record))
(define get-salary (curry apply-generic 'get-salary))
(define get-name (curry apply-generic 'get-name))

(define create-personell-file-from-names
  (proxy-to-type 'division-a 'create-personell-file-from-names))
(define create-personell-file-from-records
  (proxy-to-type 'division-a 'create-personell-file-from-records))
(define create-record-from-name-salary
  (proxy-to-type 'division-a 'create-record-from-name-salary))

(define (install-division-a)
  ;; internal procedures
  (define (create-personell-file-from-names . names)
    (attach-tag
      'division-a-personell-file
      (map (lambda (name)
             (create-record name 0))
           names)))
  (define (create-personell-file-from-records . records)
    (attach-tag
      'division-a-personell-file
      records))
  (define (get-record name file)
    (findf (lambda (record)
             (equal? name (get-name record)))
           file))

  (define (create-record name salary)
    (attach-tag
      'division-a-record
      (cons name salary)))
  (define (name-record record) (car record))
  (define (salary-record record) (cdr record))
  ;; interface to the rest of the system
  (put 'create-personell-file-from-names 'division-a create-personell-file-from-names)
  (put 'create-personell-file-from-records 'division-a create-personell-file-from-records)
  (put 'create-record-from-name-salary 'division-a create-record)
  (put 'get-name '(division-a-record) name-record)
  (put 'get-salary '(division-a-record) salary-record)
  (put 'get-record '(string division-a-personell-file) get-record))

(install-division-a)
(assert "finds record from division A personell file"
        (let ((personell-file (create-personell-file-from-names "Eva Luator"
                                                                "Alice P. Hacker")))
          (equal? "Eva Luator" (get-name (get-record "Eva Luator" personell-file)))))
(assert "finds salary from division A record"
        (let* ((record1 (create-record-from-name-salary "Eva Luator" 100))
               (record2 (create-record-from-name-salary "Alice P. Hacker" 250))
               (personell-file (create-personell-file-from-records record1 record2)))
          (and (= 100 (get-salary (get-record "Eva Luator" personell-file)))
               (= 250 (get-salary (get-record "Alice P. Hacker" personell-file))))))
