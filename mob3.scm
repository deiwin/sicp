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

(define coercion-table empty-table)
(define (get-coercion op type) (get-table op type coercion-table))
(define (put-coercion op type val)
  (set! coercion-table (put-table op type val coercion-table)))

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

(define (map-find fn values)
  (foldl (lambda (value acc)
           (if (not (equal? #f acc))
             acc
             (fn value)))
         #f
         values))

(define (always n) (lambda (i) n))
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags))
         (get-coerced-proc
           (lambda (to-type)
             (let ((coerced-types (map (always to-type) type-tags)))
               (get op coerced-types))))
         (make-generic-call (lambda (coerced-proc coercers)
                              (let* ((original-values (map contents args))
                                     (coerced-values (map
                                                       (lambda (coercer value)
                                                         (coercer value))
                                                       coercers
                                                       original-values)))
                                (lambda () (apply coerced-proc coerced-values)))))
         (find-coercers (lambda (to-type)
                          (let* ((coercers (map
                                             (lambda (from-type)
                                               (if (equal? from-type to-type)
                                                 identity
                                                 (get-coercion from-type to-type)))
                                             type-tags))
                                 (can-coerce (not (memq #f coercers))))
                            (if can-coerce
                              coercers
                              #f)))))

    (if proc
      (apply proc (map contents args))
      (let ((call-with-coercion
              (map-find (lambda (to-type)
                          (let* ((coercers (find-coercers to-type))
                                 (coerced-proc (get-coerced-proc to-type)))
                            (if (and coercers coerced-proc)
                              (make-generic-call coerced-proc coercers)
                              #f)))
                        type-tags)))
        (if call-with-coercion
          (call-with-coercion)
          (error
            "No method for these types"
            (list op type-tags)))))))


(define (attach-tag type-tag contents)
  (if (or (eq? type-tag 'scheme-number)
          (eq? type-tag 'real))
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((integer? datum) 'scheme-number)
        ((real? datum) 'real)
        (else (error "Bad tagged datum:
                     TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((or (integer? datum)
             (real? datum))
         datum)
        (else (error "Bad tagged datum:
                     CONTENTS" datum))))

(define (get-record name file)
  (apply-generic 'get-record (attach-tag 'string name) file))
(define (get-salary record)
  (apply-generic 'get-salary record))

(define (make-division-a-file records)
  ((get 'make-division-a-file 'division-a-file) records))

(define (make-division-a-record name salary address)
  ((get 'make-division-a-record 'division-a-record) name salary address))

(define (find-employee-record name files)
  (if (null? files)
      #f
      (let ((record (get-record name (car files))))
        (if (equal? record #f)
            (find-employee-record name (cdr files))
            record))))

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
             (let* ((ostap (make-division-a-record "Ostap" 1337 "Tartu"))
                    (tartu (make-division-a-file (list ostap))))
               (and (equal? ostap (get-record "Ostap" tartu))
                    (equal? 1337 (get-salary ostap))
                    (equal? #f (find-employee-record "Ostap" '()))
                    (equal? #f (find-employee-record "Vlad" (list tartu)))
                    (equal? ostap (find-employee-record "Ostap" (list tartu)))))))

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else
            (error "Unknown op:
                   MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(assert (and (= 1 ((make-from-mag-ang 1 0) 'real-part))
             (= 0 ((make-from-mag-ang 1 0) 'imag-part))))

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (square x) (* x x))
(define real-part (curry apply-generic 'real-part))
(define imag-part (curry apply-generic 'imag-part))
(define magnitude (curry apply-generic 'magnitude))
(define angle (curry apply-generic 'angle))

(define (install-complex-package)
  ;; imported procedures from rectangular
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular)
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar)
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
      (+ (real-part z1) (real-part z2))
      (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
      (- (real-part z1) (real-part z2))
      (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
      (* (magnitude z1) (magnitude z2))
      (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
      (/ (magnitude z1) (magnitude z2))
      (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (x y) (and (equ? (magnitude x) (magnitude y))
                          (equ? (angle x) (angle y)))))
  (put '=zero? '(complex)
       (lambda (x) (equ? (magnitude x) 0)))

  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'exp
     '(real real)
     (lambda (x y)
       (tag (expt x y))))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (equ? (numer x) (numer y))
                          (equ? (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (equ? (numer x) 0)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'exp
     '(scheme-number scheme-number)
     (lambda (x y)
       (tag (expt x y))))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define add (curry apply-generic 'add))
(define sub (curry apply-generic 'sub))
(define mul (curry apply-generic 'mul))
(define div (curry apply-generic 'div))
(define exp (curry apply-generic 'exp))

(install-scheme-number-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
(install-polar-package)
(install-rectangular-package)

(assert (= 4 (add (make-scheme-number 2) 2)))

(define equ? (curry apply-generic 'equ?))

(assert (and (equ? 2 2)
             (equ? (make-rational 1 2) (make-rational 2 4))
             (equ? (make-complex-from-mag-ang 1 0) (make-complex-from-real-imag 1 0))))

(define =zero? (curry apply-generic '=zero?))
(assert (and (=zero? 0)
             (and (=zero? (make-rational 0 1))
                  (=zero? (make-rational 0 100)))
             (and (=zero? (make-complex-from-real-imag 0 0))
                  (=zero? (make-complex-from-mag-ang 0 1)))))

(define (scheme-number->complex n)
  (make-complex-from-real-imag
   (contents n) 0))

(put-coercion 'scheme-number 'complex
              scheme-number->complex)

(put-coercion 'complex 'complex identity)

(assert "coercion" (equ? 1 (make-complex-from-mag-ang 1 0)))
(assert "coercion to self"
        (equal? 'failed
                (with-handlers ((exn:fail? (lambda (exn) 'failed)))
                               (exp (make-complex-from-real-imag 1 1)
                                    (make-complex-from-real-imag 1 1)))))
