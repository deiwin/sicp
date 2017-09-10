(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (flatten-terms is-instance? terms)
  (foldr (lambda (term l)
           (if (is-instance? term)
             (append (flatten-terms is-instance? (cdr term))
                     l)
             (cons term l)))
         '()
         terms))

; Reduces all numerical values in the provided list of terms using the provided
; proc and initial value. Returns the list of terms with the reduced numerical
; value being the first element in the list.
(define (reduce-numbers proc init terms)
  (define make-acc cons)
  (define numerical-value car)
  (define other-terms cdr)
  (let ((acc (foldr (lambda (term acc)
                      (if (number? term)
                        (make-acc (proc (numerical-value acc) term) (other-terms acc))
                        (make-acc (numerical-value acc) (cons term (other-terms acc)))))
                    (make-acc init '())
                    terms)))
    (cons (numerical-value acc) (other-terms acc))))

(define (sum? x)
  (and (pair? x)
       (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))
(define (make-sum a1 a2 . rest)
  (let ((terms (reduce-numbers + 0
                               (flatten-terms sum?
                                              (cons a1 (cons a2 rest))))))
    (let ((simplified-terms (if (and (= (car terms) 0)
                                     (not (null? (cdr terms))))
                              (cdr terms)
                              terms)))
      (if (null? (cdr simplified-terms))
        (car simplified-terms)
        (add-between simplified-terms '+)))))

(define (product? x)
  (and (pair? x)
       (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))
(define (make-product m1 m2 . rest)
  (let ((terms (reduce-numbers * 1
                               (flatten-terms product?
                                              (cons m1 (cons m2 rest))))))
    (let ((simplified-terms (if (and (= (car terms) 1)
                                     (not (null? (cdr terms))))
                              (cdr terms)
                              terms)))
      (cond ((=number? (car simplified-terms) 0) 0)
            ((null? (cdr simplified-terms)) (car simplified-terms))
            (else (add-between simplified-terms '*))))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent))
         (expt base exponent))
        (else (list base '** exponent))))
(define (exponentiation? exp)
  (and (pair? exp)
       (eq? (cadr exp) '**)))
(define base car)
(define exponent caddr)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product
             (multiplier exp)
             (deriv (multiplicand exp) var))
           (make-product
             (deriv (multiplier exp) var)
             (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (exponent exp)
           (make-product
             (make-exponentiation
               (base exp)
               (make-sum (exponent exp) -1))
             (deriv (base exp) var))))
        (else (error "unknown expression
                     type: DERIV" exp))))

(deriv '(x + (3 * (x + (y + 2)))) 'x)
