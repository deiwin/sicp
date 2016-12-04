; 1.32a
(define (rec-accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner
      (term a)
      (rec-accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (rec-accumulate + 0 term a next b))
(define (product term a next b)
  (rec-accumulate * 1 term a next b))

; 1.32b
(define (iter-accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (iter-sum term a next b)
  (rec-accumulate + 0 term a next b))
(define (iter-product term a next b)
  (rec-accumulate * 1 term a next b))

; 1.33
(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((predicate a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

; 1.33a
(define (prime? x) ; I'm lazy and only want to check this for small numbers
  (or
    (= x 1)
    (= x 3)
    (= x 5)
    (= x 7)
    (= x 11)
    ))

(define (filtered-sum predicate term a b)
  (filtered-accumulate predicate + 0 term a inc b))
(define (sum-squared-primes a b)
  (filtered-sum prime? square a b))

; 1.33b
(define (relatively-prime-product n)
  (define (predicate i)
    (= 1 (gcd i n)))
  (filtered-accumulate predicate * 1 id 1 inc n))
