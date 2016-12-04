; 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

; 1.31a
(define (iter-product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (approx-pi n)
  (define (term x)
    (/
      (+ 2 (- x (modulo x 2)))
      (+ 1 x (modulo x 2))))
  (define (next x) (+ 1 x))
  (* 4 (iter-product term 1 next n))
  )

(define (inexact-approx-pi n) (exact->inexact (approx-pi n)))

; 1.31b
(define (rec-product term a next b)
  (if (> a b)
    1
    (*
      (term a)
      (rec-product term (next a) next b))))
