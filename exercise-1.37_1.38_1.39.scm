; 1.37a
(define (rec-cont-frac n d k)
  (define (iter i)
    (if (>= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (iter (inc i))))))
  (iter 1))

(define (golden-ratio-approx k)
  (/ 1 (rec-cont-frac (always 1.0) (always 1.0) k)))

(define (approximate-to-precision f tolerance)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try previous-value step)
    (let (( current-value (f step)))
      (if (close-enough? previous-value current-value)
        (list step current-value)
        (try current-value (inc step)))))
  (try (f 1) 2))

; 1.37b
(define (iter-cont-frac n d k)
  (define (iter i acc)
    (if (<= i 0)
      acc
      (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))
(define (iter-golden-ratio-approx k)
  (/ 1 (iter-cont-frac (always 1.0) (always 1.0) k)))

; 1.38
(define (e-approx k)
  (define (d i)
    (if (= (modulo i 3) 2)
      (* (+ 1 (quotient i 3)) 2)
      1))
  (+ 2 (iter-cont-frac (always 1.0) d k)))

; 1.39
(define (tan-cf-approx x k)
  (define (n i)
    (if (= i 1)
      x
      (- (square x))))
  (define (d i) (- (* i 2) 1))
  (iter-cont-frac n d k))
