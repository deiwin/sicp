(define (golden-ratio-f x) (+ 1 (/ 1 x)))
(define (xx-f x) (/ (log 1000) (log x)))
(define damped-xx-f (damp xx-f))
