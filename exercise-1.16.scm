(load "lib.scm")

(define (iter-fast-expt b n)
  (define (iter-fast-expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter-fast-expt-iter a (* b b) (/ n 2)))
          (else (iter-fast-expt-iter (* a b) b (- n 1))))
    )
  (iter-fast-expt-iter 1 b n))
