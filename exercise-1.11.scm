(define (rec-f n)
  (cond ((< n 3) n)
        (else (+
                (rec-f (- n 1))
                (* 2 (rec-f (- n 2)))
                (* 3 (rec-f (- n 3)))
                ))
  )
)

(define (iter-f n)
  (define (iter-f-iter a b c i)
    (cond ((>= i n) a)
          (else
            (iter-f-iter b c (+ (* 3 a) (* 2 b) c) (+ 1 i)))
    )
  )
  (iter-f-iter 0 1 2 0)
)

