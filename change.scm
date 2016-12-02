(define (change amount denominations)
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        ((= (length denominations) 0) 0)
        (else
          (+
            (change amount (cdr denominations))
            (change (- amount (car denominations)) denominations)
          )
        )
  )
)

(define change-for-10 (change 10 '(1 5))) ; 3
(define change-for-100 (change 100 '(1 5 10 25 50))) ; 292

