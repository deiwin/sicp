(define (pascal-number x y)
  (cond ((or (> y x) (< y 0)) 0)
        ((= x 0) 1)
        (else (+
                (pascal-number (- x 1) (- y 1))
                (pascal-number (- x 1)  y)
                ))))
