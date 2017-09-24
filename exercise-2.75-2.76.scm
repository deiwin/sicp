(load "testing.rkt")

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* mag (cos ang)))
          ((eq? op 'imag-part)
           (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else
            (error "Unknown op:
                   MAKE-FROM-MAG-ANG" op))))
  dispatch)

(assert "can get real and imaginary parts from polar representation"
        (let ((cplx-number (make-from-mag-ang 5 1))
              (racket-cplx-number (make-polar 5 1)))
          (and (= (real-part racket-cplx-number) (cplx-number 'real-part))
               (= (imag-part racket-cplx-number) (cplx-number 'imag-part)))))
