(load "testing.rkt")

(define (make-accumulator initial)
  (let ((sum initial))
    (lambda (addend)
      (set! sum (+ sum addend))
      sum)))

(let* ((a (make-accumulator 5))
       (a1 (a 1))
       (a2 (a 2))
       (a3 (a 3)))
  (assert "can accumulate"
          (and (= a1 6)
               (= a2 8)
               (= a3 11))))
