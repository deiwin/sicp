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

(define (make-monitored f)
  (let ((call-count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) call-count)
            ((eq? x 'reset-count) (set! call-count 0))
            (else
              (set! call-count (add1 call-count))
              (f x))))))

(let* ((s (make-monitored sqrt))
       (sqrt-of-hundred (s 100))
       (call-count (s 'how-many-calls?))
       (call-count2
         (begin
           (s 'reset-count)
           (s 'how-many-calls?))))
  (assert "can monitor"
          (and (= sqrt-of-hundred 10)
               (= call-count 1)
               (= call-count2 0))))
