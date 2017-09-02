; for testing
(define (make-painter value) value)
(define (value painter) painter)
(define (atomic-painter? painter) (not (pair? painter)))

(define (below p1 p2) (list "below" p1 p2))
(define (below? painter) (and (pair? painter)
                              (equal? "below" (car painter))))
(define top-painter caddr)
(define bottom-painter cadr)

(define (beside p1 p2) (list "beside" p1 p2))
(define (beside? painter) (and (pair? painter)
                               (equal? "beside" (car painter))))
(define right-painter caddr)
(define left-painter cadr)

; 2.44
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((next (up-split painter (- n 1))))
      (below painter (beside next next)))))

; 2.45
(define (split first-split second-split)
  (define (iter painter n)
    (if (= n 0)
      painter
      (let ((next (iter painter (- n 1))))
        (first-split painter (second-split next next)))))
  iter)

(define up-split (split below beside))
(define right-split (split beside below))

(define painter (make-painter 1))

(up-split painter 4)
