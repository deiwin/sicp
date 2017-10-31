(load "testing.rkt")

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (average a b) (/ (+ a b) 2))
(define (midpoint-segment segment)
  (let ((start-x (x-point (start-segment segment)))
        (end-x (x-point (end-segment segment)))
        (start-y (y-point (start-segment segment)))
        (end-y (y-point (end-segment segment))))
    (make-point (average start-x end-x)
                (average start-y end-y))))

(assert "can get the midpoint of a segment"
        (and (equal? (make-point 1 2)
                     (midpoint-segment (make-segment (make-point 0 0)
                                                     (make-point 2 4))))
             (equal? (make-point 0 0)
                     (midpoint-segment (make-segment (make-point -1 -1)
                                                     (make-point 1 1))))
             (equal? (make-point -2 -2)
                     (midpoint-segment (make-segment (make-point -3 -3)
                                                     (make-point -1 -1))))
             (equal? (make-point 1 1)
                     (midpoint-segment (make-segment (make-point 2 2)
                                                     (make-point 0 0))))))

(define (square x) (* x x))
(define (segment-length segment)
  (let ((start-x (x-point (start-segment segment)))
        (end-x (x-point (end-segment segment)))
        (start-y (y-point (start-segment segment)))
        (end-y (y-point (end-segment segment))))
    (sqrt (+ (square (- start-x end-x))
             (square (- start-y end-y))))))

(assert "can calc segment length"
        (and (= 10 (segment-length (make-segment (make-point 0 0)
                                                 (make-point 0 10))))
             (= (sqrt 2) (segment-length (make-segment (make-point 0 0)
                                                       (make-point 1 1))))))

(define (make-rectangle left-side bottom-side)
  (list left-side bottom-side))
(define (rectangle-left-side r) (car r))
(define (rectangle-bottom-side r) (cadr r))

(define (rectangle-height r)
  (segment-length (rectangle-left-side r)))
(define (rectangle-width r)
  (segment-length (rectangle-bottom-side r)))

(define (rectangle-area r)
  (* (rectangle-width r) (rectangle-height r)))

(define (rectangle-perimeter r)
  (* 2 (+ (rectangle-width r)
          (rectangle-height r))))

(assert "can calc rectangle area"
        (= 50 (rectangle-area (make-rectangle (make-segment (make-point 0 5)
                                                            (make-point 0 0))
                                              (make-segment (make-point 0 0)
                                                            (make-point 10 0))))))

(assert "can calc rectangle perimeter"
        (= 30 (rectangle-perimeter (make-rectangle (make-segment (make-point 0 5)
                                                                 (make-point 0 0))
                                                   (make-segment (make-point 0 0)
                                                                 (make-point 10 0))))))

(define (my-cons a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (my-car p)
  (if (not (= 0 (remainder p 2)))
      0
      (+ 1 (my-car (/ p 2)))))
(define (my-cdr p)
  (if (not (= 0 (remainder p 3)))
      0
      (+ 1 (my-cdr (/ p 3)))))

(assert "can store pairs of numbers in integers"
        (let ((pair (my-cons 3 5)))
          (and (= 3 (my-car pair))
               (= 5 (my-cdr pair)))))
