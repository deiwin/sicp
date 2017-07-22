; 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (let ((x (average
             (x-point (start-segment s))
             (x-point (end-segment s))))
        (y (average
             (y-point (start-segment s))
             (y-point (end-segment s)))))
    (make-point x y)))

; 2.3
(define (length-segment s)
  (sqrt (+
          (square
            (abs (-
                   (x-point (start-segment s))
                   (x-point (end-segment s)))))
          (square
            (abs (-
                   (y-point (start-segment s))
                   (y-point (end-segment s))))))))

(define (make-rect bottomm-side height) (cons bottomm-side height))
(define (height-rect r) (cdr r))
(define (width-rect r)
  (length-segment (car r)))

(define (perimeter-rect r)
  (* 2 (+
         (height-rect r)
         (width-rect r))))
(define (area-rect r)
  (*
    (height-rect r)
    (width-rect r)))

(define (make-alt-rect left-side width) (cons left-side width))
; Uncomment to make perimeter-rect and area-rect work with alt-rects
; (define (width-rect r) (cdr r))
; (define (height-rect r)
;   (length-segment (car r)))

(define rect
  (make-rect
    (make-segment
      (make-point 0 0)
      (make-point 2 0))
    5))

(define alt-rect
  (make-alt-rect
    (make-segment
      (make-point 0 0)
      (make-point 0 5))
    2))
