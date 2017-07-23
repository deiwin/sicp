(define (make-interval a b)
  (if (< b a)
    (cons b a)
    (cons a b)))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((and (not (positive? lx)) (not (positive? ux)) (not (positive? ly)) (not (positive? uy)))
           (make-interval (* ux uy)
                          (* lx ly)))
          ((and (not (positive? lx)) (not (positive? ux)) (not (positive? ly)) (positive? uy))
           (make-interval (* lx uy)
                          (* lx ly)))
          ((and (not (positive? lx)) (not (positive? ux)) (positive? ly) (positive? uy))
           (make-interval (* lx uy)
                          (* ux ly)))
          ((and (not (positive? lx)) (positive? ux) (not (positive? ly)) (not (positive? uy)))
           (make-interval (* ux ly)
                          (* lx ly)))
          ((and (not (positive? lx)) (positive? ux) (not (positive? ly)) (positive? uy))
           (make-interval (min (* lx uy) (* ux ly))
                          (max (* lx ly) (* ux uy))))
          ((and (not (positive? lx)) (positive? ux) (positive? ly) (positive? uy))
           (make-interval (* lx uy)
                          (* ux uy)))
          ((and (positive? lx) (positive? ux) (not (positive? ly)) (not (positive? uy)))
           (make-interval (* ux ly)
                          (* lx uy)))
          ((and (positive? lx) (positive? ux) (not (positive? ly)) (positive? uy))
           (make-interval (* ux ly)
                          (* ux uy)))
          ((and (positive? lx) (positive? ux) (positive? ly) (positive? uy))
           (make-interval (* lx ly)
                          (* ux uy))))))

(define (div-interval x y)
  (if (or (and (> (lower-bound y) 0)
               (> (upper-bound y) 0))
          (and (< (lower-bound y) 0)
               (< (upper-bound y) 0)))
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))
    (error "Result of dividing with interval spanning 0 is undefined" y)))

(define (midpoint-interval i)
  (average (lower-bound i) (upper-bound i)))
(define (width-interval i)
  (/
    (- (upper-bound i) (lower-bound i))
    2))

(define (alt-add-interval x y)
  (let ((midpoint (+ (midpoint-interval x) (midpoint-interval y)))
        (width (+ (width-interval x) (width-interval y))))
    (make-interval (- midpoint width)
                   (+ midpoint width))))

(define (alt-sub-interval x y)
  (let ((midpoint (- (midpoint-interval x) (midpoint-interval y)))
        (width (+ (width-interval x) (width-interval y))))
    (make-interval (- midpoint width)
                   (+ midpoint width))))

(define i1 (make-interval -1 1))
(define i2 (make-interval 1 3))

; (= (width-interval i1) (width-interval i2))
; The above is true, so if the width of the multiplied interval only depended
; on the widths of the multiplied intervals then the following would also have
; to be true:
; (= (width-interval (mul-interval i1 i2))
;    (width-interval (mul-interval i2 i2)))
; But that's not the case. Therefore the width of the multiplied interval
; cannot only depend on the widths of the multiplied intervals. Same can be
; shown for division in the same manner.
