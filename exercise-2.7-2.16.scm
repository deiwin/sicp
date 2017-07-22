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
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

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
