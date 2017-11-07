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

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (to-int f)
  ((f (lambda (n) (+ n 1))) 0))

(assert "add one to zero"
        (and (= 1 (to-int (add-1 zero)))
             (= 1 (to-int one))
             (= 2 (to-int (add-1 one)))
             (= 2 (to-int two))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
(define (add a b)
  ((a add-1) b))

(assert "addition"
        (and (= 4 (to-int (add two two)))
             (= 5 (to-int (add two three)))))

(define (mul a b) (compose a b))

(assert "multiplication"
        (and (= 4 (to-int (mul two two)))
             (= 6 (to-int (mul two three)))
             (= 6 (to-int (mul three two)))))

(define (church-expt w z) (z w))

(assert "exponentiation"
        (and (= 4 (to-int (church-expt two two)))
             (= 8 (to-int (church-expt two three)))
             (= 9 (to-int (church-expt three two)))))

(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

(assert "last-pair"
        (equal? '(34) (last-pair '(23 72 149 34))))


(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

(assert "square-list"
        (equal? (square-list '(1 2 3 4)) '(1 4 9 16)))

(define (for-each f l)
  (if (null? l)
    #t
    (begin
      (f (car l))
      (for-each f (cdr l)))))

(assert
  (and (= 7 (car (cdaddr '(1 3 (5 7) 9))))
       (= 7 (caar '((7))))
       (= 7 (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7))))))))))))

(assert
  (let ((x (list 1 2 3))
        (y (list 4 5 6)))
    (and (equal? '(1 2 3 4 5 6) (append x y))
         (equal? '((1 2 3) 4 5 6) (cons x y))
         (equal? '((1 2 3) (4 5 6)) (list x y)))))
