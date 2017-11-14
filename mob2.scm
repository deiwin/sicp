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

(define (reverse l)
  (define (iter acc rest)
    (if (null? rest)
      acc
      (iter (cons (car rest) acc) (cdr rest))))
  (iter '() l))

(define (deep-reverse l)
  (define (iter acc rest)
    (if (null? rest)
      acc
      (let* ((head (car rest))
             (reversed-head (if (pair? head)
                              (deep-reverse head)
                              head)))
        (iter (cons reversed-head acc) (cdr rest)))))
  (iter '() l))

(assert
  (let ((x '((1 2) (3 4))))
       (and
            (equal? '((3 4) (1 2)) (reverse x))
            (equal? '((4 3) (2 1)) (deep-reverse x)))))

(define (fringe tree)
  (cond ((null? tree) '())
        ((pair? (car tree)) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (cons (car tree) (fringe (cdr tree))))))

(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(assert "fringe"
        (let ((x '((1 2) (3 4))))
          (and (equal? '(1 2 3 4) (fringe x))
               (equal? '(1 2 3 4 1 2 3 4) (fringe (list x x))))))

(define (make-mobile left right)
  (cons left right))
(define mobile? pair?)
(define left-branch car)
(define right-branch cdr)

(define (make-branch length structure)
  (cons length structure))
(define branch-length car)
(define branch-structure cdr)

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
       (if (mobile? structure)
           (total-weight structure)
           structure)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (mobile-balanced? mobile)
  (define (torque branch)
    (* (branch-length branch)
       (branch-weight branch)))
  (define (branch-balanced? branch)
    (let ((structure (branch-structure branch)))
         (if (mobile? structure)
             (mobile-balanced? structure)
             #t)))
  (and (= (torque (left-branch mobile))
          (torque (right-branch mobile)))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))

(assert
  (and (= 10 (total-weight (make-mobile (make-branch 5 5)
                                        (make-branch 5 (make-mobile (make-branch 2 2)
                                                                    (make-branch 3 3))))))))

(assert
 (and (mobile-balanced? (make-mobile (make-branch 5 6)
                                     (make-branch 5 (make-mobile (make-branch 3 3)
                                                                 (make-branch 3 3)))))
      (not (mobile-balanced? (make-mobile (make-branch 5 6)
                                          (make-branch 5 (make-mobile (make-branch 2 2)
                                                                      (make-branch 3 3))))))))

(define (tree-map fn tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (fn tree))
        (else (cons (tree-map fn (car tree))
                    (tree-map fn (cdr tree))))))

(define (tree-map fn tree)
  (if (null? tree)
    '()
    (let* ((head (car tree))
           (tail (cdr tree))
           (mapped-head (if (pair? head)
                          (tree-map fn head)
                          (fn head))))
      (cons mapped-head (tree-map fn tail)))))


(define (square-tree tree)
  (tree-map square tree))

(assert
  (equal? '(1 (4 (9 9 16) 25) (36 49)) (square-tree '(1 (2 (3 3 4) 5) (6 7)))))

(define (subsets s)
  (if (null? s)
      '(())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (cons (car s) subset))
                          rest)))))

(assert (equal? '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
                (subsets '(1 2 3))))

(define accumulate foldr)
(define (my-map p sequence)
  (accumulate (lambda (x acc) (cons (p x) acc))
              '() sequence))
(assert (equal? '(2 3 4) (my-map add1 '(1 2 3))))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
(assert (equal? '(1 2 3 4 5 6) (my-append '(1 2 3) '(4 5 6))))

(define (my-length sequence)
  (accumulate (lambda (x acc) (add1 acc))
              0 sequence))
(assert (= 4 (my-length '(1 2 3 4))))


(define
  (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ (* higher-terms x) this-coeff))
   0
   coefficient-sequence))

(assert (= 79 (horner-eval 2 '(1 3 0 5 0 1))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves-2 t)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves-2 x) 1)) t)))

(assert (and (= (count-leaves '()) (count-leaves-2 '()))
             (= (count-leaves '(1 2 (3 4))) (count-leaves-2 '(1 2 (3 4))))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(assert (equal? '(22 26 30) (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))))
