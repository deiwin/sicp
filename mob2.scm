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

(define (matrix-*-vector m v)
  (map (lambda (row)
         (accumulate + 0 (map * row v)))
       m))
(assert (equal? '(50 122) (matrix-*-vector '((1 2 3) (4 5 6)) '(7 8 9))))

(define (transpose mat)
  (accumulate-n cons '() mat))
(assert (equal? '((1 2 3)
                  (4 5 6))
                (transpose '((1 4)
                             (2 5)
                             (3 6)))))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(assert (equal? (matrix-*-matrix '((1 2 3)
                                   (4 5 6))
                                 '((1 4)
                                   (2 5)
                                   (3 6)))
                '((14 32)
                  (32 77))))

(define (reverse-1 sequence)
  (foldr
    (lambda (x acc) (append acc (list x))) '() sequence))

(define (reverse-2 sequence)
  (foldl
    (lambda (x acc) (cons x acc)) '() sequence))

(assert
  (and
    (equal? '((3 4) (1 2)) (reverse-1 '((1 2) (3 4))))
    (equal? '((3 4) (1 2)) (reverse-2 '((1 2) (3 4))))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (triples n s)
  (filter (lambda (l)
                  (= (foldl + 0 l) s))
          (flatmap (lambda (k)
                           (flatmap (lambda (j)
                                            (map (lambda (i)
                                                         (list i j k))
                                                 (enumerate-interval 1 (- j 1))))
                                (enumerate-interval 2 (- k 1))))
                   (enumerate-interval 3 n))))

(assert (and (equal? '((1 2 3)) (triples 3 6))
             (equal? '((1 3 4)) (triples 4 8))
             (equal? '((1 3 4) (1 2 5)) (triples 5 8))))

(assert "intuition"
        (and (equal? '(a b c) (list 'a 'b 'c))
             (equal? '((george)) (list (list 'george)))
             (equal? '((y1 y2)) (cdr '((x1 x2) (y1 y2))))
             (equal? '(y1 y2) (cadr '((x1 x2) (y1 y2))))
             (equal? #f (pair? (car '(a short list))))
             (equal? #f (memq 'red '((red shoes) (blue socks))))
             (equal? '(red shoes blue socks) (memq 'red '(red shoes blue socks)))))

(define (equalq? x y)
  (cond ((and (symbol? x) (symbol? y))
         (eq? x y))
        ((and (pair? x) (pair? y))
         (and (equalq? (car x) (car y))
              (equalq? (cdr x) (cdr y))))
        ((and (null? x) (null? y) #t))
        (else #f)))

(assert (and (equalq? '() '())
             (equalq? '(a b c) '(a b c))
             (not (equalq? '(a b) '(a b c)))
             (equalq? 'a 'a)
             (not (equalq? 'a 'b))
             (equalq? '(+ (+ a b)) '(+ (+ a b)))))

(car ''abracadabra)


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((same-variable? a1 a2)
         (make-product 2 a2))
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))


(define (elem-if-singleton xs)
  (if (and (pair? xs)
           (null? (cdr xs)))
      (car xs)
      xs))

(define (before-sym exp sym)
  (elem-if-singleton
    (takef exp (lambda (x)
                 (not (eq? sym x))))))
(define (after-sym exp sym)
  (elem-if-singleton
    (cdr (memq sym exp))))

(define (sum? x)
  (and (pair? x)
       (memq '+ x)))
(define (addend s)
  (before-sym s '+))
(define (augend s)
  (after-sym s '+))
(define (product? x)
  (and (pair? x)
       (memq '* x)))
(define (multiplier p)
  (before-sym p '*))
(define (multiplicand p)
  (after-sym p '*))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product
             (multiplier exp)
             (deriv (multiplicand exp) var))
           (make-product
             (deriv (multiplier exp) var)
             (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (make-product
             (exponent exp)
             (make-exponentiation
               (base exp)
               (make-sum (exponent exp) -1)))
           (deriv (base exp) var)))
        (else (error "unknown expression
                     type: DERIV" exp))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 1) base)
        ((=number? exponent 0) 1)
        (else (list base '** exponent))))
(define (exponent e)
  (caddr e))
(define (base e)
  (car e))
(define (exponentiation? e)
  (eq? '** (cadr e)))

(assert (and (equal? 4 (deriv '(x + (3 * (x + (y + 2)))) 'x))
             (equal? '(2 * x) (deriv '(x ** 2) 'x))
             (equal? 4 (deriv '(x + 3 * x) 'x))
             (equal? 4 (deriv '(3 * x + x) 'x))
             (equal? '(3 * (2 * x)) (deriv '(3 * x * x) 'x))
             (equal? '(3 * (2 * x)) (deriv '(3 * x ** 2) 'x))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define empty-tree? null?)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set?
          x
          (left-branch set)))
        ((> x (entry set))
         (element-of-set?
          x
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree
         elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size
            (quotient (- n 1) 2)))
      (let ((left-result
              (partial-tree
                elts left-size)))
        (let ((left-tree
                (car left-result))
              (non-left-elts
                (cdr left-result))
              (right-size
                (- n (+ left-size 1))))
          (let ((this-entry
                  (car non-left-elts))
                (right-result
                  (partial-tree
                    (cdr non-left-elts)
                    right-size)))
            (let ((right-tree
                    (car right-result))
                  (remaining-elts
                    (cdr right-result)))
              (cons (make-tree this-entry
                               left-tree
                               right-tree)
                    remaining-elts))))))))

(define (intersection-set tree1 tree2)
  (define (intersection-set-inter set1 set2)
    (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-inter
                          (cdr set1)
                          (cdr set2))))
              ((< x1 x2) (intersection-set-inter
                           (cdr set1)
                           set2))
              ((< x2 x1) (intersection-set-inter
                           set1
                           (cdr set2)))))))
  (list->tree (intersection-set-inter (tree->list tree1) (tree->list tree2))))

(define (union-set tree1 tree2)
  (define (union-set-inter set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((< (car set1) (car set2)) (cons (car set1) (union-set-inter (cdr set1) set2)))
          ((= (car set1) (car set2)) (cons (car set1) (union-set-inter (cdr set1) (cdr set2))))
          (else (cons (car set2) (union-set-inter set1 (cdr set2))))))
  (list->tree (union-set-inter (tree->list tree1) (tree->list tree2))))

(assert (and (equal? '(2) (tree->list (intersection-set (list->tree '(1 2)) (list->tree '(2 3)))))
             (equal? '(1 2 3) (tree->list (union-set (list->tree '(1 2)) (list->tree '(3)))))
             (equal? '() (tree->list (union-set (list->tree '()) (list->tree '()))))
             (equal? '(1 2 3) (tree->list (union-set (list->tree '(1 3)) (list->tree '(1 2)))))))

(define make-record cons)

(define key car)

(define value cdr)

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry set-of-records))) (entry set-of-records))
        ((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records))) (lookup given-key (right-branch set-of-records)))
        (else (error "Oh shit"))))

(assert (and (equal? #f (lookup 42 (list->tree '())))
             (let ((tony (make-record 42 "Tony"))
                   (ostap (make-record 22 "Ostap"))
                   (urmas (make-record 66 "Urmas")))
               (and (equal? tony (lookup (key tony) (list->tree (list tony))))
                    (equal? ostap (lookup (key ostap) (list->tree (list ostap tony urmas))))
                    (equal? urmas (lookup (key urmas) (list->tree (list ostap tony urmas))))))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (symbols-branch tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols-branch tree) (caddr tree))
(define (weight-branch tree) (cadddr tree))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (weight-branch tree)))

(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit:
                       CHOOSE-BRANCH" bit))))
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch
                (car bits)
                current-branch)))
        (if (leaf? next-branch)
          (cons
            (symbol-leaf next-branch)
            (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits)
                    next-branch)))))
  (decode-1 bits tree))


(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(assert "can decode"
        (equal? '(A D A B B C A) (decode sample-message sample-tree)))

(define (encode message tree)
  (define (encode-symbol sym tree)
    (cond ((leaf? tree) '())
          ((memq sym (symbols (left-branch tree))) (cons 0 (encode-symbol sym (left-branch tree))))
          ((memq sym (symbols (right-branch tree))) (cons 1 (encode-symbol sym (right-branch tree))))
          (else (error "oops"))))

  (if (null? message)
    '()
    (append
      (encode-symbol (car message)
                     tree)
      (encode (cdr message) tree))))

(assert "encodes message"
        (equal? sample-message (encode '(A D A B B C A) sample-tree)))

(define (generate-huffman-tree pairs)
  (define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set)))
           (cons x set))
          (else
            (cons (car set)
                  (adjoin-set x (cdr set))))))

  (define (make-leaf-set pairs)
    (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
          (make-leaf (car pair)    ; symbol
                     (cadr pair))  ; frequency
          (make-leaf-set (cdr pairs))))))

  (define (successive-merge tree-set)
    (cond ((null? (cdr tree-set)) (car tree-set))
          (else (successive-merge
                  (adjoin-set
                    (make-code-tree (car tree-set) (cadr tree-set))
                    (cddr tree-set))))))

  (successive-merge
    (make-leaf-set pairs)))

(assert "generate-huffman-tree"
        (equal? sample-tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))))

(define song-tree (generate-huffman-tree
                    '((BOOM 1) (WAH 1) (A 2) (GET 2) (JOB 2) (SHA 3) (YIP 9) (NA 16))))
(define song-lyrics
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA

    GET A JOB
    SHA NA NA NA NA NA NA NA NA

    WAH YIP YIP YIP YIP
    YIP YIP YIP YIP YIP
    SHA BOOM))
(define get-a-job (encode song-lyrics song-tree))

(assert (= 84 (length get-a-job)))
