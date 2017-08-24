; 2.30
(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
(define (alt-square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (alt-square-tree sub-tree)
           (square sub-tree)))
       tree))

(define test-tree '(1 (2 (3 4) 5) (6 7)))

; 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))

(define (tree-map-square-tree tree) (tree-map square tree))

; 2.32
(define (subsets s)
  (if (null? s)
    '(())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (subset)
                          (cons (car s) subset))
                        rest)))))
