(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define empty-tree '())
(define (make-leaf entry)
  (make-tree entry empty-tree empty-tree))

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

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append
      (tree->list-1
        (left-branch tree))
      (cons (entry tree)
            (tree->list-1
              (right-branch tree))))))

(define (tree->list-2 tree)
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

(define (union-ordered-list l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else (let ((x1 (car l1))
                    (rest1 (cdr l1))
                    (x2 (car l2))
                    (rest2 (cdr l2)))
                (cond ((= x1 x2)
                       (cons x1 (union-ordered-list rest1 rest2)))
                      ((< x1 x2)
                       (cons x1 (union-ordered-list rest1 l2)))
                      (else
                        (cons x2 (union-ordered-list l1 rest2))))))))

(define (union-set set1 set2)
  (list->tree
    (union-ordered-list (tree->list-2 set1)
                        (tree->list-2 set2))))

(define (intersection-ordered-list l1 l2)
  (if (or (null? l1) (null? l2))
    '()
    (let ((x1 (car l1)) (x2 (car l2)))
      (cond ((= x1 x2)
             (cons x1 (intersection-ordered-list
                        (cdr l1)
                        (cdr l2))))
            ((< x1 x2) (intersection-ordered-list
                         (cdr l1)
                         l2))
            ((< x2 x1) (intersection-ordered-list
                         l1
                         (cdr l2)))))))

(define (intersection-set set1 set2)
  (list->tree
    (intersection-ordered-list (tree->list-2 set1)
                               (tree->list-2 set2))))

(define tree1 (make-tree 7
                         (make-tree 3
                                    (make-leaf 1)
                                    (make-leaf 5))
                         (make-tree 9
                                    empty-tree
                                    (make-leaf 11))))
(define tree2 (make-tree 3
                         (make-leaf 1)
                         (make-tree 7
                                    (make-leaf 5)
                                    (make-tree 9
                                               empty-tree
                                               (make-leaf 11)))))
(define tree3 (make-tree 5
                         (make-tree 1
                                    empty-tree
                                    (make-leaf 3))
                         (make-tree 9
                                    (make-leaf 7)
                                    (make-leaf 11))))

(unless (and (equal? (union-set tree1 tree2) tree3)
             (equal? (intersection-set tree1 tree2) tree3))
  (error "union and intersection test failed"))
