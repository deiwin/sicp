; 2.24
; (list 1 (list 2 (list 3 4)))
; interpreter: (1 (2 (3 4)))
; tree:
; (1 (2 (3 4)))
;  |\
;  1 (2 (3 4))
;     |\
;     2 (3 4)
;        |\
;        3 4


; 2.25
(define first (car (cdaddr '(1 3 (5 7) 9))))
(define second (caar '((7))))
(define third (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7))))))))))

; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
; (1 2 3 4 5 6)
(cons x y)
; ((1 2 3) 4 5 6)
(list x y)
; ((1 2 3) (4 5 6))

; 2.27
(define deep-list (list (list 1 2) (list 3 4)))

(define (deep-reverse l)
  (define (deep-reverse-if-pair x)
    (if (pair? x) (deep-reverse x) x))
  (define (iter result remaining)
    (if (null? remaining)
      result
      (iter (cons (deep-reverse-if-pair (car remaining)) result) (cdr remaining))))
  (iter '() l))

; 2.28
(define tree (list (list 1 2) (list 3 4)))

(define (fringe l)
  (cond ((null? l) l)
        ((not (pair? l)) (list l))
        (else (append (fringe (car l)) (fringe (cdr l))))))

; 2.29
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
(define (branch-weight branch)
  (if (pair? (branch-structure branch))
    (total-weight (branch-structure branch))
    (branch-structure branch)))

(define test-mobile
  (make-mobile (make-branch 2 5)
               (make-branch 3 (make-mobile (make-branch 2 5)
                                           (make-branch 1 5)))))

(define a (make-mobile (make-branch 2 10)
                       (make-branch 10 2)))
(define b (make-mobile (make-branch 2 4)
                       (make-branch 4 2)))
(define balanced-mobile
  (make-mobile (make-branch 2 a)
               (make-branch 4 b)))
(define also-balanced-mobile
  (make-mobile (make-branch 2 a)
               (make-branch 4 6)))

(define (balanced? mobile)
  ; Define branch as balanced when it has just a weight, or when its mobile is
  ; balanced
  (define (balanced-branch? branch)
    (if (pair? (branch-structure branch))
      (balanced? (branch-structure branch))
      #t))
  (define (torque branch)
    (* (branch-length branch) (branch-weight branch)))

  (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
       (balanced-branch? (left-branch mobile))
       (balanced-branch? (right-branch mobile))))
