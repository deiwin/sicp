(load "lib.scm")

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

; 2.40
(define (unique-pairs n)
  (flatmap
    (lambda (j)
      (map (lambda (i) (list i j))
           (enumerate-interval (+ 1 j) n)))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
         prime-sum?
         (unique-pairs n))))

; 2.41
(define (ordered-triples-of-sum n s)
  (filter (lambda (triple)
            (= (foldr + 0 triple) s))
          (flatmap
            (lambda (pair)
              (map (lambda (k) (append pair (list k)))
                   (enumerate-interval 1 (- (cadr pair) 1))))
            (unique-pairs n))))

(ordered-triples-of-sum 8 13)
