(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; 2.33
(define (my-map p sequence)
  (accumulate (lambda (current acc) (cons (p current) acc)) '() sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (current acc) (+ acc 1)) 0 sequence))

; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(define test-tree '(1 (2 (3 4) 5) (6 7)))

; 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (sub-tree)
                         (if (pair? sub-tree)
                           (count-leaves sub-tree)
                           1))
                       t)))

; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define matrix '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(define v '(1 2 3))

; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))
(define (transpose mat)
  (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

(matrix-*-matrix matrix (transpose matrix))

; 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
; (1/(2/(3/1))) = 3/2
(fold-left / 1 (list 1 2 3))
; (((1/1)/2)/3) = 1/6
(fold-right list '() (list 1 2 3))
; '(1 (2 (3 ())))
(fold-left list '() (list 1 2 3))
; '(((() 1) 2) 3)

; 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
(reverse '(1 2 3 4))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
(reverse '(1 2 3 4))
