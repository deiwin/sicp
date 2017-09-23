(load "testing.rkt")

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

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

; 2.67
(define sample-tree
  (make-code-tree
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
; '(A D A B B C A)

; 2.68
(define (encode-symbol symbol tree)
  (cond ((and (leaf? tree) (equal? (symbol-leaf tree) symbol)) '())
        ((member symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
        ((member symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol" symbol "not encodable by tree" tree))))

(define (encode message tree)
  (if (null? message)
    '()
    (append
      (encode-symbol (car message)
                     tree)
      (encode (cdr message) tree))))

(assert "sample message is encoded"
        (equal? (encode '(A D A B B C A) sample-tree)
                sample-message))

; From the book
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

(define (generate-huffman-tree pairs)
  (successive-merge
    (make-leaf-set pairs)))

; 2.69
(define (successive-merge tree-set)
  (if (null? (cdr tree-set))
    (car tree-set)
    (successive-merge (adjoin-set (make-code-tree (car tree-set) (cadr tree-set))
                                  (cddr tree-set)))))

(assert "generates sample-tree"
        (equal? (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
                sample-tree))

; 2.70
(define rock-tree (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2)
                                                 (NA 16) (SHA 3) (YIP 9) (WAH 1))))
(define rock-message (append '(GET A JOB)
                             '(SHA NA NA NA NA NA NA NA NA)
                             '(GET A JOB)
                             '(SHA NA NA NA NA NA NA NA NA)
                             '(WAH YIP YIP YIP YIP)
                             '(YIP YIP YIP YIP YIP)
                             '(SHA BOOM)))

(assert "encodes rock message efficiently"
        (and (equal? (encode rock-message rock-tree)
                     '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1
                       1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1
                       0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1))
             (= (length (encode rock-message rock-tree))
                84)
             (< (length (encode rock-message rock-tree))
                ; with fixed-length code, every word would need 3 bits
                (* 3 (length rock-message)))))

; 2.71
(define (generate-exponential-tree n)
  (define (generate-exponential-alphabet i n)
    (if (= i n)
      '()
      (cons (list (string->symbol (format "exp~a" i)) (expt 2 i))
            (generate-exponential-alphabet (+ i 1) n))))
  (generate-huffman-tree (generate-exponential-alphabet 0 n)))

(assert "most frequent symbols take 1 bit"
        (and (= (length (encode '(exp4) (generate-exponential-tree 5)))
                1)
             (= (length (encode '(exp9) (generate-exponential-tree 10)))
                1)))
(assert "least frequent symbols take n-1 bits"
        (and (= (length (encode '(exp0) (generate-exponential-tree 5)))
                4)
             (= (length (encode '(exp0) (generate-exponential-tree 10)))
                9)))
