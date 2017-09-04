; 2.53
(list 'a 'b 'c)
; '(a b c)
(list (list 'george))
; '((george)
(cdr '((x1 x2) (y1 y2)))
; '((y1 y2)
(cadr '((x1 x2) (y1 y2)))
; '(y1 y2)
(pair? (car '(a short list)))
; #f
(memq 'red '((red shoes) (blue socks)))
; #f
(memq 'red '(red shoes blue socks))
; '(red shoes blue socks)

; 2.54
(define (equal? l1 l2)
  (if (and (pair? l1) (pair? l2))
    (and (equal? (car l1) (car l2))
         (equal? (cdr l1) (cdr l2)))
    (eq? l1 l2)))

(equal? '(this is a list)
        '(this is a list))
; #t

(equal? '(this is a list)
        '(this (is a) list))
; #f

(equal? '(this (is a) list)
        '(this (is a) list))
; #f
