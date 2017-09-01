(define (make-column column-number queen-position)
  (cons column-number queen-position))
(define (column-number c) (car c))
(define (queen-position c) (cdr c))

(define (adjoin-position queen-row column-number rest-of-queens)
  (cons (make-column column-number queen-row) rest-of-queens))

(define empty-board '())

; Only check if the first column is safe, because others will have been checked
; already. Because of the way the columns are created, I know that the first
; column is the column to check and the argument can be ignored
(define (safe? column-to-check columns)
  (if (null? columns)
    #t
    (let ((initial-queen-row (queen-position (car columns))))
      (not (car (foldl (lambda (column acc)
                         (let ((is-checked (or (car acc)
                                               (= (cadr acc) (queen-position column))
                                               (= (caddr acc) (queen-position column))
                                               (= (cadddr acc) (queen-position column)))))
                           (list is-checked (+ (cadr acc) 1) (caddr acc) (- (cadddr acc) 1))))
                       (list #f (+ initial-queen-row 1) initial-queen-row (- initial-queen-row 1))
                       (cdr columns)))))))

; The board from the book, for checking.
(define board-from-book (list (make-column 8 3)
                              (make-column 7 7)
                              (make-column 6 2)
                              (make-column 5 8)
                              (make-column 4 5)
                              (make-column 3 1)
                              (make-column 2 4)
                              (make-column 1 6)))
(safe? 3 board-from-book)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval
                  1
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (serialize-board columns)
  (string-join (apply map
                      (lambda (first . rest) (string-join (cons first rest) ""))
                      (map (lambda (column)
                             (append (repeat "O" (- (queen-position column) 1))
                                     '("X")
                                     (repeat "O" (- (length columns) (queen-position column)))))
                           columns))
               "\n"))

(display (serialize-board board-from-book))

(display (string-join (map serialize-board (queens 8)) "\n--------\n"))
