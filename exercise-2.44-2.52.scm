(load "lib.scm")

; for testing
(define (make-painter value) value)
(define (value painter) painter)
(define (atomic-painter? painter) (not (pair? painter)))

(define (below p1 p2) (list "below" p1 p2))
(define (below? painter) (and (pair? painter)
                              (equal? "below" (car painter))))
(define top-painter caddr)
(define bottom-painter cadr)

(define (beside p1 p2) (list "beside" p1 p2))
(define (beside? painter) (and (pair? painter)
                               (equal? "beside" (car painter))))
(define right-painter caddr)
(define left-painter cadr)

(define make-scale cons)
(define scale-x car)
(define scale-y cdr)

(define (min-scale painter)
  (cond ((atomic-painter? painter) (make-scale 2 2))
        ((below? painter) (let ((min-top-scale (min-scale (top-painter painter)))
                                (min-bot-scale (min-scale (bottom-painter painter))))
                            (make-scale (max (scale-x min-top-scale) (scale-x min-bot-scale))
                                        (* 2 (max (scale-y min-top-scale) (scale-y min-bot-scale))))))
        ((beside? painter) (let ((min-left-scale (min-scale (left-painter painter)))
                                 (min-right-scale (min-scale (right-painter painter))))
                             (make-scale (* 2 (max (scale-x min-left-scale) (scale-x min-right-scale)))
                                         (max (scale-y min-left-scale) (scale-y min-right-scale)))))))

(define horizontal-border "-")
(define vertical-border "|")
(define empty-point " ")
(define corner "X")

; A painting is represented as a 2 dimensional array, where X characters
; signify a corner of a sub-painting.
(define (paint painter)
  (define (iter painter scale)
    (cond ((atomic-painter? painter)
           (let ((terminal-row (cons corner
                                     (append (repeat horizontal-border (- (scale-x scale) 2)) (list corner))))
                 (inner-row (cons vertical-border
                                  (append (repeat empty-point (- (scale-x scale) 2)) (list vertical-border)))))
             (cons terminal-row (append (repeat inner-row (- (scale-y scale) 2)) (list terminal-row)))))
          ((below? painter)
           (let ((split-scale (make-scale (scale-x scale)
                                          (/ (scale-y scale) 2))))
             (append (iter (top-painter painter) split-scale)
                     (iter (bottom-painter painter) split-scale))))
          ((beside? painter)
           (let ((split-scale (make-scale (/ (scale-x scale) 2)
                                          (scale-y scale))))
             (let ((left-painting (iter (left-painter painter) split-scale))
                   (right-painting (iter (right-painter painter) split-scale)))
               (map append left-painting right-painting))))))
  (iter painter (min-scale painter)))

(define (serialize-painting painting)
  (string-join (map (lambda (row) (string-join row ""))
                    painting)
               "\n"))

; 2.44
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((next (up-split painter (- n 1))))
      (below painter (beside next next)))))

; 2.45
(define (split first-split second-split)
  (define (iter painter n)
    (if (= n 0)
      painter
      (let ((next (iter painter (- n 1))))
        (first-split painter (second-split next next)))))
  iter)

(define up-split (split below beside))
(define right-split (split beside below))

(define identity id)
(define (flip-horiz painter)
  (cond ((atomic-painter? painter) painter)
        ((below? painter) (below (flip-horiz (bottom-painter painter))
                                 (flip-horiz (top-painter painter))))
        ; Right and left are swapped for "beside" splits
        ((beside? painter) (beside (flip-horiz (right-painter painter))
                                   (flip-horiz (left-painter painter))))))
(define (flip-vert painter)
  (cond ((atomic-painter? painter) painter)
        ; Top and bottom are swapped for "below" splits
        ((below? painter) (below (flip-vert (top-painter painter))
                                 (flip-vert (bottom-painter painter))))
        ((beside? painter) (beside (flip-vert (left-painter painter))
                                   (flip-vert (right-painter painter))))))

; From the book
(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter
                              (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right
                                 right))
            (corner (corner-split painter
                                  (- n 1))))
        (beside (below painter top-left)
                (below bottom-right
                       corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter)
                        quarter)))
      (below (flip-vert half) half))))

(define painter (make-painter 1))

; 2.46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)

; 2.48
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

; From the book
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect
        (scale-vect (xcor-vect v)
                    (edge1-frame frame))
        (scale-vect (ycor-vect v)
                    (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame)
           (start-segment segment))
          ((frame-coord-map frame)
           (end-segment segment))))
      segment-list)))

; 2.49
(define frame-outline-painter
  (segments->painter (list (make-segment (make-vect 0 0)
                                         (make-vect 0 1))
                           (make-segment (make-vect 0 1)
                                         (make-vect 1 1))
                           (make-segment (make-vect 1 1)
                                         (make-vect 1 0))
                           (make-segment (make-vect 1 0)
                                         (make-vect 0 0)))))

(define x-painter
  (segments->painter (list (make-segment (make-vect 0 0)
                                         (make-vect 1 1))
                           (make-segment (make-vect 0 1)
                                         (make-vect 1 0)))))

(define diamond-painter
  (segments->painter (list (make-segment (make-vect 0 0.5)
                                         (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1)
                                         (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5)
                                         (make-vect 0.5 0))
                           (make-segment (make-vect 0.5 0)
                                         (make-vect 0 0.5)))))
