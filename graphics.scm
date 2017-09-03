(require racket/gui/base)

(define (on-screen f)
  (define frame (new frame%
                     [label "Do the wave!"]
                     [width 300]
                     [height 300]))
  (new canvas% [parent frame]
       [paint-callback
         (lambda (canvas dc)
           (define (draw-line-canvas start-x start-y end-x end-y)
             (send dc draw-line start-x start-y end-x end-y))
           (f draw-line-canvas))])
  (send frame show #t))
