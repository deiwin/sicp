(load "lib.scm")

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (simpsons-term k)
    (cond ((or (= k 0) (= k n)) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))
          ))
  (define (simpsons-next k) (+ 1 k))
  (/ (* h (sum simpsons-term 0 simpsons-next n)) 3)
  )
