(define-syntax (assert stx)
  (define (make-equal? a b)
    #`(if (equal? #,a #,b)
        #t
        (let ([message
                (string-append
                  (pretty-format #,a)
                  " does not equal? "
                  (pretty-format #,b))])
          (list 'failed (list message)))))
  (define (make= number-stx-list)
    #`(if (= #,@number-stx-list)
        #t
        (let ([message
                (string-join (map pretty-format
                                  (list  #,@number-stx-list))
                             ", "
                             #:before-last " and "
                             #:after-last " are not =")])
          (list 'failed (list message)))))
  (define (make-combine-failure f1 f2)
    #`(list 'failed (append (cadr #,f1)
                            (cadr #,f2))))
  (define (make-failed? assert-result-stx)
    #`(and (pair? #,assert-result-stx)
           (equal? (car #,assert-result-stx) 'failed)))
  (define (make-and assert-stx-list)
    #`(foldl
        (lambda (assert-result acc)
          (cond (#,(make-failed? #'assert-result) (if #,(make-failed? #'acc)
                                                    #,(make-combine-failure #'assert-result #'acc)
                                                    assert-result))
                (assert-result acc)
                (else
                  (error "macro wrong, got #f instead of (list 'failed (list))"))))
        #t
        (list #,@assert-stx-list)))
  (define (make-or assert-stx-list)
    (define (make-or-combine head rest)
      #`(cond [(not #,rest) #,head]
              [#,(make-failed? rest) #,(make-combine-failure head rest)]
              [else #t]))
    #`(foldl
        (lambda (assert-result acc)
          (cond (#,(make-failed? #'assert-result) #,(make-or-combine #'assert-result #'acc))
                (assert-result #t)
                (else
                  (error "macro wrong, got #f instead of (list 'failed (list))"))))
        #f
        (list #,@assert-stx-list)))
  (define (make-assert bool-stx)
    (syntax-case
      bool-stx
      (equal? = and)
      [(equal? a b) (make-equal? #'a #'b)]
      [(= a b ...) (make= (syntax->list #'(a b ...)))]
      [(and bools ...) (make-and (map make-assert
                                      (syntax->list #'(bools ...))))]
      [(or bools ...) (make-or (map make-assert
                                    (syntax->list #'(bools ...))))]))
  (define (make-color-red assert-stx)
    #`(string-append
        "\033[31m"
        (string-join (string-split #,assert-stx "\n")
                     "\033[0m\n\033[31m")
        "\033[0m"))
  (syntax-case stx ()
    [(assert bool-exp)
     #`(let ([result #,(make-assert #'bool-exp)])
         (if (equal? #t result)
           #t
           (error #,(make-color-red #'(string-append
                                        "Assertion error!\n  * "
                                        (string-join (cadr result)
                                                     "\n  * "))))))]))


(require macro-debugger/expand)
(syntax->datum (expand-only #'(assert (or (equal? '(1) '(2))
                                          (and (= 1 2)
                                               (= 1 1))))
                            (list #'assert)))
(assert (or (equal? '(1) '(2))
            (and (= 1 2 2)
                 (= 1 1))))
