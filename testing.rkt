(define-syntax (assert stx)
  (define (create-assertion-context operation args)
    #`(cons #,operation #,args))
  (define (context-operation context)
    #`(car #,context))
  (define (context-args context)
    #`(cdr #,context))

  (define (create-failure context)
    #`(cons 'failed (list #,context)))
  (define (combine-failure f1 f2)
    #`(cons 'failed (append #,(assertion-contexts f1)
                            #,(assertion-contexts f2))))
  (define (assertion-contexts assertion)
    #`(cdr #,assertion))
  (define (failure? assertion)
    #`(and (pair? #,assertion)
           (equal? (car #,assertion) 'failed)))

  (define (make-predicate predicate args)
    #`(if (#,predicate #,@args)
        #t
        #,(create-failure (create-assertion-context predicate #`(list #,@args)))))
  (define (make-and assert-stx-list)
    #`(foldl
        (lambda (assertion acc)
          (cond (#,(failure? #'assertion) (if #,(failure? #'acc)
                                            #,(combine-failure #'assertion #'acc)
                                            assertion))
                (assertion acc)
                (else
                  (error "macro wrong, got #f instead of (list 'failed (list))"))))
        #t
        (list #,@assert-stx-list)))
  (define (make-or assert-stx-list)
    (define (or-combine head rest)
      #`(cond [(not #,rest) #,head]
              [#,(failure? rest) #,(combine-failure head rest)]
              [else #t]))
    #`(foldl
        (lambda (assertion acc)
          (cond (#,(failure? #'assertion) #,(or-combine #'assertion #'acc))
                (assertion #t)
                (else
                  (error "macro wrong, got #f instead of (list 'failed (list))"))))
        #f
        (list #,@assert-stx-list)))
  (define (make-assert bool-stx)
    (syntax-case
      bool-stx
      (and or)
      [(and bools ...) (make-and (map make-assert
                                      (syntax->list #'(bools ...))))]
      [(or bools ...) (make-or (map make-assert
                                    (syntax->list #'(bools ...))))]
      [(predicate args ...) (make-predicate #'predicate (syntax->list #'(args ...)))]))
  (define (color-red assert-stx)
    #`(string-append
        "\033[31m"
        (string-join (string-split #,assert-stx "\n")
                     "\033[0m\n\033[31m")
        "\033[0m"))
  (define (format-context context)
    #`(string-append
        "("
        (format "~a" (object-name #,(context-operation context)))
        " "
        (string-join (map pretty-format
                          #,(context-args context))
                     " ")
        ") is false"))
  (define (format-failure assertion)
    (color-red #`(string-append
                   "Assertion error!\n  * "
                   (string-join (map (lambda (context) #,(format-context #'context))
                                     #,(assertion-contexts assertion))
                                "\n  * "))))
  (syntax-case stx ()
    [(assert bool-exp)
     #`(let ([assertion #,(make-assert #'bool-exp)])
         (if (equal? #t assertion)
           #t
           (error #,(format-failure #'assertion))))]))


(require macro-debugger/expand)
(syntax->datum (expand-only #'(assert (or (equal? '(1) '(2))
                                          (and (= 1 2)
                                               (= 1 1))))
                            (list #'assert)))
(assert (or (equal? '(1) '(2))
            (and (member 1 '(2 2 3))
                 (= 1 1))))
