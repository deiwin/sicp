(define-syntax (assert stx)
  (define (create-assertion-context operation args result)
    #`(list #,operation #,args #,result))
  (define (context-operation context)
    #`(car #,context))
  (define (context-args context)
    #`(cadr #,context))
  (define (context-result context)
    #`(caddr #,context))

  (define (create-failure contexts)
    #`(cons 'failed #,contexts))
  (define (create-success contexts)
    #`(cons 'passed #,contexts))
  (define (combine-failure f1 f2)
    (create-failure #`(append #,(assertion-contexts f1)
                              #,(assertion-contexts f2))))
  (define (combine-success f1 f2)
    (create-success #`(append #,(assertion-contexts f1)
                              #,(assertion-contexts f2))))
  (define empty-failure
    (create-failure #''()))
  (define empty-success
    (create-success #''()))
  (define (assertion-contexts assertion)
    #`(cdr #,assertion))
  (define (failure? assertion)
    #`(and (pair? #,assertion)
           (equal? (car #,assertion) 'failed)))
  (define (success? assertion)
    #`(and (pair? #,assertion)
           (equal? (car #,assertion) 'passed)))

  (define (make-predicate predicate args)
    #`(if (#,predicate #,@args)
        #,(create-success #`(list #,(create-assertion-context
                                      predicate
                                      #`(list #,@args)
                                      #t)))
        #,(create-failure #`(list #,(create-assertion-context
                                      predicate
                                      #`(list #,@args)
                                      #f)))))
  (define (make-and assert-stx-list)
    #`(foldl
        (lambda (assertion acc)
          (cond [(and #,(success? #'assertion)
                      #,(success? #'acc))
                 #,(combine-success #'assertion #'acc)]
                [(and #,(failure? #'assertion)
                      #,(failure? #'acc))
                 #,(combine-failure #'assertion #'acc)]
                [#,(failure? #'assertion) assertion]
                [#,(failure? #'acc) acc]
                [else (error "macro wrong, expecting assertions")]))
        #,empty-success
        (list #,@assert-stx-list)))
  (define (make-or assert-stx-list)
    #`(foldl
        (lambda (assertion acc)
          (cond [(and #,(success? #'assertion)
                      #,(success? #'acc))
                 #,(combine-success #'assertion #'acc)]
                [(and #,(failure? #'assertion)
                      #,(failure? #'acc))
                 #,(combine-failure #'assertion #'acc)]
                [#,(success? #'assertion) assertion]
                [#,(success? #'acc) acc]
                [else (error "macro wrong, expecting assertions")]))
        #,empty-failure
        (list #,@assert-stx-list)))
  (define (make-not assertion)
    #`(cond [#,(failure? assertion)
             #,(create-success (assertion-contexts assertion))]
            [#,(success? assertion)
             #,(create-failure (assertion-contexts assertion))]
            [else (error "macro wrong, \"not\" expects an assertion")]))
  (define (make-assert bool-stx)
    (syntax-case
      bool-stx
      (and or not)
      [(and bools ...) (make-and (map make-assert
                                      (syntax->list #'(bools ...))))]
      [(or bools ...) (make-or (map make-assert
                                    (syntax->list #'(bools ...))))]
      [(not bool) (make-not (make-assert #'bool))]
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
        (~a (object-name #,(context-operation context)))
        " "
        (string-join (map pretty-format
                          #,(context-args context))
                     " ")
        ") is "
        (~a #,(context-result context))))
  (define (format-failure message assertion)
    (color-red #`(string-append
                   #,message
                   "\n  * "
                   (string-join (map (lambda (context) #,(format-context #'context))
                                     #,(assertion-contexts assertion))
                                "\n  * "))))
  (define (error-if-failure message assertion)
    #`(let ([assertion #,assertion])
        (if #,(success? #'assertion)
          #t
          (error #,(format-failure message #'assertion)))))
  (syntax-case stx ()
    [(assert bool-exp)
     (error-if-failure "Assertion error!"
                       (make-assert #'bool-exp))]
    [(assert description bool-exp)
     (error-if-failure #`(string-append
                           "Failed to assert \""
                           description
                           "\"!")
                       (make-assert #'bool-exp))]))

(assert "basics"
        (and (equal? '(1) '(1))
             (not (equal? '(1) '(2)))
             (= 1 1)
             (not (= 1 2))
             (memq 1 '(1 2 3))
             (not (memq 1 '(2 2 3)))
             (or (= 2 2)
                 (= 2 3))
             (or (= 2 3)
                 (= 2 2))
             (or (= 2 2)
                 (= 2 2))
             (not (or (= 2 3)
                      (= 2 3)))
             (and (= 3 3)
                  (= 3 3))
             (not (and (= 3 4)
                       (= 3 3)))
             (not (and (= 3 3)
                       (= 3 4)))
             (not (and (= 3 4)
                       (= 3 4)))))
