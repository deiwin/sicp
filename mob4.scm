(load "testing.rkt")

(define (make-accumulator initial)
  (let ((sum initial))
    (lambda (addend)
      (set! sum (+ sum addend))
      sum)))

(let* ((a (make-accumulator 5))
       (a1 (a 1))
       (a2 (a 2))
       (a3 (a 3)))
  (assert "can accumulate"
          (and (= a1 6)
               (= a2 8)
               (= a3 11))))

(define (make-monitored f)
  (let ((call-count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) call-count)
            ((eq? x 'reset-count) (set! call-count 0))
            (else
              (set! call-count (add1 call-count))
              (f x))))))

(let* ((s (make-monitored sqrt))
       (sqrt-of-hundred (s 100))
       (call-count (s 'how-many-calls?))
       (call-count2
         (begin
           (s 'reset-count)
           (s 'how-many-calls?))))
  (assert "can monitor"
          (and (= sqrt-of-hundred 10)
               (= call-count 1)
               (= call-count2 0))))

(define (make-account balance password)
  (define (call-the-cops) (error "Cops are coming!"))
  (define failed-attempts 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch provided-password m)
    (if (not (eq? provided-password password))
        (begin
         (set! failed-attempts (add1 failed-attempts))
         (if (> failed-attempts 7)
           (call-the-cops)
           (error "Incorrect password")))
        (begin
         (set! failed-attempts 0)
         (cond
           ((eq? m 'withdraw) withdraw)
           ((eq? m 'deposit) deposit)
           (else (error "Unknown request:
                  MAKE-ACCOUNT" m))))))
  dispatch)

(let* ((acc (make-account 100 'secret-password))
       (balance-after-withdrawal ((acc 'secret-password 'withdraw) 40))
       (invalid-password-withdrawal
         (with-handlers ((exn:fail? exn-message))
                        ((acc 'invalid-password 'withdraw) 40)))
       (too-many-invalid-withdrawals
         (begin
           (with-handlers ((exn:fail? exn-message)) ((acc 'x 'withdraw) 40))
           (with-handlers ((exn:fail? exn-message)) ((acc 'x 'withdraw) 40))
           (with-handlers ((exn:fail? exn-message)) ((acc 'x 'withdraw) 40))
           (with-handlers ((exn:fail? exn-message)) ((acc 'x 'withdraw) 40))
           (with-handlers ((exn:fail? exn-message)) ((acc 'x 'withdraw) 40))
           (with-handlers ((exn:fail? exn-message)) ((acc 'x 'withdraw) 40))
           (with-handlers ((exn:fail? exn-message)) ((acc 'x 'withdraw) 40))))
       (unsuccessful-login-attempt-after-successful-login-attempt-after-cops
         (begin
           ((acc 'secret-password 'withdraw) 40)
           (with-handlers ((exn:fail? exn-message)) ((acc 'x 'withdraw) 40)))))

  (assert (and (= 60 balance-after-withdrawal)
               (equal? "Incorrect password" invalid-password-withdrawal)
               (equal? "Cops are coming!" too-many-invalid-withdrawals)
               (equal? "Incorrect password" unsuccessful-login-attempt-after-successful-login-attempt-after-cops))))
