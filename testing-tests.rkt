(load "testing.rkt")

(assert "allows pending specs")
(assert "fails with an error"
        (= 1 2))
