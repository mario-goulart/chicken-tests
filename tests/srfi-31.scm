; Test suite for SRFI-31
; 2004-01-01 / lth

(use test)

(test-begin "srfi-31")

(test 120 ((rec (fact n)
                (if (zero? n)
                    1
                    (* n (fact (- n 1)))))
           5))

(test 100000000 ((rec f (lambda (x y)
                          (if (zero? x)
                              y
                              (f (- x 1) (* y y)))))
                 3 10))

(test-end "srfi-31")