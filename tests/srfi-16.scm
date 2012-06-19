; Test suite for SRFI-16
; 2004-01-01 / lth

(use test)

(test-begin "srfi-16")

(define plus
  (case-lambda
   (() 0)
   ((x) x)
   ((x y) (+ x y))
   ((x y z) (+ (+ x y) z))
   (args (apply + args))))

(test 0 (plus))

(test 1 (plus 1))

(test 6 (plus 1 2 3))

(test 55 (apply plus '(1 2 3 4 5 6 7 8 9 10)))

(test #f (call-with-current-continuation
          (lambda (abort)
            (handle-exceptions exn
              (abort #f)
              ((case-lambda
                 ((a) a)
                 ((a b c) (* a b c)))
               1 2)))))

(test-end "srfi-16")

(test-exit)