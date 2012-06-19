; Test suite for SRFI-11
; 2004-01-01 / lth

(use test)

(test-begin "srfi-11")

(test '(1 2 (3 4))
      (let-values (((a b . c) (values 1 2 3 4)))
        (list a b c)))

(test '(x y a b)
      (let ((a 'a) (b 'b) (x 'x) (y 'y))
        (let-values (((a b) (values x y))
                     ((x y) (values a b)))
          (list a b x y))))

(test '(1 2 3 4)
      (let-values ((l (values 1 2 3 4)))
        l))

(test '(x y x y)
      (let ((a 'a) (b 'b) (x 'x) (y 'y))
        (let*-values (((a b) (values x y))
                      ((x y) (values a b)))
          (list a b x y))))

(test-end "srfi-11")

(test-exit)