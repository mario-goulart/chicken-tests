; Test suite for SRFI-9
; 2004-01-01 / lth

(use test)

(test-begin "srfi-9")

(define-record-type :pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (z mid set-mid!)
  (y kdr))

(test #t (pare? (kons 1 2)))

(test #f (pare? (cons 1 2)))

(test 1 (kar (kons 1 2)))

(test 2 (kdr (kons 1 2)))

(test #t (let ((x (kons 1 2)))
           (set-mid! x 37)
           (and (equal? 1 (kar x))
                (equal? 37 (mid x))
                (equal? 2 (kdr x)))))

(test 3 (let ((k (kons 1 2)))
          (set-kar! k 3)
          (kar k)))

(test-end "srfi-9")

(test-exit)
