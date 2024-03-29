; Test suite for SRFI-39
; 2004-01-02 / lth

;(cond-expand (srfi-39))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define radix
  (make-parameter "10" (lambda (x)
			 (if (number? x)
			     x
			     (string->number x)))))

(or (equal? 10 (radix))
    (fail 'get:1))
(parameterize ((radix 16))
  (or (equal? 16 (radix))
      (fail 'get:2)))
(or (equal? 10 (radix))
    (fail 'get:3))

(radix 2)

(or (equal? 2 (radix))
    (fail 'set:1))

(radix "20")

(or (equal? 20 (radix))
    (fail 'set:2))

(writeln "Done.")
