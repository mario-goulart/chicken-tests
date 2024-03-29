; CONFIDENCE TEST FOR IMPLEMENTATION OF SRFI-26
; =============================================
;
; Sebastian.Egner@philips.com, 3-Jun-2002.
;
; This file checks a few assertions about the implementation.
; If you run it and no error message is issued, the implementation
; is correct on the cases that have been tested.

; (cond-expand (srfi-26))

(define (writeln . xs)
  (for-each display xs)
  (newline))

; (check expr)
;    evals expr and issues an error if it is not #t.

(define (check expr)
  (if (not (eq? (eval expr (interaction-environment)) #t))
      (writeln "Error: test failed: " expr)))

; (check-all)
;    runs several tests on cut and reports.

(define (check-all)
  (for-each 
   check
   '( ; cuts
     (equal? ((cut list)) '())
     (equal? ((cut list <...>)) '())
     (equal? ((cut list 1)) '(1))
     (equal? ((cut list <>) 1) '(1))
     (equal? ((cut list <...>) 1) '(1))
     (equal? ((cut list 1 2)) '(1 2))
     (equal? ((cut list 1 <>) 2) '(1 2))
     (equal? ((cut list 1 <...>) 2) '(1 2))
     (equal? ((cut list 1 <...>) 2 3 4) '(1 2 3 4))
     (equal? ((cut list 1 <> 3 <>) 2 4) '(1 2 3 4))
     (equal? ((cut list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6))
     (equal? (let* ((x 'wrong) (y (cut list x))) (set! x 'ok) (y)) '(ok))
     (equal? 
      (let ((a 0))
        (map (cut + (begin (set! a (+ a 1)) a) <>)
            '(1 2))
        a)
      2)
      ; cutes
     (equal? ((cute list)) '())
     (equal? ((cute list <...>)) '())
     (equal? ((cute list 1)) '(1))
     (equal? ((cute list <>) 1) '(1))
     (equal? ((cute list <...>) 1) '(1))
     (equal? ((cute list 1 2)) '(1 2))
     (equal? ((cute list 1 <>) 2) '(1 2))
     (equal? ((cute list 1 <...>) 2) '(1 2))
     (equal? ((cute list 1 <...>) 2 3 4) '(1 2 3 4))
     (equal? ((cute list 1 <> 3 <>) 2 4) '(1 2 3 4))
     (equal? ((cute list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6))
     (equal? 
      (let ((a 0))
        (map (cute + (begin (set! a (+ a 1)) a) <>)
             '(1 2))
        a)
      1))))

; run the checks when loading
(check-all)
(writeln "Done.")
