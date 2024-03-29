; Test cases for SRFI-2
; 2004-01-07 / lth
;
; Taken from http://srfi.schemers.org/srfi-2/vland-gambit.scm
; No copyright notice in file.  Presumably written by Oleg Kiselyov.

; (cond-expand (srfi-2))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

;(##include "myenv.scm")
;(##include "catch-error.scm")

; make sure that the 'FORM' gave upon evaluation the
; EXPECTED-RESULT
; (define-macro (expect form expected-result)
;   `(begin
;     (display "evaluating ")
;     (write ',form)
;     (let ((real-result (eval ',form)))
;      (if (equal? real-result ,expected-result)
;        (cout "... gave the expected result: " real-result nl)
;        (error "... yielded: " real-result
;         " which differs from the expected result: " ,expected-result)
;       ))))

(define-syntax expect
  (syntax-rules ()
    ((expect form expected-result)
     (let ((real-result form))
       (if (not (equal? real-result expected-result))
	   (fail 'form))))))

; Check to see that 'form' has indeed a wrong syntax
; (define-macro (must-be-a-syntax-error form)
;   `(call-with-current-continuation
;     (lambda (k)
;       (##catch-signal '##signal.syntax-error
;         (lambda x 
;           (display "catching a syntax error: ") (display x) (newline)
;           (k #f))
;         (lambda ()
;           (eval ',form)
;           (error "No syntax error detected, unexpectedly"))))))

(expect  (and-let* () 1) 1)
(expect  (and-let* () 1 2) 2)
(expect  (and-let* () ) #t)

(expect (let ((x #f)) (and-let* (x))) #f)
(expect (let ((x 1)) (and-let* (x))) 1)
(expect (and-let* ((x #f)) ) #f)
(expect (and-let* ((x 1)) ) 1)
;(must-be-a-syntax-error (and-let* ( #f (x 1))) )
(expect (and-let* ( (#f) (x 1)) ) #f)
;(must-be-a-syntax-error (and-let* (2 (x 1))) )
(expect (and-let* ( (2) (x 1)) ) 1)
(expect (and-let* ( (x 1) (2)) ) 2)
(expect (let ((x #f)) (and-let* (x) x)) #f)
(expect (let ((x "")) (and-let* (x) x)) "")
(expect (let ((x "")) (and-let* (x)  )) "")
(expect (let ((x 1)) (and-let* (x) (+ x 1))) 2)
(expect (let ((x #f)) (and-let* (x) (+ x 1))) #f)
(expect (let ((x 1)) (and-let* (((positive? x))) (+ x 1))) 2)
(expect (let ((x 1)) (and-let* (((positive? x))) )) #t)
(expect (let ((x 0)) (and-let* (((positive? x))) (+ x 1))) #f)
(expect (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1)))  3)
;(must-be-a-syntax-error
;  (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1)))
;)

(expect (let ((x 1)) (and-let* (x ((positive? x))) (+ x 1))) 2)
(expect (let ((x 1)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))) 2)
(expect (let ((x 0)) (and-let* (x ((positive? x))) (+ x 1))) #f)
(expect (let ((x #f)) (and-let* (x ((positive? x))) (+ x 1))) #f)
(expect (let ((x #f)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))) #f)

(expect  (let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  (let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  (let ((x #f)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  (let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) (/ 3 2))

(writeln "Done.")
