; Test suite for SRFI 61
;
; $Id: srfi-61-test.sch 6215 2009-05-06 00:19:51Z will $

; (cond-expand (srfi-61))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define (port->char-list port)
  (cond ((read-char port) char?
         => (lambda (c) (cons c (port->char-list port))))
        (else '())))

(or (equal? (port->char-list (open-input-string "abc def"))
            (string->list "abc def"))
    (fail 'port->char-list))

(writeln "Done.")
