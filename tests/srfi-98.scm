; Test suite for SRFI 98
;
; $Id: srfi-98-test.sch 6183 2009-04-10 21:03:12Z will $

;(cond-expand (srfi-98))
(use posix srfi-1)
(define for-all every)

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (string? (get-environment-variable "PATH"))
    (fail 'PATH))

(or (eq? #f (get-environment-variable "Unlikely To Be Any Such Thing"))
    (fail 'Unlikely))

(or (let ((alist (get-environment-variables)))
      (and (list? alist)
           (for-all pair? alist)
           (assoc "PATH" alist)))
    (fail 'get-environment-variables))

(writeln "Done.")

; eof
