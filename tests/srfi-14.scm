; Test suite for SRFI-14
; 2004-01-02 / lth
;
; Includes Olin Shivers's regression test suite for SRFI-14 (at end),
; with one bug fix to that test suite.

; (cond-expand (srfi-14))
(use srfi-14)
(use (only srfi-1 remove))
(define remv delete)

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (equal? #t (char-set? (char-set)))
    (fail 'char-set?:1))
(or (equal? #t (char-set? (string->char-set "abcde")))
    (fail 'char-set?:2))
(or (equal? #f (char-set? 37))
    (fail 'char-set?:3))
(or (equal? #f (char-set? "abcde"))
    (fail 'char-set?:4))

(or (equal? #t (char-set= (char-set) (char-set)))
    (fail 'char-set=:1))
(or (equal? #t (char-set= (char-set #\a #\b #\c) (char-set #\c #\b #\a)))
    (fail 'char-set=:2))
(or (equal? #f (char-set= (char-set #\a #\b #\c) (char-set #\c #\d #\a)))
    (fail 'char-set=:3))
(or (equal? #t (char-set=))
    (fail 'char-set=:4))
(or (equal? #t (char-set= (char-set)))
    (fail 'char-set=:5))

(or (equal? #t (char-set<= (char-set) (char-set)))
    (fail 'char-set<=:1))
(or (equal? #t (char-set<= (char-set #\a #\b) (char-set #\a #\b #\c)))
    (fail 'char-set<=:2))
(or (equal? #f (char-set<= (char-set #\a #\b #\c) (char-set #\a #\b)))
    (fail 'char-set<=:3))
(or (equal? #t (char-set<=))
    (fail 'char-set<=:4))
(or (equal? #t (char-set<= (char-set)))
    (fail 'char-set<=:5))

(or (let ((h (char-set-hash (char-set #\a #\b #\c) 3737)))
      (and (<= 0 h) (< h 3737)))
    (fail 'char-set-hash:1))
(or (equal? (char-set-hash (char-set #\a #\b #\c))
	    (char-set-hash (char-set #\b #\c #\a)))
    (fail 'char-set-hash:2))

(or (equal? '(#\G #\T #\a #\c #\e #\h)
	    (let ((cs (char-set #\G #\a #\T #\e #\c #\h)))
	      (let lp ((cur (char-set-cursor cs)) (ans '()))
		(if (end-of-char-set? cur) ans
		    (lp (char-set-cursor-next cs cur)
			(cons (char-set-ref cs cur) ans))))))
    (fail 'char-set-cursor:1))

(or (let ((ms (char-set-fold cons '() (char-set #\a #\b #\c #\T))))
      (and (memv #\a ms)
	   (memv #\b ms)
	   (memv #\c ms)
	   (memv #\T ms)
	   (= (length ms) 4)))
    (fail 'char-set-fold:1))

(or (char-set= (char-set-unfold null? car cdr (string->list "abracadabra"))
	       (string->char-set "abracadabra"))
    (fail 'char-set-unfold:1))
(or (char-set= (char-set-unfold null? car cdr (string->list "abracadabra") (char-set #\f))
	       (string->char-set "abracadabraf"))
    (fail 'char-set-unfold:2))
(or (char-set= (char-set-unfold! null? car cdr (string->list "abracadabra") (char-set #\f))
	       (string->char-set "abracadabraf"))
    (fail 'char-set-unfold!:1))

(or (let ((chars (string->list "fnord")))
      (null? (begin
	       (char-set-for-each (lambda (c)
				    (if (not (memv c chars))
					(fail 'char-set-for-each:0))
				    (set! chars (remv c chars)))
				  (list->char-set chars))
	       chars)))
    (fail 'char-set-for-each:1))

(or (let ((chars (string->list "fnord")))
      (let ((newchars (char-set-map (lambda (c)
				      (if (not (memv c chars))
					  (fail 'char-set-map:0))
				      (set! chars (remv c chars))
				      c)
				    (list->char-set chars))))
	(char-set= (string->char-set "fnord") newchars)))
    (fail 'char-set-map:1))

; ...

; Shivers's tests.

(let-syntax ((test (syntax-rules ()
		     ((test form ...)
		      (cond ((begin
			       ;(writeln 'form)
			       (not form))
			     (fail "Test failed" 'form)) ...
			    (else 'OK))))))
  (let ((vowel? (lambda (c) 
		  (member c '(#\a #\e #\i #\o #\u)))))
    (test
     (not (char-set? 5))

     (char-set? (char-set #\a #\e #\i #\o #\u))

     (char-set=)
     (char-set= (char-set))

     (char-set= (char-set #\a #\e #\i #\o #\u)
		(string->char-set "ioeauaiii"))

     (not (char-set= (char-set #\e #\i #\o #\u)
		     (string->char-set "ioeauaiii")))

     (char-set<=)
     (char-set<= (char-set))

     (char-set<= (char-set #\a #\e #\i #\o #\u)
		 (string->char-set "ioeauaiii"))

     (char-set<= (char-set #\e #\i #\o #\u)
		 (string->char-set "ioeauaiii"))

     (<= 0 (char-set-hash char-set:graphic 100) 99)

     (= 4 (char-set-fold (lambda (c i) (+ i 1)) 0
			 (char-set #\e #\i #\o #\u #\e #\e)))

     (char-set= (string->char-set "eiaou2468013579999")
		(char-set-unfold null? car cdr '(#\a #\e #\i #\o #\u #\u #\u)
				 char-set:digit))

     (char-set= (string->char-set "eiaou246801357999")
		(char-set-unfold! null? car cdr '(#\a #\e #\i #\o #\u)
				  (string->char-set "0123456789")))

     (not (char-set= (string->char-set "eiaou246801357")
		     (char-set-unfold! null? car cdr '(#\a #\e #\i #\o #\u)
				       (string->char-set "0123456789"))))

     (let ((cs (string->char-set "0123456789")))
       (char-set-for-each (lambda (c) (set! cs (char-set-delete cs c)))
			  (string->char-set "02468000"))
       (char-set= cs (string->char-set "97531")))

     (not (let ((cs (string->char-set "0123456789")))
	    (char-set-for-each (lambda (c) (set! cs (char-set-delete cs c)))
			       (string->char-set "02468"))
	    (char-set= cs (string->char-set "7531"))))

     (char-set= (char-set-map char-upcase (string->char-set "aeiou"))
		(string->char-set "IOUAEEEE"))

     (not (char-set= (char-set-map char-upcase (string->char-set "aeiou"))
		     (string->char-set "OUAEEEE")))

     (char-set= (char-set-copy (string->char-set "aeiou"))
		(string->char-set "aeiou"))

     (char-set= (char-set #\x #\y) (string->char-set "xy"))
     (not (char-set= (char-set #\x #\y #\z) (string->char-set "xy")))

     (char-set= (string->char-set "xy") (list->char-set '(#\x #\y)))
     (not (char-set= (string->char-set "axy") (list->char-set '(#\x #\y))))

     (char-set= (string->char-set "xy12345")
		(list->char-set '(#\x #\y) (string->char-set "12345")))
     (not (char-set= (string->char-set "y12345")
		     (list->char-set '(#\x #\y) (string->char-set "12345"))))

     (char-set= (string->char-set "xy12345")
		(list->char-set! '(#\x #\y) (string->char-set "12345")))
     (not (char-set= (string->char-set "y12345")
		     (list->char-set! '(#\x #\y) (string->char-set "12345"))))

     (char-set= (string->char-set "aeiou12345")
		(char-set-filter vowel? char-set:ascii (string->char-set "12345")))
     (not (char-set= (string->char-set "aeou12345")
		     (char-set-filter vowel? char-set:ascii (string->char-set "12345"))))

     (char-set= (string->char-set "aeiou12345")
		(char-set-filter! vowel? char-set:ascii (string->char-set "12345")))
     (not (char-set= (string->char-set "aeou12345")
		     (char-set-filter! vowel? char-set:ascii (string->char-set "12345"))))


     (char-set= (string->char-set "abcdef12345")
		(ucs-range->char-set 97 103 #t (string->char-set "12345")))
     (not (char-set= (string->char-set "abcef12345")
		     (ucs-range->char-set 97 103 #t (string->char-set "12345"))))

     (char-set= (string->char-set "abcdef12345")
		(ucs-range->char-set! 97 103 #t (string->char-set "12345")))
     (not (char-set= (string->char-set "abcef12345")
		     (ucs-range->char-set! 97 103 #t (string->char-set "12345"))))


     (char-set= (->char-set #\x)
		(->char-set "x")
		(->char-set (char-set #\x)))

     (not (char-set= (->char-set #\x)
		     (->char-set "y")
		     (->char-set (char-set #\x))))

     (= 10 (char-set-size (char-set-intersection char-set:ascii char-set:digit)))

     (= 5 (char-set-count vowel? char-set:ascii))

     (equal? '(#\x) (char-set->list (char-set #\x)))
     (not (equal? '(#\X) (char-set->list (char-set #\x))))

     (equal? "x" (char-set->string (char-set #\x)))
     (not (equal? "X" (char-set->string (char-set #\x))))

     (char-set-contains? (->char-set "xyz") #\x)
     (not (char-set-contains? (->char-set "xyz") #\a))

     (char-set-every char-lower-case? (->char-set "abcd"))
     (not (char-set-every char-lower-case? (->char-set "abcD")))
     (char-set-any char-lower-case? (->char-set "abcd"))
     (not (char-set-any char-lower-case? (->char-set "ABCD")))

     (char-set= (->char-set "ABCD")
		(let ((cs (->char-set "abcd")))
		  (let lp ((cur (char-set-cursor cs)) (ans '()))
		    (if (end-of-char-set? cur) (list->char-set ans)
			(lp (char-set-cursor-next cs cur)
			    (cons (char-upcase (char-set-ref cs cur)) ans))))))


     (char-set= (char-set-adjoin (->char-set "123") #\x #\a)
		(->char-set "123xa"))
     (not (char-set= (char-set-adjoin (->char-set "123") #\x #\a)
		     (->char-set "123x")))
     (char-set= (char-set-adjoin! (->char-set "123") #\x #\a)
		(->char-set "123xa"))
     (not (char-set= (char-set-adjoin! (->char-set "123") #\x #\a)
		     (->char-set "123x")))

     (char-set= (char-set-delete (->char-set "123") #\2 #\a #\2)
		(->char-set "13"))
     (not (char-set= (char-set-delete (->char-set "123") #\2 #\a #\2)
		     (->char-set "13a")))
     (char-set= (char-set-delete! (->char-set "123") #\2 #\a #\2)
		(->char-set "13"))
     (not (char-set= (char-set-delete! (->char-set "123") #\2 #\a #\2)
		     (->char-set "13a")))

     (char-set= (char-set-intersection char-set:hex-digit (char-set-complement char-set:digit))
		(->char-set "abcdefABCDEF"))
     (char-set= (char-set-intersection! (char-set-complement! (->char-set "0123456789"))
					char-set:hex-digit)
		(->char-set "abcdefABCDEF"))

     (char-set= (char-set-union char-set:hex-digit
				(->char-set "abcdefghijkl"))
		(->char-set "abcdefABCDEFghijkl0123456789"))
     (char-set= (char-set-union! (->char-set "abcdefghijkl")
				 char-set:hex-digit)
		(->char-set "abcdefABCDEFghijkl0123456789"))

     (char-set= (char-set-difference (->char-set "abcdefghijklmn")
				     char-set:hex-digit)
		(->char-set "ghijklmn"))
     (char-set= (char-set-difference! (->char-set "abcdefghijklmn")
				      char-set:hex-digit)
		(->char-set "ghijklmn"))

     (char-set= (char-set-xor (->char-set "0123456789")
			      char-set:hex-digit)
		(->char-set "abcdefABCDEF"))
     (char-set= (char-set-xor! (->char-set "0123456789")
			       char-set:hex-digit)
		(->char-set "abcdefABCDEF"))

     (call-with-values (lambda ()
			 (char-set-diff+intersection char-set:hex-digit
						     char-set:letter))
       (lambda (d i)
	 (and (char-set= d (->char-set "0123456789"))
	      (char-set= i (->char-set "abcdefABCDEF")))))

     (call-with-values (lambda ()
			 (char-set-diff+intersection! (char-set-copy char-set:hex-digit)
						      (char-set-copy char-set:letter)))
       (lambda (d i)
	 (and (char-set= d (->char-set "0123456789"))
	      (char-set= i (->char-set "abcdefABCDEF"))))))

    ))

(writeln "Done.")
