; Test suite for SRFI-17
; 2004-01-01 / lth

(use test)

(test-begin "srfi-17")

(test '(37 . 2)
      (let ((v (cons 1 2)))
        (set! (car v) 37)
        v))

(test '(1 . 37)
      (let ((v (cons 1 2)))
        (set! (cdr v) 37)
        v))

(test '((37 2) 3)
      (let ((v (list (list 1 2) 3)))
        (set! (caar v) 37)
        v))

(test '(1 37 3)
      (let ((v (list 1 2 3)))
        (set! (cadr v) 37)
        v))

(test '((1 . 37) 3)
      (let ((v (list (list 1 2) 3)))
        (set! (cdar v) 37)
        v))

(test '(1 2 . 37)
      (let ((v (list 1 2 3)))
        (set! (cddr v) 37)
        v))

; Oh, I grow weak...

(test '#(1 2 37)
      (let ((v (vector 1 2 3)))
        (set! (vector-ref v 2) 37)
        v))

(test "ab%"
      (let ((v "abc"))
        (set! (string-ref v 2) #\%) ; 37
        v))

; Test the ability to define our own setters.

(define (make-glarg x) (vector x))
(define (glarg-ref x) (vector-ref x 0))
(define (glarg-set! x y) (vector-set! x 0 y))

(set! (setter glarg-ref) glarg-set!)

(test 37 (let ((v (make-glarg 0)))
           (set! (glarg-ref v) 37)
           (glarg-ref v)))

; getter-with-setters

(define (make-blarg x) (vector x))
(define blarg-ref (getter-with-setter (lambda (x) (vector-ref x 0))
                                      (lambda (x y) (vector-set! x 0 y))))

(test 37 (let ((v (make-blarg 0)))
           (set! (blarg-ref v) 37)
           (blarg-ref v)))

(test-end "srfi-17")

(test-exit)
