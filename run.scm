#! /bin/sh
#|
exec csi -s $0 "$@"
|#

(use posix utils srfi-1)
(use (only setup-api program-path))

(define tests-dir (make-parameter "tests"))
(define debug? (make-parameter #f))
(define installation-prefix (make-parameter (pathname-directory (program-path))))
(define csc-options (make-parameter ""))
(define tests (make-parameter #f)) ;; list of symbols or #f (all tests)
(define skip-tests (make-parameter '())) ;; list of symbols
(define test-modes (make-parameter '(compiled interpreted)))


(define (all-tests)
  (map string->symbol
       (sort (map pathname-file
                  (glob (make-pathname (tests-dir) "*.scm")))
             string<)))


(define (csc)
  (make-pathname (and (installation-prefix)
                      (list (installation-prefix) "bin"))
                 "csc"))


(define (csi)
  (make-pathname (and (installation-prefix)
                      (list (installation-prefix) "bin"))
                 "csi"))


(define (run-shell-command command)
  ;; Returns (values <status> <output>)
  (let* ((p (open-input-pipe (string-append command " 2>&1")))
         (output (read-all p)))
    (when (debug?) (print "Running " command))
    (values (arithmetic-shift (close-input-pipe p) -8)
            output)))


(define (compile test)
  (run-shell-command (sprintf "~a ~a ~a" (csc) (csc-options) test)))


(define (interpret test)
  (system (sprintf "~a -s ~a" (csi) test)))


(define (run bin)
  (system (make-pathname "." bin)))


(define (display-env)
  (printf "Using ~a ~a\n" (csc) (csc-options))
  (newline))


(define (display-info action test)
  (display "################ ")
  (display (case action
             ((compile) "Compiling")
             ((interpret) "Running (interpreted)")
             ((run) "Running (compiled)")))
  (display " test: ")
  (display test)
  (newline)
  (flush-output))


(define (run-all)
  (let ((here (current-directory))
        (num-tests (length (tests))))
    (change-directory (tests-dir))
    (display-env)
    (let loop ((tests (tests)))
      (unless (null? tests)
        (let* ((test (car tests))
               (bin (pathname-strip-extension test)))
          (when (memq 'interpreted (test-modes))
            (display-info 'interpret test)
            (interpret test))
          (when (memq 'compiled (test-modes))
            (display-info 'compile bin)
            (let-values (((status output) (compile test)))
              (when (zero? status)
                (display-info 'run bin)
                (run bin)))))
        (loop (cdr tests))))
    (change-directory here)))



(define (usage #!optional exit-code)
  (printf "Usage: ~a [ config file ]\n"
          (pathname-strip-directory (program-name)) )
  (when exit-code (exit exit-code)))


(let ((args (command-line-arguments)))
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (unless (null? args) ;; load config file
    (load (car args)))

  ;; Determine programs to be run
  (tests (or (tests) (all-tests)))

  ;; Remove skipped tests
  (tests (if (null? skip-tests)
                (tests)
                (remove (lambda (test)
                          (memq test (skip-tests)))
                        (tests))))

  ;; Set the correct filename
  (tests (map (lambda (test)
                (make-pathname #f (->string test) "scm"))
              (tests)))


  (when (installation-prefix)
    (setenv "LD_LIBRARY_PATH" (make-pathname (installation-prefix) "lib")))

  (run-all))
