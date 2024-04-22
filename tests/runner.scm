;; Test runner. Modified from guile-json:
;;
;;    https://github.com/aconchillo/guile-json

(define-module (tests runner)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:export (mittleff:test-runner))

(define (mittleff:test-runner)
  (let ((runner (test-runner-null))
        (logging-port #f))
    (test-runner-on-group-begin! runner
      (lambda (runner suite-name count)
        (format #t "# Testing ~a... " suite-name)
        (set! logging-port (open-file (format #f "~a.log" suite-name) "w"))
        (test-runner-aux-value! runner 1)))
    (test-runner-on-group-end! runner
      (lambda (runner)
        (close logging-port)
        (format #t "Done.\n")))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (when (eqv? 'fail (test-result-kind runner))
          (format logging-port "[~06d] ~a ~a\n"
                  (test-runner-aux-value runner)
                  (test-result-ref runner 'result-kind)
                  (test-runner-test-name runner))
          (force-output logging-port))        
        (test-runner-aux-value! runner (1+ (test-runner-aux-value runner)))
        (case (test-result-kind runner)
          ((fail xfail)
           (if (test-result-ref runner 'expected-value)
               (format #t "~a\n -> expected: ~a\n -> obtained: ~a\n"
                       (string-join (test-runner-group-path runner) "/")
                       (test-result-ref runner 'expected-value)
                       (test-result-ref runner 'actual-value))))
          (else #t))))
    (test-runner-on-final! runner
      (lambda (runner)        
        (format #t "pass = ~a, fail = ~a, score = ~1,1f%\n\n"
                (test-runner-pass-count runner)
                (test-runner-fail-count runner)
                (exact->inexact
                 (* 100 (/ (test-runner-pass-count runner)
                           (+ (test-runner-pass-count runner)
                              (test-runner-fail-count runner))))))))
    runner))
