(define-module (tests tcase)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 binary-ports)
  #:export (test-file))

(define (test-file fname fn)
  (call-with-input-file fname
    (lambda (port)      
      (let loop ((line (read-line port)))
        (if (not (eof-object? line))
            (begin
              (unless (string= (substring line 0 1) "#")
                (let* ((params
                        (map string->number
                             (string-split line #\Space))))
                  (fn params)))
              (loop (read-line port))))))))
