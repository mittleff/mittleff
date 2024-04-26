(use-modules (ice-9 readline)
             (ice-9 exceptions)
             (srfi srfi-1)
             (pyffi))

(python-initialize)
(pyimport sys)

(let* ((header (format #f "Python ~a on GNU Guile ~a" #.sys.version (version)))
       (n (string-length header))
       (filler (make-string (+ 4 n) #\#)))
  (format #t "~a\n# ~a #\n~a\n" filler header filler))

(let loop ()
  (let ((line (readline ">>> ")))
    (unless (eof-object? line)
      (with-exception-handler
          (lambda (_) (python-exec line))
        (lambda () (display (python-eval line)))
        #:unwind? #t)
      (newline)
      (loop))))
