(define-module (tests test-quad)
  #:use-module (mittleff quad)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (tests tcase)
  #:use-module (tests runner)
  #:use-module (tests test-approx))


(test-runner-factory mittleff:test-runner)

(let* ((fn (vector (lambda (x) (sin x))))
       (project-directory (getcwd))
       (tcases-directory (format #f "~a/tests/tcases/quad/" project-directory))
       (fst (file-system-tree tcases-directory))
       (tcases-files (sort (map car (cddr fst)) string<)))
  (map
   (lambda (fname)
     (let ((test-group-name
             (string-join (reverse (cdr (reverse (string-split fname #\.)))) "."))
           (file-path (format #f "~a~a" tcases-directory fname)))
       (test-begin test-group-name)
       (test-file
        file-path
        (lambda (v)
          (let* ((idx (first v))
                 (a (second v))
                 (b (third v))
                 (expected (fourth v)))
            (test-approx (format #f "[~a] I_~a = ~a" fname idx expected)
                         expected
                         (quad (vector-ref fn (- idx 1)) a b)))))
       (test-end test-group-name)
       (system* "find" "." "-iname"
                (format #f "~a.log" test-group-name)
                "-type" "f" "-empty" "-delete")))
   tcases-files))
