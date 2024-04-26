(define-module (tests test-mittleff)
  #:use-module (mittleff mittleff)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (tests tcase)
  #:use-module (tests runner)
  #:use-module (tests test-approx))

(test-runner-factory mittleff:test-runner)

(let* ((project-directory (getcwd))
       (tcases-directory (format #f "~a/tests/tcases/recursion/" project-directory))
       (fst (file-system-tree tcases-directory))
       (tcases-files (sort (map car (cddr fst)) string<)))
  (map
   (lambda (fname)
     (let* ((test-group-name
             (string-join (reverse (cdr (reverse (string-split fname #\.)))) "."))
            (file-path (format #f "~a~a" tcases-directory fname)))
       (test-begin test-group-name)
       (test-file
        file-path
        (lambda (v)
          (let* ((a (first v))
                 (b (second v))
                 (x (third v))
                 (y (fourth v))
                 (xx (fifth v))
                 (yy (sixth v))
                 (z (make-rectangular x y))
                 (expected (make-rectangular xx yy)))            
            (test-approx (format #f "[~a] ML(~a, ~a, ~4,8,3@e~4,8,3@ei) == ~4,8,3@e~4,8,3@ei"
                                 fname a b (real-part z) (imag-part z)
                                 (real-part expected) (imag-part expected))
                         expected (mittleff a b z #:acc 1e-15)))))
       (test-end test-group-name)
       (system* "find" "." "-iname"
                (format #f "~a.log" test-group-name)
                "-type" "f" "-empty" "-delete")))
   tcases-files))
