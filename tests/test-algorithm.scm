(define-module (tests test-algorithm)
  #:use-module (mittleff algorithm)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (tests tcase)
  #:use-module (tests runner)
  #:use-module (tests test-approx))

(test-runner-factory mittleff:test-runner)

(let* ((project-directory (getcwd))
       (tcases-directory (format #f "~a/tests/tcases/algorithm/" project-directory))
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
          (let* ((r0 (first v))
                 (a (second v))
                 (b (third v))
                 (x (fourth v))
                 (y (fifth v))
                 (xx (sixth v))
                 (yy (seventh v))
                 (region (eighth v))
                 (z (make-rectangular x y))
                 (acc 1e-15)
                 (expected (make-rectangular xx yy)))            
            (test-approx (format #f "[~a] (G~a), R0=~a ML(~a, ~a, ~4,8,3@e~4,8,3@ei) == ~4,8,3@e~4,8,3@ei"
                                 fname region r0 a b (real-part z) (imag-part z) (real-part expected) (imag-part expected))
                         expected
                         (let ((fn (cond
                                    ((= 0 region) (lambda (a b z eps) (taylor a b z #:acc acc)))
                                    ((or (= 1 region) (= 2 region) (= 3 region) (= 4 region))
                                     (lambda (a b z eps) (asymptotics a b z #:acc eps)))
                                    ((or (= 5 region) (= 6 region))
                                     (lambda (a b z eps) (integral-rep a b z #:acc eps))))))
                           (fn a b z acc))))))
       (test-end test-group-name)
       (system* "find" "." "-iname"
                (format #f "~a.log" test-group-name)
                "-type" "f" "-empty" "-delete")))
   tcases-files))
