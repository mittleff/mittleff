(define-module (tests test-partition)
  #:use-module (mittleff partition)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (tests tcase)
  #:use-module (tests runner))

(test-runner-factory mittleff:test-runner)

(let* ((project-directory (getcwd))
       (tcases-directory (format #f "~a/tests/tcases/partition/" project-directory))
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
          (let* ((r0 (first v))
                 (a (second v))
                 (x (third v))
                 (y (fourth v))
                 (reg (fifth v))                 
                 (z (make-rectangular x y))
                 (prec 48))            
            (test-assert
                (format #f "[~a] ~4,8,2@e~4,8,2@ei in G~d"
                        fname (real-part z) (imag-part z) reg)
              (= reg (region? z a #:prec prec #:taylor-radius r0))))))
       (test-end test-group-name)
       (system* "find" "." "-iname"
                (format #f "~a.log" test-group-name)
                "-type" "f" "-empty" "-delete")))
   tcases-files))
