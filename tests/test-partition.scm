(define-module (tests test-partition)
  #:use-module (mittleff partition)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (tests tcase)
  #:use-module (tests runner))

(test-runner-factory mittleff:test-runner)

(define* (region z a #:key (acc 1e-15) (radius 0.95))
  (if (<= (magnitude z) radius)
      0
      (let ((r1 (compute-r1 a #:acc acc)))
        (if (>= (magnitude z) r1)
            ;; 1-4
            (cond
             ((in-region-G1? z a #:acc acc #:radius radius) 1)
             ((in-region-G2? z a #:acc acc #:radius radius) 2)
             ((in-region-G3? z a #:acc acc #:radius radius) 3)
             ((in-region-G4? z a #:acc acc #:radius radius) 4))
            ;; 5-6
            (cond
             ((in-region-G5? z a #:acc acc #:radius radius) 5)
             ((in-region-G6? z a #:acc acc #:radius radius) 6))))))

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
                 (eps 1e-15))            
            (test-assert
                (format #f "[~a] ~4,8,2@e~4,8,2@ei in G~d"
                        fname (real-part z) (imag-part z) reg)
              (= reg (region z a #:acc eps #:radius r0))))))
       (test-end test-group-name)
       (system* "find" "." "-iname"
                (format #f "~a.log" test-group-name)
                "-type" "f" "-empty" "-delete")))
   tcases-files))
