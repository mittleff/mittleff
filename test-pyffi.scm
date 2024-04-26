(use-modules (pyffi)
             (ice-9 string-fun)
             (srfi srfi-1)
             (srfi srfi-69)
             (srfi srfi-64))

(define (test-alist-eq name alist1 alist2)
  (let ((sort-alist (lambda (v) (sort v (lambda (x y) (string<? (car x) (car y)))))))
    (test-equal name (sort-alist alist1) (sort-alist alist2))))

(test-begin "python-vm")

(test-group "test-interface"
  (test-assert "Is libpython*.so loaded?" ((@ (system foreign-library) foreign-library?)
                (@@ (pyffi interface) libpython)))
  (test-assert "Can we call libpython*.so functions?" (procedure? (@ (pyffi interface) pyproc))))

(test-group "python-vm-info"
  (test-assert "Is Python VM NOT initialized?" (not ((@@ (pyffi init) python-initialized?))))
  (python-initialize)
  (test-assert "Is Python VM initialized?" ((@@ (pyffi init) python-initialized?)))
  (pyimport sys)
  (test-assert "Major version is either 3 or 2?" (member (vector-ref #.sys.version-info 0) '(3 2)))
  (test-assert "sys.version is a string?" (string? #.sys.version))
  (format #t "Python version: ~a\n" #.sys.version)
  (python-finalize))

(test-group "python->scheme type conversion"
 (python-initialize)
 (test-eqv "Python integer converts to scheme?" 12345 (python-eval "12345"))
 ;; Pick a number that has an exact binary floating point representation.
 (test-eqv "Python float converts to scheme?" 0.25 (python-eval "0.25"))
 (test-eqv "Python inf converts to scheme?" +inf.0 (python-eval "float('inf')"))
 (test-eqv "Python -inf converts to scheme?" -inf.0 (python-eval "float('-inf')"))
 (test-assert "Python nan converts to scheme?" (nan? (python-eval "float('nan')")))
 (test-assert "Python string converts to scheme?" (string= "hello world" (python-eval "'hello ' + 'world'")))
 (test-eqv "Python complex converts to scheme?" 1.0+2.0i (python-eval "complex(1.0, 2.0)"))
 (test-equal "Python tuple converts to scheme vector?" #(10 20 30) (python-eval "(10, 20, 30)"))
 (test-equal "Python list converts to scheme?" '(10 20 30) (python-eval "[10, 20, 30]")) 
 (test-alist-eq "Python dict converts to scheme alist?" '(("a" . 1) ("b" . 2) ("c" . 3)) (hash-table->alist (python-eval "{'a': 1, 'b': 2, 'c': 3}")))
 (python-finalize))

(test-group "scheme->python type conversion"
  (python-initialize)
  (define (repr-compare rep x)
    (string= rep ((@@ (pyffi pyobject) python-pyrepr) (scm->python x))))
 (test-assert "scheme integer converted to python" (repr-compare "12345" 12345))
 ;; Pick a number that has an exact binary floating point representation.
 (test-assert "Scheme float converts to python" (repr-compare "0.25" 0.25))
 (test-assert "Scheme inf converts to python" (repr-compare "inf" +inf.0))
 (test-assert "Scheme inf converts to python" (repr-compare "-inf" -inf.0))
 (test-assert "Scheme nan converts to python" (repr-compare "nan" +nan.0))
 (test-assert "Scheme string converts to python" (repr-compare "'hello'" "hello"))
 (test-assert "Scheme complex converts to python" (repr-compare "(2+4j)" 2.0+4.0i))
 (test-assert "Scheme vector converts to python tuple" (repr-compare "(10, 20, 30)" #(10 20 30)))
 (test-assert "Scheme list converts to python" (repr-compare "[10, 20, 30]" '(10 20 30)))
 (test-alist-eq "Scheme alist converts to python dict" '(("a" . 1) ("b" . 2) ("c" . 3)) (hash-table->alist (python->scm (scm->python (alist->hash-table '(("a" . 1) ("b" . 2) ("c" . 3)))))))
 (python-finalize))

(test-end "python-vm")

