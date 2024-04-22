(define-module (tests test-approx)
  #:use-module (srfi srfi-64)
  #:export (test-approx))

;; From https://numpy.org/doc/stable/reference/generated/numpy.isclose.html
(define* (numpy-isclose a b #:key (atol 1e-8) (rtol 1e-5))
  (<= (magnitude (- a b)) (+ atol (* rtol (max (magnitude a) (magnitude b))))))

(define* (test-approx msg expected computed #:key (atol 1e-8) (rtol 1e-5))
  (test-assert
      (format #f "~a\n-> Expected: ~a\n-> Computed: ~a\n" msg expected computed)
    (numpy-isclose computed expected)))
