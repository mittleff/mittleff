(use-modules (pyffi))

(python-initialize)

;; Needs:
;; $ pip install numpy

(python-initialize)
(pyimport numpy)

(define a (#.numpy.arange 5))
(display a) (newline)

(define b (#.numpy.exp a))
(display b) (newline)

(python-finalize)
