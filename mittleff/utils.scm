(define-module (mittleff utils)
  #:export (accuracy-from-prec))

(define (accuracy-from-prec prec)
  (expt 10 (- (ceiling (abs (log10 (expt 2 (- prec))))))))
