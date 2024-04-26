(define-module (mittleff erfc)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:export (erfc rgamma))

(define libsf (load-foreign-library
                  "build/libsf"
                  #:global? #t))

(define* (erfc x #:key (acc 1e-15))
  ((pointer->procedure
    complex-double
    (dynamic-func "ml_erfc" libsf)
    `(,complex-double ,double))
   x acc))

(define* (rgamma x #:key (acc 1e-15))
  ((pointer->procedure
    double (dynamic-func "ml_rgamma" libsf) `(,double ,double))
   x acc))
