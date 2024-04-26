(define-module (mittleff rgamma)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:export (rgamma))

(define librgamma (load-foreign-library
                  "build/librgamma"
                  #:global? #t))

(define* (rgamma x #:key (acc 1e-15))
  ((pointer->procedure
    double (dynamic-func "ml_rgamma" librgamma) `(,double ,double))
   x acc))
