(define-module (mittleff erfc)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:export (erfc))

(define liberfc (load-foreign-library
                  "build/liberfc"
                  #:global? #t))

(define* (erfc x #:key (acc 1e-15))
  ((pointer->procedure
   complex-double
   (dynamic-func "ml_erfc" liberfc)
   `(,complex-double ,double))
   x acc))
