(define-module (mittleff constants)
  #:export (pi
            J
            *taylor-radius*
            *default-precision*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants specific for the algorithm ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Taylor series upper limit
(define *taylor-radius* 0.95)
;; Default precision
(define *default-precision* 1.0e-15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematical constants ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pi 3.141592653589793238462643383279502884197169399375105820974944592307816406286)
(define J (make-rectangular 0 1))
