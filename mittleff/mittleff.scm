(define-module (mittleff mittleff)
  #:use-module (mittleff macros)
  #:use-module (mittleff constants)
  #:use-module (mittleff partition)
  #:use-module (mittleff algorithm)
  #:export (mittleff))

;;;;;;;;;;;;;;;;;;;
;; Main function ;;
;;;;;;;;;;;;;;;;;;;
(define* (mittleff alpha beta z #:key (prec *default-precision*) (radius *taylor-radius*))
  (if (<= (magnitude z) radius)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Evaluate the Taylor Series ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (taylor alpha beta z #:prec prec)
      (if (> alpha 1)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Apply the Recursion Relation ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (let* ((m (1+ (ceiling (/ (- alpha 1) 2))))
                 (one-over-2mp1 (/ 1 (1+ (* 2 m))))
                 (a (* alpha one-over-2mp1)))
            (* one-over-2mp1
               (sum
                (lambda (l)
                  (let ((zz (* (expt z one-over-2mp1) (exp (* 2 pi J l one-over-2mp1)))))
                    (mittleff-aux a beta zz #:prec prec #:radius radius)))
                from (- m) to m)))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Apply the main algorithm for alpha <= 1 ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (mittleff-aux alpha beta z #:prec prec #:radius radius))))

(define* (mittleff-aux alpha beta z #:key (prec *default-precision*) (radius *taylor-radius*))
  (let ((r1 (compute-r1 alpha #:prec prec)))
    (if (>= (magnitude z) r1)
        ;; 1-4
        (asymptotics alpha beta z #:prec prec)
        ;; 5-6
        (integral-rep alpha beta z #:prec prec))))
