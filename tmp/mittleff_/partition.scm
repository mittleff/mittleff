(define-module (mittleff partition)
  #:use-module (mittleff constants)
  #:use-module (srfi srfi-1)
  #:export (compute-r1
            in-region-G0?
            in-region-G1?
            in-region-G2?
            in-region-G3?
            in-region-G4?
            in-region-G5?
            in-region-G6?))

(define* (in-region-G1? z a #:key (acc *default-precision*) (radius *taylor-radius*))
  (let* ((delta (* pi a 0.125))
         (phi1 (+ (* -1 pi a) delta))
         (phi2 (- (* +1 pi a) delta)))
    (open-wedge? z phi1 phi2)))

(define* (in-region-G2? z a #:key (acc *default-precision*) (radius *taylor-radius*))
  (let* ((tau (min (* 0.125 pi a) (* 0.5 pi (1+ a))))
         (phi1 (+ (* +1 pi a) tau))
         (phi2 (- phi1)))
     (open-wedge? z phi1 phi2)))

(define* (in-region-G3? z a #:key (acc *default-precision*) (radius *taylor-radius*))
  (let* ((delta (* pi a 0.125))
         (tau (min (* 0.125 pi a) (* 0.5 pi (1+ a))))
         (phi1 (- (* pi a) delta))
         (phi2 (+ (* pi a) tau)))
    (closed-wedge? z phi1 phi2)))

(define* (in-region-G4? z a #:key (acc *default-precision*) (radius *taylor-radius*))
  (let* ((delta (* pi a 0.125))
         (tau (min (* 0.125 pi a) (* 0.5 pi (1+ a))))
         (phi1 (- (* -1 pi a) tau))
         (phi2 (+ (* -1 pi a) delta)))
     (closed-wedge? z phi1 phi2)))

(define* (in-region-G5? z a #:key (acc *default-precision*) (radius *taylor-radius*))
  (let* ((phi1 (* (/ -5 6) pi a))
         (phi2 (* (/ +5 6) pi a)))
    (and (closed-wedge? z phi1 phi2) (>= (magnitude z) radius))))

(define* (in-region-G6? z a #:key (acc *default-precision*) (radius *taylor-radius*))
  (let* ((phi1 (* (/ +5 6) pi a))
         (phi2 (* (/ -5 6) pi a)))
    (and (open-wedge? z phi1 phi2) (>= (magnitude z) radius))))

(define* (between z phi1 phi2 #:key (closed? #f))
  (define* (clockwise? a b #:key (closed? #f))
    (define* (cross x y) (sin (- x y)))
    (if closed?
        (>= (cross a b) 0)
        (>  (cross a b) 0)))
  (let ((c (angle z)))
     (if (clockwise? phi1 phi2)
      (not (between z phi2 phi1 #:closed? closed?))
      (and (not (clockwise? phi1 c #:closed? closed?))
           (clockwise? phi2 c #:closed? closed?)))))

(define (open-wedge? z phi1 phi2)
  (between z phi1 phi2 #:closed? #f))

(define (closed-wedge? z phi1 phi2)
  (between z phi1 phi2 #:closed? #t))

(define* (compute-r1 a #:key (acc *default-precision*))
  (let ((c0 (/ (expt 1.3 (- 1 a)) (* pi (sin (* pi a))))))
    (expt (* -2 (log (/ acc c0))) a)))
