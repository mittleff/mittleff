(define-module (mittleff algorithm)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (mittleff sf)
  #:use-module (mittleff quad)
  #:use-module (mittleff macros)
  #:use-module (mittleff constants)
  #:use-module (mittleff partition)
  #:export (taylor
            asymptotics
            integral-rep))

(define* (series a b z #:key (acc *default-precision*) (asymptotic? #f))
  (let* ((kmin (if asymptotic? 1 0))
         (kmax (if asymptotic?
                   (inexact->exact (1+ (ceiling (* (/ 1 a) (expt (magnitude z) (/ 1 a))))))
                   (max
                    ;; k1
                    (inexact->exact (1+ (ceiling (abs (/ (- 1 b) a)))))
                    ;; k2
                    (inexact->exact (if (zero? (magnitude z)) 1
                                        (1+ (ceiling
                                             (magnitude
                                              (/ (log (* acc (1- (magnitude z))))
                                                 (log (magnitude z)))))))))))
         (s (if asymptotic? -1 +1)))
    (sum (lambda (k)
           (if (or (zero? (magnitude (expt z (* s k))))
                   (zero? (rgamma (+ (* a (* s k)) b))))
               0
               (* (expt z (* s k))
                  (rgamma (+ (* a (* s k)) b)))))
         from kmin to kmax)))

(define* (taylor a b z #:key (acc *default-precision*))
  (series a b z #:acc acc #:asymptotic? #f))

(define* (asymptotics a b z #:key (acc *default-precision*) (radius *taylor-radius*))
  (asymptotics-aux a b z #:region
                   (cond
                    ((in-region-G1? z a #:acc acc #:radius radius) 1)
                    ((in-region-G2? z a #:acc acc #:radius radius) 2)
                    ((in-region-G3? z a #:acc acc #:radius radius) 3)
                    ((in-region-G4? z a #:acc acc #:radius radius) 4)) #:acc acc))

(define* (asymptotics-aux a b z #:key (region 1) (acc *default-precision*))
  (define (c th)
    (sqrt (* 2 (make-rectangular (* 2 (expt (sin (/ th 2)) 2)) (- th (sin th))))))
  (define (rho i)
    (let ((theta (angle z)))
      (cond
       ((= i 1) 1)
       ((= i 2) 0)
       ((= i 3) (* 0.5 (erfc (* +1 (c theta) (sqrt (* 0.5 (expt (magnitude z) (/ a))))))))
       ((= i 4) (* 0.5 (erfc (* -1 (c theta) (sqrt (* 0.5 (expt (magnitude z) (/ a)))))))))))
  (let ((fac1 (/ (rho region) a))
        (fac2 (expt z (/ (- 1 b) a)))
        (fac3 (exp (expt z (/ a)))))
    (- (* fac1 fac2 fac3) (series a b z #:acc acc #:asymptotic? #t))))

(define (fn-a z a b x)
  (* (/ a) (expt z (/ (- 1 b) a)) (exp (* (expt z (/ a)) (cos (/ x a))))))

(define (omega x y a b)
  (+ (* (expt x (/ a)) (sin (/ y a))) (* y (+ 1 (/ (- 1 b) a)))))

(define (fn-b r a b z phi)
  (let ((w (omega r phi a b)))
    (* (/ pi) (fn-a r a b phi) (/ (- (* r (sin (- w phi))) (* z (sin w)))
                                  (+ (expt r 2) (* -2 r z (cos phi)) (expt z 2))))))

(define (fn-c ph a b z rho)
  (let ((w (omega rho ph a b)))
    (* (/ rho (* 2 pi)) (fn-a rho a b ph) (/ (exp (* J w)) (- (* rho (exp (* J ph))) z)))))

(define* (integral-rep a b z  #:key (region 5) (acc *default-precision*) (radius *taylor-radius*))
  (integral-rep-aux a b z #:region
                    (cond
                     ((in-region-G5? z a #:acc acc #:radius radius) 5)
                     ((in-region-G6? z a #:acc acc #:radius radius) 6)) #:acc acc))

(define* (integral-rep-aux a b z  #:key (region 5) (acc *default-precision*))
  (define (Phi i) (cond ((= i 5) (* pi a)) ((= i 6) (* pi a 2/3))))
  (define (Rho beta) (if (> beta 1) 1/2 0))
  (define (Rib i beta)
    (+ 0
       (cond
        ((and (>= b 0) (= i 5))
         (max 1 (* 2 (magnitude z)) (expt (- (log (* pi acc 1/6))) a)))
        ((and (< b 0) (= i 5))
         (max (expt (1+ (abs b)) a) (* 2 (magnitude z)) (expt (* -2 (log (/ (* pi acc) (* 6 (+ 2 (abs b)) (expt (* 2 (abs b)) (abs b)))))) a)))
        ((and (>= b 0) (= i 6))
         (max (expt 2 a) (* 2 (magnitude z)) (expt (* -2 (log (* pi acc 1/12 (expt 2 b)))) a)))
        ((and (< b 0) (= i 6))
         (max (expt (* 2 (+ 1 (abs b))) a) (* 2 (abs z)) (expt (* -4 (log (/ (* pi acc (expt 2 b)) (* 12 (+ 2 (abs b)) (expt (* 4 (abs b)) (abs b)))))) a))))))
  (let ((Ai (cond ((= region 5) (fn-a z a b 0)) ((= region 6) 0)))
        (Bi (quad (lambda (r) (fn-b r a b z (Phi region))) (Rho b) (Rib region b)))
        (Ci (if (> b 1) (quad (lambda (ph) (fn-c ph a b z 1/2)) (- (Phi region)) (Phi region)) 0)))
    (+ Ai Bi Ci)))

