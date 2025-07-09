#!guile -s
!#

(use-modules (srfi srfi-1)
             (mittleff mittleff))

(let* ((args (map string->number (cdr (command-line))))
       (a (first args))
       (b (second args))
       (x (third args))
       (y (fourth args))
       (z (make-rectangular x y))
       (prec 53))
  (display (mittleff a b z #:prec prec))
  (newline))
