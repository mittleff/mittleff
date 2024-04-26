(define-module (mittleff macros)
  #:use-module (srfi srfi-1)
  #:export (range
            sum))

(define* (range beg end #:optional (step 1))
  (map (lambda (i) (+ i beg)) (iota (1+ (- end beg)) 0 step)))

(define-syntax sum
  (syntax-rules (from to)
    ((sum body ... from beg to end )
     (fold + 0
           (map body ...
                (map
                 (lambda (i) (+ i beg))
                 (iota (1+ (- end beg)) 0)))))))
