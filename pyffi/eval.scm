;;; (pyffi eval) --- Evaluating Python expressions.

;; Copyright (C) 2020-2024 Victor Santos <victor_santos@fisica.ufc.br>
;;
;; This file is part of guile-pyffi.
;;
;; guile-pyffi is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-pyffi is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-pyffi. If not, see https://www.gnu.org/licenses/.

;;; Code:

(define-module (pyffi eval)
  #:use-module (pyffi wrapper)
  #:use-module (pyffi pyobject)
  #:use-module (pyffi init)
  #:use-module (system foreign))

(define (python-run-simple-string cmd)
  (let ((cmdptr (string->pointer cmd)))
    (zero? (PyRun_SimpleString cmdptr))))

(define (python-run-string cmd)
  (let ((cmdptr (string->pointer cmd))
        (env ((@@ (pyffi init) *py-main-module-dict*))))
    (PyRun_String cmdptr (@@ (pyffi init) +py-eval-input+) env env)))

(define-public (python-eval code)
  (python->scm (python-run-string code)))

(define-public (python-exec code)
  (python->scm
    (scm->python (python-run-simple-string code))))

;; (define (python-import-module name)
;;   ((libpyproc '* "PyImport_ImportModule" '(*))
;;     (string->pointer name)))

;; (define (python-run-string code)
;;   (let ((env ((libpyproc '* "PyModule_GetDict" '(*))
;;               (python-import-module "__main__")))) 
;;     (wrap ((libpyproc '* "PyRun_String" `(* ,int * *))
;;      (string->pointer code) 258 env env))))

;; (define (python-eval code)
;;   (python->scm (python-run-string code)))
