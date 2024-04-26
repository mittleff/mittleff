;;; (mittleff) --- Guile module for computing the Mittag-Leffler function.

;; Copyright (C) 2024 Victor Santos <victor_santos@fisica.ufc.br>
;;
;; This file is part of guile-mittleff
;;
;; guile-mittleff is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-mittleff is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-mittleff. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Guile module for computing the Mittag-Leffler function.

;;; Code:

(define-module (mittleff)
  #:use-module (mittleff mittleff))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (mittleff mittleff))

;;; (mittleff) ends here
