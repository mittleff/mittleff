;;; (tests test-hello) --- Test hello.

;; Copyright (C) 2024 Victor Santos <victor_santos@fisica.ufc.br>
;;
;; This file is part of guile-hello.
;;
;; guile-hello is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-hello is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-hello. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Unit tests the hello module

;;; Code:

(define-module (tests test-builder)
  #:use-module (hello)
  #:use-module (srfi srfi-64)
  #:use-module (tests runner))

(test-runner-factory hello:test-runner)

(test-begin "test-hello")

(test-assert (string= "1234" "1234"))

(test-end "test-hello")


;;; (tests test-hello) ends here
