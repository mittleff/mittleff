;;; (pyffi interface) --- Main interface with shared objects.

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

;;; Commentary:

;; Allows to use Python function from its shared object.

;; Calling Python via the C API does not work for some libraries (such as math),
;; because lib-dynload/math.so (for example) cannot resolve symbols that are in
;; libpython*.so. A similiar situation is described in [1].
;;
;; The solution (or hack) is to load Python shared object with the flags
;; RTLD_GLOBAL, which allows the symbols defined in libpython*.so to be
;; propagated, therefore available, for symbol resolution of subsequently loaded
;; libraries [2].
;; 
;; To be able to load the library with this flag in Guile, we should use
;; `load-foreign-library` instead of `dynamic-link` for loading the shared
;; object [3].
;;
;; [1] https://mail.python.org/pipermail/new-bugs-announce/2008-November/003322.html
;; [2] https://linux.die.net/man/3/dlopen
;; [3] https://www.gnu.org/software/guile/manual/html_node/Foreign-Libraries.html#index-load_002dforeign_002dlibrary

;;; Code:

(define-module (pyffi interface)
  #:use-module (system foreign)
  #:use-module (system foreign-library))

(define libpython (load-foreign-library PYTHONSHAREDOBJ #:global? #t))

(define-public (pyproc type name args)
  (pointer->procedure type (dynamic-func name libpython) args))

(define-public (pyptr name)
  (dynamic-pointer name libpython))
