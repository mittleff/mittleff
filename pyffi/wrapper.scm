;; (pyffi wrapper) --- Wraps functions from Python C API.

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

;; Defines class encapsulating PyObject* type from Python C API [1].
;;
;; [1] https://docs.python.org/3/c-api/structures.html#c.PyObject

;;; Code:

(define-module (pyffi wrapper)
  #:use-module (pyffi interface)
  #:use-module (system foreign)
  #:use-module (system foreign-library))

(define libpyffi (load-foreign-library
                  "libpyffi"
                  #:global? #t))

(define (pyffiproc type name args)
  (pointer->procedure type (dynamic-func name libpyffi) args))

(define-public PyObject_Repr (pyproc '* "PyObject_Repr" '(*)))
(define-public PyObject_Str (pyproc '* "PyObject_Repr" '(*)))
(define-public PyObject_IsInstance (pyproc int "PyObject_IsInstance" '(* *)))
(define-public PyErr_Fetch (pyproc void "PyErr_Fetch" '(* * *)))
(define-public PyCallable_Check (pyproc int "PyCallable_Check" '(*)))
(define-public PyErr_Clear (pyproc void "PyErr_Clear" '()))
(define-public PyErr_Occurred_toCString (pyffiproc '* "pyffi_PyErr_Occurred_toCString" '()))

(define-public PyObject_Call (pyproc '* "PyObject_Call" '(* * *)))
(define-public PyObject_CallObject (pyproc '* "PyObject_CallObject" '(* *)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization, Finalization, and Threads ;;
;;                                           ;;
;; https://docs.python.org/3/c-api/init.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public Py_IsInitialized (pyproc int "Py_IsInitialized" '()))
(define-public Py_Initialize (pyproc void "Py_Initialize" '()))
(define-public Py_FinalizeEx (pyproc int "Py_FinalizeEx" '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Importing Modules                           ;;
;;                                             ;;
;; https://docs.python.org/3/c-api/import.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public PyImport_ImportModule (pyproc '* "PyImport_ImportModule" '(*)))
(define-public PyImport_AddModule (pyproc '* "PyImport_AddModule" '(*)))
(define-public PyImport_ImportModuleEx (pyffiproc '* "pyffi_PyImport_ImportModuleEx" '(* * * *)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module Objects                              ;;
;;                                             ;;
;; https://docs.python.org/3/c-api/module.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public PyModule_GetDict (pyproc '* "PyModule_GetDict" '(*)))
(define-public PyModule_AddObject (pyproc int "PyModule_AddObject" '(* * *)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reference Counting                               ;;
;;                                                  ;;
;; https://docs.python.org/3/c-api/refcounting.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public Py_IncRef (pyproc void "Py_IncRef" '(*)))
(define-public Py_DecRef (pyproc void "Py_DecRef" '(*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integer Objects                           ;;
;;                                           ;;
;; https://docs.python.org/3/c-api/long.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public PyLong_FromLong (pyproc '* "PyLong_FromLong" `(,long)))
(define-public PyLong_AsLong (pyproc long "PyLong_AsLong" '(*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating Point Objects                     ;;
;;                                            ;;
;; https://docs.python.org/3/c-api/float.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public PyFloat_FromDouble (pyproc '* "PyFloat_FromDouble" `(,double)))
(define-public PyFloat_AsDouble (pyproc double "PyFloat_AsDouble" '(*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complex Number Objects                       ;;
;;                                              ;;
;; https://docs.python.org/3/c-api/complex.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public PyComplex_FromDoubles (pyproc '* "PyComplex_FromDoubles" `(,double ,double)))
(define-public PyComplex_RealAsDouble (pyproc double "PyComplex_RealAsDouble" '(*)))
(define-public PyComplex_ImagAsDouble (pyproc double "PyComplex_ImagAsDouble" '(*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unicode Objects and Codecs                   ;;
;;                                              ;;
;; https://docs.python.org/3/c-api/unicode.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public PyUnicode_FromString (pyproc '* "PyUnicode_FromString" '(*)))
(define-public PyUnicode_AsUTF8 (pyproc '* "PyUnicode_AsUTF8" '(*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boolean Objects                           ;;
;;                                           ;;
;; https://docs.python.org/3/c-api/bool.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public PyBool_FromLong (pyproc '* "PyBool_FromLong" `(,long)))
(define-public PyObject_IsTrue (pyproc int "PyObject_IsTrue" `(*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List Objects                              ;;
;;                                           ;;
;; https://docs.python.org/3/c-api/list.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public PyList_New (pyproc '* "PyList_New" `(,int)))
(define-public PyList_SetItem (pyproc int "PyList_SetItem" `(* ,int *)))
(define-public PyList_AsTuple (pyproc '* "PyList_AsTuple" '(*)))
(define-public PyTuple_GetItem (pyproc '* "PyTuple_GetItem" `(* ,int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PyTuple_Size                               ;;
;;                                            ;;
;; https://docs.python.org/3/c-api/tuple.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public PyTuple_Size (pyproc int "PyTuple_Size" '(*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionary Objects                        ;;
;;                                           ;;
;; https://docs.python.org/3/c-api/dict.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public PyDict_New (pyproc '* "PyDict_New" '()))
(define-public PyDict_SetItemString (pyproc int "PyDict_SetItemString" '(* * *)))
(define-public PyDict_Items (pyproc '* "PyDict_Items" '(*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Very High Level Layer                     ;;
;;                                               ;;
;; https://docs.python.org/3/c-api/veryhigh.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public PyRun_String (pyproc '* "PyRun_String" `(* ,int * *)))
(define-public PyRun_SimpleString (pyproc int "PyRun_SimpleString" '(*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract Objects Layer > Object Protocol    ;;
;;                                             ;;
;; https://docs.python.org/3/c-api/object.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public PyObject_GetAttrString (pyproc '* "PyObject_GetAttrString" '(* *)))
(define-public PyObject_SetAttrString (pyproc int "PyObject_SetAttrString" '(* * *)))
