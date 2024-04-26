;; (pyffi pyobject) --- Class for PyObject* type

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

(define-module (pyffi pyobject)
  #:use-module (pyffi interface)
  #:use-module (pyffi wrapper)
  #:use-module (system foreign)
  #:use-module (oop goops)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-69)
  #:export (python->scm scm->python PyObjectPtr?
  wrap-PyObjectPtr
  unwrap-PyObjectPtr))

;; (define-wrapped-pointer-type PyObjectPtr
;;   PyObjectPtr?
;;   wrap-PyObjectPtr
;;   unwrap-PyObjectPtr
;;   (lambda (ptr port)
;;     (format
;;      port "#<PyObjectPtr 0x~x>"              
;;      (pointer-address (unwrap-PyObjectPtr ptr)))))

(define-wrapped-pointer-type <PyObjectPtr>
  PyObjectPtr?
  wrap-PyObjectPtr
  unwrap-PyObjectPtr
  (lambda (obj port)
    (let ((pyobj (unwrap-PyObjectPtr obj)))
      (format port "#<PyObject of [~a] ~x>"
              (python-pyrepr pyobj)
              (pointer-address pyobj)))))

(define <PyObject> (class-of (wrap-PyObjectPtr #nil)))

;; (define-class <PyObject> ()
;;   (PyObjectPtr #:init-form (wrap-PyObjectPtr %null-pointer)
;;                #:init-keyword #:ptr
;;                #:accessor ptr))

;; Instantiation of a class object
;; (define-public (make-pyobject p)
;;   (make <PyObject> #:ptr (wrap-PyObjectPtr p)))

;; (define-method (expose (self <PyObject>))
;;   (unwrap-PyObjectPtr (ptr self)))

;; (define-method (write (self <PyObject>) port)
;;   (format port "#<PyObject ptr=~a>" (ptr self)))

;; (define-method (display (self <PyObject>) port)
;;   (format port "~a" (python-pyrepr self)))

(define-public (python-pyrepr pyobj)
  (pointer->string
   (PyUnicode_AsUTF8
    (PyObject_Repr pyobj))))

(define-public (python-str pyobj)
  (pointer->string
    (PyUnicode_AsUTF8
      (PyObject_Str pyobj))))

(define (python-instance? pyobj type)
  (positive? 
    (PyObject_IsInstance
     pyobj (pyptr (format #f "Py~a_Type" type)))))

(define (python-callable? pyobj)
  (positive?
    (PyCallable_Check pyobj)))


(define (python-none? self)
  (= (pointer-address self)
     (pointer-address (pyptr "_Py_NoneStruct"))))

(define (python-fetch-error)
  (let ((type (scm->pointer (gensym)))
        (value (scm->pointer (gensym)))
        (traceback (scm->pointer (gensym))))
    (PyErr_Fetch type value traceback)))

;; (define* (python-apply pyobj . rest)
;;   (when (PyObject? pyobj)
;;     (let* ((PyObject_CallObject (pyproc '* "PyObject_CallObject" '(* *))))
;;       (display "haha")
;;       (make-pyobject (PyObject_CallObject (expose pyobj) (list->vector rest))))))

;;====================================================================
;; Scheme -> Python
;;====================================================================

;; integer
(define-method (scm->python (obj <integer>))
  (PyLong_FromLong obj))
;; real
(define-method (scm->python (obj <real>))
  (PyFloat_FromDouble obj))
;; complex
(define-method (scm->python (obj <complex>))
  (PyComplex_FromDoubles (real-part obj) (imag-part obj)))
;; string
(define-method (scm->python (obj <string>))
  (PyUnicode_FromString (string->pointer obj)))
;; boolean
(define-method (scm->python (obj <boolean>))
  (if (nil? obj)
      (pyptr "_Py_NoneStruct")
      (PyBool_FromLong (if obj 1 0))))
;; list
(define-method (scm->python (obj <list>))
  (let* ((n (length obj))
                        (pyobj (PyList_New n)))
                   (let loop ((index 0))
                     (if (< index n)
                         (begin
                           (PyList_SetItem
                            pyobj index
                            (scm->python (list-ref obj index)))
                           (loop (1+ index)))
                         pyobj))))
;; hash-table
(define <hash-table> (class-of (make-hash-table)))
(define-method (scm->python (obj <hash-table>))
  (let ((pyobj (PyDict_New)))
    (for-each
     (lambda (pair)
       (PyDict_SetItemString
        pyobj (string->pointer (car pair)) (scm->python (cdr pair))))
     (hash-table->alist obj))
    pyobj))
;; vector
(define-method (scm->python (obj <vector>))
  (PyList_AsTuple
   (scm->python (vector->list obj))))

(define-method (scm->python (obj <procedure>))
  (procedure-property obj #:pyobj))

(define-method (scm->python (obj <foreign>)) obj)
(define-method (scm->python (obj <PyObject>)) (unwrap-PyObjectPtr obj))

;; PyObject
;;(define-method (scm->python (obj <PyObject>)) obj)

;; TODO finish this
;; (define-method (display (self <PythonObject>) port)
;;   (format port "*~a*" (pointer self)))


;;====================================================================
;; Python -> Scheme
;;====================================================================

(define (pyint->scm pyobj) (PyLong_AsLong pyobj))

(define (pyfloat->scm pyobj) (PyFloat_AsDouble pyobj))

(define (pycomplex->scm pyobj)
  (make-rectangular
     (PyComplex_RealAsDouble pyobj)
     (PyComplex_ImagAsDouble pyobj)))

(define (pystr->scm pyobj)
  (pointer->string (PyUnicode_AsUTF8 pyobj)))

(define (pybool->scm pyobj)
  (not (zero? (PyObject_IsTrue pyobj))))

(define (pytuple->scm pyobj)
  (let* ((n (PyTuple_Size pyobj))
         (obj (make-vector n)))
    (let loop ((index 0))
      (if (< index n)
          (begin
            (vector-set!
             obj index
             (python->scm
              (PyTuple_GetItem pyobj index)))
            (loop (1+ index)))
          obj))))

(define (pylist->scm pyobj)
  (vector->list
   (pytuple->scm
    (PyList_AsTuple pyobj))))

(define (pydict->scm pyobj)
  (alist->hash-table   
     (map (lambda (x) (cons (car x) (car (cdr x))))
          (let* ((its (pylist->scm (PyDict_Items pyobj))))
            (let loop ((its its) (alst (list)))
	          (if (null? its) alst
	              (let ((item (car its)))
		            (let ((k (vector-ref item 0))
		                  (v (vector-ref item 1)))
		              (loop (cdr its) (cons (list k v) alst))))))))))

(define (python-call pyobj args kwargs)
  (PyObject_Call pyobj args kwargs))

(define (python-call-object pyobj args)
  (PyObject_CallObject pyobj args))

(define (pycallable->scm pyobj)
  (define* (proc #:key -. #:allow-other-keys #:rest rest)
    (let* ((kwargs-list (take-right rest (* 2 (count keyword? rest))))
           (kwargs (make-hash-table))
           (args (list->vector
                  (take rest (- (length rest) (length kwargs-list))))))
      (let loop ((n 0))
        (when (< n (count keyword? rest))
          (hash-table-set!
           kwargs
           (symbol->string
            (keyword->symbol
             (list-ref kwargs-list (* n 2))))
           (list-ref kwargs-list (1+ (* n 2))))
          (loop (1+ n))))
      (python->scm
       (python-call pyobj (scm->python args) (scm->python kwargs)))))
  (set-procedure-property! proc #:pyobj pyobj)
  (set-procedure-property!
   proc 'name (format #f "from python ~a" (python-pyrepr pyobj)))
  proc)

(define (python->scm pyobj)
  (cond
   ((null-pointer? pyobj)
    (error
     (let ((info (python-fetch-error)))
       (format #f "An exception of ~a reported by python:\n~a"
               (python-pyrepr (car info)) (python-str (cadr info))))))
   ((python-none? pyobj) #nil)
   ((python-callable? pyobj)           (pycallable->scm pyobj))
   ((python-instance? pyobj "Long")    (pyint->scm pyobj))
   ((python-instance? pyobj "Float")   (pyfloat->scm pyobj))
   ((python-instance? pyobj "Complex") (pycomplex->scm pyobj))
   ((python-instance? pyobj "Unicode") (pystr->scm pyobj))
   ((python-instance? pyobj "Bool")    (pybool->scm pyobj))
   ((python-instance? pyobj "List")    (pylist->scm pyobj))
   ((python-instance? pyobj "Tuple")   (pytuple->scm pyobj))
   ((python-instance? pyobj "Dict")    (pydict->scm pyobj))
   (#t (wrap-PyObjectPtr pyobj))))
