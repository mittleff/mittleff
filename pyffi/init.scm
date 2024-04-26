(define-module (pyffi init)
  #:use-module (pyffi wrapper)
  #:use-module (system foreign))

(define *py-main-module* (make-parameter #f))
(define *py-main-module-dict* (make-parameter #f))
(define *py-functions* (make-hash-table))
(define +py-file-input+    257)
(define +py-single-input+  256)
(define +py-eval-input+    258)

(define (python-initialized?)
  (not (zero? (Py_IsInitialized))))

(define-public (python-initialize)
  (Py_Initialize)
  (*py-main-module* (PyImport_AddModule (string->pointer "__main__")))
  (*py-main-module-dict* (PyModule_GetDict (*py-main-module*)))
  (Py_IncRef (*py-main-module-dict*)))

(define-public (python-finalize)
  (when (python-initialized?)
    (Py_DecRef (*py-main-module-dict*))
    (*py-main-module* #f)
    (*py-main-module-dict* #f)
    (hash-clear! *py-functions*)
    (let ((status (Py_FinalizeEx)))
      (when (not (zero? status))
        (error "Error during finalization of Python VM")))))

(define pyerror (make-object-property))
(define (pyerror-exn x) (set! (pyerror 'message) x))
(define (raise-python-exception)
  (let* ((desc (pointer->string (PyErr_Occurred_toCString))))
    (PyErr_Clear)
    (error desc)
    (raise (pyerror-exn desc))))



;; (define (raise-python-exception)
;;   (let* ((desc (pointer->string (PyErr_Occurred_toCString))))
;;     (PyErr_Clear)
;;     (error desc)))

;; (define* (python-import name #:key (as #f))
;;   (let* ((Py_IncRef (libpyproc void "Py_IncRef" '(*)))
;;          (Py_DecRef (libpyproc void "Py_DecRef" '(*)))
;;          (PyImport_ImportModuleEx (libpyffiproc '* "pyffi_PyImport_ImportModuleEx" '(* * * *)))
;;          (PyModule_AddObject (libpyproc int "PyModule_AddObject" '(* * *)))
;;          (p (string-split name #\.))
;;          (id (if (null? p) name (car p))))
;;     (let ((m (PyImport_ImportModuleEx (string->pointer name) (*py-main-module-dict*) %null-pointer %null-pointer)))
;;       (if m
;;           (if (= -1 (PyModule_AddObject (*py-main-module*) (string->pointer id) m))
;;               (begin
;;                 (Py_DecRef m)
;;                 (raise-python-exception))
;;               (begin
;;                 (if as
;;                   (PyModule_AddObject (*py-main-module*) as m)
;;                   (Py_IncRef m))))
;;           (raise-python-exception)))))
