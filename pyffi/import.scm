(define-module (pyffi import)
  #:use-module (pyffi pyobject)
  #:use-module (pyffi wrapper)
  #:use-module (pyffi init)
  #:use-module (system foreign)
  #:use-module (ice-9 string-fun)
  #:export (pyimport))

(define* (python-import name #:key (as #f))
  (let* ((*py-main-module* (@@ (pyffi init) *py-main-module*))
         (env ((@@ (pyffi init) *py-main-module-dict*)))
         (p (string-split name #\.))
         (id (if (null? p) name (car p))))
    (let ((m (PyImport_ImportModuleEx (string->pointer name) env %null-pointer %null-pointer)))
      (if m
          (if (= -1 (PyModule_AddObject (*py-main-module*) (string->pointer id) m))
              (begin
                (Py_DecRef m)
                ((@@ (pyffi init) raise-python-exception)))
              (begin
                (if as
                  (PyModule_AddObject (*py-main-module*) (string->pointer as) m)
                  (Py_IncRef m))
                m))
          ((@@ (pyffi init) raise-python-exception))))))

(define (scm-name->py-name name)
  (string-replace-substring name "-" "_"))

(define-macro (pyimport module)
  `(define-public ,(string->symbol (car (string-split (symbol->string module) #\.)))
     ((@@ (pyffi import) python-import)  ,((@@ (pyffi import) scm-name->py-name) (symbol->string module)))))

(define (python-getattr-string pyobj attr)
  (PyObject_GetAttrString pyobj (string->pointer attr)))

(define (python-setattr-string pyobj attr value)
  (PyObject_SetAttrString pyobj (string->pointer attr) value))

(define (getattr obj . attrs)
  (for-each
    (lambda (attr)
      (let ((value (python->scm (python-getattr-string (scm->python obj) attr))))
        (set-object-property! value #:parent obj)
        (set-object-property! value #:name attr)
        ((@ (guile) set!) obj value)))
    (map scm-name->py-name attrs))
  (if (list? obj) `(,@obj) obj))

(define (read-python-syntax _ p)
  (let ((names (string-split (symbol->string (read p)) #\.)))
    `((@@ (pyffi import) getattr) ,(string->symbol (car names)) ,@(cdr names))))

(define (setattr obj value)
  (python-setattr-string
    (scm->python (object-property obj #:parent))
    (object-property obj #:name)
    (scm->python value)))

(define-syntax-rule (set! obj value)
  (let ((parent (object-property obj #:parent))
        (name (object-property obj #:name)))
    (if (and parent name)
        ((@@ (pyffi import) setattr) obj value)
        ((@ (guile) set!) obj value))))
 
(read-hash-extend #\. read-python-syntax)

;; (define (getattr obj . attrs)
;;   (for-each
;;    (lambda (attr)
;;      (let ((value (python->scm (python-getattr-string (scm->python obj) attr))))
;;        (set-object-property! value #:parent obj)
;;        (set-object-property! value #:name attr)
;;        ((@ (guile) set!) obj value)))
;;    (map scm-name->py-name attrs))
;;   (if (list? obj) `(,@obj) obj))

;; (define (read-python-syntax _ p)
;;   (let ((names (string-split (symbol->string (read p)) #\.)))
;;     (display p)
;;     `((@@ (pyffi import) getattr) ,(string->symbol (car names)) ,@(cdr names))))

;; (read-hash-extend #\. read-python-syntax)



;; (define-macro (pyimport module)
;;   `(define-public ,(string->symbol (car (string-split (symbol->string module) #\.)))
;;        ((@ (pyffi eval) python-eval) 
;;          (format #f "__import__(\"~a\")"
;;            ,((@@ (pyffi import) scm-name->py-name) (symbol->string module))))))
