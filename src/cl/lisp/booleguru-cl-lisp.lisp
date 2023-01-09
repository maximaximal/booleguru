(in-package cl-user)

(ffi:clines "#include <booleguru/cl/cl-function-c-wrappers.h>")

(defun b-var (name)
  (ffi:c-inline (name) (:cstring) :uint32-t "booleguru_cl_varop(#0)" :one-liner t))

(defun b-var-rename (op old new)
  (ffi:c-inline (op old new) (:uint32-t :cstring :cstring) :uint32-t "booleguru_cl_var_rename(#0, #1, #2)" :one-liner t))

(defun b-vars-prefix (op prefix)
  (ffi:c-inline (op prefix) (:uint32-t :cstring) :uint32-t "booleguru_cl_vars_prefix(#0, #1)" :one-liner t))

(defun b-vars-postfix (op postfix)
  (ffi:c-inline (op postfix) (:uint32-t :cstring) :uint32-t "booleguru_cl_vars_postfix(#0, #1)" :one-liner t))

(defun b-vars-wrap (op prefix postfix)
  (ffi:c-inline (op prefix postfix) (:uint32-t :cstring :cstring) :uint32-t "booleguru_cl_vars_wrap(#0, #1, #2)" :one-liner t))

(defun b-and (a b)
  (ffi:c-inline (a b) (:uint32-t :uint32-t) :uint32-t "booleguru_cl_and(#0, #1)" :one-liner t))

(defun b-or (a b)
  (ffi:c-inline (a b) (:uint32-t :uint32-t) :uint32-t "booleguru_cl_or(#0, #1)" :one-liner t))

(defun b-not (a)
  (ffi:c-inline (a) (:uint32-t) :uint32-t "booleguru_cl_not(#0)" :one-liner t))

(defun b-equi(a b)
  (ffi:c-inline (a b) (:uint32-t :uint32-t) :uint32-t "booleguru_cl_equi(#0, #1)" :one-liner t))

(defun distribute-to-cnf (a)
  (ffi:c-inline (a) (:uint32-t) :uint32-t "booleguru_cl_distribute_to_cnf(#0)" :one-liner t))

(defun distribute-or (a)
  (ffi:c-inline (a) (:uint32-t) :uint32-t "booleguru_cl_distribute_or(#0)" :one-liner t))

(defun distribute-not (a)
  (ffi:c-inline (a) (:uint32-t) :uint32-t "booleguru_cl_distribute_not(#0)" :one-liner t))

(defun distribute-implication (a)
  (ffi:c-inline (a) (:uint32-t) :uint32-t "booleguru_cl_distribute_implication(#0)" :one-liner t))

(defun b-print (op)
  (ffi:c-inline (op) (:uint32-t) :void "booleguru_cl_print(#0)" :one-liner t))

(defvar *last-op*)

(defun test-func-arg-incr (n) (+ n 1))

(defun eval-sexp-and-catch-errors (s)
  "Call a function and either return its value, or a string of its error."
  (handler-case (eval s)
	(error (c) (format nil "~a" c))))

(ffi:def-enum op-type (:exists :forall :equi :impl :lpmi :or :and :not :var))

(ffi:def-foreign-type variable-id :uint32_t)
(ffi:def-foreign-type op-id :uint32_t)

(defstruct op (id 0 :type fixnum))

(defun b-define-global-last-op (last-op)
  (setq *last-op* last-op))

(in-package cl-user)

(defun smtlib2-set-option (&key print-success)
  (declare (ignore print-success)))

(defmacro smtlib2-set-logic (logic)
  (declare (ignore logic))
  nil)

(defmacro smtlib2-check-sat ()
  nil)

(defmacro smtlib2-exit ()
  nil)

;; These bitvectors must somehow store how big they are and then handle type
;; conversions and everything here inside of lisp.

(defclass bitvector ()
  ((symb :accessor symb)
   (width :accessor width)))

(defparameter *consts* (make-hash-table))

(defun declare-const (name type)
  (if (gethash name *consts*) (error "Cannot declare same const twice!")
    (setf (gethash name *consts*) (b-var (format nil "~a" name)))))

(defmacro smtlib2-declare-const (name type)
  (declare-const name type)
  nil)

(defun const (s) (gethash s *consts*))

(defun smtlib2-and (a &rest r)
  (cond
   ((not (first r)) a)
   ((and (first r) (not (second r))) (b-and a (first r)))
   (t (b-and a (apply #'smtlib2-and (cons (first r) (rest r)))))))

(defun smtlib2-or (a &rest r)
  (cond
   ((not (first r)) a)
   ((and (first r) (not (second r))) (b-or a (first r)))
   (t (b-or a (apply #'smtlib2-or (cons (first r) (rest r)))))))

(defun smtlib2-assert (expr)
  expr)

;; https://stackoverflow.com/a/15710706
(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string
				 (mapcar #'symbol-name symbols))))

(defun smtlib2-prefixify (sexp)
  (cond ((equal sexp 'FALSE) 'smtlib2-false)
		((equal sexp 'TRUE) 'smtlib2-true)
		((equal nil sexp) nil)
		((listp sexp) (cons (symbol-append 'smtlib2- (first sexp))
							(loop for s in (rest sexp)
								  if (listp s)
								    collect (smtlib2-prefixify s)
                                  else if (const s)
                                    collect (const s)
								  else
                                    collect s)))
		(t sexp)))

(defun parse-smtlib2-toplevel (sexp)
  (let ((prefixed (smtlib2-prefixify sexp)))
    (eval prefixed)))

(defun parse-smtlib2-from-stream (stream)
  "Read stream into a list of expressions."
  (setq *consts* (make-hash-table))
  (let ((sexps (loop for sexp = (read stream nil 'eof)
               while (not (equal sexp 'eof))
               for parsed = (parse-smtlib2-toplevel sexp)
               if parsed
               collect parsed)))
    (apply #'smtlib2-and sexps)))

(defun parse-smtlib2-from-fileno (fileno)
  (parse-smtlib2-from-stream (ext:make-stream-from-fd fileno :input :element-type 'character)))

(defun parse-smtlib2-from-file (file)
  (parse-smtlib2-from-stream (open file)))

(defun parse-smtlib2-from-str (str)
  (parse-smtlib2-from-stream (make-string-input-stream str)))
