(in-package cl-user)

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

(defun var (name)
  (booleguru-get-varop-id name))
