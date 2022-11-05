(defun test-func-arg-incr (n) (+ n 1))

(defun eval-sexp-and-catch-errors (s)
  "Call a function and either return its value, or a string of its error."
  (handler-case (eval s)
	(error (c) (format nil "~a" c))))

(ffi:def-enum op-type (:exists :forall :equi :impl :lpmi :or :and :not :var))

(ffi:clines "#include <string>")
(ffi:clines "#include <booleguru/expression/var_manager.hpp>")
(ffi:clines "#include <booleguru/expression/op_manager.hpp>")

(ffi:def-foreign-type variable-id :uint32_t)
(ffi:def-foreign-type op-id :uint32_t)

(defstruct op (id 0 :type fixnum))

(defun var (name)
  (make-op :id (booleguru-get-varop-id name)))
