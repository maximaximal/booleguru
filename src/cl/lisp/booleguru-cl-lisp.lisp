(defun test-func-arg-incr (n) (+ n 1))

(defun eval-sexp-and-catch-errors (s)
  "Call a function and either return its value, or a string of its error."
  (handler-case (eval s)
	(error (c) (format nil "~a" c))))

(ffi:def-enum op-type (:exists :forall :equi :impl :lpmi :or :and :not :var))

(ffi:clines "#include <string>")
(ffi:clines "#include <booleguru/expression/var_manager.hpp>")
(ffi:clines "#include <booleguru/expression/op_manager.hpp>")

(ffi:def-foreign-type variable :uint32_t)
(ffi:def-foreign-type op :uint32_t)

(ffi:def-struct unop (c :op))
(ffi:def-struct binop (l :op) (r :op))
(ffi:def-struct quantop (v :variable) (c :op))
(ffi:def-struct varop (v :variable))

(ffi:def-union op-member
			   (un unop)
			   (bin binop)
			   (quant quantop))

(ffi:def-struct op
				(flags :uint64_t)
				(member :op-member))
