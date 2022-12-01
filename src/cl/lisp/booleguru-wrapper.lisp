(in-package cl-user)

(ffi:load-foreign-library #p "-lbooleguru-expression")
(ffi:load-foreign-library #p "-lbooleguru-transform")
(ffi:load-foreign-library #p "-lbooleguru-cl-wrapper")

(ffi:clines "#include <booleguru/cl/cl-function-c-wrappers.h>")

(defun b-var (name)
  (ffi:c-inline (name) (:cstring) :uint32-t "booleguru_cl_varop(#0)" :one-liner t))
