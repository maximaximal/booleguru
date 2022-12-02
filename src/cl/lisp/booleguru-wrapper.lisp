(in-package cl-user)

(ffi:clines "#include <booleguru/cl/cl-function-c-wrappers.h>")

(defun b-var (name)
  (ffi:c-inline (name) (:cstring) :uint32-t "booleguru_cl_varop(#0)" :one-liner t))
w
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
