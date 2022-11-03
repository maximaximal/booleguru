(defun test-func-arg-incr (n) (+ n 1))

(defun eval-sexp-and-catch-errors (s)
  "Call a function and either return it's value, or a string of it's error."
  (handler-case (eval s)
	(error (c) (format nil "~a" c))))
