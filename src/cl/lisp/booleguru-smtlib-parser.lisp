(defun smtlib2-set-option (&key print-success)
  (declare (ignore print-success)))

(defmacro smtlib2-set-logic (logic)
  (declare (ignore logic))
  nil)

(defmacro smtlib2-check-sat ()
  nil)

(defmacro smtlib2-exit ()
  nil)

(defmacro smtlib2-declare-const (name type)
  `(list 'new-declared-const (quote ,name) (quote ,type)))

(defmacro smtlib2-assert (expr)
  `(,expr))

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
								  else
									collect s)))
		(t sexp)))

(defun parse-smtlib2-toplevel (sexp)
  (eval (smtlib2-prefixify sexp)))

(defun parse-smtlib2-from-stream (stream)
  "Read stream into a list of expressions."
  (print (loop for sexp = (read stream nil 'eof)
         while (not (equal sexp 'eof))
	     for parsed = (parse-smtlib2-toplevel sexp)
	     if parsed
		 collect parsed)))

(defun parse-smtlib2-from-file (fileno)
  (parse-smtlib2-from-stream (ext:make-stream-from-fd fileno :input :element-type 'character)))

(defun parse-smtlib2-from-str (str)
  (parse-smtlib2-from-stream (make-string-input-stream str)))
