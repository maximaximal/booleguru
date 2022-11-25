(defun smtlib2-set-option (&key print-success)
  (declare (ignore print-success)))

(defmacro smtlib2-set-logic (logic)
  (declare (ignore logic))
  nil)

(defmacro smtlib2-check-sat ()
  nil)

(defmacro smtlib2-exit ()
  nil)

(defclass bitvector ()
  ((symb :accessor symb)
   (width :accessor width)))

(defparameter *consts* (make-hash-table))

(defun declare-const (name type)
  (if (gethash name *consts*) (error "Cannot declare same const twice!")
    (setf (gethash name *consts*) (var (format nil "~a" name)))))

(defmacro smtlib2-declare-const (name type)
  (declare-const name type)
  nil)

(defun const (s) (gethash s *consts*))

(defun smtlib2-and (a &rest r)
  (cond
   ((not (first r)) a)
   ((and (first r) (not (second r))) (b-and a (first r)))
   (t (b-and a (smtlib2-and (first r) (second r))))))

(defun smtlib2-or (a &rest r)
  (cond
   ((not (first r)) a)
   ((and (first r) (not (second r))) (b-or a (first r)))
   (t (b-or a (smtlib2-or (first r) (second r))))))

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
  (let ((sexps (loop for sexp = (read stream nil 'eof)
               while (not (equal sexp 'eof))
               for parsed = (parse-smtlib2-toplevel sexp)
               if parsed
               collect parsed)))
    (apply #'smtlib2-and sexps)))

(defun parse-smtlib2-from-file (fileno)
  (parse-smtlib2-from-stream (ext:make-stream-from-fd fileno :input :element-type 'character)))

(defun parse-smtlib2-from-str (str)
  (parse-smtlib2-from-stream (make-string-input-stream str)))
