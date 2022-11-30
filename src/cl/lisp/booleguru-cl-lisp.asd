(defsystem "booleguru-cl-lisp"
           :depends-on (:swank)
           :components ((:file "booleguru-cl-lisp")
                        (:file "booleguru-smtlib-parser" :depends-on ("booleguru-cl-lisp"))))
