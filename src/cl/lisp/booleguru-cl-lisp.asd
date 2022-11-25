(defsystem "booleguru-cl-lisp"
           :components ((:file "booleguru-cl-lisp")
                        (:file "booleguru-smtlib-parser" :depends-on ("booleguru-cl-lisp"))))
