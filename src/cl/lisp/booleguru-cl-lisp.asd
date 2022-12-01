(defsystem "booleguru-cl-lisp"
           :serial t
           :depends-on (#:asdf)
           :components ((:file "booleguru-wrapper")
                        (:file "booleguru-cl-lisp")
                        (:file "booleguru-smtlib-parser")))
