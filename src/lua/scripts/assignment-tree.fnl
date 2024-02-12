(fn expand [op out assignments]
  (if (or (= op.type optype.exists) (= op.type optype.forall))
      (do
        (let [label (if assignments.valid "" (tostring op))]
          (out:write (.. "  " assignments.id " [ label=\"" label "\" ];")))

        (let [a (b-and assignments (b-not op.left))]
          (out:write (.. "  " assignments.id " -> " a.id " [ label=\"" (tostring (b-not op.left)) "\" style=\"dashed\" ];"))
          (expand op.right out a))
        (let [a (b-and assignments op.left)]
          (out:write (.. "  " assignments.id " -> " a.id " [ label=\"" (tostring op.left) "\" ];"))
          (expand op.right out a)))
      (do
        (if (solve (b-and op assignments))
            (out:write (.. "  " assignments.id " [ label=\"⊤\" ];"))
            (out:write (.. "  " assignments.id " [ label=\"⊥\" ];"))))))


(lambda assignment-tree [op out-path]
  (with-open [out (io.open out-path :w)]
    (out:write "digraph {\n")
    (expand op out (opref:new))
    (out:write "}\n"))
  op)
