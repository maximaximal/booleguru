(fn expand [op out assignments stats]
  (if (or (= op.type optype.exists) (= op.type optype.forall))
      (do
        (let [label (if assignments.valid "" (tostring op))]
          (out:write (.. "  " assignments.id " [ label=\"" label "\" ];")))

        (let [a (b-and assignments (b-not op.left))]
          (out:write (.. "  " assignments.id " -> " a.id " [ label=\"" (tostring (b-not op.left)) "\" style=\"dashed\" ];"))
          (expand op.right out a stats))
        (let [a (b-and assignments op.left)]
          (out:write (.. "  " assignments.id " -> " a.id " [ label=\"" (tostring op.left) "\" ];"))
          (expand op.right out a stats)))
      (do
        (if (solve (b-and op assignments))
            (do
              (out:write (.. "  " assignments.id " [ label=\"⊤\" ];"))
              (set stats.tops (+ stats.tops 1)))
            (out:write (.. "  " assignments.id " [ label=\"⊥\" ];"))))))


(lambda assignment-tree [op out-path]
  (let [stats {:tops 0}]
    (with-open [out (io.open out-path :w)]
     (out:write "digraph {\n")
     (expand op out (opref:new) stats)
     (out:write "  label=\"tops: " stats.tops "\";\n")
     (out:write "}\n")))
  op)
