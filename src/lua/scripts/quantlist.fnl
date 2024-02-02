(fn sort-block [block]
  (table.sort block (lambda [a b] (< (tostring a) (tostring b))))
  block)

(fn print-block-of-kind [block kind]
  (var kind-str (if (= kind optype.exists)
                    "exists "
                    "forall "))
  (print kind-str (table.concat (icollect [_ q (ipairs block)]
                                  (tostring q)) " ")))

(fn quantlist [op]
  "Prints a list of quantifiers. Each block is printed as one line. The lines are
ordered after the names."
  (var i op)
  (var kind i.type)
  (var block [])
  (while (or (= i.type optype.exists) (= i.type optype.forall))
    (when (not (= kind i.type))
      (do
        (print-block-of-kind (sort-block block) kind)
        (set kind i.type)
        (set block [])))
    (table.insert block i.left)
    (set i i.right))
  (when (not (= op i)) (print-block-of-kind (sort-block block) kind))
 op)
