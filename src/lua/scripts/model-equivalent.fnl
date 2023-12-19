(fn inner-noncnf [a]
  (var q a)
  (while q.is_quantop
    (set q q.r))
  q)

(fn add-quantifier-from-expr-to-expr [from to]
  (var q from)
  (var res to)
  (while q.is_quantop
    (when (= q.t optype.exists)
      (set res (exists q.qv res)))
    (when (= q.t optype.forall)
      (set res (forall q.qv res)))
    (set q q.r))
  res)

(fn add-quantifier-from-expr-to-expr-exists [from to]
  (var q from)
  (var res to)
  (while q.is_quantop
    (when (= q.t optype.exists)
      (set res (exists q.qv res)))
    (when (= q.t optype.forall)
      (set res (exists q.qv res)))
    (set q q.r))
  res)

(lambda model-equivalent [...]
  (var a nil)
  (var mappings nil)
  (assert (= (% (length [...]) 2) 0))
  (assert *l*.is_prenex)
  (assert *r*.is_prenex)
  (each [index value (ipairs [...])]
    (if (= (% index 2) 1)
        (set a value)
        (if (= mappings nil)
            (set mappings (equi (v a) (v value)))
            (set mappings (b-and mappings (equi (v a) (v value)))))))
  (let [ll (inner-noncnf *l*)
        rr (inner-noncnf *r*)
        invariant (b-not (equi ll rr))]
    (add-quantifier-from-expr-to-expr
     *l*
     (add-quantifier-from-expr-to-expr-exists
      *r*
      (b-and invariant mappings)))))
