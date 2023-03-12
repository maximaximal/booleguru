function quantblocks(op)
  local o = op
  local quantsyms = {}
  quantsyms[optype.exists] = "?"
  quantsyms[optype.forall] = "#"
  while o.t == optype.forall or o.t == optype.exists do
    local quants = quantsyms[o.t]
    local t = o.t
    while o.t == t do
      quants = quants .. " " .. tostring(o.l)
      o = o.r
    end
    print(quants)
  end
  print("")
end
