-- Get all variables that were not quantified
function unquantified(op)
  function shallow_copy(t)
    local t2 = {}
    for k,v in pairs(t) do
      t2[k] = v
    end
    return t2
  end

  local s = { { op, {} } }

  while #s > 0 do
    local top_pair = table.remove(s, #s)
    local top = top_pair[1]
    local quantified = top_pair[2]

    if top.t == optype.var and quantified[top.v] == nil then
      print(tostring(top))
    elseif top.t == optype.forall or top.t == optype.exists then
      quantified[top.l.v] = true
    end

    if top.r ~= nil then
      table.insert(s, { top.r, shallow_copy(quantified) })
    end
    if top.l ~= nil then
      table.insert(s, { top.l, shallow_copy(quantified) })
    end
  end
  print("")
end
