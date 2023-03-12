-- Get all variables that were not quantified
function unquantified(op)
  local quantified_vars = {}

  local visited = {}
  local unvisited = {op}

  while #unvisited > 0 do
    local top = table.remove(unvisited, #unvisited)
    visited[top.id] = true

    if top.t == optype.var and quantified_vars[top.v] == nil then
      print(tostring(top))
    elseif top.t == optype.forall or top.t == optype.exists then
      quantified_vars[top.l.v] = true
    end

    if top.r ~= nil then
      if visited[top.r.id] ~= true then
        table.insert(unvisited, top.r)
      end
    end
    if top.l ~= nil then
      if visited[top.l.id] ~= true then
        table.insert(unvisited, top.l)
      end
    end
  end
end
