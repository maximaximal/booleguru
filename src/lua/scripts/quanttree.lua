require "io"

-- Builds a tree according to quantifiers.
-- Example usage: ./booleguru <formula>.boole :quanttree --null | dot -Tpng > <formula>.png
function quanttree (op, out)
  local o = nil
  if out then
    o = io.open(out, 'w')
  else
    o = io.stdout
  end

  local unvisited = {op}

  local optype_symbol = {}
  optype_symbol[optype.none] = "none"
  optype_symbol[optype.exists] = "?"
  optype_symbol[optype.forall] = "#"
  optype_symbol[optype.equi] = "<->"
  optype_symbol[optype.impl] = "->"
  optype_symbol[optype.lpmi] = "<-"
  optype_symbol[optype.or_] = "|"
  optype_symbol[optype.xor] = "^"
  optype_symbol[optype.and_] = "&"
  optype_symbol[optype.not_] = "!"
  optype_symbol[optype.var] = nil

  o:write("digraph {\n")

  -- Taken from https://stackoverflow.com/a/15278426
  function table_concat(t1, t2)
    for i=1,#t2 do
      t1[#t1+1] = t2[i]
    end
    return t1
  end

  local counter = 1

  function visit(top)
    local curr_node = ""
    local children = {}

    local symb = optype_symbol[top.t]
    if symb == nil then
      symb = tostring(top)
    end

    while top.type == optype.exists or top.type == optype.forall do
      curr_node = curr_node .. " " .. symb .. tostring(top.l)
      top = top.r
    end

    if top.l ~= nil then
      children = visit(top.l)
    end
    if top.r ~= nil then
      children = table_concat(children, visit(top.r))
    end

    if curr_node ~= "" then
      o:write("  " .. tostring(counter) .. " [ label=\"" .. curr_node .. "\" ];\n" )

      for c,_ in pairs(children) do
        o:write("  " .. tostring(counter) .. " -> " .. tostring(c) .. ";\n")
      end

      counter = counter + 1
      return {counter}
    end

    return children
  end

  visit(op)

  o:write("}\n\n")
end
