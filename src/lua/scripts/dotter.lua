require "io"

-- Ze yellow of ze egg. Prints a given tree in dot-syntax to STDOUT or to the
-- given file.
function dotter (op, out)
  local o = nil
  if out then
    o = io.open(out, 'w')
  else
    o = io.stdout
  end

  local visited = {}
  local unvisited = {op}

  local optype_symbol = {}
  optype_symbol[optype.none] = "none"
  optype_symbol[optype.exists] = "∃"
  optype_symbol[optype.forall] = "∀"
  optype_symbol[optype.equi] = "<->"
  optype_symbol[optype.impl] = "->"
  optype_symbol[optype.lpmi] = "<-"
  optype_symbol[optype.or_] = "|"
  optype_symbol[optype.xor] = "^"
  optype_symbol[optype.and_] = "&"
  optype_symbol[optype.not_] = "!"
  optype_symbol[optype.var] = nil

  o:write("digraph {\n")

  while #unvisited > 0 do
    local top = table.remove(unvisited, #unvisited)
    visited[top.id] = true

    local symb = optype_symbol[top.t]
    if symb == nil then
      symb = tostring(top)
    end
    o:write("  " .. tostring(top.id) .. " [ label=\"" .. symb .. "\" ];\n" )

    if top.l ~= nil then
      if visited[top.l.id] ~= true then
        o:write("  " .. tostring(top.id) .. " -> " .. tostring(top.l.id) .. " [ label=\"l\" ];\n")
        table.insert(unvisited, top.l)
      end
    end
    if top.r ~= nil then
      if visited[top.r.id] ~= true then
        o:write("  " .. tostring(top.id) .. " -> " .. tostring(top.r.id) .. " [ label=\"r\" ];\n")
        table.insert(unvisited, top.r)
      end
    end
  end

  o:write("}\n\n")
end
