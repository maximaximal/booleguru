io = require "io"

function print(...) io.stderr:write(table.concat({...}, '\9') .. '\n') end

function counterfactuals(formulas_in_theory, var_count, nesting_depth, clauses_per_formula, vars_per_clause, seed)
  -- Needs parameters
  assert(formulas_in_theory)
  assert(var_count)
  assert(nesting_depth)
  assert(clauses_per_formula)
  assert(vars_per_clause)
  --assert(var_count >= clauses_per_formula)

  if seed ~= nil then
    math.randomseed(seed)
  end

  function shuffle_and_negate(array)

    temparray = {}
    for i = 0, #array do
      local selected = V[math.random(var_count)]
      temparray[i] = selected
    end


    -- fisher-yates
    for i = 0,#array do
      --local j = i * math.random()
      --j = j - j%1


      if(math.random() < 0.5) then
        array[i] = temparray[i]
      else
        array[i] = -temparray[i]
      end
    end
    return array
  end

  function shallow_copy(t)
    local t2 = {}
    for i = 0,#t do
      t2[i] = t[i]
    end
    return t2
  end

  function print_table(t)
    assert(t ~= nil)
    for i = 0,#t do
      print(i .. " : " .. tostring(t[i]))
    end
  end

  -- Vars are v_1..var_count
  function generate_vars()
    local vars = {}
    for i = 0,var_count do
      vars[i] = v("v_" .. i+1)
    end
    return vars
  end

  V = generate_vars()

  phi = shuffle_and_negate(shallow_copy(V))


  --print_table(phi)

  local phi_extended = {}

  if(var_count < nesting_depth) then
    for i=0, #phi do
      phi_extended[i] = phi[i]
    end
    for i=#phi+1, nesting_depth do

      local j = i * math.random()
      j = j - j%1
      local selected = V[math.random(var_count)]

      if(math.random() < 0.5) then
        phi_extended[i] = selected
      else
        phi_extended[i] = -selected
      end
    end
    phi = phi_extended
  end
  --print_table(phi)

  S = {}
  U = {}
  for i=0,nesting_depth do
    local s = {}
    local u = {}
    for n=0,formulas_in_theory+i-1 do
      s[n] = v("s_" .. i .. "_" .. n+1)
      u[n] = v("u_" .. i .. "_" .. n+1)
    end
    S[i] = s
    U[i] = u
  end

  -- The <= relation from the paper.
  function LTE(a, b)
    assert(#a == #b)
    assert(#a >= 1)
    local res = {}
    res[0] = impl(a[0], b[0])
    for i = 1,#a do
      res[i] = impl(a[i], b[i])
    end
    return res
  end

  -- The < relation from the paper.
  function LT(a, b)
    assert(#a == #b)
    assert(#a >= 1)
    local res = {}
    res[0] = impl(a[0], b[0]) * -impl(b[0], a[0])
    for i = 1,#a do
      res[i] = impl(a[i], b[i]) * -impl(b[i], a[i])
    end
    return res
  end

  function random_select_var(previous_vars)
    while true do
      local selected = V[math.random(0, var_count)]
      if previous_vars[selected] == nil then
        previous_vars[selected] = true
        return selected
      end
    end
  end

  function random_select_var_maybe_notted(previous_vars)
    local var = random_select_var(previous_vars)
    local flip = math.random(1,2)
    if flip == 2 then
      var = -var
    end
    return var
  end

  -- Check if the new lits are actually new or if they are already somewhere.
  function match_clause_lits(clause_lits_list, new_lits)
    for i=0,#clause_lits_list do
      local lits = clause_lits_list[i]
      if lits == new_lits then
        return true
      end
    end
    return false
  end

  function generate_clause()
    previous_vars = {}
    clause_lits = {}
    assert(vars_per_clause >= 1)
    local clause = random_select_var_maybe_notted(previous_vars)
    table.insert(clause_lits, clause.id)
    for i = 2,vars_per_clause do
      local l = random_select_var_maybe_notted(previous_vars)
      clause = clause + l
      table.insert(clause_lits, l.id)
    end

    table.sort(clause_lits)

    return clause, table.concat(clause_lits)
  end

  function generate_formula()
    local formula = nil
    local lits = {}
    assert(clauses_per_formula >= 1)
    local clause, new_lits = generate_clause()
    table.insert(lits, new_lits)

    formula = clause

    for i=2,clauses_per_formula do
      repeat
        clause, new_lits = generate_clause()
      until(not match_clause_lits(lits, new_lits))
      table.insert(lits, new_lits)
      formula = formula * clause
    end

    return formula
  end

  function generate_theory()
    local theory = {}
    assert(formulas_in_theory >= 1)

    for i=0,formulas_in_theory-1 do
      theory[i] = generate_formula()
    end

    return theory
  end

  function gather_vars(top)
    local s = { top }
    local inserted_vars = {}
    local vars = {}
    local i = 0

    while #s > 0 do
      top = table.remove(s, #s)

      if top.t == optype.var then
        if inserted_vars[top.id] == nil then
          vars[i] = top
          i = i + 1
        end
        inserted_vars[top.id] = true
      end

      if top.r ~= nil then
        table.insert(s, top.r)
      end
      if top.l ~= nil then
        table.insert(s, top.l)
      end
    end

    return vars
  end

  function sum_table(t)
    assert(#t >= 1)
    local a = t[0]
    for i = 1,#t do
      a = a * t[i]
    end
    return a
  end

  local T = generate_theory()

  function M(i)
    if(i == 0) then
      return LTE(S[0], T)
    end
    local M_i = M(i - 1)
    M_i[#M_i+1] = phi[i-1]

    return LTE(S[i], M_i)
  end

  function quantify_table(qfunc, t, inner)
    for i = #t,0,-1 do
      inner = qfunc(t[i], inner)
    end
    return inner
  end

  PHI = {}

  function PHI(i)
    local left = sum_table(LT(S[i], U[i]))

    local S_i_bak = S[i]
    S[i] = U[i]
    local impl1 = impl(sum_table(M(i)), -phi[i])
    local right = quantify_table(forall, V, impl1)
    S[i] = S_i_bak

    local p = quantify_table(exists, V, sum_table(M(i)) * phi[i])

    local impl2 = impl(left, right)
    local f = quantify_table(forall, U[i], impl2)

    return p * f
  end

  psi = {}
  function PSI(i)
    if i <= nesting_depth then
      local rel_1 = nil
      local rel_2 = nil

      if nesting_depth % 2 == 0 then
        -- k is even
        rel_1 = GT
        rel_2 = NGT
      else
        -- k is odd
        rel_1 = NGT
        rel_2 = GT
      end

      local rel = nil
      if i % 2 == 0 then
        rel = rel_1
      else
        rel = rel_2
      end
      return rel(i)
    else
      return W(nesting_depth)
    end
  end

  -- function for Psi_(k+1)
  function W(k)
    local temp_inner = sum_table(M(k-1)) * phi[k-1]
    local inner = impl(temp_inner, phi[k])
    --print("phi k:")
    --print(phi[k])
    --print("phi k-1:")
    --print(phi[k-1])
    return quantify_table(forall, V, inner)
  end

  function GT(i)
    local step1 = impl(PHI(i), PSI(i+1))
    local quant = quantify_table(forall, S[i], step1)
    return quant
  end

  function NGT(i)
    local step1 = PHI(i) * PSI(i+1)
    local quant = quantify_table(exists, S[i], step1)
    return quant
  end

  return PSI(0)

  -- Where do V_i play a role?
  -- Atoms, propositional formulas, variables in T?
  -- Atom Flipping - what is being flipped when generating a theory (conjunction of clauses)?
  -- Now, everything is UNSAT. How to fix this? Is this overquantified
  -- (i.e. scopes are bleeding together somehow)? Are invalid variables chosen? What's going on?
end

return counterfactuals
