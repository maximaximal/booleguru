function mean_balance_factor(op)
  local function is_binary_op(op)
    local vault = { [optype.and_] = true
                  , [optype.or_] = true
                  , [optype.xor] = true
                  , [optype.impl] = true
                  , [optype.lpmi] = true
                  , [optype.equi] = true
                  }
    return vault[op.type] ~= nil
  end

  local function is_quantifier_op(op)
    local vault = { [optype.exists] = true
                  , [optype.forall] = true
                  }
    return vault[op.type] ~= nil
  end

  local cache = { }
  local cache_w_vars = { }
  local function node_count(root, include_vars)
    if include_vars
      then return cache_w_vars[root.id]
      else return cache[root.id]
      end
    assert(false)
  end
  local function precompute_node_counts(root)
    assert(root ~= nil)
    local postorder_stack = {}
    local stack = { root }
    while #stack > 0 do
      local new_root = table.remove(stack, #stack)
      table.insert(postorder_stack, new_root)
      if new_root.type == optype.not_ then
        table.insert(stack, new_root.l)
      elseif is_quantifier_op(new_root) then
        table.insert(stack, new_root.r)
      elseif is_binary_op(new_root) then
        table.insert(stack, new_root.l)
        table.insert(stack, new_root.r)
      end
    end
    while #postorder_stack > 0 do
      local new_root = table.remove(postorder_stack, #postorder_stack)
      if new_root.type == optype.var then
        cache[new_root.id] = 0
        cache_w_vars[new_root.id] = 1
      elseif new_root.type == optype.not_ then
        cache[new_root.id] = cache[new_root.l.id]
        cache_w_vars[new_root.id] = cache_w_vars[new_root.l.id]
      elseif is_quantifier_op(new_root) then
        cache[new_root.id] = cache[new_root.r.id]
        cache_w_vars[new_root.id] = cache_w_vars[new_root.r.id]
      elseif is_binary_op(new_root) then
        cache[new_root.id] = 1 + cache[new_root.l.id] + cache[new_root.r.id]
        cache_w_vars[new_root.id] = 1 + cache_w_vars[new_root.l.id]
                                      + cache_w_vars[new_root.r.id]
      else
        assert(false)
      end
    end
  end
  precompute_node_counts(op)

  local total_count = node_count(op, false)
  local progress = 0

  local stack = { op }
  local bf = 0.0
  while #stack > 0 do
    local root = table.remove(stack, #stack)
    assert(root ~= nil)
    if is_binary_op(root) then
      local count_l = node_count(root.l, true)
      local count_r = node_count(root.r, true)
      assert(count_l > 0 and count_r > 0)
      local ratio = count_l / count_r
      bf = bf + math.log(ratio, 2)
      table.insert(stack, 0, root.l)
      table.insert(stack, 0, root.r)
    end
  end

  -- bf := log2(r_0) + log2(r_1) + ... + log2(r_n)
  --    := log2(r_0 * r_1 * ... * r_n)
  --
  -- Then, we take the sigmoid-ish function
  --
  --                     1.0
  -- lmbf := 2.0 * -----------------
  --                 1.0 + a^(-bf)
  --
  -- with a > 1. A good choice is a := (total_count + 1.0) / total_count
  --
  -- I think this helps with precision..?
  local a = (1.0 + total_count) / total_count
  local lmbf = 2.0 / (1.0 + a^(-bf))
  print(string.format("Log-Mean Balance Factor: %01.8f", lmbf))
end
