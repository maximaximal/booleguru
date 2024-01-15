o = require "io"

function print(...) io.stderr:write(table.concat({...}, '\9') .. '\n') end

function eqkbkf(eq_num, kbkf_num)
    assert(eq_num)
    assert(kbkf_num)
    assert(tonumber(kbkf_num) >= 2)

    --print("eq_num: " .. eq_num)
    --print("kbkf_num: " .. kbkf_num)


    --equality part

    t_array = {}
    x_vars = {}
    y_vars = {}

    for i = 0, eq_num-1 do
       x_vars[i] = v("x_" .. i)
       y_vars[i] = v("y_" .. i)
    end

    --print("test1")

    function quantify_table(qfunc, t, inner)
        for i = #t,0,-1 do
          inner = qfunc(t[i], inner)
        end
        return inner
      end

      function print_table(t)
        assert(t ~= nil)
        for i = 0,#t do
          --print(i .. " : " .. tostring(t[i]))
        end
      end
    

    for i = 0, eq_num-1 do
      local term = equi(x_vars[i], y_vars[i])
      local exist_quan = exists(y_vars[i], term)
      t_array[i] = forall(x_vars[i], exist_quan)

    end

    --print("test2")
    --print_table(t_array)


    formula = {}

    for i = 0, eq_num-1 do
      if i == 0 then
        formula = t_array[i]
      else
        formula = formula + t_array[i]
      end
    end

    eq_formula = formula

    -- kbkf part


    -- kbkf_num

    kbkf_d_vars = {}
    kbkf_e_vars =  {}
    kbkf_x_vars = {}
    kbkf_f_vars = {}
    kbkf_formula = {}


    for i = 0, kbkf_num-1 do
      kbkf_d_vars[i] = v("d_" .. i)
      kbkf_e_vars[i] = v("e_" .. i)
      kbkf_x_vars[i] = v("x_" .. i)
      kbkf_f_vars[i] = v("f_" .. i)
    end

    
    kbkf_formula = -kbkf_d_vars[0] + -kbkf_e_vars[0]
    for i = 0, kbkf_num-2 do
      -- D_i
      d_clause = kbkf_d_vars[i] + kbkf_x_vars[i] + -kbkf_d_vars[i+1] + -kbkf_e_vars[i+1]

      -- E_i
      e_clause = kbkf_e_vars[i] + -kbkf_x_vars[i] + -kbkf_d_vars[i+1] + -kbkf_e_vars[i+1]

      kbkf_formula = kbkf_formula * d_clause * e_clause
      
    end

    -- D_t

    dt_clause = kbkf_d_vars[kbkf_num-1] + kbkf_x_vars[kbkf_num-1]

    for i = 0, kbkf_num-1 do
      dt_clause = dt_clause + (-kbkf_f_vars[i])
    end

    -- E_t

    et_clause = kbkf_e_vars[kbkf_num-1] + (-kbkf_x_vars[kbkf_num-1])

    for i = 0, kbkf_num-1 do
      et_clause = et_clause + (-kbkf_f_vars[i])
    end

    kbkf_formula = kbkf_formula * dt_clause * et_clause

    --f_i clauses

    for i = 0, kbkf_num-1 do
      temp_clause = kbkf_x_vars[i] + kbkf_f_vars[i]
      kbkf_formula = kbkf_formula * temp_clause
    end

    --f'_i clauses

    for i = 0, kbkf_num-1 do
      temp_clause = -kbkf_x_vars[i] + kbkf_f_vars[i]
      kbkf_formula = kbkf_formula * temp_clause
    end

    --kbkf quantifiers

    for i = 0, kbkf_num-1 do
      count = kbkf_num-1 - i
      kbkf_formula = exists(kbkf_f_vars[count], kbkf_formula)
    end

    for i = 0, kbkf_num-1 do
      count = kbkf_num-1 - i
      kbkf_formula = forall(kbkf_x_vars[count], kbkf_formula)
      kbkf_formula = exists(kbkf_e_vars[count], kbkf_formula)
      kbkf_formula = exists(kbkf_d_vars[count], kbkf_formula)
    end
    
    --print("in between")
    
    --print(tostring(kbkf_formula))

    --print("in between2")

    --for i = 0, kbkf_num

    final_formula = eq_formula * kbkf_formula
  return final_formula


    

    



end

return eqkbkf
