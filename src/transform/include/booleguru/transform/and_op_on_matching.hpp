#pragma once

#include <booleguru/expression/id.hpp>

#include "visitor.hpp"

namespace booleguru::transform {
struct and_op_on_matching : public visitor<and_op_on_matching> {
  and_op_on_matching(op_type op,
                     uint16_t q1,
                     uint16_t q2,
                     uint16_t i1,
                     uint16_t i2)
    : combined_op_type(op)
    , q1(q1)
    , q2(q2)
    , i1(i1)
    , i2(i2) {}

  op_type combined_op_type;
  uint16_t q1, q2, i1, i2;

  ankerl::unordered_dense::map<expression::var_id, std::pair<bool, bool>>
    matching;

  inline op_ref walk_var(op_ref e) {
    if(e->var.q == q1 && e->var.i == i1) {
      matching[e->var.v].first = true;
    } else if(e->var.q == q2 && e->var.i == i2) {
      matching[e->var.v].second = true;
    }
    return e;
  };

  inline op_ref post_action(op_ref o) {
    for(auto& e : matching) {
      if(e.second.first && e.second.second) {
        // Both matches were found, add the op!
        using expression::op;
        op_id left_var = o.get_mgr().get_id(op(op_type::Var, e.first, q1, i1));
        op_id right_var = o.get_mgr().get_id(op(op_type::Var, e.first, q2, i2));

        op_ref combined
          = o.get_mgr().get(op(combined_op_type, left_var, right_var));

        o = o && combined;
      }
    }
    return o;
  }
};
}
