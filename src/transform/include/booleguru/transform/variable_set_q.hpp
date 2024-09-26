#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
struct variable_set_q : public visitor<variable_set_q> {
  variable_set_q(uint16_t q)
    : q(q) {}

  uint16_t q;

  inline op_ref walk_var(op_ref e) {
    return e.get_mgr().get(
      expression::op(expression::op_type::Var, e->var.v, q, e->var.i));
  };
};
}
