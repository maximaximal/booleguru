#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
struct variable_set_i : public visitor<variable_set_i> {
  variable_set_i(uint16_t i)
    : i(i) {}

  uint16_t i;

  inline op_ref walk_var(op_ref e) {
    return e.get_mgr().get(
      expression::op(expression::op_type::Var, e->var.v, e->var.q, i));
  };
};
}
