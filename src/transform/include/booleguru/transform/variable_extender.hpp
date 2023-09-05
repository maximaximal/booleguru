#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
struct variable_extender : public visitor<variable_extender> {
  variable_extender(std::string suffix)
    : suffix(suffix) {}

  std::string suffix;

  inline op_ref walk_var(op_ref e) {
    auto& oldvar = e.get_mgr().vars().getobj(e->var.v);
    auto newvar
      = e.get_mgr().vars().get(expression::variable{ oldvar.name + suffix });
    return e.get_mgr().get(
      expression::op(expression::op_type::Var, newvar.get_id(), 0));
  };
};

expression::op_ref
operator+(expression::op_ref& e, const std::string& s);

expression::op_ref
operator+(expression::op_ref& e, int n);
}
