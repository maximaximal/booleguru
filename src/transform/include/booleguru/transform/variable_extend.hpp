#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
struct variable_extend : public visitor<variable_extend> {
  variable_extend(const std::string& prefix, const std::string& suffix)
    : prefix(prefix)
    , suffix(suffix) {}

  std::string prefix;
  std::string suffix;

  inline op_ref walk_var(op_ref e) {
    auto& oldvar = e.get_mgr().vars().getobj(e->var.v);
    auto newvar = e.get_mgr().vars().get(
      expression::variable{ prefix + oldvar.name + suffix });
    return e.get_mgr().get(expression::op(
      expression::op_type::Var, newvar.get_id(), e->var.q, e->var.i));
  };
};

expression::op_ref
operator+(expression::op_ref& e, const std::string& s);

expression::op_ref
operator+(expression::op_ref& e, int n);
}
