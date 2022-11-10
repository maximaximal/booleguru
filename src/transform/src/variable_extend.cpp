#include <string>

#include <booleguru/transform/variable_extend.hpp>

namespace booleguru::transform {
using namespace expression;

op_ref
operator+(op_ref& e, const std::string& s) {
  if(e->type != op_type::Var) {
    return variable_extend("", s)(e);
  } else {
    auto v = e->var.v;
    auto var_name = e.get_mgr().vars()[v]->name;
    auto new_var_ref = e.get_mgr().vars().get(variable{ var_name + s });
    return e.get_mgr().get(op(op_type::Var, new_var_ref.get_id(), 0));
  }
}

op_ref
operator+(op_ref& e, int n) {
  std::string n_to_s = "_";
  n_to_s += std::to_string(n);
  return e + n_to_s;
}
}
