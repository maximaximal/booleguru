#pragma once

#include <optional>

#include "visitor.hpp"

namespace booleguru::transform {
struct quantify : public visitor<quantify> {
  quantify(op_type quantifier)
    : quantifier(quantifier) {}

  op_type quantifier;
  ankerl::unordered_dense::set<op_id> vars;

  inline op_ref walk_var(op_ref e) {
    vars.emplace(e.get_id());
    return e;
  };

  inline op_ref post_action(op_ref o) {
    for(op_id var : vars) {
      o = o.get_mgr().get(op(quantifier, var, o.get_id()));
    }
    return o;
  }
};

expression::op_ref
operator+(expression::op_ref& e, const std::string& s);

expression::op_ref
operator+(expression::op_ref& e, int n);
}
