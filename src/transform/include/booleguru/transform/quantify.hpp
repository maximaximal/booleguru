#pragma once

#include <optional>

#include "visitor.hpp"

namespace booleguru::transform {
struct quantify : public visitor<quantify> {
  quantify(op_type quantifier,
           std::optional<uint16_t> only_q,
           std::optional<uint16_t> only_i)
    : quantifier(quantifier)
    , only_q(only_q)
    , only_i(only_i) {}

  op_type quantifier;
  std::optional<uint16_t> only_q;
  std::optional<uint16_t> only_i;

  ankerl::unordered_dense::set<op_id> vars;

  inline op_ref walk_var(op_ref e) {
    if(only_q && e->var.q != *only_q) {
      return e;
    }
    if(only_i && e->var.i != *only_i) {
      return e;
    }
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
}
