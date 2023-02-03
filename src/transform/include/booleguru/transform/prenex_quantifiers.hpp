#pragma once

#include "visitor.hpp"

#include <unordered_map>

namespace booleguru::transform {
struct prenex_quantifier : public visitor<prenex_quantifier> {
  std::unordered_map<uint32_t, uint32_t> bounds_map;
  std::stack<std::pair<op_type, uint32_t>> quant_stack;

  inline op_ref post_action(op_ref o) {
    while(!quant_stack.empty()) {
      auto [t, v] = quant_stack.top();
      o = o.get_mgr().get(op(t, v, o.get_id()));
      quant_stack.pop();
    }
    return o;
  }

  inline op_ref walk_quant(op_ref o) {
    const auto old_v = o.get_mgr()[o->quant.v]->var;

    uint32_t outer_bound = bounds_map[old_v.v];
    bounds_map[old_v.v] = o.get_id();

    auto bound_v = o.get_mgr().get(op(op_type::Var, old_v.v, o.get_id()));
    quant_stack.push(std::make_pair((op_type)o->type, bound_v.get_id()));

    auto e = visit(o.get_mgr()[o->quant.e]);

    bounds_map[old_v.v] = outer_bound;

    return e;
  }

  inline op_ref walk_exists(op_ref o) { return walk_quant(o); }

  inline op_ref walk_forall(op_ref o) { return walk_quant(o); }

  inline op_ref walk_var(op_ref o) {
    auto it = bounds_map.find(o->var.v);
    if(it != bounds_map.end()) {
      return o.get_mgr().get(op(op_type::Var, o->var.v, it->second));
    }
    return o;
  }
};
}
