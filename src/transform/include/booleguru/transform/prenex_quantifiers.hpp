#pragma once

#include "visitor.hpp"
#include <booleguru/expression/op.hpp>
#include <booleguru/util/reverse.hpp>

#include <unordered_map>

namespace booleguru::transform {
struct prenex_quantifier : public visitor<prenex_quantifier> {
  std::unordered_map<uint32_t, uint32_t> bounds_map;
  std::vector<std::pair<op_type, uint32_t>> quant_stack;

  inline op_ref post_action(op_ref o) {
    for(auto& e : util::reverse(quant_stack)) {
      auto [t, v] = e;
      o = o.get_mgr().get(op(t, v, o.get_id()));
    }
    quant_stack.clear();
    return o;
  }

  inline op_ref walk_quant(op_ref o) {
    const auto old_v = o.get_mgr()[o->quant.v]->var;

    uint32_t outer_bound = bounds_map[old_v.v];
    bounds_map[old_v.v] = o.get_id();

    auto bound_v = o.get_mgr().get(op(op_type::Var, old_v.v, o.get_id()));
    quant_stack.push_back(std::make_pair((op_type)o->type, bound_v.get_id()));

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

  inline op_ref walk_not(op_ref o) {
    size_t before_visit = quant_stack.size();
    o = !visit(o.left());
    size_t after_visit = quant_stack.size();
    if(after_visit > before_visit) {
      for(size_t i = before_visit; i < after_visit; ++i) {
        op_type& t = quant_stack[i].first;
        t = expression::op_type_flip_quantifier(t);
      }
    }
    return o;
  }
};
}
