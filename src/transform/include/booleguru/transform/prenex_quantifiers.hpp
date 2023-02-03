#pragma once

#include "visitor.hpp"
#include <booleguru/expression/op.hpp>
#include <booleguru/util/reverse.hpp>

#include <iosfwd>
#include <unordered_map>

#include <iostream>

namespace booleguru::transform {
struct prenex_quantifier_stack_entry {
  expression::op_type t;
  uint32_t inner;
  uint32_t nesting;
};
}

std::ostream&
operator<<(std::ostream& o,
           const booleguru::transform::prenex_quantifier_stack_entry& e);

std::ostream&
operator<<(
  std::ostream& o,
  const std::vector<booleguru::transform::prenex_quantifier_stack_entry>& e);

namespace booleguru::transform {
struct prenex_quantifier_Eup_Aup {
  bool operator()(const prenex_quantifier_stack_entry& l,
                  const prenex_quantifier_stack_entry& r) {
    static_assert(expression::op_type::Exists < expression::op_type::Forall);
    return (l.t < r.t && l.nesting < r.nesting) || l.nesting < r.nesting;
  }
};

struct prenex_quantifier_Eup_Adown {
  bool operator()(const prenex_quantifier_stack_entry& l,
                  const prenex_quantifier_stack_entry& r) {
    static_assert(expression::op_type::Exists < expression::op_type::Forall);
    return (l.t < r.t && l.nesting <= r.nesting) || l.nesting < r.nesting;
  }
};

struct prenex_quantifier_Edown_Aup {
  bool operator()(const prenex_quantifier_stack_entry& l,
                  const prenex_quantifier_stack_entry& r) {
    static_assert(expression::op_type::Exists < expression::op_type::Forall);
    return l.nesting <= r.nesting && l.t > r.t;
  }
};

struct prenex_quantifier_Edown_Adown {
  bool operator()(const prenex_quantifier_stack_entry& l,
                  const prenex_quantifier_stack_entry& r) {
    static_assert(expression::op_type::Exists < expression::op_type::Forall);
    return (l.t > r.t && l.nesting < r.nesting) || l.nesting < r.nesting;
  }
};

template<class Strategy = prenex_quantifier_Eup_Aup>
struct prenex_quantifier : public visitor<prenex_quantifier<Strategy>> {
  Strategy strategy;
  std::unordered_map<uint32_t, uint32_t> bounds_map;
  uint32_t quantifier_nesting = 0;

  std::vector<prenex_quantifier_stack_entry> quant_stack;

  inline expression::op_ref post_action(expression::op_ref o) {
    std::cout << "Before Sort: " << quant_stack << std::endl;
    std::sort(quant_stack.begin(), quant_stack.end(), strategy);
    std::cout << "After Sort: " << quant_stack << std::endl;

    for(prenex_quantifier_stack_entry& e : util::reverse(quant_stack)) {
      o = o.get_mgr().get(expression::op(e.t, e.inner, o.get_id()));
    }
    quant_stack.clear();
    return o;
  }

  inline expression::op_ref walk_quant(expression::op_ref o) {
    const auto old_v = o.get_mgr()[o->quant.v]->var;

    uint32_t outer_bound = bounds_map[old_v.v];
    bounds_map[old_v.v] = o.get_id();

    auto bound_v = o.get_mgr().get(
      expression::op(expression::op_type::Var, old_v.v, o.get_id()));
    quant_stack.push_back(prenex_quantifier_stack_entry{
      (expression::op_type)o->type, bound_v.get_id(), quantifier_nesting });

    ++quantifier_nesting;

    auto e = this->visit(o.get_mgr()[o->quant.e]);

    --quantifier_nesting;

    bounds_map[old_v.v] = outer_bound;

    return e;
  }

  inline expression::op_ref walk_exists(expression::op_ref o) {
    return walk_quant(o);
  }

  inline expression::op_ref walk_forall(expression::op_ref o) {
    return walk_quant(o);
  }

  inline expression::op_ref walk_var(expression::op_ref o) {
    auto it = bounds_map.find(o->var.v);
    if(it != bounds_map.end()) {
      return o.get_mgr().get(
        expression::op(expression::op_type::Var, o->var.v, it->second));
    }
    return o;
  }

  inline expression::op_ref walk_not(expression::op_ref o) {
    size_t before_visit = quant_stack.size();
    o = !this->visit(o.left());
    size_t after_visit = quant_stack.size();
    if(after_visit > before_visit) {
      for(size_t i = before_visit; i < after_visit; ++i) {
        expression::op_type& t = quant_stack[i].t;
        t = expression::op_type_flip_quantifier(t);
      }
    }
    return o;
  }
};
}
