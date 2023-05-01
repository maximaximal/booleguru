#pragma once

#include "visitor.hpp"
#include <algorithm>
#include <booleguru/expression/op.hpp>
#include <booleguru/util/reverse.hpp>

#include <iosfwd>
#include <iterator>
#include <limits>
#include <list>
#include <unordered_map>

#include <iostream>

namespace booleguru::transform {
struct prenex_quantifier_stack_entry {
  expression::op_type t;
  uint32_t var;
  int32_t nesting;
  bool subtree_leaf = false;

  [[nodiscard]] static bool inline is_exists(
    const prenex_quantifier_stack_entry& e) noexcept {
    return e.t == expression::op_type::Exists;
  }
  [[nodiscard]] static bool inline is_forall(
    const prenex_quantifier_stack_entry& e) noexcept {
    return e.t == expression::op_type::Forall;
  }

  static void mark_leaves(std::list<prenex_quantifier_stack_entry>& l) {
    std::list<prenex_quantifier_stack_entry>::iterator last_it = l.end();
    for(auto it = l.begin(); it != l.end(); ++it) {
      if(last_it != l.end()) {
        last_it->subtree_leaf = last_it->nesting >= it->nesting;

        // Same element on the leaf must also be a leaf. So, once a leaf is
        // found, walk backwards on the same sub-tree until there is some other
        // element with a different op_type, which is when the backwards-walk
        // ends.
        if(last_it->subtree_leaf) {
          auto rit = make_reverse_iterator(last_it);
          ++rit;
          while(rit != l.rend() && rit->t == last_it->t &&
                rit->nesting + 1 == last_it->nesting) {
            rit->subtree_leaf = true;
            ++rit;
          }
        }
      }
      last_it = it;
    }
    // Last entry MUST be a leaf.
    l.rbegin()->subtree_leaf = true;
  }
};

using prenex_quantifier_quant_stack_t =
  std::list<prenex_quantifier_stack_entry>;

struct prenex_quantifier_Eup_Aup;
struct prenex_quantifier_Eup_Adown;
struct prenex_quantifier_Edown_Aup;
struct prenex_quantifier_Edown_Adown;
template<class Strategy = prenex_quantifier_Eup_Aup>
struct prenex_quantifier : public visitor<prenex_quantifier<Strategy>> {
  std::unordered_map<uint32_t, uint32_t> bounds_map;
  int32_t quantifier_nesting = 0;
  int32_t deepest_quantifier_nesting = std::numeric_limits<int32_t>::min();

  prenex_quantifier_quant_stack_t quant_stack;
  prenex_quantifier_quant_stack_t::iterator critical_path_end;

  expression::op_ref post_action(expression::op_ref o);

  inline expression::op_ref walk_quant(expression::op_ref o) {
    const auto old_v = o.get_mgr()[o->quant.v]->var;
    auto& old_v_obj = o.get_mgr().vars().getobj(old_v.v);

    uint32_t outer_bound = bounds_map[old_v.v];
    uint32_t bound = old_v_obj.counter++;
    bounds_map[old_v.v] = bound;

    auto bound_v =
      o.get_mgr().get(expression::op(expression::op_type::Var, old_v.v, bound));

    auto it = quant_stack.emplace(
      quant_stack.begin(),
      prenex_quantifier_stack_entry{
        (expression::op_type)o->type, bound_v.get_id(), quantifier_nesting });

    if(quantifier_nesting > deepest_quantifier_nesting) {
      deepest_quantifier_nesting = quantifier_nesting;
      critical_path_end = it;
    }

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
    auto begin_before_visit = quant_stack.begin();
    o = !this->visit(o.left());
    auto begin_after_visit = quant_stack.begin();
    if(begin_before_visit != begin_after_visit) {
      for(auto it = begin_after_visit; it != begin_before_visit; ++it) {
        expression::op_type& t = it->t;
        t = expression::op_type_flip_quantifier(t);
      }
    }
    return o;
  }

  // This eliminates impls of the form ->
  inline expression::op_ref walk_impl(expression::op_ref o) {
    auto begin_before_visit = quant_stack.begin();
    auto left = !this->visit(o.left());
    auto begin_after_visit = quant_stack.begin();
    auto right = this->visit(o.right());
    if(begin_before_visit != begin_after_visit) {
      for(auto it = begin_after_visit; it != begin_before_visit; ++it) {
        expression::op_type& t = it->t;
        t = expression::op_type_flip_quantifier(t);
      }
    }
    return o.get_mgr().get(
      expression::op(expression::op_type::Or, left.get_id(), right.get_id()));
  }

  // This eliminates impls of the form <-
  inline expression::op_ref walk_lpmi(expression::op_ref o) {
    auto left = this->visit(o.left());
    auto begin_before_visit = quant_stack.begin();
    auto right = !this->visit(o.right());
    auto begin_after_visit = quant_stack.begin();
    if(begin_before_visit != begin_after_visit) {
      for(auto it = begin_after_visit; it != begin_before_visit; ++it) {
        expression::op_type& t = it->t;
        t = expression::op_type_flip_quantifier(t);
      }
    }
    return o.get_mgr().get(
      expression::op(expression::op_type::Or, left.get_id(), right.get_id()));
  }
};
}
