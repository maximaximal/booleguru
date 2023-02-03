#pragma once

#include "visitor.hpp"
#include <algorithm>
#include <booleguru/expression/op.hpp>
#include <booleguru/util/reverse.hpp>

#include <iosfwd>
#include <list>
#include <unordered_map>

#include <iostream>

namespace booleguru::transform {
struct prenex_quantifier_stack_entry {
  expression::op_type t;
  uint32_t inner;
  int32_t nesting;
};
}

std::ostream&
operator<<(std::ostream& o,
           const booleguru::transform::prenex_quantifier_stack_entry& e);

std::ostream&
operator<<(
  std::ostream& o,
  const std::vector<booleguru::transform::prenex_quantifier_stack_entry>& e);

std::ostream&
operator<<(
  std::ostream& o,
  const std::list<booleguru::transform::prenex_quantifier_stack_entry>& e);

namespace booleguru::transform {
struct prenex_quantifier_Eup_Aup {
  const prenex_quantifier_stack_entry& e;
  inline constexpr prenex_quantifier_Eup_Aup(
    const prenex_quantifier_stack_entry& e) noexcept
    : e(e) {}

  [[nodiscard]] inline bool operator()(
    const prenex_quantifier_stack_entry& o) const noexcept {
    static_assert(expression::op_type::Exists < expression::op_type::Forall);
    return e.t == o.t && e.nesting <= o.nesting;
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
  std::unordered_map<uint32_t, uint32_t> bounds_map;
  int32_t quantifier_nesting = 0;
  int32_t deepest_quantifier_nesting = std::numeric_limits<int32_t>::min();

  using quant_stack_t = std::list<prenex_quantifier_stack_entry>;
  quant_stack_t quant_stack;
  quant_stack_t::iterator critical_path_end;

  inline expression::op_ref post_action(expression::op_ref o) {
    std::list<prenex_quantifier_stack_entry> critical_path;

    int32_t current_nesting = deepest_quantifier_nesting + 1;
    for(auto it = critical_path_end; it != quant_stack.end();) {
      prenex_quantifier_stack_entry& e = *it;
      if(e.nesting >= current_nesting) {
        it++;
        continue;
      }
      --current_nesting;
      critical_path.push_front(e);
      it = quant_stack.erase(it);
    }

    quant_stack_t s;
    while(!quant_stack.empty()) {
      auto& front = quant_stack.front();
      s.emplace_front(front);
      quant_stack.pop_front();
    }

    auto cit = critical_path.begin();
    prenex_quantifier_stack_entry* ce = &*cit;

    while(!s.empty()) {
      ce = &*cit;

      while(true) {
        auto it =
          std::find_if(s.begin(),
                       s.end(),
                       [this, ce](const prenex_quantifier_stack_entry& e) {
                         return e.t == ce->t && (e.nesting == ce->nesting ||
                                                 e.nesting == ce->nesting + 1);
                       });

        if(it == s.end()) {
          ++cit;
          break;
        }

        auto next_cit = cit;
        ++next_cit;
        cit = critical_path.emplace(next_cit, *it);
        s.erase(it);
      }

      if(cit == critical_path.end()) {
        std::copy(s.begin(), s.end(), std::back_inserter(critical_path));
        s.clear();
      }
    }

    for(prenex_quantifier_stack_entry& e : util::reverse(critical_path)) {
      o = o.get_mgr().get(expression::op(e.t, e.inner, o.get_id()));
    }
    return o;
  }

  inline expression::op_ref walk_quant(expression::op_ref o) {
    const auto old_v = o.get_mgr()[o->quant.v]->var;

    uint32_t outer_bound = bounds_map[old_v.v];
    bounds_map[old_v.v] = o.get_id();

    auto bound_v = o.get_mgr().get(
      expression::op(expression::op_type::Var, old_v.v, o.get_id()));

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
};
}
