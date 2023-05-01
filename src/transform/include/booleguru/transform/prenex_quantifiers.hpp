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
  static void mark_leaves(std::list<prenex_quantifier_stack_entry>& l);
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

  expression::op_ref walk_quant(expression::op_ref o);

  expression::op_ref walk_exists(expression::op_ref o);

  expression::op_ref walk_forall(expression::op_ref o);

  expression::op_ref walk_var(expression::op_ref o);

  expression::op_ref walk_not(expression::op_ref o);

  expression::op_ref walk_impl(expression::op_ref o);

  expression::op_ref walk_lpmi(expression::op_ref o);
};
}
