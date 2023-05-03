#pragma once

#include <booleguru/expression/quantvec.hpp>

#include "visitor.hpp"

namespace booleguru::transform {
struct prenex_quantifier_Eup_Aup;
struct prenex_quantifier_Eup_Adown;
struct prenex_quantifier_Edown_Aup;
struct prenex_quantifier_Edown_Adown;
template<class Strategy = prenex_quantifier_Eup_Aup>
struct prenex_quantifier : public visitor<prenex_quantifier<Strategy>> {
  std::unordered_map<uint32_t, uint32_t> bounds_map;
  int32_t quantifier_nesting = 0;
  int32_t deepest_quantifier_nesting = std::numeric_limits<int32_t>::min();

  expression::quantlist quants;
  expression::quantlist::iterator critical_path_end;

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
