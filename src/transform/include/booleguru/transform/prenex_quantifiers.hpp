#pragma once

#include <memory>

#include <booleguru/expression/op_manager.hpp>

namespace booleguru::transform {
struct prenex_quantifier {
  enum {
    Eup_Aup,
    Eup_Adown,
    Edown_Aup,
    Edown_Adown,
  };

  prenex_quantifier();

  inline expression::op_ref operator()(expression::op_ref o);

  private:
  struct inner;
  std::unique_ptr<inner> i_;

  struct walker_action;

  walker_action post_action(expression::op_ref o);

  walker_action walk_quant(expression::op_ref o);

  walker_action walk_exists(expression::op_ref o);

  walker_action walk_forall(expression::op_ref o);

  walker_action walk_var(expression::op_ref o);

  walker_action walk_not(expression::op_ref o);

  walker_action walk_impl(expression::op_ref o);

  walker_action walk_lpmi(expression::op_ref o);
};
}
