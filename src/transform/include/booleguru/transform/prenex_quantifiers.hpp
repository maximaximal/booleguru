#pragma once

#include <memory>

#include <booleguru/expression/op_manager.hpp>

namespace booleguru::transform {
struct prenex_quantifier {
  enum kind {
    Eup_Aup,
    Eup_Adown,
    Edown_Aup,
    Edown_Adown,
  };

  prenex_quantifier(kind k = Eup_Aup);
  ~prenex_quantifier();

  expression::op_ref operator()(expression::op_ref o);

  void animate(const std::string& path);

  private:
  struct inner;
  std::unique_ptr<inner> i_;

  expression::op_ref walk(expression::op_ref o);

  expression::op_ref walk_quant(expression::op_ref o);

  expression::op_ref walk_not(expression::op_ref o);

  expression::op_ref walk_impl(expression::op_ref o);

  expression::op_ref walk_lpmi(expression::op_ref o);

  expression::op_ref walk_equi(expression::op_ref o);

  expression::op_ref walk_bin(expression::op_ref o);

  expression::op_ref walk_xor(expression::op_ref o);
};
}
