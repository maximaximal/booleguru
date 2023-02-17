#pragma once

namespace booleguru::expression {
struct op_ref;
}

namespace booleguru::transform {
template<class O>
class tseitin {
  O o_;

  public:
  using TransformResult = typename O::TransformResult;
  using initarg = typename O::initarg;
  using ref = typename O::ref;

  tseitin(initarg init)
    : o_(init) {}

  TransformResult operator()(expression::op_ref o);
};
}
