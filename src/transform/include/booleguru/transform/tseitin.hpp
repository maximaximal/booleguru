#pragma once

namespace booleguru::expression {
class op_ref;
}

namespace booleguru::transform {
template<class O>
class tseitin {
  O o_;
  bool mapping_comments_ = true;

  public:
  using TransformResult = typename O::TransformResult;
  using initarg = typename O::initarg;
  using id = typename O::id;

  tseitin(initarg init)
    : o_(init) {}

  TransformResult operator()(expression::op_ref o);

  void mapping_comments(bool m) { mapping_comments_ = m; }
};
}
