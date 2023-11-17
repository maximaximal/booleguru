#pragma once

#include <cstdint>

namespace booleguru::expression {
class op_ref;
}

namespace booleguru::transform {
template<class O>
class tseitin {
  O o_;
  bool mapping_comments_ = true;

  uint32_t clauses_ = 0;
  uint32_t variables_ = 0;

  public:
  using TransformResult = typename O::TransformResult;
  using initarg = typename O::initarg;
  using id = typename O::id;

  uint32_t clauses() const { return clauses_; };
  uint32_t variables() const { return variables_; };

  tseitin(initarg init)
    : o_(init) {}

  TransformResult operator()(expression::op_ref o);

  void mapping_comments(bool m) { mapping_comments_ = m; }
};
}
