#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
namespace actors {
template<class I>
struct eliminate_equivalence : public I {
  using ret = typename I::ret;
  using I::I;

  inline ret walk_equi(expression::op_ref e) {
    using namespace expression;
    auto left = I::l(e);
    auto right = I::r(e);
    op_ref l_or_not_r = left || !right;
    op_ref not_l_or_r = !left || right;
    return l_or_not_r && not_l_or_r;
  }
};
}
struct eliminate_equivalence
  : public visitor<eliminate_equivalence, actors::eliminate_equivalence> {};
}
