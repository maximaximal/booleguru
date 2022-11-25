#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
struct eliminate_equivalence : public visitor<eliminate_equivalence> {
  inline op_ref walk_equi(op_ref e) {
    using namespace expression;
    auto left = l(e);
    auto right = r(e);
    op_ref l_or_not_r = left || !right;
    op_ref not_l_or_r = !left || right;
    return l_or_not_r && not_l_or_r;
  }
};
}
