#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
namespace actors {
template<class I>
struct eliminate_xor : public I {
  using ret = typename I::ret;
  using I::I;
  inline constexpr ret walk_xor(expression::op_ref e) {
    auto l = I::l(e);
    auto r = I::r(e);
    return (!l && r) || (l && !r);
  }
};
}
struct eliminate_xor : public visitor<eliminate_xor, actors::eliminate_xor> {};
}
