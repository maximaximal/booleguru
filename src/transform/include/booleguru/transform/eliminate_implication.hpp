#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
namespace actors {
template<class I>
struct eliminate_implication : public I {
  using ret = typename I::ret;
  using I::I;

  inline ret walk_impl(expression::op_ref e) { return !I::l(e) || I::r(e); }
  inline ret walk_pmil(expression::op_ref e) { return I::l(e) || !I::r(e); }
};
}
struct eliminate_implication
  : public visitor<eliminate_implication, actors::eliminate_implication> {};
}
