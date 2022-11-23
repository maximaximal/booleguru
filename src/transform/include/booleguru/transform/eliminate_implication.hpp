#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
struct eliminate_implication : public visitor<eliminate_implication> {
  template<typename impl>
  struct actor : public impl {
    using ret = typename impl::ret;
    using impl::impl;

    inline ret walk_impl(expression::op_ref e) { return !l(e) || r(e); }
    inline ret walk_pmil(expression::op_ref e) { return l(e) || !r(e); }
  };
};
}
