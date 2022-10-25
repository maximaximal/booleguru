#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
struct eliminate_implication : public visitor<eliminate_implication> {
  inline op_ref walk_impl(op_ref e) { return !l(e) || r(e); }
  inline op_ref walk_pmil(op_ref e) { return l(e) || !r(e); }
};
}
