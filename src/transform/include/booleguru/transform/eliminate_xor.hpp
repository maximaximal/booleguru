#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
struct eliminate_xor : public visitor<eliminate_xor> {
  inline op_ref walk_xor(op_ref e) {
    return (!l(e) && r(e)) || (l(e) && !r(e));
  }
};
}
