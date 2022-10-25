#pragma once

#include <set>

#include "visitor.hpp"

namespace booleguru::transform {
struct hash_variables : public visitor<hash_variables> {
  hash_variables(std::set<int32_t>& s)
    : s(s) {
    s.clear();
  }
  size_t hash() {
    size_t result = 4017271;
    for(int32_t e : s)
      result ^= 70200511 * (static_cast<size_t>(e) + 1);
    return result;
  }
  std::set<int32_t>& s;
  inline op_ref walk_var(op_ref e) {
    s.insert(e->var.v);
    return e;
  }
};
}
