#pragma once

#include <set>

#include "visitor.hpp"

namespace booleguru::transform {
template<class Set = std::set<int32_t>>
struct hash_variables : public visitor<hash_variables<Set>> {
  hash_variables(Set& s)
    : s(s) {
    s.clear();
  }
  size_t hash() {
    size_t result = 4017271;
    for(int32_t e : s)
      result ^= 70200511 * (static_cast<size_t>(e) + 1);
    return result;
  }
  Set& s;
  inline expression::op_ref walk_var(expression::op_ref e) {
    s.insert(e.get_id());
    return e;
  }
};
}
