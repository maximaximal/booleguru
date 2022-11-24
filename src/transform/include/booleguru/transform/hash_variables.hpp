#pragma once

#include <set>

#include "visitor.hpp"

namespace booleguru::transform {
namespace actors {
template<class I>
struct hash_variables : public I {
  using ret = typename I::ret;
  using I::I;

  std::set<int32_t>* s;

  inline constexpr ret walk_var(expression::op_ref e) {
    if constexpr(std::is_same_v<ret, visitor_descent_query>) {
      return I::vd(e);
    } else {
      assert(s);
      s->insert(e->var.v);
      return e;
    }
  }
};
}
struct hash_variables : public visitor<hash_variables, actors::hash_variables> {
  hash_variables(std::set<int32_t>& s)
    : s(s) {
    s.clear();
    collect_.s = &s;
    traverse_.s = &s;
  }
  size_t hash() {
    size_t result = 4017271;
    for(int32_t e : s)
      result ^= 70200511 * (static_cast<size_t>(e) + 1);
    return result;
  }
  std::set<int32_t>& s;
};
}
