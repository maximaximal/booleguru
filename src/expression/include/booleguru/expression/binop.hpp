#pragma once

#include <cstddef>
#include <cstdint>

#include <booleguru/expression/id.hpp>

namespace booleguru::expression {
struct binop {
  op_id l = 0;
  op_id r = 0;

  inline constexpr binop(op_id l, op_id r)
    : l(l)
    , r(r) {}

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(l) + 70200511 * static_cast<size_t>(r);
  }
  inline constexpr op_id left() const { return l; }
  inline constexpr op_id right() const { return r; }

  inline constexpr bool operator==(const binop& o) const {
    return l == o.l && r == o.r;
  }
};
}
