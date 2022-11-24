#pragma once

#include <cstdint>
#include <cstddef>

namespace booleguru::expression {
struct binop {
  uint32_t l = 0;
  uint32_t r = 0;

  inline constexpr binop(uint32_t l, uint32_t r) : l(l), r(r) {}

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(l) + 70200511 * static_cast<size_t>(r);
  }
  inline constexpr uint32_t left() const { return l; }
  inline constexpr uint32_t right() const { return r; }

  inline constexpr bool operator==(const binop& o) const {
    return l == o.l && r == o.r;
  }
};
}
