#pragma once

#include <cstdint>
#include <cstddef>

namespace booleguru::expression {
struct quantop {
  uint32_t v; /// a reference to some varop
  uint32_t e; /// a reference to the nested expression

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(v) + 70200511 * static_cast<size_t>(e);
  }

  inline constexpr uint32_t left() const { return v; }
  inline constexpr uint32_t right() const { return e; }

  inline constexpr bool operator==(const quantop& o) const {
    return v == o.v && e == o.e;
  }
};
}
