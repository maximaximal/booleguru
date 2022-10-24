#pragma once

#include <cstdint>
#include <cstddef>

namespace booleguru::expression {
struct quantop {
  uint32_t v;
  uint32_t e;

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(v) + 70200511 * static_cast<size_t>(e);
  }

  inline constexpr uint32_t left() const { return e; }
  inline constexpr uint32_t right() const { return -1; }

  inline constexpr bool operator==(const quantop& o) const {
    return v == o.v && e == o.e;
  }
};
}
