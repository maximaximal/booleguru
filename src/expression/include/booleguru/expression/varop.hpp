#pragma once

#include <cstdint>
#include <cstddef>

namespace booleguru::expression {
struct varop {
  uint32_t v;

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(v);
  }

  inline constexpr uint32_t left() const { return -1; }
  inline constexpr uint32_t right() const { return -1; }

  inline constexpr bool operator==(const varop& o) const { return v == o.v; }
};
}
