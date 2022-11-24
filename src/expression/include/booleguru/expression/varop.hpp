#pragma once

#include <cstddef>
#include <cstdint>

namespace booleguru::expression {
struct varop {
  uint32_t v = 0;// Reference to variable in vars manager.

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(v);
  }

  inline constexpr uint32_t left() const { return 0; }
  inline constexpr uint32_t right() const { return 0; }

  inline constexpr bool operator==(const varop& o) const { return v == o.v; }
};
}
