#pragma once

#include <cstdint>
#include <cstddef>

namespace booleguru::expression {
struct unop {
  uint32_t c = 0;

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(c);
  }
  inline constexpr uint32_t left() const { return c; }
  inline constexpr uint32_t right() const { return 0; }

  inline constexpr bool operator==(const unop& o) const { return c == o.c; }
};
}
