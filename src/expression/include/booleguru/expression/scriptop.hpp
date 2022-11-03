#pragma once

#include <cstdint>
#include <cstddef>

namespace booleguru::expression {
struct scriptop {
  uint32_t c : 30;
  uint32_t script_id;

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(c) +
           70200511 * static_cast<size_t>(script_id);
  }
  inline constexpr uint32_t left() const { return c; }
  inline constexpr uint32_t right() const { return -1; }

  inline constexpr bool operator==(const scriptop& o) const {
    return c == o.c && script_id == o.script_id;
  }
};
}
