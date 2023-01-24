#pragma once

#include <cstddef>
#include <cstdint>

namespace booleguru::expression {
struct varop {
  uint32_t v = 0;// Reference to variable in vars manager.
  uint32_t q = 0;// Some number that binds the variable into some other namespace that has no true meaning.

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(v) + 70200511 * static_cast<size_t>(q);
  }

  inline constexpr uint32_t left() const { return 0; }
  inline constexpr uint32_t right() const { return 0; }

  inline constexpr bool operator==(const varop& o) const { return v == o.v && q == o.q; }
};
}
