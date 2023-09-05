#pragma once

#include <cstddef>
#include <cstdint>

namespace booleguru::expression {
struct varop {
  uint32_t v = 0;/// Reference to variable in vars manager.

  uint16_t q = 0;/// Some number that binds the variable to some
                 /// namespace/quantifier. Must not be bigger than 2^16, which
                 /// the authors think is a reasonable limit.

  uint16_t i = 0;/// i'th vector entry of the same variable. Used for
                 /// translating bit-vectors. Must not be bigger than 2^16.

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(v) + 35100255 * static_cast<size_t>(i)
           + 70200511 * static_cast<size_t>(q);
  }

  inline constexpr uint32_t left() const { return 0; }
  inline constexpr uint32_t right() const { return 0; }

  inline constexpr bool operator==(const varop& o) const {
    return v == o.v && i == o.i && q == o.q;
  }
};
}
