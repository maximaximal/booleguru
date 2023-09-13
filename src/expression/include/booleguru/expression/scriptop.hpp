#pragma once

#include <cstddef>
#include <cstdint>

#include <booleguru/expression/id.hpp>

namespace booleguru::expression {
struct scriptop {
  // NOTE(Marcel): Why was this limited to 30 bits before?
  op_id c;
  script_id script;

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(c)
           + 70200511 * static_cast<size_t>(script);
  }
  inline constexpr op_id left() const { return c; }
  inline constexpr op_id right() const { return -1; }

  inline constexpr bool operator==(const scriptop& o) const {
    return c == o.c && script == o.script;
  }
};
}
