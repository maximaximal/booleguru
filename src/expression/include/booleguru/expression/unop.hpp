#pragma once

#include <cstddef>
#include <cstdint>

#include <booleguru/expression/id.hpp>

namespace booleguru::expression {
struct unop {
  op_id c = 0;

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(c);
  }
  inline constexpr op_id left() const { return c; }
  inline constexpr op_id right() const { return 0; }

  inline constexpr bool operator==(const unop& o) const { return c == o.c; }
};
}
