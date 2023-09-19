#pragma once

#include <cstddef>
#include <cstdint>

#include <booleguru/expression/id.hpp>

namespace booleguru::expression {
struct quantop {
  op_id v = 0;/// a reference to some varop
  op_id e = 0;/// a reference to the nested expression

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(v) + 70200511 * static_cast<size_t>(e);
  }

  inline constexpr op_id left() const { return v; }
  inline constexpr op_id right() const { return e; }

  inline constexpr bool operator==(const quantop& o) const {
    return v == o.v && e == o.e;
  }
};
}
