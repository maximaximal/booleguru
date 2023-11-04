#pragma once

#include <cstdint>
#include <iosfwd>

namespace booleguru::expression {
enum class mutation : uint8_t {
  push,
  pop,
  change,

  last = change,
};

std::ostream&
operator<<(std::ostream& o, const mutation& m);
}
