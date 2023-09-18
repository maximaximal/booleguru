#pragma once

#include <cassert>
#include <cstdint>
#include <string>
#include <unordered_map>

#include <booleguru/expression/op_manager.hpp>

namespace booleguru::py {
struct model {
  using map = std::unordered_map<uint32_t, bool>;
  map values_;

  std::string to_string() const;

  inline bool operator[](uint32_t op) {
    assert(values_.contains(op));
    return values_[op];
  }
};
}
