#pragma once

#include <cstdint>

#include <charconv>
#include <stdexcept>
#include <string_view>

#include <fmt/format.h>

#include "is_number.hpp"

namespace booleguru::util {
struct bv_literal_malformed_exception : std::runtime_error {
  bv_literal_malformed_exception(std::string_view lit)
    : std::runtime_error(fmt::format("Literal {} not of form bvINT", lit)) {}
};

struct bv_literal {
  const int64_t n;

  explicit bv_literal(std::string_view lit)
    : n(parse(lit)) {}

  static int64_t parse(std::string_view lit) {
    if(!lit.starts_with("bv")) {
      throw bv_literal_malformed_exception(lit);
    }
    std::string_view num = lit.substr(2);
    if(!is_number(num)) {
      throw bv_literal_malformed_exception(lit);
    }

    int64_t n = 0;
    std::from_chars(num.data(), num.data() + num.size(), n);
    return n;
  }
};
}
