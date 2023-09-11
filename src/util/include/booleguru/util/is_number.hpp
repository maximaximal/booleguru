#pragma once

#include <algorithm>
#include <cctype>
#include <stdexcept>
#include <string>
#include <string_view>

#include <fmt/format.h>

namespace booleguru::util {
struct not_a_number : std::runtime_error {
  not_a_number(std::string_view s)
    : std::runtime_error(
      fmt::format("Expected a number, but received string \"{}\"", s)) {}
};

inline bool
is_number(std::string_view s) {
  return !s.empty() && std::find_if(s.begin(), s.end(), [](unsigned char c) {
                         return !std::isdigit(c);
                       }) == s.end();
}

inline void
ensure_is_number(std::string_view s) {
  if(!is_number(s)) {
    throw not_a_number(s);
  }
}
}
