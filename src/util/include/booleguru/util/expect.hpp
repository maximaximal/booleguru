#pragma once

#include <source_location>
#include <stdexcept>
#include <string_view>

#include <fmt/format.h>

/* @file This is the main error handling entry point of Booleguru. Different
 * variants of error handling are used in debug or release builds. Performance
 * builds may completely turn off assert/expect checking. */

namespace booleguru::util {
enum class error : unsigned int { generic = 0, tree_traversal, count_ };

constexpr std::string_view error_names[] = { "generic", "tree traversal" };

struct unexpected_error : std::runtime_error {
  unexpected_error(error e, std::string_view msg, const std::source_location& l)
    : std::runtime_error(build_msg(e, msg, l)) {
    static_assert((sizeof(error_names) / sizeof(std::string_view))
                  == static_cast<size_t>(error::count_));
  }

  static std::string build_msg(error e,
                               std::string_view msg,
                               const std::source_location& l) {
    if(msg != "") {
      return fmt::format("{} error, at {}:{}:{} in {}, message: {}",
                         error_names[static_cast<size_t>(e)],
                         l.file_name(),
                         l.line(),
                         l.column(),
                         l.function_name(),
                         msg);
    } else {
      return fmt::format("{} error, at {}:{}:{} in {}",
                         error_names[static_cast<size_t>(e)],
                         l.file_name(),
                         l.line(),
                         l.column(),
                         l.function_name());
    }
  }
};

enum class error_action { ignore, throwing, terminating, logging };

constexpr error_action default_error_action = error_action::throwing;

template<class C, error_action action = default_error_action>
inline constexpr void
expect(C cond,
       error e = error::generic,
       std::string_view msg = "",
       const std::source_location location = std::source_location::current()) {
  if(cond())
    return;

  if constexpr(action == error_action::ignore) {
    return;
  } else if constexpr(action == error_action::throwing) {
    throw unexpected_error(e, msg, location);
  } else if constexpr(action == error_action::terminating) {
    fmt::println("{}", unexpected_error::build_msg(e, msg, location));
    std::terminate();
  } else if constexpr(action == error_action::logging) {
    fmt::println("{}", unexpected_error::build_msg(e, msg, location));
  }
}
}

#define EXPECT(C) booleguru::util::expect([&]() -> bool { return C; })
#define EXPECTE(C, E)                                  \
  booleguru::util::expect([&]() -> bool { return C; }, \
                          booleguru::util::error::E)
