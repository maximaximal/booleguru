#pragma once

#include <memory>
#include <variant>

namespace booleguru::cl {
/** @brief Wraps ECL being initialized.
 *
 * Only one instance of this may be in the entire codebase, so this is a
 * singleton.
 *
 * The common lisp runtime is shutdown at exit.
 */
class ecl_wrapper {
  ecl_wrapper();
  ~ecl_wrapper();

  struct deleter {
    void operator()(ecl_wrapper* w) { delete w; }
  };
  static std::unique_ptr<ecl_wrapper, deleter> wrapper_;

  public:
  static ecl_wrapper& get();
  using supported_return_types = std::variant<std::monostate, long int>;
  supported_return_types eval(const char* code);
};
}
