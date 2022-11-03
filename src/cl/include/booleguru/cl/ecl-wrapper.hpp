#pragma once

#include <memory>
#include <variant>

#include <booleguru/expression/op_manager.hpp>

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

  bool interactive_debugger_ = false;

  public:
  static ecl_wrapper& get();
  using supported_return_types =
    std::variant<std::monostate, long int, std::string, expression::op_ref>;
  supported_return_types eval(
    const char* code,
    std::shared_ptr<expression::op_manager> ops = nullptr);

  void interactive_debugger(bool enable);
};
}
