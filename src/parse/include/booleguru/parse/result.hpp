#pragma once

#include <memory>

#include <booleguru/expression/op_manager.hpp>

namespace booleguru::parse {
struct result {
  expression::op_ref expr;
  std::shared_ptr<expression::var_manager> vars;
  std::shared_ptr<expression::script_manager> scripts;
  std::shared_ptr<expression::op_manager> ops;

  int line = 0;
  int column = 0;
  std::string message;

  operator bool() { return expr.valid(); }
};
}
