#pragma once

#include <cassert>
#include <memory>

#include <booleguru/expression/op_manager.hpp>

namespace booleguru::parse {
struct result {
  expression::op_ref expr;

  int line = 0;
  int column = 0;
  std::string message;

  enum error_code {
    OTHER,
    INCORRECT_IDENT_FOLLOWUP,
    LISP_NO_RETURN_EXPRESSION,
  } code;

  inline operator bool() const { return expr.valid(); }
  inline expression::op_ref* operator->() { return &**this; }
  inline expression::op_ref& operator*() {
    assert(*this);
    return expr;
  }
};
}
