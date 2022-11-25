#pragma once

#include <memory>

#include "base.hpp"

namespace booleguru::expression {
class op_manager;
class var_manager;
}

namespace booleguru::parse {
class smtlib2 : public base {
  int fileno_ = -1;
  std::string str_;

  public:
  explicit smtlib2(int fileno, std::shared_ptr<expression::op_manager> ops);
  explicit smtlib2(std::string_view str,
                   std::shared_ptr<expression::op_manager> ops);
  virtual result operator()() override;
};
}
