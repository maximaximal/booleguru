#pragma once

#include <vector>

#include "argument.hpp"

namespace booleguru::expression {
class op_ref;
}

namespace booleguru::cli {
class input_file {
  std::string_view path_;
  std::vector<argument> args_;

  public:
  input_file(std::string_view path, std::vector<argument>&& args);

  expression::op_ref process();
};
}
