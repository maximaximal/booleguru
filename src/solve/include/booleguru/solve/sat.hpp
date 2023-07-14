#pragma once

#include <string>
#include <vector>
#include <optional>

#include "result.hpp"

#include <booleguru/expression/op_manager.hpp>

namespace booleguru::solve {
class sat {
  std::string solver_;
  std::string solver_path_;
  std::vector<std::string> solver_args_;

  public:
  sat(std::string solver = "kissat", std::vector<std::string> args = { "-q" });

  std::optional<expression::op_ref> solve(expression::op_ref o);
};
}
