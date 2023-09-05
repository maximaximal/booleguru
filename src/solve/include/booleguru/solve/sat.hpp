#pragma once

#include <optional>
#include <string>
#include <vector>

#include "result.hpp"

#include <booleguru/expression/op_manager.hpp>

struct xcc_sat_solver;

namespace booleguru::solve {
class sat {
  std::string solver_;
  std::string solver_path_;
  std::vector<std::string> solver_args_;
  char* command_ = nullptr;
  std::vector<char*> args_;

  void free_command_and_args();

  int solve_with_solver(expression::op_ref& o, xcc_sat_solver& solver);

  public:
  sat(std::string solver = "kissat", std::vector<std::string> args = { "-q" });
  ~sat();

  std::optional<expression::op_ref> solve(expression::op_ref o);
  std::optional<std::unordered_map<expression::op_ref, bool>> solve_resultmap(
    expression::op_ref o);
};
}
