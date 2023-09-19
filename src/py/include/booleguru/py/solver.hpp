#pragma once

#include <cassert>
#include <unordered_map>

#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>

#include "check_sat_result.hpp"
#include "model.hpp"

namespace booleguru::py {
class solver {
  expression::op_ref op_;

  py::model model_;
  check_sat_result current_result_;

  public:
  void add(expression::op_ref op);
  check_sat_result check();
  py::model& model();

  inline const expression::op_ref& op() const { return op_; }
};
}
