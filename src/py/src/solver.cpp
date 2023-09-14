#include "booleguru/lua/binding-helpers.hpp"
#include <booleguru/py/solver.hpp>
#include <booleguru/solve/sat.hpp>

namespace booleguru::py {
void
solver::add(expression::op_ref op) {
  if(op_.valid()) {
    op_ = op_ && op;
  } else {
    op_ = op;
  }
}

check_sat_result
solver::check() {
  solve::sat s;
  auto res = s.solve_resultmap(op_);
  if(res) {
    model_.values_ = *res;
    current_result_ = check_sat_result(10);
  } else {
    model_.values_.clear();
    current_result_ = check_sat_result(20);
  }
  return current_result_;
}

model&
solver::model() {
  return model_;
}
}
