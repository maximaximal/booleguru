#pragma once

#include <vector>

#include <booleguru/expression/id.hpp>
#include <booleguru/expression/op_manager.hpp>

namespace booleguru::transform {
struct exact_1 {
  exact_1(expression::op_manager& ops)
    : ops(ops) {}

  void add(expression::op_id o) {
    assert(o);
    subs.emplace_back(o);
  }

  expression::op_id finalize() {
    expression::op_id disj = 0;
    assert(subs.size() > 0);
    for(expression::op_id s : subs) {
      expression::op_id conj = 0;
      for(expression::op_id o : subs) {
        if(s == o) {
          conj = ops.encode_conjunct(conj, o);
        } else {
          conj = ops.encode_conjunct(conj, ops.encode_not(o));
        }
        assert(conj);
      }
      disj = ops.encode_disjunct(disj, conj);
      assert(disj);
    }
    assert(disj);
    return disj;
  }

  std::vector<expression::op_id> subs;
  expression::op_manager& ops;
};
}
