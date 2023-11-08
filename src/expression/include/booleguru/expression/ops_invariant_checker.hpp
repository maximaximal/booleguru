#pragma once

#include <cstddef>

#include "op.hpp"

namespace booleguru::expression {
struct op;

class ops_invariant_checker {
  const op* first_;

  public:
  ops_invariant_checker(const op* first)
    : first_(first) {}

  inline bool operator()(const op& op) const {
    const struct op* op_ptr = &op;
    using enum op_type;
    op_id id = op_ptr - first_;
    if(op_ptr->type > op_type::Var) {
      return false;
    }
    switch(op_ptr->type) {
      case None:
        return false;
      case Exists:
        [[fallthrough]];
      case Forall:
        if(op_ptr->quant.v >= id || op_ptr->quant.e >= id)
          return false;
        break;
      case Equi:
        [[fallthrough]];
      case Impl:
        [[fallthrough]];
      case Lpmi:
        [[fallthrough]];
      case Or:
        [[fallthrough]];
      case And:
        [[fallthrough]];
      case Xor:
        if(op_ptr->bin.l >= id || op_ptr->bin.r >= id)
          return false;
        break;
      case Not:
        if(op_ptr->un.c >= id)
          return false;
        break;
      case Var:
        break;
    }
    return true;
  }
};
}
