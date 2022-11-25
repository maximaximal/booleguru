#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
struct distribute_ors : public visitor<distribute_ors> {
  inline op_ref walk_or(op_ref e) {
    auto left = ld(e);
    auto right = rd(e);
    if(left->type == op_type::And) {
      auto re = rd(e);
      return (*this)(ld(left) || re) && (*this)(rd(left) || re);
    } else if(right->type == op_type::And) {
      auto le = ld(e);
      return (*this)(le || ld(right)) && (*this)(le || rd(right));
    } else if(e->and_inside) {
      return (*this)(l(e) || r(e));
    } else {
      return l(e) || r(e);
    }
  }
};
}
