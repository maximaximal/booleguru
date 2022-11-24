#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
namespace actors {
template<class I>
struct distribute_nots : public I {
  using ret = typename I::ret;
  using op_type = expression::op_type;
  using I::I;

  inline ret walk_not(expression::op_ref e) {
    auto child = e.left();
    if(child->type == op_type::And) {
      I::repeat_inner_lr = true;
      return !I::l(child) || !I::r(child);
    } else if(child->type == op_type::Or) {
      I::repeat_inner_lr = true;
      return !I::l(child) && !I::r(child);
    } else if(child->type == op_type::Not) {
      return I::c(child);
    } else {
      return I::ld(e);
    }
  }
};
}
struct distribute_nots
  : public visitor<distribute_nots, actors::distribute_nots> {};
}
