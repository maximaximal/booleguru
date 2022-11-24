#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
namespace actors {
template<class I>
struct distribute_ors : public I {
  using ret = typename I::ret;
  using op_type = expression::op_type;
  using I::I;

  inline ret walk_or(expression::op_ref e) {
    auto left = I::ld(e);
    auto right = I::rd(e);
    if(left->type == op_type::And) {
      auto re = I::rd(e);
      I::repeat_inner_lr = true;
      return (I::ld(left) || re) && (I::rd(left) || re);
    } else if(right->type == op_type::And) {
      auto le = I::ld(e);
      I::repeat_inner_lr = true;
      return (le || I::ld(right)) && (le || I::rd(right));
    } else if(e->and_inside) {
      I::repeat = true;
      return I::l(e) || I::r(e);
    } else {
      return I::l(e) || I::r(e);
    }
  }
};
}

struct distribute_ors
  : public visitor<distribute_ors, actors::distribute_ors> {};
}
