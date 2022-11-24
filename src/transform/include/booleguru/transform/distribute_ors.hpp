#pragma once

#include "visitor.hpp"

#include <iostream>

namespace booleguru::transform {
namespace actors {
template<class I>
struct distribute_ors : public I {
  using ret = typename I::ret;
  using op_type = expression::op_type;
  using I::I;

  inline ret walk_or(expression::op_ref e) {
    auto left = e.left();
    auto right = e.right();
    if(left->type == op_type::And) {
      std::cout << "Left was and! " << left << std::endl;
      auto re = I::rd(e);
      I::repeat_inner_lr = true;
      return (I::l(left) || re) && (I::r(left) || re);
    } else if(right->type == op_type::And) {
      auto le = I::ld(e);
      I::repeat_inner_lr = true;
      return (le || I::l(right)) && (le || I::r(right));
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
