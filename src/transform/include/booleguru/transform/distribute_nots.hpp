#pragma once

#include "visitor.hpp"

namespace booleguru::transform {
struct distribute_nots : public visitor<distribute_nots> {
  inline op_ref walk_not(op_ref e) {
    auto child = cd(e);
    if(child->type == op_type::And) {
      return (*this)(!l(child)) || (*this)(!r(child));
    } else if(child->type == op_type::Or) {
      return (*this)(!l(child)) && (*this)(!r(child));
    } else if(child->type == op_type::Not) {
      return c(child);
    } else {
      return e.get_mgr().get(op(op_type::Not, e->un.c, 0));
    }
  }
};
}
