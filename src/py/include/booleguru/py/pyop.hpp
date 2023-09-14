#pragma once

#include <cassert>

#include <booleguru/expression/op_manager.hpp>

/** @file
 *
 * Such a wrapper is required in order to support op == op returning an Equi op
 * and to keep clean between the regular op interface and the pythonic
 * interface.
 */

namespace booleguru::py {
struct pyop_ref : expression::op_ref {
  using expression::op_ref::op_ref;

  pyop_ref(const expression::op_ref& o)
    : op_ref(o) {}
  pyop_ref(expression::op_ref&& o)
    : op_ref(o) {}
};

inline pyop_ref
operator==(pyop_ref& l, pyop_ref& r) {
  assert(&l.get_mgr() == &r.get_mgr());

  expression::op_ref eq = l.get_mgr().get(
    expression::op(expression::op_type::Equi, l.get_id(), r.get_id()));

  return eq;
}
}

namespace std {
template<>
struct hash<booleguru::py::pyop_ref> {
  size_t operator()(const booleguru::py::pyop_ref& x) const noexcept {
    return x.hash();
  }
};
}
