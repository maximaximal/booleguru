#pragma once

#include <cassert>
#include <functional>
#include <memory>

#include "manager.hpp"
#include "op.hpp"
#include "reference.hpp"

namespace booleguru::expression {
struct op;
class op_manager;
class var_manager;

class op_ref : public reference<op, op_manager> {
  public:
  using reference<op, op_manager>::reference;

  op_ref left();
  op_ref right();
};

class op_manager : public manager<op_ref, op_manager> {
  public:
  using base = manager<op_ref, op_manager>;

  protected:
  std::shared_ptr<var_manager> vars_;

  public:
  op_manager(std::shared_ptr<var_manager> vars);

  base::objref insert(T&& obj, size_t obj_hash);

  inline var_manager& vars() { return *vars_; }
  inline const var_manager& vars() const { return *vars_; }

  using modifier = std::function<void(op&)>;
  void modify_ops(modifier&& mod);
  void unmark();
  void mark_through_tree(uint32_t);
  void reset_op_user_vars();
};

op_ref inline
operator!(op_ref r) {
  assert(r.valid());
  return r.get_mgr().get(op(op_type::Not, r.get_id(), 0));
}

op_ref inline constexpr
operator&&(op_ref l, op_ref r) {
  assert(l.valid());
  assert(r.valid());
  return l.get_mgr().get(op(op_type::And, l.get_id(), r.get_id()));
}

op_ref inline constexpr
operator||(op_ref l, op_ref r) {
  assert(l.valid());
  assert(r.valid());
  return l.get_mgr().get(op(op_type::Or, l.get_id(), r.get_id()));
}

std::ostream&
operator<<(std::ostream& o, const op_ref& e);
}
