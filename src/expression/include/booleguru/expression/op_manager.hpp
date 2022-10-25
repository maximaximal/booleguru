#pragma once

#include <cassert>
#include <memory>

#include "manager.hpp"
#include "op.hpp"
#include "reference.hpp"

namespace booleguru::expression {
struct op;
class op_manager;
class var_manager;
class script_manager;

class op_ref : public reference<op, op_manager> {
  public:
  using reference<op, op_manager>::reference;
};

class op_manager : public manager<op_ref, op_manager> {
  public:
  using base = manager<op_ref, op_manager>;

  protected:
  std::shared_ptr<var_manager> vars_;
  std::shared_ptr<script_manager> scripts_;

  public:
  op_manager(std::shared_ptr<var_manager> vars,
             std::shared_ptr<script_manager> scripts);

  base::objref insert(T&& obj, size_t obj_hash);

  inline var_manager& vars() { return *vars_; }
  inline const var_manager& vars() const { return *vars_; }
  inline script_manager& scripts() { return *scripts_; }
  inline const script_manager& scripts() const { return *scripts_; }
};

op_ref inline
operator!(op_ref& r) {
  assert(r);
  return r.get_mgr().get(op(op_type::Not, r.get_id(), 0));
}

op_ref inline constexpr
operator&&(op_ref& l, op_ref& r) {
  assert(l);
  assert(r);
  return l.get_mgr().get(op(op_type::And, l.get_id(), r.get_id()));
}

op_ref inline constexpr
operator||(op_ref& l, op_ref& r) {
  assert(l);
  assert(r);
  return l.get_mgr().get(op(op_type::Or, l.get_id(), r.get_id()));
}

std::ostream&
operator<<(std::ostream& o, const op_ref& e);
}
