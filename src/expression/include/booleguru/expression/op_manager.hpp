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

  std::string to_string() const;
};

class op_manager : public manager<op_ref, op_manager> {
  public:
  using base = manager<op_ref, op_manager>;

  protected:
  std::shared_ptr<var_manager> vars_;

  public:
  op_manager();
  op_manager(std::shared_ptr<var_manager> vars);

  base::objref insert(T&& obj);

  inline var_manager& vars() { return *vars_; }
  inline std::shared_ptr<var_manager> vars_ptr() { return vars_; }
  inline const var_manager& vars() const { return *vars_; }

  using modifier = std::function<void(const op&)>;
  void modify_ops(modifier&& mod);
  void unmark();
  void mark_through_tree(uint32_t);
  void traverse_depth_first_through_tree(
    uint32_t root,
    std::function<void(uint32_t, const op&)>& visit);
  void traverse_unmarked_depth_first_through_tree(
    uint32_t root,
    std::function<void(uint32_t, const op&)> visit);
  void reset_op_user_vars();

  template<typename Visitor>
  void traverse_postorder_with_stack(ref root, Visitor visit) {
    std::stack<ref> s;

    do {
      while(root) {
        auto right = getobj(root).right();
        if(right)
          s.push(right);
        s.push(root);
        auto left = getobj(root).left();
        root = left;
      }

      root = s.top();
      s.pop();

      auto right = getobj(root).right();
      if(right && s.top() == right) {
        s.pop();
        s.push(root);
        root = right;
      } else {
        visit(this, root);
        root = 0;
      }
    } while(!s.empty());
  }
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

op_ref inline constexpr
operator^(op_ref l, op_ref r) {
  assert(l.valid());
  assert(r.valid());
  return l.get_mgr().get(op(op_type::Xor, l.get_id(), r.get_id()));
}

std::ostream&
operator<<(std::ostream& o, const op_ref& e);
}
