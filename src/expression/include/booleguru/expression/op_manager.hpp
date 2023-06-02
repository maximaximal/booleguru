#pragma once

#include <cassert>
#include <functional>
#include <memory>
#include <type_traits>

#include <iostream>

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

  base::ref insert_id(T&& obj);

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

  /** @brief Traverse the expression tree in postorder and provide a facility to
   * return a changed tree.
   */
  template<typename Visitor>
  std::invoke_result_t<Visitor, op_manager*, ref> traverse_postorder_with_stack(
    ref orig_root,
    Visitor visit) {

    // A reference to an op in the op_manager and a reference to the stack
    // itself to the parent of the currently traversed node.
    struct entry {
      ref op;
      uint32_t parent;
      bool left;

      entry(ref op, uint32_t parent, bool left)
        : op(op)
        , parent(parent)
        , left(left) {}
    };
    std::vector<entry> s;

    ref root = orig_root;
    uint32_t r = 0;
    bool l = false;
    ref parent = std::numeric_limits<uint32_t>::max();

    do {
      while(root) {
        auto right = getobj(root).right();
        if(right)
          s.emplace_back(right, s.size(), false);
        s.emplace_back(root, parent, l);
        auto left = getobj(root).left();
        parent = s.size() - 1;
        root = left;
        l = true;
      }

      root = s.back().op;
      parent = s.back().parent;
      l = s.back().left;
      s.pop_back();

      ref right = getobj(root).right();
      if(right && s.back().op == right) {
        s.pop_back();
        s.emplace_back(root, parent, l);
        parent = s.size() - 1;
        root = right;
        l = false;
      } else {
        if constexpr(std::is_same<
                       std::invoke_result_t<Visitor, op_manager*, ref>,
                       ref>()) {
          uint32_t new_root = visit(this, root);
          if(parent != std::numeric_limits<uint32_t>::max()) {
            assert(parent < s.size());
            if(l) {
              op_ref o = (*this)[s[parent].op];
              s[parent].op = get_id(op(o->type, new_root, o->right()));
            } else {
              op_ref o = (*this)[s[parent].op];
              s[parent].op = get_id(op(o->type, o->left(), new_root));
            }
          }
          r = new_root;
        } else {
          visit(this, root);
        }
        root = 0;
        parent = std::numeric_limits<uint32_t>::max();
      }
    } while(!s.empty());

    if constexpr(std::is_same<std::invoke_result_t<Visitor, op_manager*, ref>,
                              ref>()) {
      return r;
    }
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
