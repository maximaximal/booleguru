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

  /** @brief Traverse the expression tree in postorder and provide a facility to
   * return a changed tree.
   *
   * If visit(op_manager*,ref) returns a non-0 result that is different from the
   * ref it was given, the parent node that is traversed later will get the
   * replaced node in the correct child slot. This makes ad-hoc changes of the
   * tree without recursion more elegant.
   */
  template<typename Visitor>
  std::invoke_result_t<Visitor, op_manager*, ref> traverse_postorder_with_stack(
    ref orig_root,
    Visitor visit) {
    enum class dir { left, right, none };
    std::vector<std::pair<ref, dir>> s;

    auto propagate = [this, &s](ref old_ref, ref new_ref, dir d) -> void {
      ssize_t i = s.size() - 1;

      while(old_ref != new_ref && i >= 0) {
        switch(d) {
          case dir::left: {
            // The stack entry above the current one could already be the
            // parent, if there was no right entry. Check the direction of the
            // parent first.

            dir immediate_parent_dir = s[i].second;
            ssize_t dist = immediate_parent_dir == dir::right ? 1 : 0;

            assert(i >= dist);
            ref& parent = s[i - dist].first;
            d = s[i - dist].second;
            auto& parent_obj = getobj(parent);
            assert(parent_obj.left() == old_ref);
            old_ref = parent;
            new_ref = get_id(op(parent_obj.type, new_ref, parent_obj.right()));
            parent = new_ref;
            i = i - 1 - dist;
            break;
          }
          case dir::right: {
            assert(i >= 1);
            ref& parent = s[i].first;
            d = s[i].second;
            auto& parent_obj = getobj(parent);
            assert(parent_obj.right() == old_ref);
            old_ref = parent;
            new_ref = get_id(op(parent_obj.type, parent_obj.left(), new_ref));
            parent = new_ref;
            i -= 1;
            break;
          }
          case dir::none:
            break;
        }
      }
    };

    ref root = orig_root;
    uint32_t r = 0;
    dir d = dir::none;

    do {
      while(root) {
        auto right = getobj(root).right();
        if(right)
          s.emplace_back(std::make_pair(right, dir::right));
        s.emplace_back(std::make_pair(root, d));
        auto left = getobj(root).left();
        root = left;
        d = dir::left;
      }

      root = s.back().first;
      d = s.back().second;
      s.pop_back();

      ref right = getobj(root).right();
      if(right && s.back().first == right) {
        s.pop_back();
        s.emplace_back(std::make_pair(root, d));
        root = right;
        d = dir::right;
      } else {
        if constexpr(std::is_same<
                       std::invoke_result_t<Visitor, op_manager*, ref>,
                       ref>()) {
          uint32_t new_root = visit(this, root);
          if(new_root && new_root != root) {
            // Change the old entry and modify the parent in the tree to have
            // the new child node.
            propagate(root, new_root, d);
          } else {
            new_root = root;
          }
          r = new_root;
        } else {
          visit(this, root);
        }
        root = 0;
        d = dir::none;
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
