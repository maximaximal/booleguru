#pragma once

#include <cassert>
#include <functional>
#include <memory>
#include <type_traits>

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
    ref root,
    Visitor visit) {
    enum class dir { left, right, none };
    std::vector<std::pair<ref, dir>> s;

    uint32_t r = 0;

    do {
      while(root) {
        auto right = getobj(root).right();
        if(right)
          s.emplace_back(std::make_pair(right, dir::right));
        s.emplace_back(std::make_pair(root, s.empty() ? dir::none : dir::left));
        auto left = getobj(root).left();
        root = left;
      }

      root = s.back().first;
      dir d = s.back().second;
      s.pop_back();

      auto right = getobj(root).right();
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
            switch(d) {
              case dir::left:
                assert(s.size() >= 2);
                assert(getobj(s[s.size() - 2].first).left() == root);
                break;
              case dir::right:
                assert(s.size() >= 1);
                assert(getobj(s[s.size() - 1].first).right() == root);
                break;
              case dir::none:
                break;
            }
          } else {
            new_root = root;
          }
          r = new_root;
        } else {
          visit(this, root);
        }
        root = 0;
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
