#pragma once

#include <cassert>
#include <functional>
#include <memory>
#include <stdint.h>
#include <type_traits>

#include <iostream>

#include <booleguru/expression/id.hpp>
#include <booleguru/expression/manager.hpp>
#include <booleguru/expression/op.hpp>
#include <booleguru/expression/reference.hpp>

namespace booleguru::expression {
struct op;
class op_manager;
class var_manager;

class op_ref : public reference<op, op_manager, op_id> {
  public:
  using reference<op, op_manager, op_id>::reference;

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

  id insert_id(objtype&& obj);

  inline var_manager& vars() { return *vars_; }
  inline std::shared_ptr<var_manager> vars_ptr() { return vars_; }
  inline const var_manager& vars() const { return *vars_; }

  op_ref top();
  op_ref bottom();

  using modifier = std::function<void(const op&)>;
  void modify_ops(modifier&& mod);
  void unmark();
  void mark_through_tree(id);

  template<typename F>
  inline void traverse_depth_first_through_tree(id root, F visit) {
    std::stack<op_manager::id> unvisited;
    unvisited.push(root);

    while(!unvisited.empty()) {
      op_id id = unvisited.top();
      const op& current = getobj(id);
      unvisited.pop();

      visit(id, current);

      switch(current.type) {
        case op_type::Exists:
          [[fallthrough]];
        case op_type::Forall:
          unvisited.push(current.quant.e);
          unvisited.push(current.quant.v);
          break;
        case op_type::Not:
          unvisited.push(current.un.c);
          break;
        case op_type::And:
          [[fallthrough]];
        case op_type::Or:
          [[fallthrough]];
        case op_type::Equi:
          [[fallthrough]];
        case op_type::Impl:
          [[fallthrough]];
        case op_type::Lpmi:
          [[fallthrough]];
        case op_type::Xor:
          unvisited.push(current.bin.r);
          unvisited.push(current.bin.l);
          break;
        case op_type::Var:
          [[fallthrough]];
        case op_type::None:
          break;
      }
    }
  }

  void traverse_unmarked_depth_first_through_tree(
    id root,
    std::function<void(id, const op&)> visit);
  void reset_op_user_vars();
  void reset_op_user_vars_and_mark();

  /** @brief Traverse the expression tree in postorder and provide a facility to
   * return a changed tree.
   */
  template<typename Visitor>
  std::invoke_result_t<Visitor, op_manager*, id> traverse_postorder_with_stack(
    id orig_root,
    Visitor visit) {

    // A reference to an op in the op_manager and a reference to the stack
    // itself to the parent of the currently traversed node.
    struct entry {
      id op;
      uint32_t parent;
      bool left;

      entry(id op, uint32_t parent, bool left)
        : op(op)
        , parent(parent)
        , left(left) {}
    };
    std::vector<entry> s;

    id root = orig_root;
    id r = 0;
    bool l = false;
    uint32_t parent = std::numeric_limits<uint32_t>::max();

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

      op_id right = getobj(root).right();
      if(right && !s.empty() && s.back().op == right) {
        s.pop_back();
        s.emplace_back(root, parent, l);
        parent = s.size() - 1;
        root = right;
        l = false;
      } else {
        if constexpr(std::is_same<
                       std::invoke_result_t<Visitor, op_manager*, id>,
                       id>()) {
          id new_root = visit(this, root);
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

    if constexpr(std::is_same<std::invoke_result_t<Visitor, op_manager*, id>,
                              id>()) {
      return r;
    }
  }

  template<typename Visitor>
  void traverse_preorder_with_stack(id root, Visitor visit) {
    if(root == 0)
      return;

    std::stack<id> s;
    s.emplace(root);

    while(!s.empty()) {
      id current = s.top();
      const auto& current_obj = getobj(current);
      s.pop();
      visit(current);

      if(current_obj.right())
        s.emplace(current_obj.right());
      if(current_obj.left())
        s.emplace(current_obj.left());
    }
  }

  // Some shorthands that come in handy when working purely with IDs.
  id encode_conjunct(id l, id r) {
    if(l && r) {
      return encode_and(l, r);
    } else if(l) {
      return l;
    } else if(r) {
      return r;
    }
    return 0;
  }
  id encode_disjunct(id l, id r) {
    if(l && r) {
      return encode_or(l, r);
    } else if(l) {
      return l;
    } else if(r) {
      return r;
    }
    return 0;
  }
  id encode_not(id l) { return get_id(op(op_type::Not, l)); }
  id encode_and(id l, id r) { return get_id(op(op_type::And, l, r)); }
  id encode_or(id l, id r) { return get_id(op(op_type::Or, l, r)); }
  id encode_equi(id l, id r) { return get_id(op(op_type::Equi, l, r)); }
  id encode_impl(id l, id r) { return get_id(op(op_type::Impl, l, r)); }
  id encode_xor(id l, id r) {
    id not_l = get_id(op(op_type::Not, l, 0));
    id not_r = get_id(op(op_type::Not, r, 0));
    id l_and = get_id(op(op_type::And, not_l, r));
    id r_and = get_id(op(op_type::And, l, not_r));
    return get_id(op(op_type::Or, l_and, r_and));
  }
  id encode_ite(id pred, id then, id otherwise) {
    id l = get_id(op(op_type::And, pred, then));
    id not_pred = get_id(op(op_type::Not, pred, 0));
    id r = get_id(op(op_type::And, not_pred, otherwise));
    return get_id(op(op_type::Or, l, r));
  }
};

op_ref inline
operator!(op_ref r) {
  assert(r.valid());
  return r.get_mgr().get(op(op_type::Not, r.get_id(), 0));
}

op_ref inline constexpr
operator&&(op_ref l, op_ref r) {
  if(!l.valid() && r.valid()) [[unlikely]]
    return r;
  if(l.valid() && !r.valid()) [[unlikely]]
    return l;
  assert(l.valid());
  assert(r.valid());
  return l.get_mgr().get(op(op_type::And, l.get_id(), r.get_id()));
}

op_ref inline constexpr
operator||(op_ref l, op_ref r) {
  if(!l.valid() && r.valid()) [[unlikely]]
    return r;
  if(l.valid() && !r.valid()) [[unlikely]]
    return l;
  assert(l.valid());
  assert(r.valid());
  return l.get_mgr().get(op(op_type::Or, l.get_id(), r.get_id()));
}

op_ref inline constexpr
operator^(op_ref l, op_ref r) {
  if(!l.valid() && r.valid()) [[unlikely]]
    return r;
  if(l.valid() && !r.valid()) [[unlikely]]
    return l;
  assert(l.valid());
  assert(r.valid());
  return l.get_mgr().get(op(op_type::Xor, l.get_id(), r.get_id()));
}

std::ostream&
operator<<(std::ostream& o, const op_ref& e);
}

namespace std {
template<>
struct hash<booleguru::expression::op_ref> {
  size_t operator()(const booleguru::expression::op_ref& x) const noexcept {
    return x.hash();
  }
};
}
