#pragma once

#include <cassert>

#include <iostream>

#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

namespace booleguru::transform {
struct visitor_descent_query {
  constexpr inline visitor_descent_query operator&&(
    visitor_descent_query o) noexcept {
    (void)o;
    return visitor_descent_query(*this, o);
  }
  constexpr inline visitor_descent_query operator||(
    visitor_descent_query o) noexcept {
    (void)o;
    return visitor_descent_query(*this, o);
  }
  constexpr inline visitor_descent_query operator!() const noexcept {
    return visitor_descent_query(*this);
  }

  explicit constexpr inline visitor_descent_query(visitor_descent_query l,
                                                  visitor_descent_query r)
    : descent(l.descent | r.descent) {}
  explicit constexpr inline visitor_descent_query(bool left = false,
                                                  bool right = false)
    : descent((left ? 1u << 1 : 0) | (right ? 1u : 0)) {}

  inline constexpr bool left() const { return descent & (1u << 1); }
  inline constexpr bool right() const { return descent & (1u << 0); }

  int descent = 0;
};

template<typename Derived,
         template<class impl>
         class Actor,
         typename ReturnType = expression::op_ref>
struct visitor {
  using descent_query = visitor_descent_query;

  struct collect_tree {
    using ret = descent_query;
    using op_ref = expression::op_ref;

    // Unused, but included for API-compatibility with traverse_stack.
    bool repeat_inner_lr = false;
    bool repeat = false;

    inline constexpr descent_query l(expression::op_ref o) {
      (void)o;
      return descent_query(true, false);
    }
    inline constexpr descent_query r(expression::op_ref o) {
      (void)o;
      return descent_query(false, true);
    }
    inline constexpr descent_query ld(expression::op_ref o) {
      (void)o;
      return descent_query(false, false);
    }
    inline constexpr descent_query rd(expression::op_ref o) {
      (void)o;
      return descent_query(false, false);
    }

    inline constexpr descent_query c(expression::op_ref o) {
      (void)o;
      return descent_query(true, false);
    }
    inline constexpr descent_query cd(expression::op_ref o) {
      (void)o;
      return descent_query(false, false);
    }

    inline constexpr descent_query v(expression::op_ref o) {
      (void)o;
      return descent_query(false, false);
    }
    inline constexpr descent_query vd(expression::op_ref o) {
      (void)o;
      return descent_query(false, false);
    }

    inline constexpr descent_query ev(expression::op_ref o) {
      (void)o;
      return descent_query(true, false);
    }
    inline constexpr descent_query evd(expression::op_ref o) {
      (void)o;
      return descent_query(false, false);
    }

    inline constexpr descent_query e(expression::op_ref o) {
      (void)o;
      return descent_query(false, true);
    }
    inline constexpr descent_query ed(expression::op_ref o) {
      (void)o;
      return descent_query(false, false);
    };

    inline constexpr ret walk_exists(op_ref ex) { return l(ex) || r(ex); }
    inline constexpr ret walk_forall(op_ref ex) { return l(ex) || r(ex); }
    inline constexpr ret walk_not(op_ref ex) { return l(ex); }
    inline constexpr ret walk_and(op_ref ex) { return l(ex) && r(ex); }
    inline constexpr ret walk_or(op_ref ex) { return l(ex) || r(ex); }
    inline constexpr ret walk_equi(op_ref ex) { return l(ex) || r(ex); }
    inline constexpr ret walk_impl(op_ref ex) { return l(ex) || r(ex); }
    inline constexpr ret walk_lpmi(op_ref ex) { return l(ex) || r(ex); }
    inline constexpr ret walk_xor(op_ref ex) { return l(ex) || r(ex); }
    inline constexpr ret walk_var(op_ref ex) { return l(ex); }
  };

  struct traverse_stack {
    using ret = ReturnType;
    using op_ref = expression::op_ref;

    // Repeats the recursion with the direct children of the produced
    // expression. This is a feature mostly needed for distribute_or.
    bool repeat_inner_lr = false;

    // Repeats the recursion with the produced expression.
    bool repeat = false;

    inline constexpr expression::op_ref l(expression::op_ref o) {
      return o.left();
    }
    inline constexpr ret r(expression::op_ref o) { return o.right(); }
    inline constexpr ret ld(expression::op_ref o) { return o.left(); }
    inline constexpr ret rd(expression::op_ref o) { return o.right(); }

    inline constexpr ret c(expression::op_ref o) { return o.left(); }
    inline constexpr ret cd(expression::op_ref o) { return o.left(); }

    inline constexpr ret ev(expression::op_ref o) { return o.left(); }
    inline constexpr ret evd(expression::op_ref o) { return o.left(); };

    inline constexpr ret e(expression::op_ref o) { return o.right(); }
    inline constexpr ret ed(expression::op_ref o) { return o.right(); };

    inline constexpr ret walk_exists(op_ref ex) {
      return ex.get_mgr().get(op(op_type::Exists, ex->quant.v, e(ex).get_id()));
    }
    inline constexpr ret walk_forall(op_ref ex) {
      return ex.get_mgr().get(op(op_type::Forall, ex->quant.v, e(ex).get_id()));
    }
    inline constexpr ret walk_not(op_ref ex) {
      return ex.get_mgr().get(op(op_type::Not, c(ex).get_id(), 0));
    }
    inline constexpr ret walk_and(op_ref ex) {
      return ex.get_mgr().get(op(op_type::And, l(ex).get_id(), r(ex).get_id()));
    }
    inline constexpr ret walk_or(op_ref ex) {
      return ex.get_mgr().get(op(op_type::Or, l(ex).get_id(), r(ex).get_id()));
    }
    inline constexpr ret walk_equi(op_ref ex) {
      return ex.get_mgr().get(
        op(op_type::Equi, l(ex).get_id(), r(ex).get_id()));
    }
    inline constexpr ret walk_impl(op_ref ex) {
      return ex.get_mgr().get(
        op(op_type::Impl, l(ex).get_id(), r(ex).get_id()));
    }
    inline constexpr ret walk_lpmi(op_ref ex) {
      return ex.get_mgr().get(
        op(op_type::Lpmi, l(ex).get_id(), r(ex).get_id()));
    }
    inline constexpr ret walk_xor(op_ref ex) {
      return ex.get_mgr().get(op(op_type::Xor, l(ex).get_id(), r(ex).get_id()));
    }
    inline constexpr ret walk_var(op_ref ex) {
      // Nothing more to walk for variables, this is always some leaf.
      return ex;
    }
  };

  using collect_tree_actor = Actor<collect_tree>;
  using traverse_stack_actor = Actor<traverse_stack>;

  struct op_stack_entry {
    bool left : 1;
    bool ignore : 1;
    uint32_t id;

    explicit op_stack_entry(uint32_t id, bool left = false, bool ignore = false)
      : left(left)
      , ignore(ignore)
      , id(id) {}
  };
  using op_stack = std::stack<op_stack_entry>;

  op_stack stack;

  collect_tree_actor collect_;
  traverse_stack_actor traverse_;

  template<class Walker>
  inline constexpr auto walk(Walker& w, expression::op_ref o) noexcept {
    using expression::op_type;

    assert(o.valid());
    switch(o->type) {
      case op_type::Exists:
        return w.walk_exists(o);
      case op_type::Forall:
        return w.walk_forall(o);
      case op_type::Not:
        return w.walk_not(o);
      case op_type::And:
        return w.walk_and(o);
      case op_type::Or:
        return w.walk_or(o);
      case op_type::Equi:
        return w.walk_equi(o);
      case op_type::Impl:
        return w.walk_impl(o);
      case op_type::Lpmi:
        return w.walk_lpmi(o);
      case op_type::Xor:
        return w.walk_xor(o);
      case op_type::Var:
        return w.walk_var(o);
      case op_type::None:
        assert(false);
        throw std::runtime_error("MUST visit some op!");
    }
    // Must never occur, but silences compiler warnings.
    assert(false);
    throw std::runtime_error("MUST visit some op!");
  }

  inline constexpr bool should_walk_left(expression::op_ref o) {
    if(!o.valid())
      return false;
    auto left = o.left();
    if(!left.valid() || left->mark)
      return false;
    return walk(collect_, o).left();
  }
  inline constexpr bool should_walk_right(expression::op_ref o) {
    if(!o.valid())
      return false;
    auto right = o.right();
    if(!right.valid() || right->mark)
      return false;
    return walk(collect_, o).right();
  }

  inline constexpr ReturnType traverse(expression::op_ref root) {
    using expression::op_ref;

    assert(root.valid());
    if(!root.valid())
      throw std::runtime_error("Invalid root supplied!");

    if(!stack.empty())
      stack = op_stack();

    expression::op_manager* mgr = &root.get_mgr();
    mgr->unmark();

    op_ref pre = op_ref(), last = root, last_pre = op_ref();
    bool came_from_left = true;
    while(root.valid() || !stack.empty()) {
      if(root.valid()) {
        stack.push(op_stack_entry(root.get_id(), came_from_left));
        root = should_walk_left(root) ? root.left() : op_ref();
        came_from_left = true;
      } else {
        auto top = stack.top();
        root.set_id(top.id);
        root.set_mgr(mgr);
        auto root_r = should_walk_right(root) ? root.right() : op_ref();
        if(!root_r.valid() || root_r == pre) {
          last_pre = pre;
          pre = root;
          if(!top.ignore) {
            do {
              traverse_.repeat = false;
              traverse_.repeat_inner_lr = false;
              assert(root.valid());
              if constexpr(std::same_as<ReturnType, op_ref>) {
                auto res = walk(traverse_, root);
                std::cout << "Left: " << root.left() << std::endl;
                std::cout << "Right: " << root.right() << std::endl;
                std::cout << "From " << root << " became " << res << std::endl;
                root = res;
                root->mark = true;
              } else {
                walk(traverse_, root);
              }
              assert(root.valid());
            } while(traverse_.repeat);
          }
          last = root;
          stack.pop();
          if(!stack.empty() /* require some parent */) {
            // Have to replace this op also in the parent op! Do this via a
            // different entry on the stack.
            op_ref parent = op_ref(*mgr, stack.top().id);
            bool left = stack.top().left;
            bool ignore = stack.top().ignore;
            if(top.left) {
              if(parent->left() != root.get_id()) {
                expression::op new_parent(
                  parent->type, root.get_id(), parent->right());
                stack.pop();
                auto id = mgr->get(std::move(new_parent)).get_id();
                stack.push(op_stack_entry(id, left, ignore));
              }
            } else {
              if(parent->right() != root.get_id()) {
                expression::op new_parent(
                  parent->type, parent->left(), root.get_id());
                stack.pop();
                auto id = mgr->get(std::move(new_parent)).get_id();
                stack.push(op_stack_entry(id, left, ignore));
              }
            }
          }
          if(traverse_.repeat_inner_lr) {
            std::cout << root << " traverse lr : " << root.left() << ", "
                      << root.right() << std::endl;
            // Must repeat traversal with left and right of this node again! As
            // if left and right were never visited before.
            root->mark = false;
            stack.push(op_stack_entry(root.get_id(), top.left, true));
            stack.push(op_stack_entry(root->left(), true));
          }
          root.set_id(0);
        } else {
          root = root_r;
          came_from_left = false;
        }
      }
    }
    return last;
  }

  public:
  using op_ref = expression::op_ref;
  using op_type = expression::op_type;
  using op = expression::op;
  using variable = expression::variable;

  inline constexpr ReturnType operator()(op_ref o) { return traverse(o); }
};
}
