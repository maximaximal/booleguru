#pragma once

#include <cassert>

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

    inline constexpr ret v(expression::op_ref o) { return o.left(); }
    inline constexpr ret vd(expression::op_ref o) { return o.left(); }

    inline constexpr ret ev(expression::op_ref o) { return o.left(); }
    inline constexpr ret evd(expression::op_ref o) { return o.left(); };

    inline constexpr ret e(expression::op_ref o) { return o.right(); }
    inline constexpr ret ed(expression::op_ref o) { return o.right(); };

    inline constexpr ret walk_exists(op_ref ex) {
      return ex.get_mgr().get(
        op(op_type::Exists, v(ex).get_id(), e(ex).get_id()));
    }
    inline constexpr ret walk_forall(op_ref ex) {
      return ex.get_mgr().get(op(op_type::Forall, ex.get_id(), e(ex).get_id()));
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
  using op_stack = std::stack<uint32_t>;

  op_stack stack;

  collect_tree_actor collect_;
  traverse_stack_actor traverse_;

  inline constexpr ReturnType traverse(expression::op_ref o) {
    using namespace expression;
    assert(o.valid());
    switch(o->type) {
      case op_type::Exists:
        return traverse_.walk_exists(o);
      case op_type::Forall:
        return traverse_.walk_forall(o);
      case op_type::Not:
        return traverse_.walk_not(o);
      case op_type::And:
        return traverse_.walk_and(o);
      case op_type::Or:
        return traverse_.walk_or(o);
      case op_type::Equi:
        return traverse_.walk_equi(o);
      case op_type::Impl:
        return traverse_.walk_impl(o);
      case op_type::Lpmi:
        return traverse_.walk_lpmi(o);
      case op_type::Xor:
        return traverse_.walk_xor(o);
      case op_type::Var:
        return traverse_.walk_var(o);
      case op_type::None:
        return o;
    }
    // Must never occur, but silences compiler warnings.
    assert(false);
    return ReturnType();
  }

  public:
  using op_ref = expression::op_ref;
  using op_type = expression::op_type;
  using op = expression::op;
  using variable = expression::variable;

  inline constexpr ReturnType operator()(op_ref o) { return traverse(o); }
};
}
