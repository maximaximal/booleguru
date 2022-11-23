#pragma once

#include <cassert>

#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

namespace booleguru::transform {
template<typename Derived, typename ReturnType = expression::op_ref>
struct visitor {
  private:
  struct dummy_op_ref {
    constexpr inline dummy_op_ref operator&&(dummy_op_ref o) noexcept {
      (void)o;
      return dummy_op_ref();
    }
    constexpr inline dummy_op_ref operator||(dummy_op_ref o) noexcept {
      (void)o;
      return dummy_op_ref();
    }
    constexpr inline dummy_op_ref operator!() const noexcept {
      return dummy_op_ref();
    }
  };

  using op_stack = std::stack<uint32_t>;

  struct collect_tree {
    using ret = dummy_op_ref;
    op_stack& stack;

    collect_tree(op_stack& stack)
      : stack(stack) {}

    inline ret l(expression::op o) { stack.push(o.bin.l); }
    inline ret r(expression::op o) { stack.push(o.bin.r); }
  };

  struct traverse_stack {
    using ret = ReturnType;
    using op_ref = expression::op_ref;
  };

  using collect_tree_actor = typename Derived::template actor<collect_tree>;
  using traverse_stack_actor = typename Derived::template actor<traverse_stack>;

  op_stack stack;

  collect_tree_actor collect_{ stack };
  collect_tree_actor traverse_;

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

  inline ReturnType operator()(op_ref o) {}

  inline ReturnType walk_exists(op_ref ex) {
    return ex.get_mgr().get(
      op(op_type::Exists, v(ex).get_id(), e(ex).get_id()));
  }
  inline ReturnType walk_forall(op_ref ex) {
    return ex.get_mgr().get(
      op(op_type::Forall, v(ex).get_id(), e(ex).get_id()));
  }
  inline ReturnType walk_not(op_ref ex) {
    return ex.get_mgr().get(op(op_type::Not, c(ex).get_id(), 0));
  }
  inline ReturnType walk_and(op_ref ex) {
    return ex.get_mgr().get(op(op_type::And, l(ex).get_id(), r(ex).get_id()));
  }
  inline ReturnType walk_or(op_ref ex) {
    return ex.get_mgr().get(op(op_type::Or, l(ex).get_id(), r(ex).get_id()));
  }
  inline ReturnType walk_equi(op_ref ex) {
    return ex.get_mgr().get(op(op_type::Equi, l(ex).get_id(), r(ex).get_id()));
  }
  inline ReturnType walk_impl(op_ref ex) {
    return ex.get_mgr().get(op(op_type::Impl, l(ex).get_id(), r(ex).get_id()));
  }
  inline ReturnType walk_lpmi(op_ref ex) {
    return ex.get_mgr().get(op(op_type::Lpmi, l(ex).get_id(), r(ex).get_id()));
  }
  inline ReturnType walk_xor(op_ref ex) {
    return ex.get_mgr().get(op(op_type::Xor, l(ex).get_id(), r(ex).get_id()));
  }
  inline ReturnType walk_var(op_ref ex) { return ex; }

  inline constexpr ReturnType l(op_ref e) {
    return (*static_cast<Derived*>(this))(e.get_mgr()[e->bin.l]);
  }
  inline constexpr ReturnType ld(op_ref e) { return e.get_mgr()[e->bin.l]; }
  inline constexpr ReturnType r(op_ref e) {
    return (*static_cast<Derived*>(this))(e.get_mgr()[e->bin.r]);
  }
  inline constexpr ReturnType rd(op_ref e) { return e.get_mgr()[e->bin.r]; }

  inline constexpr ReturnType c(op_ref e) {
    return (*static_cast<Derived*>(this))(e.get_mgr()[e->un.c]);
  }
  inline constexpr ReturnType cd(op_ref e) { return e.get_mgr()[e->un.c]; }

  inline constexpr ReturnType v(op_ref e) {
    return (*static_cast<Derived*>(this))(e.get_mgr()[e->quant.v]);
  }
  inline constexpr ReturnType vd(op_ref e) { return e.get_mgr()[e->quant.v]; }
  inline constexpr const std::string& name(op_ref e) {
    return e.get_mgr().vars()[e->var.v]->name;
  }

  inline constexpr ReturnType e(op_ref e) {
    return (*static_cast<Derived*>(this))(e.get_mgr()[e->quant.e]);
  }
  inline constexpr ReturnType ed(op_ref e) { return e.get_mgr()[e->quant.e]; }
};
}
