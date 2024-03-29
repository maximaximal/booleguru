#pragma once

#include <cassert>

#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

namespace booleguru::transform {
template<typename Derived, typename ReturnType = expression::op_ref>
struct visitor {
  using op_ref = expression::op_ref;
  using op_type = expression::op_type;
  using op = expression::op;
  using op_id = expression::op_id;
  using variable = expression::variable;

  inline ReturnType operator()(op_ref o) {
    auto out = visit(o);
    return static_cast<Derived*>(this)->post_action(out);
  }

  inline ReturnType post_action(op_ref o) { return o; }

  inline ReturnType visit(op_ref o) {
    using namespace expression;
    assert(o.valid());
    switch(o->type) {
      case op_type::Exists:
        return static_cast<Derived*>(this)->walk_exists(o);
      case op_type::Forall:
        return static_cast<Derived*>(this)->walk_forall(o);
      case op_type::Not:
        return static_cast<Derived*>(this)->walk_not(o);
      case op_type::And:
        return static_cast<Derived*>(this)->walk_and(o);
      case op_type::Or:
        return static_cast<Derived*>(this)->walk_or(o);
      case op_type::Equi:
        return static_cast<Derived*>(this)->walk_equi(o);
      case op_type::Impl:
        return static_cast<Derived*>(this)->walk_impl(o);
      case op_type::Lpmi:
        return static_cast<Derived*>(this)->walk_lpmi(o);
      case op_type::Xor:
        return static_cast<Derived*>(this)->walk_xor(o);
      case op_type::Var:
        return static_cast<Derived*>(this)->walk_var(o);
      case op_type::None:
        return o;
    }
    // Must never occur, but silences compiler warnings.
    assert(false);
    return ReturnType();
  }

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
    op_id left = l(ex).get_id();
    op_id right = r(ex).get_id();
    return ex.get_mgr().get(op(op_type::And, left, right));
  }
  inline ReturnType walk_or(op_ref ex) {
    op_id left = l(ex).get_id();
    op_id right = r(ex).get_id();
    return ex.get_mgr().get(op(op_type::Or, left, right));
  }
  inline ReturnType walk_equi(op_ref ex) {
    op_id left = l(ex).get_id();
    op_id right = r(ex).get_id();
    return ex.get_mgr().get(op(op_type::Equi, left, right));
  }
  inline ReturnType walk_impl(op_ref ex) {
    op_id left = l(ex).get_id();
    op_id right = r(ex).get_id();
    return ex.get_mgr().get(op(op_type::Impl, left, right));
  }
  inline ReturnType walk_lpmi(op_ref ex) {
    op_id left = l(ex).get_id();
    op_id right = r(ex).get_id();
    return ex.get_mgr().get(op(op_type::Lpmi, left, right));
  }
  inline ReturnType walk_xor(op_ref ex) {
    op_id left = l(ex).get_id();
    op_id right = r(ex).get_id();
    return ex.get_mgr().get(op(op_type::Xor, left, right));
  }
  inline ReturnType walk_var(op_ref ex) { return ex; }

  inline constexpr ReturnType l(op_ref e) {
    return (*static_cast<Derived*>(this)).visit(e.get_mgr()[e->bin.l]);
  }
  inline constexpr ReturnType ld(op_ref e) { return e.get_mgr()[e->bin.l]; }
  inline constexpr ReturnType r(op_ref e) {
    return (*static_cast<Derived*>(this)).visit(e.get_mgr()[e->bin.r]);
  }
  inline constexpr ReturnType rd(op_ref e) { return e.get_mgr()[e->bin.r]; }

  inline constexpr ReturnType c(op_ref e) {
    return (*static_cast<Derived*>(this)).visit(e.get_mgr()[e->un.c]);
  }
  inline constexpr ReturnType cd(op_ref e) { return e.get_mgr()[e->un.c]; }

  inline constexpr ReturnType v(op_ref e) {
    return (*static_cast<Derived*>(this)).visit(e.get_mgr()[e->quant.v]);
  }
  inline constexpr ReturnType vd(op_ref e) { return e.get_mgr()[e->quant.v]; }
  inline constexpr const std::string& name(op_ref e) {
    return e.get_mgr().vars()[e->var.v]->name;
  }

  inline constexpr ReturnType e(op_ref e) {
    return (*static_cast<Derived*>(this)).visit(e.get_mgr()[e->quant.e]);
  }
  inline constexpr ReturnType ed(op_ref e) { return e.get_mgr()[e->quant.e]; }
};
}
