#pragma once

#include <cassert>
#include <ostream>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

namespace booleguru::serialize {
template<class Derived>
class base {
  protected:
  std::ostream& o_;

  using op_ref = expression::op_ref;
  using op_type = expression::op_type;
  using op = expression::op;
  using variable = expression::variable;

  inline void walk(op_ref o) {
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
        return;
    }
    // Should never occur, but silences compiler warnings.
    return;
  }

  inline void walk_exists(op_ref o) { (void)o; };
  inline void walk_forall(op_ref o) { (void)o; };
  inline void walk_not(op_ref o) { (void)o; };
  inline void walk_and(op_ref o) { (void)o; };
  inline void walk_or(op_ref o) { (void)o; };
  inline void walk_equi(op_ref o) { (void)o; };
  inline void walk_impl(op_ref o) { (void)o; };
  inline void walk_lpmi(op_ref o) { (void)o; };
  inline void walk_xor(op_ref o) { (void)o; };
  inline void walk_var(op_ref o) { (void)o; };

  public:
  base(std::ostream& o)
    : o_(o) {}
  ~base() = default;
};
}
