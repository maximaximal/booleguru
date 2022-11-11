#include <cassert>
#include <ostream>

#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

namespace booleguru::expression {
inline constexpr static bool
parens_required(op_type p, op_type c) {
  return static_cast<size_t>(p) > static_cast<size_t>(c);
}

template<typename T>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const T& expr,
               op_type parent_type = op_type::None) {
  (void)o;
  (void)exprs;
  (void)t;
  (void)expr;
  (void)parent_type;
  assert(false);
}

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::un)& op,
               op_type parent_type);

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::bin)& op,
               op_type parent_type);

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::var)& op,
               op_type parent_type);

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::quant)& op,
               op_type parent_type);

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::bin)& op,
               op_type parent_type) {
  bool parens = parens_required(parent_type, t);
  if(parens)
    o << "(";
  exprs[op.l]->visit([&o, &exprs, t](op_type ct, auto& e) {
    op_tostr_visit(o, exprs, ct, e, t);
  });
  o << " " << op_type_to_sym(t) << " ";
  exprs[op.r]->visit([&o, &exprs, t](op_type ct, auto& e) {
    op_tostr_visit(o, exprs, ct, e, t);
  });
  if(parens)
    o << ")";
  return o;
}

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::quant)& expr,
               op_type parent_type) {
  (void)parent_type;
  o << op_type_to_sym(t) << exprs.vars()[expr.v]->name << " ";
  exprs[expr.e]->visit([&o, &exprs, t](op_type ct, auto& e) {
    op_tostr_visit(o, exprs, ct, e, t);
  });
  return o;
}

op_ref
op_ref::left() {
  return get_mgr()[(*this)->left()];
}

op_ref
op_ref::right() {
  return get_mgr()[(*this)->right()];
}

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::un)& expr,
               op_type parent_type) {
  (void)parent_type;
  o << op_type_to_sym(t);
  exprs[expr.c]->visit([&o, &exprs, t](op_type ct, auto& e) {
    op_tostr_visit(o, exprs, ct, e, t);
  });
  return o;
}

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::var)& expr,
               op_type parent_type) {
  (void)t;
  (void)parent_type;
  return o << exprs.vars()[expr.v]->name;
}

std::ostream&
operator<<(std::ostream& o, const op_ref& e) {
  if(!e.valid())
    return o << "(-)";
  e->visit([&o, &e](op_type t, const auto& ex) {
    op_tostr_visit(o, e.get_mgr(), t, ex);
  });
  return o;
}

op_manager::op_manager(std::shared_ptr<var_manager> vars)
  : vars_(vars) {}

op_manager::base::objref
op_manager::insert(T&& obj, size_t obj_hash) {
  switch(obj.type) {
    case op_type::And:
    case op_type::Or:
    case op_type::Lpmi:
    case op_type::Impl:
    case op_type::Equi:
      obj.and_inside =
        (*this)[obj.left()]->and_inside || (*this)[obj.right()]->and_inside;
      break;
    case op_type::Not:
      obj.and_inside = (*this)[obj.left()]->and_inside;
      break;
    case op_type::Exists:
    case op_type::Forall:
      obj.and_inside = (*this)[obj.left()]->and_inside;
      break;
    default:
      break;
  }

  return base::insert(std::move(obj), obj_hash);
}
}
