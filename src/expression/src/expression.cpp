#include <booleguru/expression/op.hpp>

#include "expression.hpp"
#include "transforms.hpp"
#include <iostream>

namespace booleguru::expression {
const char*
op_type_to_str(op_type t) {
  switch(t) {
    case op_type::And:
      return "And";
    case op_type::Equi:
      return "Equivalence";
    case op_type::Exists:
      return "Exists";
    case op_type::Forall:
      return "Forall";
    case op_type::Impl:
      return "Impl";
    case op_type::Lpmi:
      return "Lpmi";
    case op_type::Not:
      return "Not";
    case op_type::Or:
      return "Or";
    case op_type::Var:
      return "Var";
    case op_type::Script:
      return "Script";
    case op_type::None:
      return "None";
  }
  return "Unknown";
}

const char*
op_type_to_sym(op_type t) {
  switch(t) {
    case op_type::And:
      return "&";
    case op_type::Equi:
      return "<->";
    case op_type::Exists:
      return "?";
    case op_type::Forall:
      return "#";
    case op_type::Impl:
      return "->";
    case op_type::Lpmi:
      return "<-";
    case op_type::Not:
      return "!";
    case op_type::Or:
      return "|";
    case op_type::None:
      return "(-)";
    default:
      return "No Symbol";
  }
}

inline constexpr static bool
parens_required(op_type p, op_type c) {
  return static_cast<size_t>(p) > static_cast<size_t>(c);
}

template<typename T>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               op_mgr& exprs,
               var_mgr& vars,
               script_mgr& scripts,
               op_type t,
               const T& expr,
               op_type parent_type = op_type::None) {
  assert(false);
}

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
                 op_mgr& exprs,
                 var_mgr& vars,
                 script_mgr& scripts,
                 op_type t,
                 const decltype(op::un)& op,
                 op_type parent_type);

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
                 op_mgr& exprs,
                 var_mgr& vars,
                 script_mgr& scripts,
                 op_type t,
                 const decltype(op::bin)& op,
                 op_type parent_type);

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
                 op_mgr& exprs,
                 var_mgr& vars,
                 script_mgr& scripts,
                 op_type t,
                 const decltype(op::var)& op,
                 op_type parent_type);

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
                 op_mgr& exprs,
                 var_mgr& vars,
                 script_mgr& scripts,
                 op_type t,
                 const decltype(op::quant)& op,
                 op_type parent_type);

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
                op_mgr& exprs,
                 var_mgr& vars,
                 script_mgr& scripts,
                 op_type t,
                 const decltype(op::script)& op,
                 op_type parent_type);

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
                 op_mgr& exprs,
                 var_mgr& vars,
                 script_mgr& scripts,
                 op_type t,
                 const decltype(op::bin)& op,
                 op_type parent_type) {
  bool parens = parens_required(parent_type, t);
  if(parens)
    o << "(";
  ops[op.l]->visit([&o, &ops, &vars, &scripts, t](op_type ct, auto& e) {
    op_tostr_visit(o, ops, vars, scripts, ct, e, t);
  });
  o << " " << op_type_to_sym(t) << " ";
  ops[op.r]->visit([&o, &ops, &vars, &scripts, t](op_type ct, auto& e) {
    op_tostr_visit(o, ops, vars, scripts, ct, e, t);
  });
  if(parens)
    o << ")";
  return o;
}

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
                 op_mgr& exprs,
                 var_mgr& vars,
                 script_mgr& scripts,
                 op_type t,
                 const decltype(expr::quant)& expr,
                 op_type parent_type) {
  o << op_type_to_sym(t) << vars[expr.v]->name << " ";
  exprs[expr.e]->visit([&o, &exprs, &vars, &scripts, t](op_type ct, auto& e) {
    op_tostr_visit(o, exprs, vars, scripts, ct, e, t);
  });
  return o;
}

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
                 op_mgr& exprs,
                 var_mgr& vars,
                 script_mgr& scripts,
                 op_type t,
                 const decltype(expr::un)& expr,
                 op_type parent_type) {
  o << op_type_to_sym(t);
  exprs[expr.c]->visit([&o, &exprs, &vars, &scripts, t](op_type ct, auto& e) {
    op_tostr_visit(o, exprs, vars, scripts, ct, e, t);
  });
  return o;
}

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
                 op_mgr& exprs,
                 var_mgr& vars,
                 script_mgr& scripts,
                 op_type t,
                 const decltype(expr::var)& expr,
                 op_type parent_type) {
  return o << vars[expr.v]->name;
}

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
                 op_mgr& exprs,
                 var_mgr& vars,
                 script_mgr& scripts,
                 op_type t,
                 const decltype(expr::script)& expr,
                 op_type parent_type) {
  exprs[expr.c]->visit([&o, &exprs, &vars, &scripts, t](op_type ct, auto& e) {
    op_tostr_visit(o, exprs, vars, scripts, ct, e, t);
  });
  return o << " [ " << scripts[expr.script_id]->code << " ]";
}

std::ostream&
operator<<(std::ostream& o, op_type t) {
  return o << op_type_to_str(t);
}

std::ostream&
operator<<(std::ostream& o, const op_ref& e) {
  if(!static_cast<bool>(e))
    return o << "(-)";
  e->visit([&o, &e](op_type t, const auto& ex) {
    op_tostr_visit(o, *e.mgr, *e.mgr->vars, *e.mgr->scripts, t, ex);
  });
  return o;
}

std::ostream&
operator<<(std::ostream& o, const var_ref& v) {
  return o << v->name;
}

std::ostream&
operator<<(std::ostream& o, const variable& v) {
  return o << "variable{" << v.name << "}";
}

op_ref
operator+(const op_ref& e, const std::string& s) {
  if(e->type != op_type::Var) {
    return transform::extend_vars(s)(e);
  } else {
    auto v = e->var.v;
    auto var_name = (*e.mgr->vars)[v]->name;
    auto new_var_ref = e.mgr->vars->get(variable{ var_name + s });
    return e.mgr->get(expr(op_type::Var, new_var_ref.id, 0));
  }
}
}

