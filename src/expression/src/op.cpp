#include <booleguru/expression/op.hpp>

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

std::ostream&
operator<<(std::ostream& o, op_type t) {
  return o << op_type_to_str(t);
}
}
