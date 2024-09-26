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
    case op_type::Xor:
      return "Xor";
    case op_type::Var:
      return "Var";
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
    case op_type::Xor:
      return "^";
    case op_type::None:
      return "(-)";
    default:
      return "No Symbol";
  }
}

op_type
str_to_op_type(const char* s) {
  if(std::strcmp("and", s) == 0) {
    return op_type::And;
  } else if(std::strcmp("or", s) == 0) {
    return op_type::Or;
  } else if(std::strcmp("equi", s) == 0) {
    return op_type::Equi;
  } else if(std::strcmp("impl", s) == 0) {
    return op_type::Impl;
  } else if(std::strcmp("lpmi", s) == 0) {
    return op_type::Lpmi;
  } else if(std::strcmp("not", s) == 0) {
    return op_type::Not;
  } else if(std::strcmp("xor", s) == 0) {
    return op_type::Xor;
  } else if(std::strcmp("forall", s) == 0) {
    return op_type::Forall;
  } else if(std::strcmp("exists", s) == 0) {
    return op_type::Exists;
  } else {
    return op_type::None;
  }
}

std::ostream&
operator<<(std::ostream& o, const op_type& t) {
  return o << op_type_to_str(t);
}

std::ostream&
operator<<(std::ostream& o, const op& op) {
  o << op.type << " ";
  if(op.is_binop()) {
    o << "l:" << op.bin.l.id_ << " r:" << op.bin.r.id_;
  } else if(op.is_unop()) {
    o << "c:" << op.un.c.id_;
  } else if(op.is_quant()) {
    o << "v:" << op.quant.v.id_ << " e:" << op.quant.e.id_;
  } else if(op.type == op_type::Var) {
    o << "v:" << op.var.v.id_ << " i:" << op.var.i << " q:" << op.var.q;
  }

  if(op.and_inside)
    o << " and_inside";
  if(op.is_ors)
    o << " is_ors";
  if(op.is_cnf)
    o << " is_cnf";
  if(op.is_prenex)
    o << " is_prenex";
  if(op.mark)
    o << " mark";
  if(op.user_flag3)
    o << " user_flag_3";
  if(op.user_flag4)
    o << " user_flag_4";
  if(op.user_flag5)
    o << " user_flag_5";
  if(op.user_int16)
    o << " user_int16:" << op.user_int16;
  if(op.user_int32)
    o << " user_int16:" << op.user_int32;

  return o;
}
}
