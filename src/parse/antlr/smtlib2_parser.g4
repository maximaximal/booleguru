parser grammar smtlib2_parser;
options { tokenVocab=smtlib2_lexer; }

@header {
#include <ankerl/unordered_dense.h>

#include <booleguru/util/is_number.hpp>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/expression/bvop_manager.hpp>

#include <booleguru/parse/smtlib2_variable.hpp>
}

@members {
  using variable = expression::variable;
  using op = expression::op;
  using bvop = expression::bvop;
  using op_id = expression::op_id;
  using var_id = expression::var_id;
  using bvop_id = expression::bvop_id;
  using op_ref = expression::op_ref;
  using op_type = expression::op_type;
  using op_manager = expression::op_manager;
  using bvop_manager = expression::bvop_manager;

  using enum expression::bvop_type;

  ankerl::unordered_dense::map<std::string, smtlib2_variable> smtvars;
  std::shared_ptr<op_manager> ops;
  std::shared_ptr<bvop_manager> bvops;

  op_ref assertions;
}

formula
    : (L stmt R)* EOF
    ;

stmt
    : CHECK_SAT
    | SET_LOGIC ( QF_BV | BV )
    | DEFINE_FUN name=ID NIL t=fun_type
        { smtlib2_variable::define_variable(smtvars, ops->vars(), $name.text, $t.t, $t.w); }
    | DECLARE_CONST name=ID t=fun_type
        { smtlib2_variable::define_variable(smtvars, ops->vars(), $name.text, $t.t, $t.w); }
    | ASSERT e=expr { assertions = assertions && (*bvops)[$e.o].export_as_ops(*ops); }
    ;

fun_type returns [ smtlib2_variable::type t, uint16_t w ]
    : BOOL { $t = smtlib2_variable::Bool; $w = 1; }
    | L UNDERSCORE BITVEC width=INT R { $t = smtlib2_variable::BitVec; $w = atoi($width.text.c_str()); }
    ;

expr returns [ bvop_id o ]
    : name=ID {
            const auto &v = smtvars.at($name.text);
            $o = bvops->get_id(bvop(bvvar, v.id, v.width));
        }
    | L AND l=expr {$o = $l.o;} (r=expr { $o = bvops->get_id(bvop(and_, $o, $r.o)); })* R
    | L OR l=expr {$o = $l.o;} (r=expr { $o = bvops->get_id(bvop(or_, $o, $r.o)); })* R
    | L BVAND l=expr {$o = $l.o;} (r=expr { $o = bvops->get_id(bvop(bvand, $o, $r.o)); })* R
    | L BVOR l=expr {$o = $l.o;} (r=expr { $o = bvops->get_id(bvop(bvor, $o, $r.o)); })* R
    | L EQUALS l=expr {$o = $l.o;} (r=expr { $o = bvops->get_id(bvop(bveq, $o, $r.o)); })* R
    | L NOT c=expr { $o = bvops->get_id(bvop(bvnot, $c.o)); } R
    ;
