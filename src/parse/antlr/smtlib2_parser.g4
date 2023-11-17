parser grammar smtlib2_parser;
options { tokenVocab=smtlib2_lexer; }

@header {
#include <ankerl/unordered_dense.h>

#include <booleguru/util/is_number.hpp>
#include <booleguru/util/bv_literal.hpp>

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

  std::vector<ankerl::unordered_dense::map<std::string, smtlib2_variable>> smtvars;
  std::shared_ptr<op_manager> ops;
  std::shared_ptr<bvop_manager> bvops;

  op_ref assertions;
}

formula
    : {smtvars.resize(1);} (L stmt R)* EOF
    ;

stmt
    : CHECK_SAT
    | GET_MODEL
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
            var_id id = 0;
            uint16_t width;
            for(ssize_t i = smtvars.size() - 1; i >= 0; --i) {
                const auto &it = smtvars[i].find($name.text);
                if(it != smtvars[i].end()) {
                    id = it->second.id;
                    width = it->second.width;
                }
            }
            if(!id)
                throw std::runtime_error(fmt::format("Variable {} not defined!", $name.text));
            $o = bvops->get_id(bvop(bvvar, id, width));
        }
    | L AND l=expr {$o = $l.o;} (r=expr { $o = bvops->get_id(bvop(and_, $o, $r.o)); })* R
    | L OR l=expr {$o = $l.o;} (r=expr { $o = bvops->get_id(bvop(or_, $o, $r.o)); })* R
    | L BVAND l=expr {$o = $l.o;} (r=expr { $o = bvops->get_id(bvop(bvand, $o, $r.o)); })* R
    | L BVOR l=expr {$o = $l.o;} (r=expr { $o = bvops->get_id(bvop(bvor, $o, $r.o)); })* R
    | L EQUALS l=expr {$o = $l.o;} (r=expr { $o = bvops->get_id(bvop(bveq, $o, $r.o)); })* R
    | L NOT c=expr { $o = bvops->get_id(bvop(bvnot, $c.o)); } R
    | L {expression::bvop_type qt; std::vector<expression::bvop_id> vars;}
            ( FORALL {qt = bvforall;} | EXISTS {qt = bvexists;})
            L {smtvars.emplace_back();}
              (L name=ID t=fun_type R {
                expression::var_id varid =
                  smtlib2_variable::define_variable(smtvars, ops->vars(), $name.text, $t.t, $t.w);
                bvop var = bvop(bvvar, varid, $t.w);
                bvop_id vid = bvops->get_id(std::move(var));
                vars.emplace_back(vid);
              })+
            R
            c=expr {
                  $o = $c.o;
                  assert(vars.size() > 0);
                  for(ssize_t i = vars.size() - 1; i >= 0; --i) {
                      assert(i < vars.size());
                      $o = bvops->get_id(bvop(qt, vars[i], $o));
                  }
              } R
    | L UNDERSCORE n=ID w=INT R {
            util::bv_literal lit($n.text);
            int width = atoi($w.text.c_str());
            $o = bvops->get_id(bvop(bvconst, lit, width));
        }
    ;
