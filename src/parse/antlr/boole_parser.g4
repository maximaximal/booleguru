parser grammar boole_parser;
options { tokenVocab=boole_lexer; }

@header {
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/lua/lua-context.hpp>
}

@members {
  using variable = expression::variable;
  using op = expression::op;
  using op_ref = expression::op_ref;
  using op_type = expression::op_type;
  using op_manager = expression::op_manager;
  std::shared_ptr<op_manager> ops;
  std::shared_ptr<lua::lua_context> lua;
  bool eval = false;
}

formula returns [op_ref o]
    : e=expr { $o = (*ops)[$e.o]; }
    | <EOF>
    ;

expr returns [uint32_t o]:
      NOT l=expr { $o = ops->get_id(op(op_type::Not, $l.o, 0)); }
    | l=expr AND r=expr { $o = ops->get_id(op(op_type::And, $l.o, $r.o)); }
    | l=expr OR r=expr { $o = ops->get_id(op(op_type::Or, $l.o, $r.o)); }
    | l=expr XOR r=expr { $o = ops->get_id(op(op_type::Xor, $l.o, $r.o)); }
    | l=expr IMPL r=expr { $o = ops->get_id(op(op_type::Impl, $l.o, $r.o)); }
    | l=expr LPMI r=expr { $o = ops->get_id(op(op_type::Lpmi, $l.o, $r.o)); }
    | l=expr EQUI r=expr { $o = ops->get_id(op(op_type::Equi, $l.o, $r.o)); }
    | LPAR l=expr RPAR { $o = $l.o; }
    | FORALL v=var r=expr {
            $o = ops->get_id(op(op_type::Forall,
                                ops->get_id(op(op_type::Var, $v.v, 0)),
                                $r.o));
        }
    | EXISTS v=var r=expr {
            $o = ops->get_id(op(op_type::Exists,
                                ops->get_id(op(op_type::Var, $v.v, 0)),
                                $r.o));
        }
    | v=var { $o = ops->get_id(op(op_type::Var, $v.v, 0)); }
    | last_op=expr FENNEL_SUBST c=MATCHING_PAREN {
            if(eval) {
                auto ret = lua->eval_fennel("(" + $c.text + ")", (*ops)[$last_op.o]);
                $o = std::get<op_ref>(ret).get_id();
            } else {
                $o = $last_op.o;
            }
        }
    | last_op=expr FENNEL c=MATCHING_PAREN {
            if(eval) {
                lua->eval_fennel("(" + $c.text + ")", (*ops)[$last_op.o]);
            }
            $o = $last_op.o;
        }
    | last_op=expr LUA_SUBST c=MATCHING_PAREN {
            if(eval) {
                auto ret = lua->eval($c.text, (*ops)[$last_op.o]);
                $o = std::get<op_ref>(ret).get_id();
            } else {
                $o = $last_op.o;
            }
        }
    | last_op=expr LUA c=MATCHING_PAREN {
            if(eval) {
                auto ret = lua->eval($c.text, (*ops)[$last_op.o]);
            }
            $o = $last_op.o;
        }
    ;

var returns [uint32_t v]:
      ID { auto text = $ID.text;
           $v = ops->vars().get_id(variable{std::move(text)}); }
    ;
