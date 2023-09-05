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
    : e=expr EOF { $o = (*ops)[$e.o]; }
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
    | FORALL ID r=expr {
            auto text = $ID.text;
            uint32_t var_id = ops->vars().get_id(variable{std::move(text)});
            uint32_t varop_id = ops->get_id(op(op_type::Var, var_id, 0));
            $o = ops->get_id(op(op_type::Forall,
                                varop_id,
                                $r.o));
        }
    | EXISTS ID r=expr {
            auto text = $ID.text;
            uint32_t var_id = ops->vars().get_id(variable{std::move(text)});
            uint32_t varop_id = ops->get_id(op(op_type::Var, var_id, 0));
            $o = ops->get_id(op(op_type::Exists,
                                varop_id,
                                $r.o));
        }
    | TOP { $o = ops->top().get_id(); }
    | BOTTOM { $o = ops->bottom().get_id(); }
    | ID { auto text = $ID.text;
           uint32_t var_id = ops->vars().get_id(variable{std::move(text)});
           $o = ops->get_id(op(op_type::Var, var_id, 0)); }
    | last_op=expr FENNEL_SUBST c=MATCHING_PAREN {
            if(eval) {
                auto ret = lua->eval_fennel("(" + $c.text + ")", (*ops)[$last_op.o]);
                if(std::holds_alternative<std::string>(ret)) {
                    notifyErrorListeners(std::get<std::string>(ret));
                    $o = $last_op.o;
                } else if(std::holds_alternative<op_ref>(ret)) {
                    $o = std::get<op_ref>(ret).get_id();
                } else {
                    $o = $last_op.o;
                }
            } else {
                $o = $last_op.o;
            }
        }
    | last_op=expr LUA_SUBST c=MATCHING_PAREN {
            if(eval) {
                auto ret = lua->eval($c.text, (*ops)[$last_op.o]);
                if(std::holds_alternative<std::string>(ret)) {
                    notifyErrorListeners(std::get<std::string>(ret));
                    $o = $last_op.o;
                } else if(std::holds_alternative<op_ref>(ret)) {
                    $o = std::get<op_ref>(ret).get_id();
                } else {
                    $o = $last_op.o;
                }
            }
            else {
                $o = $last_op.o;
            }
        }
    | FENNEL c=MATCHING_PAREN {
            if(!eval) {
                notifyErrorListeners("Cannot execute in non-eval formula!");
            } else {
                auto ret = lua->eval_fennel("(" + $c.text + ")");
                if(std::holds_alternative<std::string>(ret)) {
                    notifyErrorListeners(std::get<std::string>(ret));
                    $o = $last_op.o;
                } else if(std::holds_alternative<op_ref>(ret)) {
                    $o = std::get<op_ref>(ret).get_id();
                } else {
                    notifyErrorListeners("Fennel returned invalid return type!");
                }
            }
        }
    | LUA c=MATCHING_PAREN {
            if(!eval) {
                notifyErrorListeners("Cannot execute in non-eval formula!");
            } else {
                auto ret = lua->eval($c.text);
                if(std::holds_alternative<std::string>(ret)) {
                    notifyErrorListeners(std::get<std::string>(ret));
                    $o = $last_op.o;
                } else if(std::holds_alternative<op_ref>(ret)) {
                    $o = std::get<op_ref>(ret).get_id();
                } else {
                    notifyErrorListeners("Lua returned invalid return type!");
                }
            }
        }
    ;
