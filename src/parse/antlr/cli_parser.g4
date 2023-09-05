parser grammar cli_parser;
options { tokenVocab=cli_lexer; }

@header {
#include <functional>
#include <string_view>
#include <exception>
#include <fmt/format.h>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/lua/lua-context.hpp>
#include <booleguru/parse/type.hpp>
#include <booleguru/util/trim.hpp>
}

@members {
  using variable = expression::variable;
  using op = expression::op;
  using op_ref = expression::op_ref;
  using op_type = expression::op_type;
  using op_manager = expression::op_manager;
  std::shared_ptr<op_manager> ops;
  std::shared_ptr<lua::lua_context> lua;

  using parse_file_function =
    std::function<expression::op_ref(std::string_view, booleguru::parse::type)>;
  parse_file_function parse_file_function_;

  parse::type out_type = parse::type::boole;
}

invocation returns [uint32_t o]: e=expr {$o = $e.o;}
        ( t=format { out_type = $t.t; })?
        ( f=EOL_FENNEL_SUBST f=COMMAND {
        std::string command = $f.text;
        util::trim(command);
        auto last_op = (*ops)[$o];
        if(command[0] != '(') {
            command = "(" + command + " **)";
        }
        auto res = lua->eval_fennel_to_op_or_throw(command, last_op);
        if(res.valid()) {
            $o = res.get_id();
        }
    } )?
        EOF;

expr returns [uint32_t o]:
      e=expr {$o = $e.o;} FENNEL_SUBST f=MATCHING_PAREN {
            auto last_op1 = (*ops)[$o];
            auto res1 = lua->eval_fennel_to_op_or_throw($f.text, last_op1);
            if(res1.valid()) {
                $o = res1.get_id();
            }
        }
    | e=expr {$o = $e.o;} FENNEL_CALL f=CALL_CODE {
            auto last_op2 = (*ops)[$o];
            auto res2 = lua->eval_fennel_to_op_or_throw("(" + $f.text + " **)", last_op2);
            if(res2.valid()) {
                $o = res2.get_id();
            }
        }
    | NOT l=expr { $o = ops->get_id(op(op_type::Not, $l.o, 0)); }
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
    | ID { auto text = $ID.text;
           uint32_t var_id = ops->vars().get_id(variable{std::move(text)});
           $o = ops->get_id(op(op_type::Var, var_id, 0)); }
    | {parse::type file_format = parse::type::boole;} (fo=format {file_format = $fo.t;})?
        p=PATH {
            if(!parse_file_function_)
              throw std::invalid_argument("missing parse file function in CLI parser!");
            $o = parse_file_function_($p.text, file_format).get_id(); }
    | FENNEL_SUBST f=MATCHING_PAREN {
            auto res3 = lua->eval_fennel_to_op_or_throw("(" + $f.text + ")");
            if(res3.valid()) {
                $o = res3.get_id();
            } else {
                throw std::invalid_argument("standalone fennel call must return some op!");
            }
        }
    | FENNEL_CALL f=CALL_CODE {
            auto res4 = lua->eval_fennel_to_op_or_throw("(" + $f.text + ")");
            if(res4.valid()) {
                $o = res4.get_id();
            } else {
                throw std::invalid_argument("standalone fennel call must return some op!");
            }
        }
    ;

format returns [parse::type t]:
      DIMACS { $t = parse::type::qdimacs; }
    | SMTLIB { $t = parse::type::smtlib; }
    | BOOLE  { $t = parse::type::boole; }
    | QCIR   { $t = parse::type::qcir; }
    | PYTHON { $t = parse::type::py; }
    | LUA    { $t = parse::type::lua; }
    | NONE   { $t = parse::type::none; }
    ;
