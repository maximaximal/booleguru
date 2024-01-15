parser grammar cli_parser;
options { tokenVocab=cli_lexer; }

@header {
#include <functional>
#include <string_view>
#include <exception>
#include <fmt/format.h>

#include <booleguru/expression/expression_graph.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/lua/lua-context.hpp>
#include <booleguru/util/type.hpp>
#include <booleguru/util/trim.hpp>
#include <booleguru/util/is_number.hpp>
#include <booleguru/util/str_replace.hpp>

#include <booleguru/parse/error.hpp>
}

@members {
  using variable = expression::variable;
  using op = expression::op;
  using op_id = expression::op_id;
  using var_id = expression::var_id;
  using op_ref = expression::op_ref;
  using op_type = expression::op_type;
  using op_manager = expression::op_manager;
  using enum util::type;

  expression::expression_graph *g = nullptr;

  util::type out_type = util::type::boole;
}

invocation returns [op_id o]: e=expr {$o = $e.o;}
        ( t=format { out_type = $t.t; })?
        ( f=EOL_FENNEL_SUBST f=COMMAND {
        std::string command = $f.text;
        util::trim(command);
        op_id res = g->fennel_("(" + command + ")", $o);
        if(res > 0)
            $o = res;
    } )?
        EOF;

expr returns [op_id o]:
      e=expr {$o = $e.o;} FENNEL_SUBST f=MATCHING_PAREN {
            std::string text{$f.text};
            if(text.find("@") != std::string::npos) {
                text += "\"";
            }
            util::str_replace_first_rest( text, "@", "\"", "\"\"");
            std::string code = "(" + text + ")";
            auto res1 = g->fennel_(code, $o);
            if(res1 > 0) {
                $o = res1;
            }
        }
    | e=expr {$o = $e.o;} FENNEL_CALL f=CALL_CODE {
            std::string text{$f.text};
            if(text.find("@") != std::string::npos) {
                text += "\"";
            }
            bool replaced = util::str_replace_first_rest( text, "@", " ** \"", "\"\"");
            std::string code;
            if(replaced)
                code = "(" + text + ")";
            else
                code = "(" + text + " **)";
            auto res2 = g->fennel_(code, $o);
            if(res2 > 0) {
                $o = res2;
            }
        }
    | NOT l=expr { $o = g->not_($l.o); }
    | l=expr AND r=expr { $o = g->and_($l.o, $r.o); }
    | l=expr OR r=expr { $o = g->or_($l.o, $r.o); }
    | l=expr XOR r=expr { $o = g->xor_($l.o, $r.o); }
    | l=expr IMPL r=expr { $o = g->impl_($l.o, $r.o); }
    | l=expr LPMI r=expr { $o = g->lpmi_($l.o, $r.o); }
    | l=expr EQUI r=expr { $o = g->equi_($l.o, $r.o); }
    | LPAR l=expr RPAR { $o = $l.o; }
    | FORALL v=var r=expr { $o = g->forall_($v.o, $r.o); }
    | EXISTS v=var r=expr { $o = g->exists_($v.o, $r.o); }
    | TOP { $o = g->top_(); }
    | BOTTOM { $o = g->bottom_(); }
    | v=var { $o = $v.o; }
    | {util::type file_format = util::type::boole;} (fo=format {file_format = $fo.t;})?
        p=PATH { $o = g->file_($p.text, file_format); }
    | FENNEL_SUBST f=MATCHING_PAREN {
            std::string text{$f.text};
            std::string code = "(" + text + ")";
            op_id res3 = g->fennel_(code);
            if(res3 > 0) {
                $o = res3;
            } else {
                throw error::fennel_did_not_return_op(code);
            }
        }
    | FENNEL_CALL f=CALL_CODE {
            std::string text{$f.text};
            if(text.find("@") != std::string::npos) {
                text += "\"";
            }
            util::str_replace_first_rest( text, "@", "\"", "\"\"");
            std::string code = "(" + text + ")";
            op_id res4 = g->fennel_(code);
            if(res4 > 0) {
                $o = res4;
            } else {
                throw error::fennel_did_not_return_op(code);
            }
        }
    ;

format returns [util::type t]:
      DIMACS { $t = util::type::qdimacs; }
    | SMTLIB { $t = util::type::smtlib; }
    | BOOLE  { $t = util::type::boole; }
    | QCIR   { $t = util::type::qcir; }
    | PYTHON { $t = util::type::py; }
    | LUA    { $t = util::type::lua; }
    | NONE   { $t = util::type::none; }
    ;

var returns [op_id o]:
        { var_id var_id = 0; uint16_t i = 0, q = 0;}
        ( ( VEC { var_id = expression::var_manager::LITERAL_VEC; } )
      | ( TSEITIN { var_id = expression::var_manager::LITERAL_TSEITIN; } )
      | ( ID { auto text = $ID.text;
           var_id = g->variable(std::move(text)); } )
        )
        ( LCURL ID { util::ensure_is_number($ID.text); i = atoi($ID.text.c_str()); } RCURL )?
        ( LBRACK ID { util::ensure_is_number($ID.text); q = atoi($ID.text.c_str()); } RBRACK )?
        { $o = g->var_(var_id, i, q); }
    ;
