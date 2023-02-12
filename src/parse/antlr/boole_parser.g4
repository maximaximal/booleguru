parser grammar boole_parser;
options { tokenVocab=boole_lexer; }

@header {
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>
}

@members {
  using variable = expression::variable;
  using op = expression::op;
  using op_ref = expression::op_ref;
  using op_type = expression::op_type;
  using op_manager = expression::op_manager;
  std::shared_ptr<op_manager> ops;
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
    | ID { auto text = $ID.text;
           uint32_t v = ops->vars().get_id(variable{std::move(text)});
           $o = ops->get_id(op(op_type::Var, v, 0)); }
    ;
