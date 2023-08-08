parser grammar qcir_parser;
options { tokenVocab=qcir_lexer; }

/* Format from [here](http://www.qbflib.org/qcir.pdf).
 *
 * Comments have to be in the parser, as they are dependant on the line they are
 * on. The format_id is technically also a comment.
 *
 * The parser was written to be as flexible as possible, to avoid malformed QCIR
 * or old formats causing issues. I think this is important, because the
 * standard has been very vague so far. Once good tooling exists, maybe formulas
 * can be normalized to a more strict standard.
 */

@header {
    #include <booleguru/expression/op_manager.hpp>
    #include <booleguru/expression/var_manager.hpp>

    using op          = booleguru::expression::op;
    using op_ref      = booleguru::expression::op_ref;
    using op_type     = booleguru::expression::op_type;
    using op_manager  = booleguru::expression::op_manager;
    using var_manager = booleguru::expression::var_manager;
}

@members {
    std::shared_ptr<op_manager> ops;
    std::vector<uint32_t> free_variables;
    std::unordered_map<std::string, uint32_t> gate_variables;
}

formula returns [op_ref op]
    : EOL* q=qcir { $op = (*ops)[$q.output_id]; } EOF
    ;

qcir returns [uint32_t output_id]
    : FORMAT_ID EOL+
      ( qblock_free EOL+ )?
      ( qb+=qblock_quant EOL+ )*
      outs=output_statement EOL+
      ( gs=gate_statement { gate_variables[$gs.gvar] = $gs.id; }
           ( EOL+ | ( EOL* EOF ) ) )*
        { $output_id = gate_variables[$outs.name];
          // Reverse-iterate the quantifier statements, to preserve the binary
          // tree prefix order. (We add quantifiers from top-down, but we add
          // them from bottom-up!)
          for (auto qblock_quant = $qb.rbegin();
               qblock_quant != $qb.rend(); ++qblock_quant) {
            op_type ot = (*qblock_quant)->ot;
            for (auto var = (*qblock_quant)->vars.rbegin();
                 var != (*qblock_quant)->vars.rend(); ++var) {
              $output_id = ops->get_id(op(ot, (*var)->id, $output_id));
            }
          }
        }
    ;

qblock_free
    : FREE LPAR vl=var_list RPAR
        { for (auto var : $vl.vars) free_variables.push_back(var->id); }
    ;

qblock_quant returns [op_type ot, std::vector<VariableContext *> vars]
    : ( EXISTS { $ot = op_type::Exists; } | FORALL { $ot = op_type::Forall; } )
      LPAR vl=var_list RPAR { $vars = $vl.vars; }
    ;

// We use this literal ID as the root node of the binary tree, excluding the
// quantifier prefix. The prefix is prepended at the end.
// NOTE: Output gates allow for literals, e.g. 'output(-a3)'!
output_statement returns [std::string name]
    : OUTPUT LPAR IDENT RPAR { $name = std::move($IDENT.text); }
    ;

gate_statement returns [std::string gvar, uint32_t id]
    : IDENT { $gvar = std::move($IDENT.text); } EQ (
          AND LPAR ll=lit_list[op_type::And, var_manager::LITERAL_TOP]
            { $id = $ll.id; }
        | OR  LPAR ll=lit_list[op_type::Or,  var_manager::LITERAL_BOTTOM]
            { $id = $ll.id; }
        | XOR LPAR l0=literal COMMA l1=literal
            { $id = ops->get_id(op(op_type::Xor, $l0.id, $l1.id)); }
        | ITE LPAR l0=literal COMMA l1=literal COMMA l2=literal
            { uint32_t neg_l0 = ops->get_id(op(op_type::Not, $l0.id, 0));
              uint32_t if_branch   = ops->get_id(op(op_type::And,
                                                    $l0.id, $l1.id)),
                       else_branch = ops->get_id(op(op_type::And,
                                                    neg_l0, $l2.id));
              $id = ops->get_id(op(op_type::Or, if_branch, else_branch));
            }
        | EXISTS LPAR vl=var_list SEMICOLON l=literal
            { // This *must* crash if there is no first element, see 'var_list'
              // rule for context.
              auto vars = $vl.vars;
              assert(vars.size() > 1);
              $id = ops->get_id(op(op_type::Exists, $l.id, vars.front()->id));
              for (auto vc = vars.begin() + 1; vc != vars.end(); ++vc) {
                  $id = ops->get_id(op(op_type::Exists, $id, (*vc)->id));
              }
            }
        | FORALL LPAR vl=var_list SEMICOLON l=literal
            { // See option above.
              auto vars = $vl.vars;
              assert(vars.size() > 1);
              $id = ops->get_id(op(op_type::Forall, $l.id, vars.front()->id));
              for (auto vc = vars.begin() + 1; vc != vars.end(); ++vc) {
                  $id = ops->get_id(op(op_type::Forall, $id, (*vc)->id));
              }
            }
    ) RPAR
    ;

// Since for quantifier gates we need to parse the full list to get to the
// literal, which is required to build up the binary tree, we need to just
// collect the contexts here. We cannot produce a tree in-place.
var_list returns [std::vector<VariableContext *> vars]
    : vars_+=variable ( COMMA vars_+=variable )* { $vars = $vars_; }
    ;

// NOTE: Literal lists can be empty, hence the 'empty_id' parameter!
lit_list [op_type ot, uint32_t empty_id] returns [uint32_t id]
    : l0=literal { $id = $l0.id; }
        ( COMMA ln=literal { $id = ops->get_id(op(ot, $id, $ln.id)); } )*
    | { $id = ops->get_id(op(op_type::Var, empty_id, 0)); }
    ;

// Literals always create a node in the binary tree.
literal returns [uint32_t id]
    : NEG var=variable { $id = ops->get_id(op(op_type::Not, $var.id, 0)); }
    | var=variable     { $id = $var.id; }
    ;

// Variables only reserve a spot in the variable manager, without creating a
// variable node in the binary tree.
variable returns [uint32_t id]
    : IDENT
        { std::string const &text = $IDENT.text;
          if (gate_variables.contains(text)) {
            $id = gate_variables[text];
          } else {
            $id = ops->get_id(op(op_type::Var,
                                 ops->vars().get_id({ std::move(text) }), 0));
          }
        }
    ;

