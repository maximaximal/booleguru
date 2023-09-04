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
    #include <ankerl/unordered_dense.h>
    #include <fmt/format.h>

    #include <booleguru/expression/op_manager.hpp>
    #include <booleguru/expression/var_manager.hpp>
    #include <booleguru/util/reverse.hpp>

    using namespace booleguru;

    using op          = expression::op;
    using op_ref      = expression::op_ref;
    using op_type     = expression::op_type;
    using op_manager  = expression::op_manager;
    using var_ref     = expression::var_ref;
    using var_manager = expression::var_manager;
}

@members {
    std::shared_ptr<op_manager> ops;
    ankerl::unordered_dense::map<std::string, uint32_t> free_variables;
    ankerl::unordered_dense::map<std::string, uint32_t> quantified_variables;
    ankerl::unordered_dense::map<std::string, uint32_t> gate_variables;

    void check_var_dup(std::string const &var_name) {
      std::string_view constexpr fmt_str = "{} variable '{}' clashes with"
        " gate variable definition of same name";
      // Check free clash
      if (free_variables.count(var_name) != 0)
        notifyErrorListeners(fmt::format(fmt_str, "Free", var_name));
      // Check quantified clash
      if (quantified_variables.count(var_name) != 0)
        notifyErrorListeners(fmt::format(fmt_str, "Quantified", var_name));
      // Check earlier gate variable clash
      if (gate_variables.count(var_name) != 0)
        notifyErrorListeners(fmt::format(fmt_str, "Gate", var_name));
    }
}

formula returns [op_ref op]
    : EOL* q=qcir { $op = (*ops)[$q.output_id]; } EOF
    ;

qcir returns [uint32_t output_id]
    : format_id EOL+
      ( qblock_free EOL+ )?
      ( qb+=qblock_quant EOL+ )*
      outs=output_statement EOL+
      ( gate_statement ( EOL+ | ( EOL* EOF ) ) )*
        { $output_id = gate_variables[$outs.name];
          // Handle negated output
          if ($outs.negated)
            $output_id = ops->get_id(op(op_type::Not, $output_id, 0));
          // Reverse-iterate the quantifier statements, to preserve the binary
          // tree prefix order. (We parse quantifiers from top-down, but we add
          // them from bottom-up!)
          for (auto const qblock_quant : util::reverse($qb)) {
            op_type ot = qblock_quant->ot;
            for (auto const var : util::reverse(qblock_quant->vars)) {
              $output_id = ops->get_id(op(ot, var->id, $output_id));
            }
          }
          // Now, we can make sure that no variables were left undefined, we
          // need to do this at the end because a quantifier gate might come
          // after a variable usage...
          std::unordered_set<std::string_view> seen_names;
          ops->traverse_preorder_with_stack($output_id,
            [this, &seen_names](uint32_t const &curr) {
              op_ref const &o = (*ops)[curr];
              switch (o->type) {
                case op_type::Exists:
                case op_type::Forall:;
                  {
                    auto const &var_op = (*ops)[o->quant.v];
                    seen_names.insert(ops->vars()[var_op->var.v]->name);
                  } break;
                case op_type::Var:;
                  {
                    auto const &var_id = o->var.v;
                    auto const &name = ops->vars()[var_id]->name;
                    if (seen_names.count(name) == 0
                          && free_variables.count(name) == 0
                          && var_id != var_manager::LITERAL_TOP
                          && var_id != var_manager::LITERAL_BOTTOM)
                      notifyErrorListeners(fmt::format(
                        "Variable '{}' was never declared free, quantified,"
                        " or defined as gate variable", name));
                  } break;
              }
            });
        }
    ;

format_id
    : FORMAT_ID_G14
    | FORMAT_ID_14
    | FORMAT_ID_13
    | FORMAT_ID_ILL
        { notifyErrorListeners(fmt::format(
            "Unknown format ID '{}'", $FORMAT_ID_ILL.text));
        }
    ;

qblock_free
    : FREE LPAR vl=var_list RPAR
        { for (auto const var : $vl.vars) {
            check_var_dup(var->text);
            free_variables[var->text] = var->var_id;
          }
        }
    ;

qblock_quant returns [op_type ot, std::vector<VariableContext *> vars]
    : ( EXISTS { $ot = op_type::Exists; } | FORALL { $ot = op_type::Forall; } )
      LPAR vl=var_list RPAR
        { $vars = std::move($vl.vars);
          for (auto const var : $vl.vars) {
            check_var_dup(var->text);
            quantified_variables[var->text] = var->var_id;
          }
        }
    ;

// We use this literal ID as the root node of the binary tree, excluding the
// quantifier prefix. The prefix is prepended at the end.
// NOTE: Output gates allow for literals, e.g. 'output(-a3)'!
output_statement returns [std::string name, bool negated]
    : OUTPUT LPAR { $negated = false; } ( NEG { $negated = !$negated; } )*
      IDENT RPAR
        { $name = std::move($IDENT.text);
          check_var_dup($name);
        }
    ;

gate_statement returns [std::string gvar, uint32_t id]
    : IDENT
        { $gvar = std::move($IDENT.text);
          check_var_dup($gvar);
        }
        EQ (
          AND LPAR ll=lit_list[op_type::And, var_manager::LITERAL_TOP, $gvar]
            { $id = $ll.id; }
        | OR  LPAR ll=lit_list[op_type::Or,  var_manager::LITERAL_BOTTOM, $gvar]
            { $id = $ll.id; }
        | XOR LPAR l0=literal[$gvar] COMMA l1=literal[$gvar]
            { $id = ops->get_id(op(op_type::Xor, $l0.id, $l1.id)); }
        | ITE LPAR l0=literal[$gvar] COMMA l1=literal[$gvar]
              COMMA l2=literal[$gvar]
            { uint32_t const neg_l0 = ops->get_id(op(op_type::Not, $l0.id, 0));
              uint32_t const if_branch   = ops->get_id(op(op_type::And,
                                                          $l0.id, $l1.id)),
                             else_branch = ops->get_id(op(op_type::And,
                                                          neg_l0, $l2.id));
              $id = ops->get_id(op(op_type::Or, if_branch, else_branch));
            }
        | ( qu=EXISTS | qu=FORALL ) LPAR vl=var_list SEMICOLON l=literal[$gvar]
            { // An empty variable list should never occur.
              assert($vl.vars.size() > 0);
              $id = $l.id;
              op_type const ot
                = ($qu.type == qcir_parser::EXISTS) ? op_type::Exists
                                                    : op_type::Forall;
              for (auto const vc : util::reverse($vl.vars)) {
                  check_var_dup(vc->text);
                  $id = ops->get_id(op(ot, vc->id, $id));
              }
            }
    ) RPAR { gate_variables[$gvar] = $id; }
    ;

// Since for quantifier gates we need to parse the full list to get to the
// literal, which is required to build up the binary tree, we need to just
// collect the contexts here. We cannot produce a tree in-place.
var_list returns [std::vector<VariableContext *> vars]
    : vars_+=variable ( COMMA vars_+=variable )* { $vars = std::move($vars_); }
    ;

// NOTE: Literal lists can be empty, hence the 'empty_id' parameter!
lit_list [op_type ot, uint32_t empty_id, std::string gvar] returns [uint32_t id]
    : l0=literal[$gvar] { $id = $l0.id; }
        ( COMMA ln=literal[gvar] { $id = ops->get_id(op(ot, $id, $ln.id)); } )*
    | { $id = ops->get_id(op(op_type::Var, empty_id, 0)); }
    ;

// Literals always create a node in the binary tree.
literal [std::string gvar] returns [uint32_t id]
    : ( NEG var=variable { $id = ops->get_id(op(op_type::Not, $var.id, 0)); }
          | var=variable { $id = $var.id; } )
        { // Check that the literal is not cyclic with its gate variable
          if ($gvar == $var.text)
            notifyErrorListeners(fmt::format(
              "Gate variable '{}' is referenced cyclically within statement",
              $var.text));
        }
    ;

// Variables only reserve a spot in the variable manager, without creating a
// variable node in the binary tree.
variable returns [uint32_t id, uint32_t var_id, std::string text]
    : IDENT
        { $text = std::move($IDENT.text);
          if (gate_variables.count($text) != 0) {
            $var_id = 0;
            $id = gate_variables[$text];
          } else {
            $var_id = ops->vars().get_id({ $text });
            $id = ops->get_id(op(op_type::Var, $var_id, 0));
          }
        }
    ;
