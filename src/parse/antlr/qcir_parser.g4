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

  #include <booleguru/expression/id.hpp>
  #include <booleguru/expression/op_manager.hpp>
  #include <booleguru/expression/var_manager.hpp>
  #include <booleguru/util/reverse.hpp>

  using namespace booleguru;

  using op          = expression::op;
  using op_ref      = expression::op_ref;
  using op_id       = expression::op_id;
  using op_type     = expression::op_type;
  using op_manager  = expression::op_manager;
  using var_ref     = expression::var_ref;
  using var_id      = expression::var_id;
  using var_manager = expression::var_manager;
}

@members {
  std::shared_ptr<op_manager> ops;
  ankerl::unordered_dense::set<var_id> free_variables;
  ankerl::unordered_dense::set<var_id> quantified_variables;
  ankerl::unordered_dense::map<var_id, op_id> gate_variables;
  // This is a temporary set for checking
  ankerl::unordered_dense::set<var_id> unknown_variables;

  static constexpr char const* const handle_variable_overlap_fmt_str
    = "{} variable '{}' clashes with gate variable definition of same name";

  void handle_variable_overlap(var_id const v_id) {
    // Check free clash
    if (free_variables.count(v_id) != 0)
      notifyErrorListeners(fmt::format(
        handle_variable_overlap_fmt_str, "Free", ops->vars()[v_id]->name));
    // Check quantified clash
    if (quantified_variables.count(v_id) != 0)
      notifyErrorListeners(fmt::format(
        handle_variable_overlap_fmt_str, "Quantified", ops->vars()[v_id]->name));
    // Check earlier gate variable clash
    if (gate_variables.count(v_id) != 0)
      notifyErrorListeners(fmt::format(
        handle_variable_overlap_fmt_str, "Gate", ops->vars()[v_id]->name));
  }
}

formula returns [op_ref op]
  : EOL* q=qcir { $op = (*ops)[$q.output_id]; } EOF
  ;

qcir returns [op_id output_id]
  : format_id EOL+
    ( qblock_free EOL+ )?
    ( qb+=qblock_quant EOL+ )*
    outs=output_statement EOL+
    ( gate_statement ( EOL+ | ( EOL* EOF ) ) )*
      { $output_id = gate_variables[$outs.v_id];
        // Handle negated output
        if ($outs.negated)
          $output_id = ops->get_id(op(op_type::Not, $output_id, 0));
        // Reverse-iterate the quantifier statements, to preserve the binary
        // tree prefix order. (We parse quantifiers from top-down, but we add
        // them from bottom-up!)
        for (auto const& qblock_quant : util::reverse($qb)) {
          op_type ot = qblock_quant->ot;
          for (auto const& var : util::reverse(qblock_quant->vl->vars)) {
            $output_id = ops->get_id(op(ot, var->id, $output_id));
          }
        }
        // Mark any variables left in the 'unknown' state as nevber having been
        // declared, see quantifier gate statement for NOTE
        for (var_id const v_id : unknown_variables)
          notifyErrorListeners(fmt::format(
            "Variable '{}' was never declared free, quantified,"
            " or defined as gate variable", ops->vars()[v_id]->name));
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
      { for (auto const& var : $vl.ctx->vars) {
          handle_variable_overlap(var->v_id);
          free_variables.insert(var->v_id);
        }
      }
  ;

qblock_quant returns [op_type ot]
  : ( EXISTS { $ot = op_type::Exists; } | FORALL { $ot = op_type::Forall; } )
    LPAR vl=var_list RPAR
      { for (auto const& var : $vl.ctx->vars) {
          handle_variable_overlap(var->v_id);
          quantified_variables.insert(var->v_id);
        }
      }
  ;

// We use this literal ID as the root node of the binary tree, excluding the
// quantifier prefix. The prefix is prepended at the end.
// NOTE: Output gates allow for literals, e.g. 'output(-a3)'!
output_statement returns [var_id v_id, bool negated]
  : OUTPUT LPAR { $negated = false; } ( NEG { $negated = !$negated; } )*
    var=variable { $v_id = $var.v_id; }
    RPAR
  ;

gate_statement locals [op_id id]
  : var=variable { handle_variable_overlap($var.v_id); } EQ (
      AND LPAR ll=lit_list[op_type::And, var_manager::LITERAL_TOP, $var.v_id]
        { $id = $ll.id; }
    | OR  LPAR ll=lit_list[op_type::Or,  var_manager::LITERAL_BOTTOM, $var.v_id]
        { $id = $ll.id; }
    | XOR LPAR l0=literal[$var.v_id] COMMA l1=literal[$var.v_id]
        { $id = ops->get_id(op(op_type::Xor, $l0.id, $l1.id)); }
    | ITE LPAR l0=literal[$var.v_id] COMMA l1=literal[$var.v_id]
        COMMA  l2=literal[$var.v_id]
          { $id = ops->encode_ite($l0.id, $l1.id, $l2.id); }
    | ( qu=EXISTS | qu=FORALL ) LPAR vl=var_list SEMICOLON l=literal[$var.v_id]
        { // An empty variable list should never occur.
          assert($vl.ctx->vars.size() > 0);
          $id = $l.id;
          op_type const ot
            = ($qu.type == qcir_parser::EXISTS) ? op_type::Exists
                                                : op_type::Forall;
          for (auto const& vc : util::reverse($vl.ctx->vars)) {
              handle_variable_overlap(vc->v_id);
              if (unknown_variables.count(vc->v_id) != 0)
                unknown_variables.erase(vc->v_id);
              $id = ops->get_id(op(ot, vc->id, $id));
          }
        }
    ) RPAR { gate_variables[$var.v_id] = $id; }
  ;

// Since for quantifier gates we need to parse the full list to get to the
// literal, which is required to build up the binary tree, we need to just
// collect the contexts here. We cannot produce a tree in-place.
var_list
  : vars+=variable ( COMMA vars+=variable )*
  ;

// NOTE: Literal lists can be empty, hence the 'empty_id' parameter!
lit_list [op_type ot, var_id empty_id, var_id gvar_id] returns [op_id id]
  : l0=literal[$gvar_id] { $id = $l0.id; }
      ( COMMA ln=literal[$gvar_id]
          { $id = ops->get_id(op(ot, $id, $ln.id)); } )*
    | { $id = ops->get_id(op(op_type::Var, empty_id, 0, 0)); }
  ;

// Literals always create a node in the binary tree.
literal [var_id gvar_id] returns [op_id id]
  : ( NEG var=variable { $id = ops->get_id(op(op_type::Not, $var.id, 0)); }
        | var=variable { $id = $var.id; } )
      { // Check that the literal is not cyclic with its gate variable
        if ($gvar_id == $var.v_id) {
          notifyErrorListeners(fmt::format(
            "Gate variable '{}' is referenced cyclically within statement",
            $var.name));
        }
        // Check that the literal has been defined.
        // NOTE: We have to add it to the unknown variables set, and then
        //       'tick off' each variable as we find it in quantifier gates...
        //       Missing variables will be left over in the set.
        if (free_variables.count($var.v_id) == 0
            && quantified_variables.count($var.v_id) == 0
            && gate_variables.count($var.v_id) == 0)
          unknown_variables.insert($var.v_id);
      }
  ;

// Variables only reserve a spot in the variable manager, without creating a
// variable node in the binary tree.
variable returns [op_id id, var_id v_id, std::string name]
  : IDENT
      { $name = std::move($IDENT.text);
        $v_id = ops->vars().get_id({ $name });
        if (gate_variables.count($v_id) != 0) {
          $id = gate_variables[$v_id];
        } else {
          $id = ops->get_id(op(op_type::Var, $v_id, 0, 0));
        }
      }
  ;
