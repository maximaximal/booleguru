parser grammar qcir_parser;
options { tokenVocab=qcir_lexer; }

// Format from
// http://www.qbflib.org/qcir.pdf
//
// Comments have to be in the parser, as they are
// dependant on the line they are on. The format_id
// is technically also a comment.

formula
    : format_id
      qblock_statement
      output_statement
      gate_statement*
    ;

format_id
    : FORMAT_SPECIFIER INT? EOL
    ;

qblock_statement
    : free_statement? qblock_quant*
    ;

qblock_quant
    : (EXISTS | FORALL) L var_list R EOL
    ;

free_statement
    : FREE L var_list R EOL
    ;

output_statement
    : OUTPUT L ID R EOL
    ;

gate_statement
    : ID EQ AND L lit_list R
    | ID EQ OR L lit_list R
    | ID EQ XOR L lit COMMA lit R
    | ID EQ ITE L lit COMMA lit COMMA lit R
    | ID EQ FORALL L var_list SEMICOLON lit R
    | ID EQ EXISTS L var_list SEMICOLON lit R
    ;

var_list
    : ID
    | ID COMMA var_list
    ;

lit_list
    : lit
    | lit COMMA lit_list
    ;

lit
    : NEGATE? ID
    ;
