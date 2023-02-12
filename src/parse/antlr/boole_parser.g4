parser grammar boole_parser;
options { tokenVocab=boole_lexer; }

formula
    : expr | <EOF>
    ;

expr: NOT expr
    | expr AND expr
    | expr OR expr
    | expr XOR expr
    | expr IMPL expr
    | expr LPMI expr
    | expr EQUI expr
    | LPAR expr RPAR
    ;
