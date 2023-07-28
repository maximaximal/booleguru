parser grammar cli_parser;
options { tokenVocab=cli_lexer; }

invocation : expr EOL_FENNEL_SUBST?;

expr :
      expr FENNEL_SUBST
    | NOT expr
    | expr AND expr
    | expr OR expr
    | expr XOR expr
    | expr IMPL expr
    | expr LPMI expr
    | expr EQUI expr
    | LPAR expr RPAR
    | FORALL ID expr
    | EXISTS ID expr
    | ID
    | PATH
    | FENNEL_SUBST
    | FENNEL_CALL
    ;
