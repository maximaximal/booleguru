parser grammar smtlib2_parser;
options { tokenVocab=smtlib2_lexer; }

formula
    : expr*
    ;

stmt
    : L stmt_inner R
    ;

stmt_inner
    : CHECK_SAT
    | SET_LOGIC ( QF_BV | BV )
    | DEFINE_FUN ID NIL BOOL
    | ASSERT expr
    ;

expr
    : ID
    | L AND expr+ R
    | L OR expr+ R
    | L NOT expr R
    ;
