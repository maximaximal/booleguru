lexer grammar smtlib2_lexer;

// OPS

AND: ('and' | 'AND' | '&') ;
OR: ('or' | 'OR' | '|') ;
NOT: ('not' | '!') ;
L: '(' ;
R: ')' ;
NIL: ('()' | 'nil') ;

// Keywords:

QF_BV: 'QF_BV' ;
BV: 'BV' ;
CHECK_SAT: ('check-sat' | 'CHECK_SAT') ;
DEFINE_FUN: ('define-fun' | 'DEFINE-FUN') ;
SET_LOGIC: ('set-logic' | 'SET_LOGIC') ;
ASSERT: ('assert' | 'ASSERT') ;
BOOL: ('bool' | 'Bool' | 'BOOL') ;

ID: [0-9A-Za-z\u0080-\u2199\u22FF-\uFFFF_'"\][]+ ;
