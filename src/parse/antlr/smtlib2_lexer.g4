lexer grammar smtlib2_lexer;

// Keywords
AND           options { caseInsensitive=true; } : ('and' | '&') ;
OR            options { caseInsensitive=true; } : ('or' | '|') ;
NOT           options { caseInsensitive=true; } : ('not' | '!') ;
BVAND         options { caseInsensitive=true; } : ('bvand') ;
BVOR          options { caseInsensitive=true; } : ('bvor') ;
BVNOT         options { caseInsensitive=true; } : ('bvnot') ;
NIL           options { caseInsensitive=true; } : ('()' | 'nil') ;
QF_BV         options { caseInsensitive=true; } : 'QF_BV' ;
BV            options { caseInsensitive=true; } : 'BV' ;
CHECK_SAT     options { caseInsensitive=true; } : 'check-sat' ;
DEFINE_FUN    options { caseInsensitive=true; } : 'define-fun' ;
DECLARE_CONST options { caseInsensitive=true; } : 'declare-const' ;
SET_LOGIC     options { caseInsensitive=true; } : 'set-logic' ;
ASSERT        options { caseInsensitive=true; } : 'assert' ;
BOOL          options { caseInsensitive=true; } : 'bool' ;
BITVEC        options { caseInsensitive=true; } : 'BitVec' ;

// Simple
EQUALS     : '=' ;
L          : '(' ;
R          : ')' ;
UNDERSCORE : '_' ;


ID: [A-Za-z\u0080-\u2199\u22FF-\uFFFF_'"][0-9A-Za-z\u0080-\u2199\u22FF-\uFFFF_'"]* ;
INT: [0-9]+ ;
WS : (' ' | '\t' | '\n')+ -> channel(HIDDEN);
