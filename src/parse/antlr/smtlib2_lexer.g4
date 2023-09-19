lexer grammar smtlib2_lexer;

// Keywords
AND         options { caseInsensitive=true; } : ('and' | '&') ;
OR          options { caseInsensitive=true; } : ('or' | '|') ;
NOT         options { caseInsensitive=true; } : ('not' | '!') ;
NIL         options { caseInsensitive=true; } : ('()' | 'nil') ;
QF_BV       options { caseInsensitive=true; } : 'QF_BV' ;
BV          options { caseInsensitive=true; } : 'BV' ;
CHECK_SAT   options { caseInsensitive=true; } : 'check-sat' ;
DEFINE_FUN  options { caseInsensitive=true; } : 'define-fun' ;
SET_LOGIC   options { caseInsensitive=true; } : 'set-logic' ;
ASSERT      options { caseInsensitive=true; } : 'assert' ;
BOOL        options { caseInsensitive=true; } : 'bool' ;

// Simple
EQUALS : '=' ;
L      : '(' ;
R      : ')' ;


ID: [0-9A-Za-z\u0080-\u2199\u22FF-\uFFFF_'"]+ ;
WS : (' ' | '\t' | '\n')+ -> channel(HIDDEN);
