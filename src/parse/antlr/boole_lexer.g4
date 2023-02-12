lexer grammar boole_lexer;

AND : '&' | '&&' | '\u2227';
OR : '/' | '|' | '||' | '\u2228';
NOT : '!' | '-' | '~' ;
XOR : '^';
LPAR : '(' ;
RPAR : ')' ;
IMPL : '->' ;
LPMI : '<-' ;
EQUI : '<->' ;
FORALL : '#' | '\u2200' ;
EXISTS : '?' | '\u2203';

FENNEL_SUBST : ':F(' | ':(' | ':f(' | 'f:(' { pushMode(LISP); };
FENNEL : 'F(' | 'f(' { pushMode(LISP); };

ID: [0-9A-Za-z\u0080-\uFFFF_]+ ;
WS: [ \t\n\r\f]+ -> skip ;

mode LISP;
MATCHING_PAREN : '(' ( MATCHING_PAREN | ~[()] )* ')' ;
END_PAREN : ')' -> popMode ;
