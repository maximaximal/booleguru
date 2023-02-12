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
SUBST_MODIFIER : ':' ;
LUA_MODIFIER : 'L' ;
FENNEL_MODIFIER : 'F' ;

ID: [0-9A-Za-z\u0080-\uFFFF_]+ ;
WS: [ \t\n\r\f]+ -> skip ;
