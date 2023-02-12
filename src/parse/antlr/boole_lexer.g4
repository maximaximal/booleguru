lexer grammar boole_lexer;

AND : '&' | '&&' | '\u8743';
OR : '/' | '|' | '||' | '\u8744';
NOT : '!' | '-' | '~' ;
XOR : '^';
LPAR : '(' ;
RPAR : ')' ;
IMPL : '->' ;
LPMI : '<-' ;
EQUI : '<->' ;
SUBST_MODIFIER : ':' ;
LUA_MODIFIER : 'L' ;
FENNEL_MODIFIER : 'F' ;

ID: [A-Za-z][0-9A-Za-z\u0080-\uFFFF_]+ ;
WS: [ \t\n\r\f]+ -> skip ;
