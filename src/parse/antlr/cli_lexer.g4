lexer grammar cli_lexer;

AND : '--and' | 'and' | '\u2227';
OR : '--or' | 'or' | '/' | '\u2228';
NOT : '!' | '-' | '~' ;
XOR : '--xor' | 'xor';
LPAR : '(' | '[' | '{' | '--lpar' | 'lpar' ;
RPAR : ')' | ']' | '}' | '--rpar' | 'rpar' ;
IMPL : '--impl' | 'impl' | '->' ;
LPMI : '--lpmi' | 'lpmi' | '<-' ;
EQUI : '--equi' | '--equivalent' | '--equivalence' | 'equi' | 'equivalent' | 'equivalence' |'<->' ;
FORALL : '--forall' | 'forall' | '@' | '\u2200' ;
EXISTS : '--exists' | 'exists' | '?' | '\u2203';

FENNEL_SUBST : ( ':(' ) { pushMode(CODE); };
EOL_FENNEL_SUBST : '::' { pushMode(EOL_CODE); };
FENNEL_CALL : ':' { pushMode(CALL); };

ID : [0-9A-Za-z\u0080-\u2199\u22FF-\uFFFF_'"\][]+ ;

PATH : ( '.' | '/' ) [./0-9A-Za-z\u0080-\u2199\u22FF-\uFFFF_'"\][]+ ;

WS : [ \t\f]+ -> skip ;

mode CODE;
MATCHING_PAREN : ~[()]+ MATCHING_PAREN* | ( '(' ( MATCHING_PAREN | ~[()] )* ')' );
END_PAREN : ')' -> skip,popMode ;

mode EOL_CODE;
COMMAND : ~[\r\n]+ ;
EOL : ('\r\n' | '\r') -> skip,popMode ;

mode CALL;
CALL_CODE : ~[ \r\n\t\f]+ ;
END_CALL : ('\r' | '\n' | '\t' | '\f' | ' ') -> skip,popMode ;
