lexer grammar boole_lexer;

AND : '&' | '&&' | '∧';
OR : '/' | '|' | '||' | '∨';
NOT : '!' | '-' | '~' | '¬';
XOR : '^' | '⊕' | '↮';
LPAR : '(' ;
RPAR : ')' ;
IMPL : '->' | '⇒' ;
LPMI : '<-' ;
EQUI : '<->' ;
TOP : '\u8868' | '⊤';
BOTTOM : '\u8869' | '⊥';
FORALL : '#' | '@' | '∀';
EXISTS : '?' | '∃';

FENNEL_SUBST : ( ':F(' | ':(' | ':f(' | 'f:(' | 'L:(' ) { pushMode(CODE); };
FENNEL : ( 'F(' | 'f(' ) { pushMode(CODE); };

LUA_SUBST : ( ':L(' | ':l(' | 'l:(' | 'L:(' ) { pushMode(CODE); };
LUA : ( 'L(' | 'l(' ) { pushMode(CODE); };

// Exclude mathematical operators from the ID range, other unicode is allowed.
ID: [0-9A-Za-z\u0080-\u2199\u22FF-\uFFFF_'"\][]+ ;
WS: [ \t\n\r\f]+ -> skip ;

mode CODE;
MATCHING_PAREN : ~[()]+ MATCHING_PAREN* | ( '(' ( MATCHING_PAREN | ~[()] )* ')' );
END_PAREN : ')' -> skip,popMode ;
