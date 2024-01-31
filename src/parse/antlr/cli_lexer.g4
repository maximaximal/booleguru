lexer grammar cli_lexer;

@header {
#include <filesystem>
}

AND : '--and' | '\u2227' | '&';
OR : '--or'| '/' | '\u2228' | '|';
NOT : '!' | '~' ;
XOR : '--xor';
LPAR : '(' | '--lpar';
RPAR : ')' | '--rpar';
IMPL : '--impl' | 'impl' | '->' ;
LPMI : '--lpmi' | 'lpmi' | '<-' ;
EQUI : '--equi' | '--equivalent' | '<->' ;
FORALL : '--forall' | '#' | '@' | '\u2200' ;
EXISTS : '--exists' | '?' | '\u2203';
LCURL : '{';
RCURL : '}';
LBRACK : '[';
RBRACK : ']';
TSEITIN : '𝑡';
VEC : '𝑣';
TOP : '\u8868' | '⊤';
BOTTOM : '\u8869' | '⊥';

DIMACS : '--dimacs' | '--qdimacs';
SMTLIB : '--smt' | '--smtlib' | '--smtlib2';
BOOLE : '--boole' | '--bool' | '--limboole';
QCIR : '--qcir';
PYTHON : '--py' | '--python';
LUA : '--lua';
NONE : '--none';

FENNEL_SUBST : ( ':(' ) { pushMode(CODE); };
FENNEL_CALL : ( ':' ) { pushMode(CALL); };
EOL_FENNEL_SUBST : ( '::' ) { pushMode(EOL_CODE); };

ID : [0-9A-Za-z\u0080-\u2199\u22FF-\uFFFF_'"]+ {!std::filesystem::exists(getText()) && getText() != "fuzz";}? ;
PATH : (( '.' | '/' | '~')? [,#\-./0-9A-Za-z\u0080-\u2199\u22FF-\uFFFF_'"\][]+ {getText() == "-" || std::filesystem::exists(getText());}?) | 'fuzz' ;

WS : [ \t\f]+ -> skip ;

mode CODE;
MATCHING_PAREN : ~[()]+ MATCHING_PAREN* | ( '(' ( MATCHING_PAREN | ~[()] )* ')' );
END_PAREN : ')' -> skip,popMode ;

mode EOL_CODE;
COMMAND : ~[\r\n]+ ;
EOL : ('\r\n' | '\n') -> skip,popMode ;

mode CALL;
CALL_CODE : ~[ \r\n\t\f]+ ;
END_CALL : ('\r' | '\n' | '\t' | '\f' | ' ' | EOF) -> skip,popMode ;
