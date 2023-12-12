lexer grammar smtlib2_lexer;

// Keywords
AND           options { caseInsensitive=true; } : ('and' | '&') ;
OR            options { caseInsensitive=true; } : ('or' | '|') ;
NOT           options { caseInsensitive=true; } : ('not' | '!') ;
ITE           options { caseInsensitive=true; } : 'ite' ;
BVAND         options { caseInsensitive=true; } : 'bvand' ;
BVADD         options { caseInsensitive=true; } : 'bvadd' ;
BVNEG         options { caseInsensitive=true; } : 'bvneg' ;
BVOR          options { caseInsensitive=true; } : 'bvor' ;
BVNOT         options { caseInsensitive=true; } : 'bvnot' ;
BVULT         options { caseInsensitive=true; } : 'bvult' ;
IMPLIES       options { caseInsensitive=true; } : ('=>') ;
NIL           options { caseInsensitive=true; } : ('()' | 'nil') ;
QF_BV         options { caseInsensitive=true; } : 'QF_BV' ;
BV            options { caseInsensitive=true; } : 'BV' ;
CHECK_SAT     options { caseInsensitive=true; } : 'check-sat' ;
GET_MODEL     options { caseInsensitive=true; } : 'get-model' ;
DECLARE_FUN   options { caseInsensitive=true; } : 'declare-fun' ;
DECLARE_CONST options { caseInsensitive=true; } : 'declare-const' ;
SET_LOGIC     options { caseInsensitive=true; } : 'set-logic' ;
SET_INFO      options { caseInsensitive=true; } : 'set-info' ;
ASSERT        options { caseInsensitive=true; } : 'assert' ;
BOOL          options { caseInsensitive=true; } : 'bool' ;
BITVEC        options { caseInsensitive=true; } : 'BitVec' ;
FORALL        options { caseInsensitive=true; } : 'forall' ;
EXISTS        options { caseInsensitive=true; } : 'exists' ;
LET           options { caseInsensitive=true; } : 'let' ;
EXIT          options { caseInsensitive=true; } : 'exit' ;

// Simple
EQUALS     : '=' ;
L          : '(' ;
R          : ')' ;
UNDERSCORE : '_' ;


ID: ('|' ~'|'* '|') | ([$%?A-Za-z\u0080-\u2199\u22FF-\uFFFF_'"][$!%.0-9A-Za-z\u0080-\u2199\u22FF-\uFFFF_'"]*) ;
KEY: ':'[-0-9A-Za-z\u0080-\u2199\u22FF-\uFFFF_'"]* ;
STR: '"' STR_CONTENT '"' ;
TEXTBLOCK : '|' TEXTBLOCK_CONTENT '|';

INT: [0-9]+ ;
NUMBER: [0-9]+ '.' [0-9]+ ;
COMMENT : ';' ~('\r' | '\n')* -> channel(HIDDEN);
WS : (' ' | '\t' | '\n')+ -> channel(HIDDEN);

fragment TEXTBLOCK_CONTENT : ~'|'* ;
fragment STR_CONTENT : ~'"'* ;
