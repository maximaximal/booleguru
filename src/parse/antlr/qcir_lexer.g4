lexer grammar qcir_lexer;

// Keywords
FREE   options { caseInsensitive=true; } : 'free' ;
EXISTS options { caseInsensitive=true; } : 'exists' ;
FORALL options { caseInsensitive=true; } : 'forall' ;
OUTPUT options { caseInsensitive=true; } : 'output' ;
AND    options { caseInsensitive=true; } : 'and' ;
OR     options { caseInsensitive=true; } : 'or' ;
XOR    options { caseInsensitive=true; } : 'xor' ;
ITE    options { caseInsensitive=true; } : 'ite' ;

// Simple symbols
LPAR      : '(' ;
RPAR      : ')' ;
EQ        : '=' ;
COMMA     : ',' ;
SEMICOLON : ';' ;
NEG       : '-' ;
/* TODO: Should allow for support of LF, CR, and CRLF? */
EOL       : '\n' | '\r' | '\r\n' ;

// Identifiers and numbers
IDENT  : ( '_' | DIGIT | LETTER )+ ;
NUMBER : DIGIT+ ;
DIGIT  : '0'..'9' ;
LETTER : 'a'..'z' | 'A'..'Z' ;

// Comments (including format ID) and whitespace
FORMAT_ID    : '#' ( 'QCIR-G14' | 'QCIR-14'  | 'QCIR-13'  ) DIGIT? ;
LINE_COMMENT : '#' ~( '\r' | '\n' )* -> channel(HIDDEN) ;
WS           : ( ' ' | '\t' )+ -> channel(HIDDEN) ;

