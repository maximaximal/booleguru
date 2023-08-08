lexer grammar qcir_lexer;

channels { COMMENTS, WHITESPACE }

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
fragment DIGIT  : '0'..'9' ;
fragment LETTER : 'a'..'z' | 'A'..'Z' ;

// Format ID, a special form of a comment, should match before LINE_COMMENT
FORMAT_ID : '#' WS? ( 'QCIR-G14' | 'QCIR-14'  | 'QCIR-13' ) WS? NUMBER? WS? ;

// Tokens sent to other channels, we don't need these in the parser, but might
// want to do something with them
LINE_COMMENT : '#' ~( '\r' | '\n' )* -> channel(COMMENTS) ;
WS           : ( ' ' | '\t' )+ -> channel(WHITESPACE) ;

