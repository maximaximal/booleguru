lexer grammar qcir_lexer;

LINE_COMMENT
    : '#' ~('\r' | '\n')*
    ;

FORMAT_SPECIFIER : ( '#QCIR-G14' | '#QCIR-14');
FREE : 'free';
EXISTS : 'exists';
FORALL : 'forall';
OUTPUT : 'output';
AND : 'and';
OR : 'or';
XOR : 'xor';
ITE : 'ite';

L: '(' ;
R: ')' ;
EQ: '=' ;
COMMA: ',' ;
SEMICOLON: ';' ;
NEGATE : '-';
EOL : '\n' ;
ID : ('_'|LETTER) ('_'|LETTER|DIGIT)* ;
INT : DIGIT+ ;
DIGIT : '0'..'9' ;
LETTER : ('a'..'z'|'A'..'Z') ;

WS : (' ' | '\t')+ -> channel(HIDDEN);