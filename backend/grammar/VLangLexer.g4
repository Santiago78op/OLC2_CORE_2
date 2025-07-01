lexer grammar VLangLexer;

// Reglas Lexicas

// Palabras clave
MUT   : 'mut';
FUNC  : 'fn';

// Estructuras
STR         : 'struct';

// Control de flujo - wk => keyWord
IF_KW       : 'if';
ELSE_KW     : 'else';
SWITCH_KW   : 'switch';
CASE_KW     : 'case';
DEFAULT_KW  : 'default';
FOR_KW      : 'for';
WHILE_KW    : 'while';
IN_KW       : 'in';
BREAK_KW    : 'break';
CONTINUE_KW : 'continue';
RETURN_KW   : 'return';


// Incremento y Decremento
DEC     : '--';
INC     : '++' ;

// Operadores Aritmeticos
PLUS     : '+';
MINUS    : '-';
MULT     : '*';
DIV      : '/';
MOD      : '%';

// Operadores de Asignacion
ASSIGN      : '=';
PLUS_ASSIGN : '+=';
MINUS_ASSIGN: '-=';

// Operadores de Comparacion
EQ       : '==';
NE       : '!=';
LT       : '<';
LE       : '<=';
GT       : '>';
GE       : '>=';

// Operadores Logicos
AND      : '&&';
OR       : '||';
NOT      : '!';

// Delimitadores
LPAREN   : '(';
RPAREN   : ')';
LBRACE   : '{';
RBRACE   : '}';
LBRACK   : '[';
RBRACK   : ']';
SEMI     : ';';
COLON    : ':';
DOT      : '.';
COMMA    : ',';

// Token para el símbolo $ (interpolación)
DOLLAR   : '$';

// Literales
fragment DIGIT : [0-9];
fragment LETTER : [a-zA-Z];
fragment UNDERSCORE : '_';

INT_LITERAL    : DIGIT+;
FLOAT_LITERAL  : DIGIT+ '.' DIGIT+;
STRING_LITERAL: '"' (~["\r\n\\] | ESC_SEQ)* '"';
BOOL_LITERAL   : 'true' | 'false';
NIL_LITERAL    : 'nil';

// Identificador
ID : (LETTER | UNDERSCORE) (LETTER | DIGIT | UNDERSCORE)*;

// Secuencia de escape
fragment ESC_SEQ: '\\' [btnfr"'\\]
    ;

// Commentarios
WS : [ \t\r\n]+ -> skip ;
LINE_COMMENT  : '//' ~[\r\n]* -> skip ;
BLOCK_COMMENT : '/*' .*? '*/' -> skip ;