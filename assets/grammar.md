# Grammar for the Brakion language

```
source ->
    (declaration | comment)* EOF ;
comment ->
    "#" COMMENT EOL;

declaration ->
    module | function | typeDeclaration | traitDeclaration | traitImplementation;
module ->
    visibility "mod" IDENTIFIER "{" declaration* "}";
function ->
    functionSignature executableBlock;
typeDeclaration ->
    visibility "type" IDENTIFIER ( typeDefinition | ";" );
traitDeclaration ->
    visibility "trait" IDENTIFIER "{" functionStub* "}";
traitImplementation ->
    "impl" namespacedIdentifier "for" IDENTIFIER "{" function* "}";

functionSignature ->
    visibility "fn" IDENTIFIER "(" functionParameters? ")" returnType?;
functionStub ->
    functionSignature ";";
functionParameters ->
    (functionParameter | "self") (",", functionParameter)*;
functionParameter ->
    typedIdentifier ("?" namespacedIdentifier);
typedIdentifier ->
    IDENTIFIER ":" type;
returnType ->
    "->" type;

typeDefinition ->
    "{" variantDeclaration* function* "}";
variantDeclaration ->
    ("self" | IDENTIFIER) variantDefinition;
variantDefinition ->
    ";" | "{" typedIdentifier ("," typedIdentifier)* ","? "}";

type ->
    typePrimary ("|" typePrimary)*;
typePrimary ->
    namespacedIdentifier | ("[" type "]");

visibility ->
    "pub"?;
executableBlock ->
    "{" statement* expression? "}";

statement ->
    (expression ";") | varStmt | forStmt | ifStmt | returnStmt | whileStmt | matchStmt | breakStmt | continueStmt | executableBlock;

varStmt ->
    "var" typedIdentifier "=" expression;
forStmt ->
    "for" IDENTIFIER "in" expression executableBlock;
ifStmt ->
    "if" expression statement ("else" statement)?;
returnStmt ->
    "return" expression? ";";
whileStmt ->
    "while" expression statement;
matchStmt ->
    "match" IDENTIFIER matchBody;
breakStmt ->
    "break" ";";
continueStmt ->
    "continue" ";";

matchBody ->
    "{" (onClause ("," onCase)*)? ("," elseCase)? ","? "}";

onCase ->
    "on" (type | expression) statement;
elseCase ->
    "else" statement;

expression -> assignment;

assignment -> type_is | ( call "." )? IDENTIFIER "=" assignment;
type_is -> logic_or ("is" logic_or)*;
logic_or -> logic_and ("or" logic_and)*;
logic_and -> equality ("and" equality)*;
equality -> comparison (("==" | "!=") comparison)*;
comparison -> term ((">" | ">=" | "<" | "<=") term)*;
term -> factor (("-" | "+") factor)*;
factor -> unary (("/" | "*") unary)*;
unary -> ("!" | "-") (unary | call);
call -> access (("(" arguments? ")") | ("[" expression "]") | ("." IDENTIFIER))*;
access ->
    primary ("?" namespacedIdentifier?)?;

arguments ->
    expression ("," expression)* ","?;
primary ->
    "true" | "false" | "void" | "self" |
    NUMBER | STRING | CHAR | constructor |
    "(" expression ")";

constructor ->
    namespacedIdentifier ("{"
        fieldConstructor?
        ("," fieldConstructor)*
        ","?
    "}")?;
fieldConstructor ->
    IDENTIFIER (":" expression)?;
namespacedIdentifier ->
    IDENTIFIER ("::" IDENTIFIER)*;
```

# Lexical grammar
## Similar notation
```
COMMENT -> [^\n\r]*
NUMBER ->
    DIGIT+ ( "." DIGIT+ )? ;
STRING ->
    "\"" <any char except "\"">* "\"" ;
CHAR ->
    "'" <any char except "'"> "'";
IDENTIFIER ->
    ALPHA ( ALPHA | DIGIT )* ;
ALPHA ->
    "a" ... "z" | "A" ... "Z" | "_" ;
DIGIT ->
    "0" ... "9" ;
```
## Regex
```
NUMBER = [0-9]+(\.[0-9]+)?;
STRING = \"[^\"]*\";
IDENTIFIER = [a-zA-Z_][a-zA-Z_0-9]*
```
