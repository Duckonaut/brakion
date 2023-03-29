# Grammar for the Brakion language

```
source =
    {declaration | comment}, EOF;
comment =
    "#", COMMENT, EOL;

declaration =
    module | function | typeDeclaration | traitDeclaration | traitImplementation;
module =
    visibility, "mod", IDENTIFIER, "{", {declaration}, "}";
function =
    functionSignature, executableBlock;
typeDeclaration =
    visibility, "type", IDENTIFIER, ( typeDefinition | ";" );
traitDeclaration =
    visibility, "trait", IDENTIFIER, "{", {functionStub}, "}";
traitImplementation =
    "impl", namespacedIdentifier, "for", IDENTIFIER, "{", {function}, "}";

functionSignature =
    visibility, "fn", IDENTIFIER, "(" [functionParameters] ")", [returnType];
functionStub =
    functionSignature, ";";
functionParameters =
    (functionParameter | "self"), {",", functionParameter};
functionParameter =
    typedIdentifier, ["?", namespacedIdentifier];
typedIdentifier =
    IDENTIFIER, ":", type;
returnType =
    "->", type;

typeDefinition =
    "{", {variantDeclaration}, {function}, "}";
variantDeclaration =
    IDENTIFIER, variantDefinition;
variantDefinition =
    ";" | ("{", typedIdentifier, {"," typedIdentifier}, [","], "}");

type =
    typePrimary, {"|", typePrimary};
typePrimary =
    namespacedIdentifier | ("[", type, "]");

visibility =
    ["pub"];
executableBlock =
    "{", {statement}, [expression], "}";

statement =
    (expression, ";") | varStmt | forStmt | ifStmt | returnStmt | whileStmt | matchStmt | breakStmt | continueStmt | executableBlock;

varStmt =
    "var", typedIdentifier, "=", expression;
forStmt =
    "for", IDENTIFIER, "in", expression, executableBlock;
ifStmt =
    "if", expression, statement, ["else", statement];
returnStmt =
    "return", [expression], ";";
whileStmt =
    "while", expression, statement;
matchStmt =
    "match", IDENTIFIER, matchBody;
breakStmt =
    "break", ";";
continueStmt =
    "continue", ";";

matchBody =
    "{", [onClause, {",", onCase}], ["," elseCase], [","], "}";

onCase =
    "on", (type | expression), statement;
elseCase =
    "else", statement;

expression = assignment;

assignment = typeIs | [call, "."], IDENTIFIER, "=", assignment;
typeIs = logicOr, ["is", logicOr];
logicOr = logicAnd, ["or", logicAnd];
logicAnd = equality, ["and", equality];
equality = comparison, [("==" | "!="), comparison];
comparison = term, [(">" | ">=" | "<" | "<="), term];
term = factor, [("-" | "+"), factor];
factor = unary, [("/" | "*"), unary];
unary = ("!" | "-"), (unary | primary);

arguments =
    expression, {",", expression}, [","];

call = primary, "(", [arguments], ")";
listAccess = primary, "[", expression, "]";
coalesceAccess = primary, "?", type;
fieldAccess = primary, {".", IDENTIFIER};

constructor =
    namespacedIdentifier, [
        "{",
        [
            fieldConstructor,
            {",", fieldConstructor},
            [","]
        ],
        "}"
    ];
fieldConstructor =
    IDENTIFIER, [":", expression];
namespacedIdentifier =
    IDENTIFIER, {"::", IDENTIFIER};

primary = ("(", expression, ")") | literal | constructor | call | listAccess | coalesceAccess | fieldAccess;

literal =
    "true" | "false" | "void" | "self" |
    NUMBER | STRING | CHAR;

NUMBER =
    DIGIT_NON_ZERO, {DIGIT}, [".", DIGIT, {DIGIT}];
STRING =
    '"', {ANY_CHAR - '"' | '\"'}, '"';
CHAR =
    "'", ((ANY_CHAR - "'" - "\") | "\'" | "\n" | "\r" | "\t" | "\\"), "'";
IDENTIFIER =
    ALPHA, {ALPHA | DIGIT};
ALPHA =
      "A" | "B" | "C" | "D" | "E" | "F" | "G"
    | "H" | "I" | "J" | "K" | "L" | "M" | "N"
    | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
    | "V" | "W" | "X" | "Y" | "Z"
    | "a" | "b" | "c" | "d" | "e" | "f" | "g"
    | "h" | "i" | "j" | "k" | "l" | "m" | "n"
    | "o" | "p" | "q" | "r" | "s" | "t" | "u"
    | "v" | "w" | "x" | "y" | "z";
ANY_CHAR = ? any characters ?;

DIGIT_NON_ZERO =
    "1" | "2" | "3" | "4" | "5" |
    "6" | "7" | "8" | "9";
DIGIT = "0" | DIGIT_NON_ZERO;
```
