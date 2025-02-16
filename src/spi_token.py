from enum import Enum


class ElementType(Enum):
    INTEGER = "INTEGER"  # may be should prefix with ARRAY
    REAL = "REAL"
    BOOL = "BOOL"
    STRING = "STRING"
    ARRAY = "ARRAY"
    CLASS = "CLASS"
    INSTANCE = "INSTANCE"
    ENUM = "ENUM"
    RECORD = "RECORD"


class TokenType(Enum):
    # single-character token types
    PLUS = "+"
    MINUS = "-"
    MUL = "*"
    FLOAT_DIV = "/"
    LPAREN = "("
    RPAREN = ")"
    LBRACKET = "["
    RBRACKET = "]"
    SEMI = ";"
    DOT = "."
    RANGE = ".."
    COLON = ":"
    COMMA = ","
    EQ = "="
    NE = "<>"
    GT = ">"
    LT = "<"
    GE = ">="
    LE = "<="
    # block of reserved words
    PROGRAM = "PROGRAM"  # marks the beginning of the block
    CONST = "CONST"
    FUNCTION = "FUNCTION"
    TYPE = "TYPE"
    CLASS = "CLASS"
    RECORD = "RECORD"
    ENUM = "ENUM"
    PRIVATE = "PRIVATE"
    PUBLIC = "PUBLIC"
    CONSTRUCTOR = "CONSTRUCTOR"
    INTEGER = "INTEGER"
    REAL = "REAL"
    BOOLEAN = "BOOLEAN"
    STRING = "STRING"
    INTEGER_DIV = "DIV"
    TRUE = "TRUE"
    FALSE = "FALSE"
    AND = "AND"
    OR = "OR"
    NOT = "NOT"
    VAR = "VAR"
    CASE = "CASE"
    IF = "IF"
    THEN = "THEN"
    ELSE = "ELSE"
    WHILE = "WHILE"
    DO = "DO"
    FOR = "FOR"
    TO = "TO"
    ARRAY = "ARRAY"
    OF = "OF"
    PROCEDURE = "PROCEDURE"
    BEGIN = "BEGIN"
    END = "END"  # marks the end of the block
    # misc
    ID = "ID"
    INTEGER_CONST = "INTEGER_CONST"
    REAL_CONST = "REAL_CONST"
    STRING_CONST = "STRING_CONST"
    ASSIGN = ":="
    EOF = "EOF"
    VOID = "VOID"


class Token:
    def __init__(
        self,
        type: TokenType | None,
        value,
        lineno: int = -1,
        column: int = -1,
    ) -> None:
        self.type = type
        self.value = value
        self.lineno = lineno
        self.column = column

    def __str__(self) -> str:
        """String representation of the class instance.

        Example:
            >>> Token(TokenType.INTEGER, 7, lineno=5, column=10)
            Token(TokenType.INTEGER, 7, position=5:10)
        """
        return "Token({type}, {value}, position={lineno}:{column})".format(
            type=self.type,
            value=repr(self.value),
            lineno=self.lineno,
            column=self.column,
        )

    def __repr__(self) -> str:
        return self.__str__()
