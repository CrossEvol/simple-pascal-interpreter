from __future__ import annotations

from enum import Enum


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
    HASH = "#"
    # block of reserved words
    PROGRAM = "PROGRAM"  # marks the beginning of the block
    RECORD = "RECORD"
    FUNCTION = "FUNCTION"
    INTEGER = "INTEGER"
    REAL = "REAL"
    BOOLEAN = "BOOLEAN"
    STRING = "STRING"
    CHAR = "CHAR"
    INTEGER_DIV = "DIV"
    MOD = "MOD"
    TRUE = "TRUE"
    FALSE = "FALSE"
    AND = "AND"
    OR = "OR"
    NOT = "NOT"
    IN = "IN"
    VAR = "VAR"
    CONST = "CONST"
    IF = "IF"
    THEN = "THEN"
    ELSE = "ELSE"
    WHILE = "WHILE"
    DO = "DO"
    FOR = "FOR"
    TO = "TO"
    BREAK = "BREAK"
    CONTINUE = "CONTINUE"
    ARRAY = "ARRAY"
    OF = "OF"
    CASE = "CASE"
    PROCEDURE = "PROCEDURE"
    FORWARD = "FORWARD"
    TYPE = "TYPE"
    BEGIN = "BEGIN"
    END = "END"  # marks the end of the block
    # misc
    __ENUM__ = "__ENUM__"
    __RECORD__ = "__RECORD__"
    ID = "ID"
    INTEGER_CONST = "INTEGER_CONST"
    REAL_CONST = "REAL_CONST"
    STRING_CONST = "STRING_CONST"
    CHAR_CONST = "CHAR_CONST"
    ASSIGN = ":="
    EOF = "EOF"


class Token:
    def __init__(
        self,
        type: TokenType | None,
        value,
        lineno: int | None = None,
        column: int | None = None,
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


def _build_reserved_keywords():
    """Build a dictionary of reserved keywords.

    The function relies on the fact that in the TokenType
    enumeration the beginning of the block of reserved keywords is
    marked with PROGRAM and the end of the block is marked with
    the END keyword.

    Result:
        {'PROGRAM': <TokenType.PROGRAM: 'PROGRAM'>,
         'INTEGER': <TokenType.INTEGER: 'INTEGER'>,
         'REAL': <TokenType.REAL: 'REAL'>,
         'DIV': <TokenType.INTEGER_DIV: 'DIV'>,
         'MOD': <TokenType.MOD: 'MOD'>,
         'VAR': <TokenType.VAR: 'VAR'>,
         'PROCEDURE': <TokenType.PROCEDURE: 'PROCEDURE'>,
         'BEGIN': <TokenType.BEGIN: 'BEGIN'>,
         'END': <TokenType.END: 'END'>},
         'FUNCTION':<TokenType.FUNCTION: 'FUNCTION'>,
    """
    # enumerations support iteration, in definition order
    tt_list = list(TokenType)
    start_index = tt_list.index(TokenType.PROGRAM)
    end_index = tt_list.index(TokenType.END)
    reserved_keywords = {
        token_type.value: token_type
        for token_type in tt_list[start_index : end_index + 1]
    }
    return reserved_keywords
