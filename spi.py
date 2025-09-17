"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass
from enum import Enum
from typing import Any, Callable, cast

_SHOULD_LOG_SCOPE = False  # see '--scope' command line option
_SHOULD_LOG_STACK = False  # see '--stack' command line option

RETURN_NUM_FOR_LENGTH = "RETURN_NUM_FOR_LENGTH"


class SpiUtil:
    @staticmethod
    def print_w(message: Any):
        print(f"\033[91m{message}\033[0m", file=sys.stderr)


class ElementType(Enum):
    INTEGER = "INTEGER"
    REAL = "REAL"
    BOOL = "BOOL"
    STRING = "STRING"
    ARRAY = "ARRAY"


###############################################################################
#                                                                             #
#  OBJECT SYSTEM                                                              #
#                                                                             #
###############################################################################


class Object:
    """Base class for all Pascal values in the interpreter"""

    def __init__(self, value=None):
        self.value = value

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return f"{self.__class__.__name__}({self.value})"

    def to_bool(self):
        """Convert to boolean for conditional expressions"""
        return bool(self.value)


class NumberObject(Object):
    """Base class for numeric types"""

    def __add__(self, other):
        if isinstance(other, NumberObject):
            return self._create_result(self.value + other.value)
        return NotImplemented

    def __sub__(self, other):
        if isinstance(other, NumberObject):
            return self._create_result(self.value - other.value)
        return NotImplemented

    def __mul__(self, other):
        if isinstance(other, NumberObject):
            return self._create_result(self.value * other.value)
        return NotImplemented

    def __truediv__(self, other):
        if isinstance(other, NumberObject):
            return RealObject(float(self.value) / float(other.value))
        return NotImplemented

    def __floordiv__(self, other):
        if isinstance(other, NumberObject):
            return self._create_result(self.value // other.value)
        return NotImplemented

    def __pos__(self):
        return self._create_result(+self.value)

    def __neg__(self):
        return self._create_result(-self.value)

    def __lt__(self, other):
        if isinstance(other, NumberObject):
            return BooleanObject(self.value < other.value)
        return NotImplemented

    def __le__(self, other):
        if isinstance(other, NumberObject):
            return BooleanObject(self.value <= other.value)
        return NotImplemented

    def __gt__(self, other):
        if isinstance(other, NumberObject):
            return BooleanObject(self.value > other.value)
        return NotImplemented

    def __ge__(self, other):
        if isinstance(other, NumberObject):
            return BooleanObject(self.value >= other.value)
        return NotImplemented

    def __eq__(self, other):
        if isinstance(other, NumberObject):
            return BooleanObject(self.value == other.value)
        return NotImplemented

    def __ne__(self, other):
        if isinstance(other, NumberObject):
            return BooleanObject(self.value != other.value)
        return NotImplemented

    def _create_result(self, value):
        """Create appropriate result type based on value"""
        if isinstance(value, int):
            return IntegerObject(value)
        else:
            return RealObject(value)


class IntegerObject(NumberObject):
    """Integer value object"""

    def __init__(self, value: int = 0):
        super().__init__(int(value))

    def _create_result(self, value):
        if isinstance(value, int):
            return IntegerObject(value)
        else:
            return RealObject(value)


class RealObject(NumberObject):
    """Real/Float value object"""

    def __init__(self, value: float = 0.0):
        super().__init__(float(value))

    def _create_result(self, value):
        return RealObject(float(value))


class BooleanObject(Object):
    """Boolean value object"""

    def __init__(self, value: bool = False):
        super().__init__(bool(value))

    def __and__(self, other):
        if isinstance(other, BooleanObject):
            return BooleanObject(self.value and other.value)
        return NotImplemented

    def __or__(self, other):
        if isinstance(other, BooleanObject):
            return BooleanObject(self.value or other.value)
        return NotImplemented

    def __invert__(self):
        return BooleanObject(not self.value)

    def __eq__(self, other):
        if isinstance(other, BooleanObject):
            return BooleanObject(self.value == other.value)
        return NotImplemented

    def __ne__(self, other):
        if isinstance(other, BooleanObject):
            return BooleanObject(self.value != other.value)
        return NotImplemented


class StringObject(Object):
    """String value object"""

    def __init__(self, value: str = "", limit: int = -1):
        if limit > 0 and len(value) > limit:
            value = value[:limit]
        super().__init__(str(value))
        self.limit = limit

    def __add__(self, other):
        if isinstance(other, StringObject):
            result_value = self.value + other.value
            # Don't apply limits during concatenation operations
            return StringObject(result_value, -1)
        return NotImplemented

    def __getitem__(self, index):
        """Get character at index (1-based indexing for Pascal)"""
        if 1 <= index <= len(self.value):
            return CharObject(self.value[index - 1])
        return CharObject("")

    def __len__(self):
        return len(self.value)

    def set_length(self, new_length: int):
        """Set new length for string"""
        if new_length < len(self.value):
            self.value = self.value[:new_length]
        # Note: Pascal strings don't automatically extend with spaces


class CharObject(Object):
    """Character value object"""

    def __init__(self, value: str = ""):
        super().__init__(str(value)[:1] if value else "")


class ArrayObject(Object):
    """Array value object"""

    def __init__(
        self,
        element_type: ElementType,
        lower_bound: int = 0,
        upper_bound: int = 0,
        dynamic: bool = False,
    ):
        super().__init__({})
        self.element_type = element_type
        self.lower_bound = lower_bound
        self.upper_bound = upper_bound
        self.dynamic = dynamic

        # Initialize static arrays
        if not dynamic and lower_bound <= upper_bound:
            for i in range(lower_bound, upper_bound + 1):
                self.value[i] = self._create_default_element()

    def _create_default_element(self):
        """Create default element based on element type"""
        if self.element_type == ElementType.INTEGER:
            return IntegerObject(0)
        elif self.element_type == ElementType.REAL:
            return RealObject(0.0)
        elif self.element_type == ElementType.BOOL:
            return BooleanObject(False)
        elif self.element_type == ElementType.STRING:
            return StringObject("")
        elif self.element_type == ElementType.ARRAY:
            return ArrayObject(ElementType.INTEGER, 0, 0, True)  # Default nested array
        else:
            return Object()

    def __getitem__(self, index):
        """Get element at index"""
        if index in self.value:
            return self.value[index]
        else:
            # Return default value for out-of-bounds access
            return self._create_default_element()

    def __setitem__(self, index, value):
        """Set element at index"""
        self.value[index] = value

    def __len__(self):
        """Return length of array"""
        if self.dynamic:
            return len(self.value)
        else:
            return self.upper_bound - self.lower_bound + 1

    def set_length(self, new_length: int):
        """Set new length for dynamic array"""
        if not self.dynamic:
            raise InterpreterError(
                error_code=ErrorCode.INTERPRETER_STATIC_ARRAY_MODIFY_LENGTH,
                token=None,
                message="Cannot modify length of static array",
            )

        # Add new elements if extending
        for i in range(len(self.value), new_length):
            self.value[i] = self._create_default_element()

        # Remove elements if shrinking
        if new_length < len(self.value):
            keys_to_remove = [k for k in self.value.keys() if k >= new_length]
            for k in keys_to_remove:
                del self.value[k]


class ErrorCode(Enum):
    # Common errors
    UNEXPECTED_TOKEN = "Unexpected token"
    ID_NOT_FOUND = "Identifier not found"
    DUPLICATE_ID = "Duplicate id found"
    NULL_POINTER = "Null pointer exception"
    CURRENT_SCOPE_NOT_FOUND = "Current scope not found"
    UNKNOWN_BIN_OP = "Unknown binary operator"
    UNKNOWN_UNARY_OP = "Unknown unary operator"
    MODIFY_LOOP_VAR_NOT_ALLOW = (
        "modify loop control variable is not allowed inside for-statement"
    )
    MISSING_CURRENT_SCOPE = "Missing current scope"

    # Lexer errors
    LEXER_INVALID_CHARACTER = "Lexer invalid character"
    LEXER_STRING_ERROR = "Lexer string error"

    # Parser errors
    PARSER_UNEXPECTED_TOKEN = "Parser unexpected token"

    # Semantic errors
    SEMANTIC_UNKNOWN_TYPE = "Semantic unknown type"
    SEMANTIC_UNKNOWN_ARRAY_ELEMENT_TYPE = "Semantic unknown array element type"
    SEMANTIC_UNKNOWN_SYMBOL = "Semantic unknown symbol"
    SEMANTIC_UNKNOWN_BOOLEAN = "Semantic unknown boolean"

    # Interpreter errors
    INTERPRETER_STATIC_ARRAY_MODIFY_LENGTH = "Interpreter static array modify length"
    INTERPRETER_UNKNOWN_BUILTIN_FUNCTION = "Interpreter unknown builtin function"
    INTERPRETER_UNKNOWN_BUILTIN_PROCEDURE = "Interpreter unknown builtin procedure"
    INTERPRETER_ARRAY_RANGE_INVALID = "Interpreter array range invalid"
    INTERPRETER_UNKNOWN_OPERATOR = "Interpreter unknown operator"
    INTERPRETER_UNKNOWN_BOOLEAN = "Interpreter unknown boolean"


class Error(Exception):
    def __init__(self, error_code=None, token=None, message=None) -> None:
        self.error_code = error_code
        self.token = token
        # add exception class name before the message
        self.message = f"{self.__class__.__name__}: {message}"


###############################################################################
#                                                                             #
#  LexerError                                                                 #
#                                                                             #
###############################################################################


class LexerError(Error):
    pass


###############################################################################
#                                                                             #
#  ParserError                                                                #
#                                                                             #
###############################################################################


class ParserError(Error):
    pass


###############################################################################
#                                                                             #
#  SemanticError                                                              #
#                                                                             #
###############################################################################


class SemanticError(Error):
    pass


###############################################################################
#                                                                             #
#  InterpreterError                                                           #
#                                                                             #
###############################################################################


class InterpreterError(Error):
    pass


###############################################################################
#                                                                             #
#  LEXER                                                                      #
#                                                                             #
###############################################################################


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
    FUNCTION = "FUNCTION"
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


RESERVED_KEYWORDS: dict[str, TokenType] = _build_reserved_keywords()


@dataclass
class LexerStatus:
    pos: int
    current_char: str | None
    lineno: int
    column: int


class Lexer:
    def __init__(self, text: str) -> None:
        # client string input, e.g. "4 + 2 * 3 - 6 / 2"
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.current_char: str | None = self.text[self.pos]
        # token line number and column number
        self.lineno = 1
        self.column = 1

    def error(self):
        s = "Lexer error on '{lexeme}' line: {lineno} column: {column}".format(
            lexeme=self.current_char,
            lineno=self.lineno,
            column=self.column,
        )
        raise LexerError(message=s)

    def status(self) -> LexerStatus:
        """return the `pos` pointer`"""
        return LexerStatus(self.pos, self.current_char, self.lineno, self.column)

    def advance(self) -> None:
        """Advance the `pos` pointer and set the `current_char` variable."""
        if self.current_char == "\n":
            self.lineno += 1
            self.column = 0

        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None  # Indicates end of input
        else:
            self.current_char = self.text[self.pos]
            self.column += 1

    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def skip_whitespace(self) -> None:
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def skip_comment(self) -> None:
        while self.current_char != "}":
            self.advance()
        self.advance()  # the closing curly brace

    def number(self) -> Token:
        """Return a (multidigit) integer or float consumed from the input."""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        result = ""
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == "." and self.peek() == ".":
            token.type = TokenType.INTEGER_CONST
            token.value = int(result)
        elif self.current_char == ".":
            result += self.current_char
            self.advance()

            while self.current_char is not None and self.current_char.isdigit():
                result += self.current_char
                self.advance()

            token.type = TokenType.REAL_CONST
            token.value = float(result)
        else:
            token.type = TokenType.INTEGER_CONST
            token.value = int(result)

        return token

    def __string(self) -> Token:
        """Handle string const"""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        if self.current_char == "'":
            self.advance()
        else:
            raise LexerError(
                error_code=ErrorCode.LEXER_STRING_ERROR,
                token=token,
                message="Unterminated string",
            )
        value = ""
        while self.current_char is not None and self.current_char != "'":
            value += self.current_char
            self.advance()

        if self.current_char == "'":
            self.advance()
        else:
            raise LexerError(
                error_code=ErrorCode.LEXER_STRING_ERROR,
                token=token,
                message="Unterminated string",
            )

        token.type = TokenType.STRING_CONST
        token.value = value
        return token

    def __id(self) -> Token:
        """Handle identifiers and reserved keywords"""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        value = ""
        while self.current_char is not None and self.current_char.isalnum():
            value += self.current_char
            self.advance()

        token_type = RESERVED_KEYWORDS.get(value.upper())
        if token_type is None:
            token.type = TokenType.ID
            token.value = value
        else:
            # reserved keyword
            token.type = token_type
            token.value = value.upper()

        return token

    def comparison(self) -> Token:
        """handle six comparison operators, [ '<', '>', '<>', '=', '<=', '>=']"""
        if self.current_char == "=":
            token = Token(
                type=TokenType.EQ,
                value=TokenType.EQ.value,  # '='
                lineno=self.lineno,
                column=self.column,
            )
            self.advance()
            return token

        if self.current_char == ">":
            if self.peek() == "=":
                token = Token(
                    type=TokenType.GE,
                    value=TokenType.GE.value,  # '>='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token
            else:
                token = Token(
                    type=TokenType.GT,
                    value=TokenType.GT.value,  # '>'
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token

        if self.current_char == "<":
            if self.peek() == "=":
                token = Token(
                    type=TokenType.LE,
                    value=TokenType.LE.value,  # '<='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token
            elif self.peek() == ">":
                token = Token(
                    type=TokenType.NE,
                    value=TokenType.NE.value,  # '<>'
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token
            else:
                token = Token(
                    type=TokenType.LT,
                    value=TokenType.LT.value,  # '<'
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token
        raise LexerError(
            error_code=ErrorCode.LEXER_INVALID_CHARACTER,
            token=Token(
                type=None,
                value=self.current_char,
                lineno=self.lineno,
                column=self.column,
            ),
            message=f"{ErrorCode.LEXER_INVALID_CHARACTER.value} -> '{self.current_char}'",
        )

    def get_next_token(self) -> Token:
        """Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char in ["<", ">", "="]:
                return self.comparison()

            if self.current_char == "{":
                self.advance()
                self.skip_comment()
                continue

            if self.current_char == "'":
                return self.__string()

            if self.current_char.isalpha():
                return self.__id()

            if self.current_char == "." and self.peek() == ".":
                token = Token(
                    type=TokenType.RANGE,
                    value=TokenType.RANGE.value,  # ':='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            if self.current_char.isdigit():
                return self.number()

            if self.current_char == ":" and self.peek() == "=":
                token = Token(
                    type=TokenType.ASSIGN,
                    value=TokenType.ASSIGN.value,  # ':='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            # single-character token
            try:
                # get enum member by value, e.g.
                # TokenType(';') --> TokenType.SEMI
                token_type = TokenType(self.current_char)
            except ValueError:
                # no enum member with value equal to self.current_char
                self.error()
            else:
                # create a token with a single-character lexeme as its value
                token = Token(
                    type=token_type,
                    value=token_type.value,  # e.g. ';', '.', etc
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token

        # EOF (end-of-file) token indicates that there is no more
        # input left for lexical analysis
        return Token(type=TokenType.EOF, value=None)

    def peek_next_token(self) -> Token:
        prev_status = self.status()
        token = self.get_next_token()
        self.revert(prev_status)
        return token

    def revert(self, status: LexerStatus) -> None:
        """revert the current lexer status before call self.get_next_token()"""
        self.pos = status.pos
        self.current_char = status.current_char
        self.lineno = status.lineno
        self.column = status.column


###############################################################################
#                                                                             #
#  PARSER                                                                     #
#                                                                             #
###############################################################################
class AST:
    def __init__(self) -> None:
        self._num: int | None = None
        self.value: Any = None


class Statement(AST):
    def __init__(self):
        pass


class Declaration(Statement):
    def __init__(self):
        super().__init__()


class Expression(AST):
    def __init__(self):
        super().__init__()


class BinOp(Expression):
    def __init__(self, left: AST, op: Token, right: AST) -> None:
        self.left = left
        self.token = self.op = op
        self.right = right


type Number = int | float


class Num(Expression):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value: Number = token.value


class Bool(Expression):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value: bool = token.value


class String(Expression):
    def __init__(self, token: Token):
        self.token = token
        self.value: str = token.value


class UnaryOp(Expression):
    def __init__(self, op: Token, expr: AST) -> None:
        self.token = self.op = op
        self.expr = expr


class Compound(Statement):
    """Represents a 'BEGIN ... END' block"""

    def __init__(self) -> None:
        self.children: list[AST] = []


class IfStatement(Statement):
    """Represents a 'if ... then... elseif ... then ... else' statement"""

    def __init__(
        self,
        condition: AST,
        then_branch: AST,
        else_if_branches: list[IfStatement],
        else_branch: AST | None,
    ) -> None:
        self.condition = condition
        self.then_branch = then_branch
        self.else_if_branches = else_if_branches
        self.else_branch = else_branch


class WhileStatement(Statement):
    """Represents a 'WHILE ... DO ... BEGIN ... END' block"""

    def __init__(self, condition: AST, block: Compound) -> None:
        self.condition = condition
        self.block = block


class ForStatement(Statement):
    """Represents a 'FOR left:= right TO ... DO (BEGIN ... END) or statement' block"""

    def __init__(self, initialization: Assign, bound: AST, block: AST) -> None:
        self.initialization = initialization
        self.bound = bound
        self.block = block


class Assign(Statement):
    def __init__(self, left: Var, op: Token, right) -> None:
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(Expression):
    """The Var node is constructed out of ID token."""

    def __init__(self, token: Token) -> None:
        self.token = token
        self.value = token.value


class IndexVar(Var):
    """The IndexVar is for ID[index]"""

    def __init__(self, token, index: AST):
        super().__init__(token)
        self.index = index


class NoOp(Statement):
    pass


class Program(Statement):
    def __init__(self, name: str, block: Block) -> None:
        self.name = name
        self.block = block


class Block(Statement):
    def __init__(
        self, declarations: list[Declaration], compound_statement: Compound
    ) -> None:
        self.declarations = declarations
        self.compound_statement = compound_statement


class VarDecl(Declaration):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node


class Type(Expression):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value = token.value


class PrimitiveType(Type):
    def __init__(self, token):
        super().__init__(token)

    def __str__(self):
        return self.value


class StringType(Type):
    def __init__(self, token, limit: Expression | None = None):
        super().__init__(token)
        self.limit = limit

    def __str__(self):
        return super().__str__()


class ArrayType(Type):
    def __init__(
        self,
        token,
        element_type: Type,
        lower: Expression,
        upper: Expression,
        dynamic: bool = False,
    ):
        super().__init__(token)
        self.element_type = element_type
        self.lower = lower
        self.upper = upper
        self.dynamic = dynamic

    def __str__(self):
        return "Array[{element_type_name}]".format(
            element_type_name=str(self.element_type)
        )


class Param(Expression):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node


class ProcedureDecl(Declaration):
    def __init__(
        self, proc_name: str, formal_params: list[Param], block_node: Block
    ) -> None:
        self.proc_name = proc_name
        self.formal_params = formal_params  # a list of Param nodes
        self.block_node = block_node


class FunctionDecl(Declaration):
    def __init__(
        self,
        func_name: str,
        formal_params: list[Param],
        return_type: Type,
        block_node: Block,
    ) -> None:
        self.func_name = func_name
        self.formal_params = formal_params  # a list of Param nodes
        self.return_type = return_type
        self.block_node = block_node


class ProcedureCall(Statement):
    def __init__(
        self, proc_name: str, actual_params: list[Expression], token: Token
    ) -> None:
        self.proc_name = proc_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.proc_symbol: ProcedureSymbol | None = None


class FunctionCall(Expression):
    def __init__(
        self, func_name: str, actual_params: list[Expression], token: Token
    ) -> None:
        self.func_name = func_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.func_symbol: FunctionSymbol | None = None


class Parser:
    def __init__(self, lexer: Lexer) -> None:
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.current_token = self.get_next_token()

    def newZeroNum(self, lineno: int = -1, column: int = -1) -> Num:
        return Num(
            token=Token(TokenType.INTEGER_CONST, value=0, lineno=lineno, column=column)
        )

    def get_next_token(self):
        return self.lexer.get_next_token()

    def peek_next_token(self):
        return self.lexer.peek_next_token()

    def error(self, error_code: ErrorCode, token: Token):
        raise ParserError(
            error_code=error_code,
            token=token,
            message=f"{error_code.value} -> {token}",
        )

    def eat(self, token_type: TokenType) -> None:
        # compare the current token type with the passed token
        # type and if they match then "eat" the current token
        # and assign the next token to the self.current_token,
        # otherwise raise an exception.
        if self.current_token.type == token_type:
            self.current_token = self.get_next_token()
        else:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )

    def program(self) -> Program:
        """program : PROGRAM variable SEMI block DOT"""
        self.eat(TokenType.PROGRAM)
        var_node = self.variable()
        prog_name = var_node.value
        self.eat(TokenType.SEMI)
        block_node = self.block()
        program_node = Program(prog_name, block_node)
        self.eat(TokenType.DOT)
        return program_node

    def block(self) -> Block:
        """block : declarations compound_statement"""
        declaration_nodes = self.declarations()
        compound_statement_node = self.compound_statement()
        node = Block(declaration_nodes, compound_statement_node)
        return node

    def declarations(self) -> list[Declaration]:
        """
        declarations : (VAR (variable_declaration SEMI)+)? procedure_declaration* function_declaration*
        """
        declarations: list[Declaration] = []

        if self.current_token.type == TokenType.VAR:
            self.eat(TokenType.VAR)
            while self.current_token.type == TokenType.ID:
                var_decl = self.variable_declaration()
                declarations.extend(var_decl)
                self.eat(TokenType.SEMI)

        while self.current_token.type == TokenType.PROCEDURE:
            proc_decl = self.procedure_declaration()
            declarations.append(proc_decl)

        while self.current_token.type == TokenType.FUNCTION:
            func_decl = self.function_declaration()
            declarations.append(func_decl)

        return declarations

    def formal_parameters(self) -> list[Param]:
        """formal_parameters : ID (COMMA ID)* COLON type_spec"""
        param_nodes: list[Param] = []

        param_tokens = [self.current_token]
        self.eat(TokenType.ID)
        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            param_tokens.append(self.current_token)
            self.eat(TokenType.ID)

        self.eat(TokenType.COLON)
        type_node = self.type_spec()

        for param_token in param_tokens:
            param_node = Param(Var(param_token), type_node)
            param_nodes.append(param_node)

        return param_nodes

    def formal_parameter_list(self) -> list[Param]:
        """formal_parameter_list : formal_parameters
        | formal_parameters SEMI formal_parameter_list
        """
        # procedure Foo();
        if not self.current_token.type == TokenType.ID:
            return []

        param_nodes = self.formal_parameters()

        while self.current_token.type == TokenType.SEMI:
            self.eat(TokenType.SEMI)
            param_nodes.extend(self.formal_parameters())

        return param_nodes

    def variable_declaration(self) -> list[VarDecl]:
        """variable_declaration : ID (COMMA ID)* COLON type_spec"""
        var_nodes = [Var(self.current_token)]  # first ID
        self.eat(TokenType.ID)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            var_nodes.append(Var(self.current_token))
            self.eat(TokenType.ID)

        self.eat(TokenType.COLON)

        type_node = self.type_spec()
        var_declarations = [VarDecl(var_node, type_node) for var_node in var_nodes]
        return var_declarations

    def procedure_declaration(self) -> ProcedureDecl:
        """procedure_declaration :
        PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI
        """
        self.eat(TokenType.PROCEDURE)
        proc_name = self.current_token.value
        self.eat(TokenType.ID)
        formal_params = []

        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            formal_params = self.formal_parameter_list()
            self.eat(TokenType.RPAREN)

        self.eat(TokenType.SEMI)
        block_node = self.block()
        proc_decl = ProcedureDecl(proc_name, formal_params, block_node)
        self.eat(TokenType.SEMI)
        return proc_decl

    def function_declaration(self) -> FunctionDecl:
        """function_declaration :
        FUNCTION ID LPAREN (formal_parameter_list)? RPAREN COLON type_spec SEMI block SEMI
        """
        self.eat(TokenType.FUNCTION)
        func_name = self.current_token.value
        self.eat(TokenType.ID)

        formal_params = []
        self.eat(TokenType.LPAREN)
        formal_params = self.formal_parameter_list()
        self.eat(TokenType.RPAREN)
        self.eat(TokenType.COLON)

        return_type = self.type_spec()

        self.eat(TokenType.SEMI)
        block_node = self.block()
        func_decl = FunctionDecl(func_name, formal_params, return_type, block_node)
        self.eat(TokenType.SEMI)
        return func_decl

    def type_spec(self) -> Type:
        """
        type_spec : primitive_type_spec | string_type_spec | array_type_spec
        """
        if self.current_token.type in (
            TokenType.INTEGER,
            TokenType.REAL,
            TokenType.BOOLEAN,
        ):
            return self.primitive_type_spec()
        elif self.current_token.type == TokenType.STRING:
            return self.string_type_spec()
        elif self.current_token.type == TokenType.ARRAY:
            return self.array_type_spec()
        else:
            raise SemanticError(
                error_code=ErrorCode.SEMANTIC_UNKNOWN_TYPE,
                token=self.current_token,
                message=f"{ErrorCode.SEMANTIC_UNKNOWN_TYPE.value} -> {self.current_token}",
            )

    def primitive_type_spec(self) -> Type:
        """
        primitive_type_spec : INTEGER | REAL | BOOLEAN
        """
        token = self.current_token
        if self.current_token.type == TokenType.INTEGER:
            self.eat(TokenType.INTEGER)
        elif self.current_token.type == TokenType.REAL:
            self.eat(TokenType.REAL)
        elif self.current_token.type == TokenType.BOOLEAN:
            self.eat(TokenType.BOOLEAN)
        node = PrimitiveType(token)
        return node

    def string_type_spec(self) -> StringType:
        """
        string_type_spec: STRING ( LBRACKET INTEGER_CONST RBRACKET )?
        """
        token = self.current_token
        if self.current_token.type == TokenType.STRING:
            self.eat(TokenType.STRING)
            if self.current_token.type == TokenType.LBRACKET:
                self.eat(TokenType.LBRACKET)
                limit = self.factor()
                self.eat(TokenType.RBRACKET)
                return StringType(token=token, limit=limit)
            else:
                return StringType(token=token)
        else:
            raise ParserError(
                error_code=ErrorCode.PARSER_UNEXPECTED_TOKEN,
                token=self.current_token,
                message=f"{ErrorCode.PARSER_UNEXPECTED_TOKEN.value} -> {self.current_token}",
            )

    def array_type_spec(self) -> ArrayType:
        """array_type_spec : ARRAY (LBRACKET INTEGER_CONST RANGE INTEGER_CONST RBRACKET)? of type_spec"""
        token = self.current_token
        self.eat(TokenType.ARRAY)
        lower: Expression = self.newZeroNum(
            lineno=self.current_token.lineno, column=self.current_token.column
        )
        upper: Expression = self.newZeroNum(
            lineno=self.current_token.lineno, column=self.current_token.column
        )
        dynamic: bool = True
        if self.current_token.type == TokenType.LBRACKET:
            self.eat(TokenType.LBRACKET)
            lower = self.factor()
            self.eat(TokenType.RANGE)
            upper = self.factor()
            self.eat(TokenType.RBRACKET)
            dynamic = False
        self.eat(TokenType.OF)
        element_type = self.type_spec()
        node = ArrayType(
            token=token,
            element_type=element_type,
            lower=lower,
            upper=upper,
            dynamic=dynamic,
        )
        return node

    def compound_statement(self) -> Compound:
        """
        compound_statement: BEGIN statement_list END
        """
        self.eat(TokenType.BEGIN)
        nodes = self.statement_list()
        self.eat(TokenType.END)

        root = Compound()
        for node in nodes:
            root.children.append(node)

        return root

    def statement_list(self) -> list[Statement]:
        """
        statement_list : statement
                       | statement SEMI statement_list
        """
        node = self.statement()

        results = [node]

        while self.current_token.type == TokenType.SEMI:
            self.eat(TokenType.SEMI)
            results.append(self.statement())

        return results

    # it should deal with func_call_expr
    def statement(self) -> Statement:
        """
        statement : compound_statement
                  | proccall_statement
                  | assignment_statement
                  | if_statement
                  | empty
        """
        node: Statement
        if self.current_token.type == TokenType.BEGIN:
            node = self.compound_statement()
        elif self.current_token.type == TokenType.IF:
            node = self.if_statement()
        elif self.current_token.type == TokenType.WHILE:
            node = self.while_statement()
        elif self.current_token.type == TokenType.FOR:
            node = self.for_statement()
        elif self.current_token.type == TokenType.ID and self.lexer.current_char == "(":
            node = self.proccall_statement()
        elif self.current_token.type == TokenType.ID:
            node = self.assignment_statement()
        else:
            node = self.empty()
        return node

    def if_statement(self) -> IfStatement:
        """
        if_statement: IF logic_expr THEN (statement | compound_statement)
                    (ELSE IF logic_expr THEN (statement | compound_statement))*
                    (ELSE (statement | compound_statement))? SEMI
        """
        self.eat(TokenType.IF)
        condition = self.logic_expr()
        self.eat(TokenType.THEN)
        then_branch: AST
        if self.current_token.type == TokenType.BEGIN:
            then_branch = self.compound_statement()
        else:
            then_branch = self.statement()

        else_if_branches: list[IfStatement] = []
        else_branch: AST | None = None
        while self.current_token.type == TokenType.ELSE:
            self.eat(TokenType.ELSE)
            sub_then_branch: AST
            if self.current_token.type == TokenType.IF:
                self.eat(TokenType.IF)
                sub_condition = self.logic_expr()
                self.eat(TokenType.THEN)
                if self.current_token.type == TokenType.BEGIN:
                    sub_then_branch = self.compound_statement()
                else:
                    sub_then_branch = self.statement()
                sub_node = IfStatement(sub_condition, sub_then_branch, [], None)
                else_if_branches.append(sub_node)
            else:
                if self.current_token.type == TokenType.BEGIN:
                    else_branch = self.compound_statement()
                else:
                    else_branch = self.statement()
                break

        node = IfStatement(
            condition,
            then_branch,
            else_if_branches,
            else_branch,
        )
        return node

    def while_statement(self) -> WhileStatement:
        """while_statement:  WHILE logic_expr DO compound_statement SEMI"""
        self.eat(TokenType.WHILE)
        condition = self.logic_expr()
        self.eat(TokenType.DO)
        block = self.compound_statement()
        self.eat(TokenType.SEMI)
        node = WhileStatement(condition, block)
        return node

    def for_statement(self) -> ForStatement:
        """for_statement:  FOR assignment_statement TO summation_expr DO (statement | compound_statement) SEMI"""
        self.eat(TokenType.FOR)
        initialization = self.assignment_statement()
        self.eat(TokenType.TO)
        bound = self.summation_expr()
        self.eat(TokenType.DO)
        block: AST
        if self.current_token.type == TokenType.BEGIN:
            block = self.compound_statement()
        else:
            block = self.statement()
        node = ForStatement(initialization, bound, block)
        return node

    def proccall_statement(self) -> ProcedureCall:
        """proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN"""
        token = self.current_token

        proc_name = self.current_token.value
        self.eat(TokenType.ID)
        self.eat(TokenType.LPAREN)
        actual_params: list[Expression] = []
        if self.current_token.type != TokenType.RPAREN:
            expr = self.expr()
            actual_params.append(expr)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            expr = self.expr()
            actual_params.append(expr)

        self.eat(TokenType.RPAREN)

        node = ProcedureCall(
            proc_name=proc_name,
            actual_params=actual_params,
            token=token,
        )
        return node

    def func_call_expr(self) -> FunctionCall:
        """func_call_expr : ID LPAREN (expr (COMMA expr)*)? RPAREN"""
        token = self.current_token

        fun_name = self.current_token.value
        self.eat(TokenType.ID)
        self.eat(TokenType.LPAREN)
        actual_params: list[Expression] = []

        actual_params.append(
            Var(Token(TokenType.ID, fun_name, token.lineno, token.column))
        )

        if self.current_token.type != TokenType.RPAREN:
            expr = self.summation_expr()
            actual_params.append(expr)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            expr = self.summation_expr()
            actual_params.append(expr)

        self.eat(TokenType.RPAREN)

        node = FunctionCall(
            func_name=fun_name,
            actual_params=actual_params,
            token=token,
        )
        return node

    def assignment_statement(self) -> Assign:
        """
        assignment_statement : variable ASSIGN expr
        """
        left = self.variable()
        if not isinstance(left, Var):
            self.error(ErrorCode.UNEXPECTED_TOKEN, left.token)
        token = self.current_token
        self.eat(TokenType.ASSIGN)
        right = self.expr()
        assert isinstance(left, Var)
        node = Assign(left, token, right)
        return node

    def variable(self) -> Var:
        """
        variable: ID (LBRACKET summation_expr RBRACKET)?
        """
        node = Var(self.current_token)
        self.eat(TokenType.ID)
        if self.current_token.type == TokenType.LBRACKET:
            self.eat(TokenType.LBRACKET)
            index = self.summation_expr()
            self.eat(TokenType.RBRACKET)
            return IndexVar(token=node.token, index=index)
        return node

    def empty(self) -> NoOp:
        """An empty production"""
        return NoOp()

    def expr(self) -> Expression:
        """
        expr : logic_expr
        """
        return self.logic_expr()

    def logic_expr(self) -> Expression:
        """logic_expr : comparison_expr ((and | or ) comparison_expr)*"""
        node = self.comparison_expr()

        while self.current_token.type in (TokenType.AND, TokenType.OR):
            token = self.current_token
            if token.type == TokenType.AND:
                self.eat(TokenType.AND)
            elif token.type == TokenType.OR:
                self.eat(TokenType.OR)

            node = BinOp(left=node, op=token, right=self.comparison_expr())

        return node

    def comparison_expr(self) -> Expression:
        """comparison_expr : summation_expr ( (EQ | NE | GT | GE | LT | LE) summation_expr )*"""
        node = self.summation_expr()
        while self.current_token.type in (
            TokenType.EQ,
            TokenType.NE,
            TokenType.GT,
            TokenType.GE,
            TokenType.LT,
            TokenType.LE,
        ):
            token = self.current_token
            if token.type == TokenType.EQ:
                self.eat(TokenType.EQ)
            elif token.type == TokenType.NE:
                self.eat(TokenType.NE)
            elif token.type == TokenType.GT:
                self.eat(TokenType.GT)
            elif token.type == TokenType.GE:
                self.eat(TokenType.GE)
            elif token.type == TokenType.LT:
                self.eat(TokenType.LT)
            elif token.type == TokenType.LE:
                self.eat(TokenType.LE)

            node = BinOp(left=node, op=token, right=self.summation_expr())

        return node

    def summation_expr(self) -> Expression:
        """
        summation_expr : multiplication_expr ((PLUS | MINUS) multiplication_expr)*
        """
        node = self.multiplication_expr()

        while self.current_token.type in (TokenType.PLUS, TokenType.MINUS):
            token = self.current_token
            if token.type == TokenType.PLUS:
                self.eat(TokenType.PLUS)
            elif token.type == TokenType.MINUS:
                self.eat(TokenType.MINUS)

            node = BinOp(left=node, op=token, right=self.multiplication_expr())

        return node

    def multiplication_expr(self) -> Expression:
        """multiplication_expr : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*"""
        node = self.factor()

        while self.current_token.type in (
            TokenType.MUL,
            TokenType.INTEGER_DIV,
            TokenType.FLOAT_DIV,
        ):
            token = self.current_token
            if token.type == TokenType.MUL:
                self.eat(TokenType.MUL)
            elif token.type == TokenType.INTEGER_DIV:
                self.eat(TokenType.INTEGER_DIV)
            elif token.type == TokenType.FLOAT_DIV:
                self.eat(TokenType.FLOAT_DIV)

            node = BinOp(left=node, op=token, right=self.factor())

        return node

    def factor(self) -> Expression:
        """
        factor : not comparison_expr
               | PLUS factor
               | MINUS factor
               | INTEGER_CONST
               | STRING_CONST
               | REAL_CONST
               | TRUE_CONST
               | FALSE_CONST
               | LPAREN expr RPAREN
               | func_call_expr
               | variable
        """
        token = self.current_token
        node: Expression
        # logic OP
        if token.type == TokenType.NOT:
            self.eat(TokenType.NOT)
            node = UnaryOp(token, self.comparison_expr())
            return node

        # arithmetic OP
        if token.type == TokenType.PLUS:
            self.eat(TokenType.PLUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == TokenType.MINUS:
            self.eat(TokenType.MINUS)
            node = UnaryOp(token, self.factor())
            return node

        # value
        if token.type == TokenType.INTEGER_CONST:
            self.eat(TokenType.INTEGER_CONST)
            return Num(token)
        elif token.type == TokenType.REAL_CONST:
            self.eat(TokenType.REAL_CONST)
            return Num(token)
        elif token.type == TokenType.TRUE:
            self.eat(TokenType.TRUE)
            return Bool(token)
        elif token.type == TokenType.FALSE:
            self.eat(TokenType.FALSE)
            return Bool(token)

        # parent take precedence
        if token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            node = self.summation_expr()
            self.eat(TokenType.RPAREN)
            return node

        # parse string expr
        if self.current_token.type == TokenType.STRING_CONST:
            token = self.current_token
            self.eat(TokenType.STRING_CONST)
            return String(token=token)

        # call
        if (
            token.type == TokenType.ID
            and self.peek_next_token().type == TokenType.LPAREN
        ):
            node = self.func_call_expr()
            return node
        else:
            node = self.variable()
            return node

    def parse(self):
        """
        program : PROGRAM variable SEMI block DOT

        block : declarations compound_statement

        declarations : (VAR (variable_declaration SEMI)+)? procedure_declaration* function_declaration*

        variable_declaration : ID (COMMA ID)* COLON type_spec

        procedure_declaration :
             PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI

        function_declaration :
             FUNCTION ID LPAREN (formal_parameter_list)? RPAREN COLON type_spec SEMI block SEMI

        formal_params_list : formal_parameters
                           | formal_parameters SEMI formal_parameter_list

        formal_parameters : ID (COMMA ID)* COLON type_spec

        type_spec : primitive_type_spec | string_type_spec | array_type_spec

        primitive_type_spec : INTEGER | REAL | BOOLEAN

        string_type_spec: STRING ( LBRACKET INTEGER_CONST RBRACKET )?

        array_type_spec : ARRAY ( LBRACKET INTEGER_CONST RANGE INTEGER_CONST RBRACKET )? of type_spec

        compound_statement : BEGIN statement_list END

        statement_list : statement
                       | statement SEMI statement_list

        statement : compound_statement
                  | proccall_statement
                  | assignment_statement
                  | if_statement
                  | for_statement
                  | while_statement
                  | empty

        if_statement: IF logic_expr THEN (statement | compound_statement)
                    (ELSE IF logic_expr THEN (statement | compound_statement))*
                    (ELSE (statement | compound_statement))? SEMI

        while_statement:  WHILE logic_expr DO compound_statement SEMI

        for_statement:  FOR assignment_statement TO summation_expr DO (statement | compound_statement) SEMI

        proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN

        func_call_expr : ID LPAREN (expr (COMMA expr)*)? RPAREN

        assignment_statement : variable ASSIGN expr

        empty :

        expr : string_expr | logic_expr

        logic_expr : comparison_expr ((and | or ) comparison_expr)*

        comparison_expr : summation_expr ( (EQ | NE | GT | GE | LT | LE) summation_expr )*

        summation_expr : multiplication_expr ((PLUS | MINUS) multiplication_expr)*

        multiplication_expr : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*

        factor : not comparison_expr
               | PLUS factor
               | MINUS factor
               | INTEGER_CONST
               | STRING_CONST
               | REAL_CONST
               | TRUE_CONST
               | FALSE_CONST
               | LPAREN expr RPAREN
               | func_call_expr
               | variable

        variable: ID (LBRACKET summation_expr RBRACKET)?
        """
        node = self.program()
        if self.current_token.type != TokenType.EOF:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )

        return node


###############################################################################
#                                                                             #
#  AST visitors (walkers)                                                     #
#                                                                             #
###############################################################################


class NodeVisitor:
    def visit(self, node: AST):
        method_name = "visit_" + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node: AST):
        raise Exception("No visit_{} method".format(type(node).__name__))


###############################################################################
#                                                                             #
#  SYMBOLS, TABLES, SEMANTIC ANALYSIS                                         #
#                                                                             #
###############################################################################


class NativeMethod(Enum):
    WRITE = "WRITE"
    WRITELN = "WRITELN"
    LENGTH = "LENGTH"
    SETLENGTH = "SETLENGTH"


class Symbol:
    def __init__(self, name: str, type: Symbol | None = None) -> None:
        self.name = name
        self.type = type
        self.scope_level: int = 0


class VarSymbol(Symbol):
    def __init__(self, name: str, type: Symbol | None) -> None:
        super().__init__(name, type)

    def __str__(self) -> str:
        return "<{class_name}(name='{name}', type='{type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
        )

    __repr__ = __str__


class BuiltinTypeSymbol(Symbol):
    def __init__(self, name: str) -> None:
        super().__init__(name)

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return "<{class_name}(name='{name}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
        )


class StringTypeSymbol(Symbol):
    def __init__(self, name: str, limit: int = 255) -> None:
        super().__init__(name)
        self.limit = limit

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return "<{class_name}(name='{name}', limit='{limit}')>".format(
            class_name=self.__class__.__name__, name=self.name, limit=self.limit
        )


class ArrayTypeSymbol(Symbol):
    def __init__(self, name: str, element_type: Symbol) -> None:
        super().__init__(name)
        self.element_type = element_type

    def __str__(self) -> str:
        return "{name}[]".format(name=self.name)

    def __repr__(self) -> str:
        return "<{class_name}[{element_type_name}](name='{name}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            element_type_name=self.element_type.name,
        )


class ProcedureSymbol(Symbol):
    def __init__(self, name: str, formal_params: list[Symbol] | None = None) -> None:
        super().__init__(name)
        # a list of VarSymbol objects
        self.formal_params: list[Symbol] = (
            [] if formal_params is None else formal_params
        )
        # a reference to procedure's body (AST sub-tree)
        self.block_ast: Block | None = None

    def __str__(self) -> str:
        return "<{class_name}(name={name}, parameters={params})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.formal_params,
        )

    __repr__ = __str__


class BuiltinProcedureSymbol(Symbol):
    def __init__(self, name: str, output_params: list[Symbol] | None = None) -> None:
        super().__init__(name)
        # a list of VarSymbol objects
        self.output_params: list[Symbol] = (
            [] if output_params is None else output_params
        )

    def __str__(self) -> str:
        return "<{class_name}(name={name}, parameters={params})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.output_params,
        )

    __repr__ = __str__


class FunctionSymbol(Symbol):
    def __init__(
        self, name: str, return_type: Type, formal_params: list[Symbol] | None = None
    ) -> None:
        super().__init__(name)
        # a list of VarSymbol objects
        self.formal_params: list[Symbol] = (
            [] if formal_params is None else formal_params
        )
        self.return_type = return_type
        # a reference to procedure's body (AST sub-tree)
        self.block_ast: Block | None = None

    def __str__(self) -> str:
        return "<{class_name}(name={name},return_type={return_type} parameters={params})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            return_type=self.return_type,
            params=self.formal_params,
        )

    __repr__ = __str__


class BuiltinFunctionSymbol(Symbol):
    def __init__(
        self, name: str, return_type: Type, formal_params: list[Symbol] | None = None
    ) -> None:
        super().__init__(name)
        # a list of VarSymbol objects
        self.formal_params: list[Symbol] = (
            [] if formal_params is None else formal_params
        )
        self.return_type = return_type
        # a reference to procedure's body (AST sub-tree)
        self.block_ast: Block | None = None

    def __str__(self) -> str:
        return "<{class_name}(name={name},return_type={return_type} parameters={params})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            return_type=self.return_type,
            params=self.formal_params,
        )

    __repr__ = __str__


class ScopedSymbolTable:
    def __init__(
        self,
        scope_name: str,
        scope_level: int,
        enclosing_scope: "ScopedSymbolTable" | None,
    ) -> None:
        self._symbols: dict[str, Symbol] = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope

    def _init_builtins(self) -> None:
        self.insert(BuiltinTypeSymbol("INTEGER"))
        self.insert(BuiltinTypeSymbol("REAL"))
        self.insert(BuiltinTypeSymbol("BOOLEAN"))
        self.insert(BuiltinTypeSymbol("STRING"))
        self.insert(
            BuiltinProcedureSymbol(name=NativeMethod.WRITE.name, output_params=[])
        )
        self.insert(
            BuiltinProcedureSymbol(name=NativeMethod.WRITELN.name, output_params=[])
        )
        self.insert(
            BuiltinProcedureSymbol(name=NativeMethod.SETLENGTH.name, output_params=[])
        )
        self.insert(
            BuiltinFunctionSymbol(
                name=NativeMethod.LENGTH.name,
                return_type=Type(
                    token=Token(type=TokenType.INTEGER, value=0, lineno=-1, column=-1)
                ),
                formal_params=[],
            )
        )

    def __str__(self) -> str:
        h1 = "SCOPE (SCOPED SYMBOL TABLE)"
        lines = ["\n", h1, "=" * len(h1)]
        for header_name, header_value in (
            ("Scope name", self.scope_name),
            ("Scope level", self.scope_level),
            (
                "Enclosing scope",
                self.enclosing_scope.scope_name if self.enclosing_scope else None,
            ),
        ):
            lines.append(f"{header_name:<15}: {header_value}")
        h2 = "Scope (Scoped symbol table) contents"
        lines.extend([h2, "-" * len(h2)])
        lines.extend(f"{key:>7}: {value}" for key, value in self._symbols.items())
        lines.append("\n")
        s = "\n".join(lines)
        return s

    __repr__ = __str__

    def log(self, msg: str) -> None:
        if _SHOULD_LOG_SCOPE:
            print(msg)

    def insert(self, symbol: Symbol) -> None:
        self.log(f"Insert: {symbol.name}")
        symbol.scope_level = self.scope_level
        self._symbols[symbol.name] = symbol

    def lookup(self, name: str, current_scope_only: bool = False) -> Symbol | None:
        self.log(f"Lookup: {name}. (Scope name: {self.scope_name})")
        # 'symbol' is either an instance of the Symbol class or None
        symbol = self._symbols.get(name)

        if symbol is not None:
            return symbol

        #  variables , identifiers,  function and procedure names in Pascal are not case-sensitive
        symbol = self._symbols.get(name.upper())
        if symbol is not None:
            return symbol

        if current_scope_only:
            return None

        # recursively go up the chain and lookup the name
        if self.enclosing_scope is not None:
            return self.enclosing_scope.lookup(name)
        return None


class SemanticAnalyzer(NodeVisitor):
    __string_type_limit: int = 255

    def __init__(self) -> None:
        self.current_scope: ScopedSymbolTable | None = None
        self.unmodified_vars: list[str] = []

    def log(self, msg) -> None:
        if _SHOULD_LOG_SCOPE:
            print(msg)

    @staticmethod
    def string_type_name(size: int):
        return "STRING[{size}]".format(size=size)

    def error(self, error_code: ErrorCode, token: Token):
        raise SemanticError(
            error_code=error_code,
            token=token,
            message=f"{error_code.value} -> {token}",
        )

    def visit_Block(self, node: Block) -> None:
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_Program(self, node: Program) -> None:
        self.log("ENTER scope: global")
        global_scope = ScopedSymbolTable(
            scope_name="global",
            scope_level=1,
            enclosing_scope=self.current_scope,  # None
        )
        global_scope._init_builtins()
        self.current_scope = global_scope

        # visit subtree
        self.visit(node.block)

        self.log(global_scope)

        self.current_scope = self.current_scope.enclosing_scope
        self.log("LEAVE scope: global")

    def visit_Compound(self, node: Compound) -> None:
        for child in node.children:
            self.visit(child)

    def visit_NoOp(self, node: NoOp) -> None:
        pass

    def visit_WhileStatement(self, node: WhileStatement) -> None:
        self.visit(node.condition)
        self.visit(node.block)

    def visit_ForStatement(self, node: ForStatement) -> None:
        self.visit(node.initialization)
        self.visit(node.bound)
        var_name = node.initialization.left.value
        self.unmodified_vars.append(var_name)
        self.visit(node.block)
        self.unmodified_vars.remove(var_name)

    def visit_IfStatement(self, node: IfStatement) -> None:
        self.visit(node.condition)
        self.visit(node.then_branch)
        for branch in node.else_if_branches:
            self.visit(branch)
        if node.else_branch is not None:
            self.visit(node.else_branch)

    def visit_BinOp(self, node: BinOp) -> None:
        self.visit(node.left)
        self.visit(node.right)

    def visit_Type(self, node: Type):
        pass

    def visit_PrimitiveType(self, node: PrimitiveType):
        pass

    def visit_StringType(self, node: StringType):
        if isinstance(node.limit, Num):
            limit = int(node.limit.value)
            self.__string_type_limit = limit
            if self.current_scope is None:
                raise SemanticError(
                    error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                    token=node.token,
                    message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value} -> {node.token}",
                )
            self.current_scope.insert(
                StringTypeSymbol(
                    name=SemanticAnalyzer.string_type_name(limit), limit=int(limit)
                )
            )
        pass

    def visit_ArrayType(self, node: ArrayType) -> None:
        if isinstance(node.element_type, ArrayType):
            self.visit_ArrayType(node.element_type)
        if self.current_scope is None:
            raise SemanticError(
                error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                token=node.token,
                message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value} -> {node.token}",
            )
        element_type_symbol = self.current_scope.lookup(str(node.element_type))
        if element_type_symbol is None:
            raise SemanticError(
                error_code=ErrorCode.SEMANTIC_UNKNOWN_ARRAY_ELEMENT_TYPE,
                token=node.token,
                message=f"{ErrorCode.SEMANTIC_UNKNOWN_ARRAY_ELEMENT_TYPE.value} -> {node.token}",
            )
        type_name = str(node)
        type_symbol = self.current_scope.lookup(type_name)
        if type_symbol is None:
            self.current_scope.insert(
                ArrayTypeSymbol(name=type_name, element_type=element_type_symbol)
            )

    def visit_VarDecl(self, node: VarDecl) -> None:
        type_name = node.type_node.value
        if isinstance(node.type_node, ArrayType):
            self.visit(node.type_node)
            type_name = str(node.type_node)
        elif isinstance(node.type_node, StringType):
            self.visit(node.type_node)
            type_name = SemanticAnalyzer.string_type_name(size=self.__string_type_limit)
        if self.current_scope is None:
            raise SemanticError(
                error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                token=node.var_node.token,
                message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value} -> {node.var_node.token}",
            )
        type_symbol = self.current_scope.lookup(type_name)

        # We have all the information we need to create a variable symbol.
        # Create the symbol and insert it into the symbol table.
        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)

        # Signal an error if the table already has a symbol
        # with the same name
        if self.current_scope.lookup(var_name, current_scope_only=True):
            self.error(
                error_code=ErrorCode.DUPLICATE_ID,
                token=node.var_node.token,
            )

        self.current_scope.insert(var_symbol)

    def visit_Assign(self, node: Assign) -> None:
        # right-hand side
        self.visit(node.right)
        # left-hand side
        if node.left.value in self.unmodified_vars:
            self.error(ErrorCode.MODIFY_LOOP_VAR_NOT_ALLOW, token=node.left.token)
        self.visit(node.left)
        if self.current_scope is None:
            raise SemanticError(
                error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                token=node.left.token,
                message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value} -> {node.left.token}",
            )
        var_symbol = self.current_scope.lookup(node.left.value)
        if var_symbol is None:
            raise SemanticError(
                error_code=ErrorCode.SEMANTIC_UNKNOWN_SYMBOL,
                token=node.left.token,
                message=f"{ErrorCode.SEMANTIC_UNKNOWN_SYMBOL.value} -> {node.left.token}",
            )
        if var_symbol.type is None:
            raise SemanticError(
                error_code=ErrorCode.SEMANTIC_UNKNOWN_SYMBOL,
                token=node.left.token,
                message=f"{ErrorCode.SEMANTIC_UNKNOWN_SYMBOL.value} -> {node.left.token}",
            )
        if isinstance(var_symbol.type, StringTypeSymbol):
            # string_size = var_symbol.type.limit
            # if isinstance(node.right, String):
            #     string_value = node.right.value
            #     if len(string_value) > string_size:
            #         message = f"Warning: String literal has more characters[{len(string_value)}] than short string length[{string_size}]"
            #         SpiUtil.print_w(message=message)
            pass

    def visit_Var(self, node: Var) -> None:
        var_name = node.value
        if self.current_scope is None:
            self.error(error_code=ErrorCode.NULL_POINTER, token=node.token)
            return

        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)
            return

    def visit_IndexVar(self, node: IndexVar) -> None:
        var_name = node.value
        if self.current_scope is None:
            self.error(error_code=ErrorCode.NULL_POINTER, token=node.token)
            return

        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)
            return
        self.visit(node.index)

    def visit_Num(self, node: Num) -> None:
        pass

    def visit_Bool(self, node: Bool) -> None:
        pass

    def visit_String(self, node: String) -> None:
        pass

    def visit_UnaryOp(self, node: UnaryOp) -> None:
        pass

    def visit_ProcedureDecl(self, node: ProcedureDecl) -> None:
        proc_name = node.proc_name
        proc_symbol = ProcedureSymbol(proc_name)
        if self.current_scope is None:
            raise SemanticError(
                error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                token=None,
                message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value}",
            )
        self.current_scope.insert(proc_symbol)

        self.log(f"ENTER scope: {proc_name}")
        # Scope for parameters and local variables
        procedure_scope = ScopedSymbolTable(
            scope_name=proc_name,
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope,
        )
        self.current_scope = procedure_scope

        # Insert parameters into the procedure scope
        for param in node.formal_params:
            param_type = self.current_scope.lookup(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            self.current_scope.insert(var_symbol)
            proc_symbol.formal_params.append(var_symbol)

        self.visit(node.block_node)

        self.log(procedure_scope)

        self.current_scope = self.current_scope.enclosing_scope
        self.log(f"LEAVE scope: {proc_name}")

        # accessed by the interpreter when executing procedure call
        proc_symbol.block_ast = node.block_node

    def visit_ProcedureCall(self, node: ProcedureCall) -> None:
        for param_node in node.actual_params:
            self.visit(param_node)

        if self.current_scope is None:
            self.error(error_code=ErrorCode.CURRENT_SCOPE_NOT_FOUND, token=node.token)
            return
        proc_symbol = self.current_scope.lookup(node.proc_name)
        # accessed by the interpreter when executing procedure call
        node.proc_symbol = cast(ProcedureSymbol, proc_symbol)

    def visit_FunctionDecl(self, node: FunctionDecl) -> None:
        func_name = node.func_name
        return_type = node.return_type
        func_symbol = FunctionSymbol(func_name, return_type)
        if self.current_scope is None:
            raise SemanticError(
                error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                token=None,
                message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value}",
            )
        self.current_scope.insert(func_symbol)

        self.log(f"ENTER scope: {func_name}")
        # Scope for parameters and local variables
        function_scope = ScopedSymbolTable(
            scope_name=func_name,
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope,
        )
        self.current_scope = function_scope

        # insert return value into the function scope
        # pascal support implicit return the value has the same name to the function name
        return_var_symbol = VarSymbol(
            func_name, self.current_scope.lookup(return_type.value)
        )
        self.current_scope.insert(return_var_symbol)
        func_symbol.formal_params.append(return_var_symbol)

        # Insert parameters into the function scope
        for param in node.formal_params:
            param_type = self.current_scope.lookup(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            self.current_scope.insert(var_symbol)
            func_symbol.formal_params.append(var_symbol)

        self.visit(node.block_node)

        self.log(function_scope)

        self.current_scope = self.current_scope.enclosing_scope
        self.log(f"LEAVE scope: {func_name}")

        # accessed by the interpreter when executing procedure call
        func_symbol.block_ast = node.block_node

    def visit_FunctionCall(self, node: FunctionCall) -> None:
        for param_node in node.actual_params:
            self.visit(param_node)

        if self.current_scope is None:
            self.error(error_code=ErrorCode.CURRENT_SCOPE_NOT_FOUND, token=node.token)
            return
        func_symbol = self.current_scope.lookup(node.func_name)
        # accessed by the interpreter when executing procedure call
        node.func_symbol = cast(FunctionSymbol, func_symbol)


###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################

# Built-in procedures and functions registry
BUILTIN_PROCEDURES: dict[str, Callable[..., None]] = {}
BUILTIN_FUNCTIONS: dict[str, Callable[..., Object]] = {}


def register_builtin_procedure(name, handler):
    """Register a built-in procedure handler"""
    BUILTIN_PROCEDURES[name.upper()] = handler


def register_builtin_function(name, handler):
    """Register a built-in function handler"""
    BUILTIN_FUNCTIONS[name.upper()] = handler


# Built-in procedure handlers
def handle_write(interpreter, node):
    """Handle WRITE built-in procedure"""
    proc_name = node.proc_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))

    # output actual params
    for argument_node in actual_params:
        obj = interpreter.visit(argument_node)
        print(obj.value if hasattr(obj, "value") else obj, end=" ")

    interpreter.log(f"LEAVE: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))


def handle_writeln(interpreter, node):
    """Handle WRITELN built-in procedure"""
    proc_name = node.proc_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))

    # output actual params
    for argument_node in actual_params:
        obj = interpreter.visit(argument_node)
        print(obj.value if hasattr(obj, "value") else obj)

    interpreter.log(f"LEAVE: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))


def handle_setlength(interpreter, node):
    """Handle SETLENGTH built-in procedure"""
    proc_name = node.proc_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))

    # core
    arr_name = actual_params[0].value
    new_length_obj = interpreter.visit(actual_params[1])
    new_length = new_length_obj.value if isinstance(new_length_obj, NumberObject) else 0

    pre_ar = (
        interpreter.call_stack._records[-2]
        if len(interpreter.call_stack._records) >= 2
        else ar
    )
    var_obj = pre_ar.get(arr_name)

    if isinstance(var_obj, StringObject):
        var_obj.set_length(new_length)
    elif isinstance(var_obj, ArrayObject):
        var_obj.set_length(new_length)

    interpreter.log(f"LEAVE: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))


# Built-in function handlers
def handle_length(interpreter, node):
    """Handle LENGTH built-in function"""
    func_name = node.func_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    # Get the array/string object and return its length
    param_obj = interpreter.visit(actual_params[1])  # Skip the function name param
    if isinstance(param_obj, (ArrayObject, StringObject)):
        length_value = len(param_obj)
    else:
        length_value = 0

    result = IntegerObject(length_value)
    ar[RETURN_NUM_FOR_LENGTH] = result

    interpreter.log(f"LEAVE: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    return result


class ARType(Enum):
    PROGRAM = "PROGRAM"
    PROCEDURE = "PROCEDURE"
    FUNCTION = "FUNCTION"


class CallStack:
    def __init__(self) -> None:
        self._records: list[ActivationRecord] = []

    def push(self, ar: ActivationRecord) -> None:
        self._records.append(ar)

    def pop(self) -> ActivationRecord:
        if len(self._records) >= 2:
            self._records[-2].copy_from(self._records[-1], True)
        return self._records.pop()

    def peek(self) -> ActivationRecord:
        return self._records[-1]

    def __str__(self) -> str:
        s = "\n".join(repr(ar) for ar in reversed(self._records))
        s = f"CALL STACK\n{s}\n\n"
        return s

    def __repr__(self) -> str:
        return self.__str__()


class ActivationRecord:
    def __init__(self, name: str, type: ARType, nesting_level: int) -> None:
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.members: dict[str | int, Object] = {}

    def __setitem__(self, key: str | int, value: Object) -> None:
        self.members[key] = value

    def __getitem__(self, key: str | int) -> Object:
        return self.members[key]

    def copy_from(self, other: "ActivationRecord", override: bool):
        for name, val in other.members.items():
            if override or name not in self.members:
                self.members[name] = val

    def get(self, key: str) -> Object:
        return self.members.get(key)

    def __str__(self) -> str:
        lines = [
            "{level}: {type} {name}".format(
                level=self.nesting_level,
                type=self.type.value,
                name=self.name,
            )
        ]
        for name, val in self.members.items():
            lines.append(f"   {name:<20}: {val}")

        s = "\n".join(lines)
        return s

    def __repr__(self) -> str:
        return self.__str__()


class Interpreter(NodeVisitor):
    def __init__(self, tree: Program) -> None:
        self.tree = tree
        self.call_stack = CallStack()

        # Register built-in procedures and functions
        register_builtin_procedure(NativeMethod.WRITE.name, handle_write)
        register_builtin_procedure(NativeMethod.WRITELN.name, handle_writeln)
        register_builtin_procedure(NativeMethod.SETLENGTH.name, handle_setlength)
        register_builtin_function(NativeMethod.LENGTH.name, handle_length)

    def log(self, msg) -> None:
        if _SHOULD_LOG_STACK:
            print(msg)

    def visit_Program(self, node: Program) -> None:
        program_name = node.name
        self.log(f"ENTER: PROGRAM {program_name}")

        ar = ActivationRecord(
            name=program_name,
            type=ARType.PROGRAM,
            nesting_level=1,
        )
        self.call_stack.push(ar)

        self.log(str(self.call_stack))

        self.visit(node.block)

        self.log(f"LEAVE: PROGRAM {program_name}")
        self.log(str(self.call_stack))

        self.call_stack.pop()

    def visit_Block(self, node: Block) -> None:
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node: VarDecl) -> None:
        ar = self.call_stack.peek()
        if node.type_node.token.type == TokenType.BOOLEAN:
            ar[node.var_node.value] = BooleanObject(False)
        elif node.type_node.token.type == TokenType.INTEGER:
            ar[node.var_node.value] = IntegerObject(0)
        elif node.type_node.token.type == TokenType.REAL:
            ar[node.var_node.value] = RealObject(0.0)
        elif node.type_node.token.type == TokenType.STRING:
            string_node = cast(StringType, node.type_node)
            limit: int = 255
            if string_node.limit is not None:
                limit = self.visit(string_node.limit).value
            ar[node.var_node.value] = StringObject("", limit)
        elif node.type_node.token.type == TokenType.ARRAY:
            ar[node.var_node.value] = self.__initArray(node.type_node)
        pass

    def __initArray(self, node: Type) -> ArrayObject:
        if isinstance(node, ArrayType):
            lower_bound: int = self.visit(node.lower).value
            upper_bound: int = self.visit(node.upper).value
            if lower_bound > upper_bound:
                raise InterpreterError(
                    error_code=ErrorCode.INTERPRETER_ARRAY_RANGE_INVALID,
                    token=node.token,
                    message=f"{ErrorCode.INTERPRETER_ARRAY_RANGE_INVALID.value} -> {node.token}",
                )

            # Determine element type
            element_type = ElementType.INTEGER  # default
            if node.element_type.token.type == TokenType.BOOLEAN:
                element_type = ElementType.BOOL
            elif node.element_type.token.type == TokenType.INTEGER:
                element_type = ElementType.INTEGER
            elif node.element_type.token.type == TokenType.REAL:
                element_type = ElementType.REAL
            elif node.element_type.token.type == TokenType.STRING:
                element_type = ElementType.STRING
            elif node.element_type.token.type == TokenType.ARRAY:
                element_type = ElementType.ARRAY

            return ArrayObject(
                element_type=element_type,
                lower_bound=lower_bound,
                upper_bound=upper_bound,
                dynamic=node.dynamic,
            )

        raise SemanticError(
            error_code=ErrorCode.SEMANTIC_UNKNOWN_TYPE,
            token=node.token,
            message=f"{ErrorCode.SEMANTIC_UNKNOWN_TYPE.value} -> {node.token}",
        )

    def visit_Type(self, node: Type) -> None:
        # Do nothing
        pass

    def visit_StringType(self, node: StringType) -> None:
        # Do nothing
        pass

    def visit_PrimitiveType(self, node: PrimitiveType) -> None:
        # Do nothing
        pass

    def visit_ArrayType(self, node: ArrayType) -> None:
        # Do nothing
        pass

    def visit_BinOp(self, node: BinOp) -> Object:
        left_obj = self.visit(node.left)
        right_obj = self.visit(node.right)

        # logic operator
        if node.op.type == TokenType.AND:
            if isinstance(left_obj, BooleanObject) and isinstance(
                right_obj, BooleanObject
            ):
                return left_obj & right_obj
        elif node.op.type == TokenType.OR:
            if isinstance(left_obj, BooleanObject) and isinstance(
                right_obj, BooleanObject
            ):
                return left_obj | right_obj

        # arithmetic operator
        if node.op.type == TokenType.PLUS:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj + right_obj
            elif isinstance(left_obj, StringObject) and isinstance(
                right_obj, StringObject
            ):
                # For string concatenation, don't apply limits during intermediate operations
                return StringObject(left_obj.value + right_obj.value, -1)
            # Handle string + other types conversion
            elif isinstance(left_obj, StringObject):
                if hasattr(right_obj, "value"):
                    return StringObject(left_obj.value + str(right_obj.value), -1)
                else:
                    return StringObject(left_obj.value + str(right_obj), -1)
            elif isinstance(right_obj, StringObject):
                if hasattr(left_obj, "value"):
                    return StringObject(str(left_obj.value) + right_obj.value, -1)
                else:
                    return StringObject(str(left_obj) + right_obj.value, -1)
        elif node.op.type == TokenType.MINUS:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj - right_obj
        elif node.op.type == TokenType.MUL:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj * right_obj
        elif node.op.type == TokenType.INTEGER_DIV:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj // right_obj
        elif node.op.type == TokenType.FLOAT_DIV:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj / right_obj

        # comparison operator
        if node.op.type == TokenType.LT:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj < right_obj
        elif node.op.type == TokenType.GT:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj > right_obj
        elif node.op.type == TokenType.EQ:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj == right_obj
            elif isinstance(left_obj, BooleanObject) and isinstance(
                right_obj, BooleanObject
            ):
                return left_obj == right_obj
        elif node.op.type == TokenType.NE:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj != right_obj
            elif isinstance(left_obj, BooleanObject) and isinstance(
                right_obj, BooleanObject
            ):
                return left_obj != right_obj
        elif node.op.type == TokenType.LE:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj <= right_obj
        elif node.op.type == TokenType.GE:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj >= right_obj

        # !!
        raise InterpreterError(
            error_code=ErrorCode.INTERPRETER_UNKNOWN_OPERATOR,
            token=node.token,
            message=f"{ErrorCode.INTERPRETER_UNKNOWN_OPERATOR.value} -> {node.token}",
        )

    def visit_Num(self, node: Num) -> NumberObject:
        if isinstance(node.value, int):
            return IntegerObject(node.value)
        else:
            return RealObject(node.value)

    def visit_String(self, node: String) -> StringObject:
        return StringObject(node.value)

    def visit_Bool(self, node: Bool) -> BooleanObject:
        if node.token.type == TokenType.TRUE:
            return BooleanObject(True)
        elif node.token.type == TokenType.FALSE:
            return BooleanObject(False)
        raise InterpreterError(
            error_code=ErrorCode.INTERPRETER_UNKNOWN_BOOLEAN,
            token=node.token,
            message=f"{ErrorCode.INTERPRETER_UNKNOWN_BOOLEAN.value} -> {node.token}",
        )

    def visit_UnaryOp(self, node: UnaryOp) -> Object:
        expr_obj = self.visit(node.expr)
        op = node.op.type

        # negative bang
        if op == TokenType.NOT:
            if isinstance(expr_obj, BooleanObject):
                return ~expr_obj
            else:
                return BooleanObject(not expr_obj.to_bool())

        # signal bang
        if op == TokenType.PLUS:
            if isinstance(expr_obj, NumberObject):
                return +expr_obj
        elif op == TokenType.MINUS:
            if isinstance(expr_obj, NumberObject):
                return -expr_obj

        raise InterpreterError(
            error_code=ErrorCode.INTERPRETER_UNKNOWN_OPERATOR,
            token=node.token,
            message=f"{ErrorCode.INTERPRETER_UNKNOWN_OPERATOR.value} -> {node.token}",
        )

    def visit_Compound(self, node: Compound) -> None:
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node: Assign) -> None:
        var_name = node.left.value
        var_value = self.visit(node.right)
        ar = self.call_stack.peek()

        if isinstance(node.left, IndexVar):
            # array [index] = value
            index_obj = self.visit(node.left.index)
            index = index_obj.value if isinstance(index_obj, NumberObject) else 0
            array_obj = ar[var_name]
            if isinstance(array_obj, ArrayObject):
                array_obj[index] = var_value
        else:
            # identifier = value
            existing_var = ar.get(var_name)
            if isinstance(existing_var, StringObject) and isinstance(
                var_value, StringObject
            ):
                # Handle string assignment with limit checking
                if existing_var.limit > 0 and len(var_value.value) > existing_var.limit:
                    message = f"Warning: String literal has more characters[{len(var_value.value)}] than short string length[{existing_var.limit}]"
                    SpiUtil.print_w(message=message)
                    ar[var_name] = StringObject(
                        var_value.value[: existing_var.limit], existing_var.limit
                    )
                else:
                    ar[var_name] = StringObject(var_value.value, existing_var.limit)
            else:
                ar[var_name] = var_value

    def visit_Var(self, node: Var) -> Object:
        var_name = node.value
        ar = self.call_stack.peek()
        var_value = ar.get(var_name)
        return var_value if var_value is not None else Object()

    def visit_IndexVar(self, node: IndexVar) -> Object:
        var_name = node.value
        index_obj = self.visit(node.index)
        index = index_obj.value if isinstance(index_obj, NumberObject) else 0

        ar = self.call_stack.peek()
        var_obj = ar.get(var_name)

        if isinstance(var_obj, StringObject):
            return var_obj[index]
        elif isinstance(var_obj, ArrayObject):
            return var_obj[index]
        else:
            # Return default object for unknown types
            return Object()

    def visit_NoOp(self, node: NoOp) -> None:
        pass

    def visit_WhileStatement(self, node: WhileStatement) -> None:
        while self.visit(node.condition).to_bool():
            self.visit(node.block)

    def visit_ForStatement(self, node: ForStatement) -> None:
        ar = self.call_stack.peek()
        var_name = node.initialization.left.value
        self.visit(node.initialization)
        bound_obj = self.visit(node.bound)
        bound_value = bound_obj.value if isinstance(bound_obj, NumberObject) else 0

        var_obj = ar[var_name]
        var_value = var_obj.value if isinstance(var_obj, NumberObject) else 0

        while var_value <= bound_value:
            self.visit(node.block)
            var_value += 1
            if var_value <= bound_value:
                ar[var_name] = IntegerObject(var_value)

    def visit_ProcedureDecl(self, node: ProcedureDecl) -> None:
        pass

    def visit_ProcedureCall(self, node: ProcedureCall) -> None:
        proc_name = node.proc_name
        proc_symbol = node.proc_symbol

        if proc_symbol is None:
            raise InterpreterError(
                error_code=ErrorCode.NULL_POINTER,
                token=node.token,
                message=f"{ErrorCode.NULL_POINTER.value} -> {node.token}",
            )

        ar = ActivationRecord(
            name=proc_name,
            type=ARType.PROCEDURE,
            nesting_level=proc_symbol.scope_level + 1,
        )

        pre_ar = self.call_stack.peek()
        if pre_ar is not None:
            ar.copy_from(pre_ar, False)

        # deal with built-in procedure first
        if isinstance(proc_symbol, BuiltinProcedureSymbol):
            # Look up the built-in procedure in the registry
            handler = BUILTIN_PROCEDURES.get(proc_symbol.name.upper())
            if handler:
                # Prepare parameters
                actual_params = node.actual_params
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                # Call the handler
                handler(self, node)

                self.call_stack.pop()
                return
            else:
                raise InterpreterError(
                    error_code=ErrorCode.INTERPRETER_UNKNOWN_BUILTIN_PROCEDURE,
                    token=node.token,
                    message=f"{ErrorCode.INTERPRETER_UNKNOWN_BUILTIN_PROCEDURE.value} -> {node.token}",
                )
        else:
            formal_params = proc_symbol.formal_params
            actual_params = node.actual_params

            for param_symbol, argument_node in zip(formal_params, actual_params):
                ar[param_symbol.name] = self.visit(argument_node)

            self.call_stack.push(ar)

            self.log(f"ENTER: PROCEDURE {proc_name}")
            self.log(str(self.call_stack))

            # evaluate procedure body
            if proc_symbol.block_ast is None:
                raise InterpreterError(
                    error_code=ErrorCode.NULL_POINTER,
                    token=None,
                    message=f"{ErrorCode.NULL_POINTER.value}",
                )
            self.visit(proc_symbol.block_ast)

            self.log(f"LEAVE: PROCEDURE {proc_name}")
            self.log(str(self.call_stack))

            self.call_stack.pop()
            pass

    def visit_FunctionDecl(self, node: FunctionDecl) -> None:
        pass

    def visit_IfStatement(self, node: IfStatement) -> None:
        flag = self.visit(node.condition).to_bool()

        if flag:
            self.visit(node.then_branch)
            return
        else:
            for branch in node.else_if_branches:
                sub_flag = self.visit(branch.condition).to_bool()
                if sub_flag:
                    self.visit(branch.then_branch)
                    return

        if node.else_branch is not None:
            self.visit(node.else_branch)

    def visit_FunctionCall(self, node: FunctionCall) -> Object:
        func_name = node.func_name
        func_symbol = node.func_symbol

        if func_symbol is None:
            raise InterpreterError(
                error_code=ErrorCode.NULL_POINTER,
                token=None,
                message=f"{ErrorCode.NULL_POINTER.value}",
            )

        ar = ActivationRecord(
            name=func_name,
            type=ARType.FUNCTION,
            nesting_level=func_symbol.scope_level + 1,
        )

        pre_ar = self.call_stack.peek()
        if pre_ar is not None:
            ar.copy_from(pre_ar, False)

        # deal with built-in function first
        if isinstance(func_symbol, BuiltinFunctionSymbol):
            # Look up the built-in function in the registry
            handler = BUILTIN_FUNCTIONS.get(func_symbol.name.upper())
            if handler:
                # Prepare parameters
                actual_params = node.actual_params
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                # Call the handler and get the result
                result = handler(self, node)

                self.call_stack.pop()
                return result
            else:
                raise InterpreterError(
                    error_code=ErrorCode.INTERPRETER_UNKNOWN_BUILTIN_FUNCTION,
                    token=None,
                    message=f"{ErrorCode.INTERPRETER_UNKNOWN_BUILTIN_FUNCTION.value}",
                )
        else:
            formal_params = func_symbol.formal_params
            actual_params = node.actual_params

            for param_symbol, argument_node in zip(formal_params, actual_params):
                ar[param_symbol.name] = self.visit(argument_node)

            self.call_stack.push(ar)

            self.log(f"ENTER: FUNCTION {func_name}")
            self.log(str(self.call_stack))

            # evaluate procedure body
            if func_symbol.block_ast is None:
                raise InterpreterError(
                    error_code=ErrorCode.NULL_POINTER,
                    token=None,
                    message=f"{ErrorCode.NULL_POINTER.value}",
                )
            self.visit(func_symbol.block_ast)

            self.log(f"LEAVE: FUNCTION {func_name}")
            self.log(str(self.call_stack))

            result = ar.get(func_name)
            self.call_stack.pop()

            return result if result is not None else Object()

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ""
        return self.visit(tree)


def main() -> None:
    arg_parser = argparse.ArgumentParser(description="SPI - Simple Pascal Interpreter")
    arg_parser.add_argument("inputfile", help="Pascal source file")
    arg_parser.add_argument(
        "--scope",
        help="Print scope information",
        action="store_true",
    )
    arg_parser.add_argument(
        "--stack",
        help="Print call stack",
        action="store_true",
    )
    args = arg_parser.parse_args()

    global _SHOULD_LOG_SCOPE, _SHOULD_LOG_STACK
    _SHOULD_LOG_SCOPE = args.scope
    _SHOULD_LOG_STACK = args.stack

    text = open(args.inputfile, "r").read()

    lexer = Lexer(text)
    try:
        parser = Parser(lexer)
        tree = parser.parse()
    except (LexerError, ParserError) as e:
        print(e.message)
        sys.exit(1)

    semantic_analyzer = SemanticAnalyzer()
    try:
        semantic_analyzer.visit(tree)
    except SemanticError as e:
        print(e.message)
        sys.exit(1)

    interpreter = Interpreter(tree)
    interpreter.interpret()


if __name__ == "__main__":
    main()
