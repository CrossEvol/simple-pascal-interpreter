"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

import argparse
import re
import sys
from enum import Enum
from dataclasses import dataclass
from typing import Any, cast

_SHOULD_LOG_SCOPE = False  # see '--scope' command line option
_SHOULD_LOG_STACK = False  # see '--stack' command line option

RETURN_NUM = "RETURN_NUM"


class SpiUtil:
    @staticmethod
    def print_w(message: Any):
        print(f"\033[91m{message}\033[0m", file=sys.stderr)

    @staticmethod
    def getField(s: str, n):
        list = s.split(".")
        v: Any = n[list[0]]
        for item in list[1:]:
            v = v[item]
        return v

    @staticmethod
    def toClassName(class_name: str):
        return "Class[{class_name}]".format(class_name=class_name)

    @staticmethod
    def extraClassName(class_name: str):
        pattern = r"Class\[(\w+)\]"
        match = re.search(pattern, class_name)
        if match:
            raw_class_name = match.group(1)
            return raw_class_name
        else:
            SpiUtil.print_w(
                "Could not find pattern {pattern} in {str}".format(
                    pattern=pattern, str=class_name
                )
            )
            return class_name


class ElementType(Enum):
    INTEGER = "INTEGER"  # may be should prefix with ARRAY
    REAL = "REAL"
    BOOL = "BOOL"
    STRING = "STRING"
    ARRAY = "ARRAY"
    CLASS = "CLASS"
    INSTANCE = "INSTANCE"


class ErrorCode(Enum):
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


class Error(Exception):
    def __init__(self, error_code=None, token=None, message=None) -> None:
        self.error_code = error_code
        self.token = token
        # add exception class name before the message
        self.message = f"{self.__class__.__name__}: {message}"


###############################################################################
#                                                                             #
#  LexerError                                                                      #
#                                                                             #
###############################################################################


class LexerError(Error):
    pass


class LexerStringError(LexerError):
    pass


class FindNextTokenError(LexerError):
    pass


###############################################################################
#                                                                             #
#  ParserError                                                                      #
#                                                                             #
###############################################################################


class ParserError(Error):
    pass


class VarDuplicateInScopeError(ParserError):
    pass


###############################################################################
#                                                                             #
#  SemanticError                                                                      #
#                                                                             #
###############################################################################


class SemanticError(Error):
    pass


class DuplicateClassError(Error):
    pass


class UnknownTypeError(SemanticError):
    pass


class UnknownArrayElementTypeError(UnknownTypeError):
    pass


class UnknownClassTypeError(UnknownTypeError):
    pass


class UnknownBooleanError(UnknownTypeError):
    pass


class UnknownSymbolError(Error):
    pass


class MissingCurrentScopeError(SemanticError):
    pass


class ParameterCountError(SemanticError):
    pass


class LackOfParametersError(ParameterCountError):
    pass


class TooManyParametersError(ParameterCountError):
    pass


###############################################################################
#                                                                             #
#  InterpreterError                                                                      #
#                                                                             #
###############################################################################


class InterpreterError(Error):
    pass


class StaticArrayModifyLengthError(InterpreterError):
    pass


class UnknownBuiltinFunctionError(InterpreterError):
    pass


class UnknownBuiltinProcedureError(InterpreterError):
    pass


class UnknownMethodSymbolError(InterpreterError):
    pass


class NullPointerError(InterpreterError):
    pass


class ArrayRangeInvalidError(InterpreterError):
    pass


class UnknownOperatorError(InterpreterError):
    def __init__(
        self,
        error_code: ErrorCode,
        token: Token | None = None,
    ) -> None:
        self.error_code = error_code
        self.token = token
        # add exception class name before the message
        self.message = f"{self.__class__.__name__}: {None}"


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
    TYPE = "TYPE"
    CLASS = "CLASS"
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

    def skip_backslash_comment(self) -> None:
        while self.current_char != "\n":
            self.advance()
        self.advance()

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
            raise LexerStringError()
        value = ""
        while self.current_char is not None and self.current_char != "'":
            value += self.current_char
            self.advance()

        if self.current_char == "'":
            self.advance()
        else:
            raise LexerStringError()

        token.type = TokenType.STRING_CONST
        token.value = value
        return token

    def __id(self) -> Token:
        """Handle identifiers and reserved keywords"""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        value = ""
        while self.current_char is not None and (
            self.current_char.isalnum() or self.current_char == "_"
        ):
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
        raise LexerError

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

            if self.current_char == "/" and self.peek() == "/":
                self.advance()
                self.advance()
                self.skip_backslash_comment()
                continue

            if self.current_char == "'":
                return self.__string()

            if self.current_char.isalpha() or self.current_char == "_":
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

    def peek_next_token_list(self, step: int) -> list[Token]:
        """step should greater or equal to 1. If step is equal to 1, it has the same effect as peek_next_token"""
        if step < 1:
            raise FindNextTokenError()
        tokens: list[Token] = []
        prev_status = self.status()
        for _ in range(0, step):
            token = self.get_next_token()
            tokens.append(token)
        self.revert(prev_status)
        return tokens

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


class Decl(AST):
    def __init__(self):
        super().__init__()


class Def(AST):
    def __init__(self):
        super().__init__()


class BinOp(AST):
    def __init__(self, left: AST, op: Token, right: AST) -> None:
        self.left = left
        self.token = self.op = op
        self.right = right


type Number = int | float


class Num(AST):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value: Number = token.value


class Bool(AST):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value: bool = token.value


class String(AST):
    def __init__(self, token: Token):
        self.token = token
        self.value: str = token.value


class UnaryOp(AST):
    def __init__(self, op: Token, expr: AST) -> None:
        self.token = self.op = op
        self.expr = expr


class Compound(AST):
    """Represents a 'BEGIN ... END' block"""

    def __init__(self) -> None:
        self.children: list[AST] = []


class IfStatement(AST):
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


class WhileStatement(AST):
    """Represents a 'WHILE ... DO ... BEGIN ... END' block"""

    def __init__(self, condition: AST, block: Compound) -> None:
        self.condition = condition
        self.block = block


class ForStatement(AST):
    """Represents a 'FOR left:= right TO ... DO (BEGIN ... END) or statement' block"""

    def __init__(self, initialization: Assign, bound: AST, block: AST) -> None:
        self.initialization = initialization
        self.bound = bound
        self.block = block


class Assign(AST):
    def __init__(self, left: Var, op: Token, right) -> None:
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(AST):
    """The Var node is constructed out of ID token."""

    def __init__(self, token: Token) -> None:
        self.token = token
        self.value = token.value


class IndexVar(Var):
    """The IndexVar is for ID[index]"""

    def __init__(self, token, index: AST):
        super().__init__(token)
        self.index = index


class NoOp(AST):
    pass


class Program(AST):
    def __init__(self, name: str, block: Block) -> None:
        self.name = name
        self.block = block


class Block(AST):
    def __init__(
        self,
        declarations: list[Decl],
        compound_statement: Compound,
        meta_classes: dict[int, list[str]] = {},
    ) -> None:
        self.declarations = declarations
        self.compound_statement = compound_statement
        self.meta_classes = meta_classes


class VarDecl(Decl):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node

    def to_FieldDecl(self) -> FieldDecl:
        return FieldDecl(var_node=self.var_node, type_node=self.type_node)


class Member(Decl):
    def __init__(self):
        super().__init__()


class FieldDecl(Member):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node

    def to_VarDecl(self) -> VarDecl:
        return VarDecl(var_node=self.var_node, type_node=self.type_node)


class MethodType(Enum):
    CONSTRUCTOR = "CONSTRUCTOR"
    DESTRUCTOR = "DESTRUCTOR"
    PROCEDURE = "PROCEDURE"
    FUNCTION = "FUNCTION"
    UNDEFINED = "UNDEFINED"


class MethodDef(Member):
    def __init__(
        self,
        method_name: str,
        params: list[Param],
        return_type: Type,
        method_type: MethodType,
    ) -> None:
        self.method_name = method_name
        self.params = params
        self.return_type = return_type
        self.method_type = method_type


class MethodDecl(Member):
    def __init__(
        self,
        method_full_name: str,
        params: list[Param],
        return_type: Type,
        method_type: MethodType,
        block: Block,
    ) -> None:
        self.method_full_name = method_full_name
        self.params = params
        self.return_type = return_type
        self.method_type = method_type
        self.block = block

    def to_def(self) -> MethodDef:
        return MethodDef(
            method_name=self.method_full_name.split(".")[1],
            params=self.params,
            return_type=self.return_type,
            method_type=self.method_type,
        )


class ClassDecl(Decl):
    def __init__(
        self,
        class_name: str,
        fields: list[FieldDecl],
        methods: list[MethodDef],
        constructor: MethodDecl | None = None,
        destructor: MethodDecl | None = None,
    ):
        super().__init__()
        self.class_name = class_name
        self.fields = fields
        self.methods = methods
        self.constructor = constructor
        self.destructor = destructor


class Type(AST):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value = token.value


class VoidType(Type):
    def __init__(self, token):
        super().__init__(token)

    def __str__(self):
        return "VOID"


class PrimitiveType(Type):
    def __init__(self, token):
        super().__init__(token)

    def __str__(self):
        return self.value


class StringType(Type):
    def __init__(self, token, limit: Factor | None = None):
        super().__init__(token)
        self.limit = limit

    def __str__(self):
        return super().__str__()


class ArrayType(Type):
    def __init__(
        self,
        token,
        element_type: Type,
        lower: Factor,
        upper: Factor,
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


class ClassType(Type):
    def __init__(self, token):
        super().__init__(token)

    def __str__(self):
        return "Class[{class_name}]".format(class_name=self.value)


class Param(AST):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node


class ProcedureDecl(Decl):
    def __init__(
        self, proc_name: str, formal_params: list[Param], block_node: Block
    ) -> None:
        self.proc_name = proc_name
        self.formal_params = formal_params  # a list of Param nodes
        self.block_node = block_node


class ProcedureDef(Def):
    def __init__(self, proc_name: str, formal_params: list[Param]) -> None:
        self.proc_name = proc_name
        self.formal_params = formal_params


class ConstructorDecl(Decl):
    def __init__(
        self, constructor_name: str, formal_params: list[Param], block_node: Block
    ) -> None:
        self.constructor_name = constructor_name
        self.formal_params = formal_params  # a list of Param nodes
        self.block_node = block_node


class ConstructorDef(Def):
    def __init__(self, constructor_name: str, formal_params: list[Param]) -> None:
        self.constructor_name = constructor_name
        self.formal_params = formal_params


class FunctionDecl(Decl):
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


class FunctionDef(Def):
    def __init__(
        self,
        func_name: str,
        formal_params: list[Param],
        return_type: Type,
    ) -> None:
        self.func_name = func_name
        self.formal_params = formal_params
        self.return_type = return_type


class AbstractCall(AST):
    def __init__(self):
        super().__init__()


class ProcedureCall(AbstractCall):
    def __init__(self, proc_name: str, actual_params: list[Expr], token: Token) -> None:
        self.proc_name = proc_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.proc_symbol: ProcedureSymbol | None = None


class FunctionCall(AbstractCall):
    def __init__(self, func_name: str, actual_params: list[Expr], token: Token) -> None:
        self.func_name = func_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.func_symbol: FunctionSymbol | None = None


class MethodCall(AbstractCall):
    def __init__(
        self,
        method_full_name: str,
        actual_params: list[Expr],
        token: Token,
    ) -> None:
        self.method_full_name = method_full_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.method_symbol: MethodSymbol | None = None
        self.method_type: MethodType = MethodType.UNDEFINED


type Statement = Compound | ProcedureCall | MethodCall | Assign | NoOp | IfStatement | WhileStatement | ForStatement
type Expr = "Factor"
type Term = "Factor"
type Factor = UnaryOp | BinOp | Num | Bool | Var | FunctionCall | MethodCall | String


class Parser:

    def __init__(self, lexer: Lexer) -> None:
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.current_token = self.get_next_token()
        self.current_class_id = ""

    def newZeroNum(self, lineno: int = -1, column: int = -1) -> Num:
        return Num(
            token=Token(TokenType.INTEGER_CONST, value=0, lineno=lineno, column=column)
        )

    def newVoidType(self, lineno: int = -1, column: int = -1) -> VoidType:
        return VoidType(
            token=Token(
                type=TokenType.VOID,
                value=TokenType.VOID.value,
                lineno=lineno,
                column=column,
            )
        )

    def get_next_token(self):
        return self.lexer.get_next_token()

    def peek_next_token(self):
        return self.lexer.peek_next_token()

    def peek_next_two_token(self) -> tuple[Token, Token]:
        tokens = self.lexer.peek_next_token_list(2)
        return (tokens[0], tokens[1])

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

    def declarations(self) -> list[Decl]:
        """
        declarations :
            (type_declaration)?
            (VAR (variable_declaration SEMI)+)?
            procedure_declaration*
            function_declaration*
            (VAR (variable_declaration SEMI)+)?
        """
        """
        pascal does not emphasize the order of var/proc/func , the var before or after proc/func just for simplicity
        """
        hasVar: bool = False
        declarations: list[Decl] = []

        if self.current_token.type == TokenType.TYPE:
            type_decl_list = self.type_declaration()
            declarations.extend(type_decl_list)

        if self.current_token.type == TokenType.VAR:
            hasVar = True
            self.eat(TokenType.VAR)
            while self.current_token.type == TokenType.ID:
                var_decl = self.variable_declaration()
                declarations.extend(var_decl)
                self.eat(TokenType.SEMI)
        while self.current_token.type in [
            TokenType.CONSTRUCTOR,
            TokenType.PROCEDURE,
            TokenType.FUNCTION,
        ]:
            first, second = self.peek_next_two_token()
            if first.type == TokenType.ID and second.type == TokenType.DOT:
                method_decl = self.method_declaration()
                declarations.append(method_decl)
            elif self.current_token.type == TokenType.CONSTRUCTOR:
                constructor_decl = self.constructor_declaration()
                declarations.append(constructor_decl)
            elif self.current_token.type == TokenType.PROCEDURE:
                proc_decl = self.procedure_declaration()
                declarations.append(proc_decl)
            elif self.current_token.type == TokenType.FUNCTION:
                func_decl = self.function_declaration()
                declarations.append(func_decl)

        if hasVar and self.current_token.type == TokenType.VAR:
            raise VarDuplicateInScopeError()
        if self.current_token.type == TokenType.VAR:
            hasVar = True
            self.eat(TokenType.VAR)
            while self.current_token.type == TokenType.ID:
                var_decl = self.variable_declaration()
                declarations.extend(var_decl)
                self.eat(TokenType.SEMI)

        return declarations

    def type_declaration(self) -> list[Decl]:
        """
        type_declaration:
            TYPE (class_definition)*
        """
        decl_list: list[Decl] = []
        self.eat(TokenType.TYPE)
        while self.current_token.type == TokenType.ID:
            # will be used in class_definition()
            self.current_class_id = SpiUtil.toClassName(
                cast(str, self.current_token.value)
            )
            self.eat(TokenType.ID)
            self.eat(TokenType.EQ)
            if self.current_token.type == TokenType.CLASS:
                self.eat(TokenType.CLASS)
                class_def = self.class_definition()
                decl_list.append(class_def)
        return decl_list

    def class_definition(self) -> ClassDecl:
        """
        class_definition:
            ID = CLASS (PRIVATE (field_definition SEMI)+)? (PUBLIC  (method_definition)+)?
        """
        self.eat(TokenType.PRIVATE)
        fields: list[FieldDecl] = []
        while self.current_token.type == TokenType.ID:
            var_decl = self.variable_declaration()[0]
            self.eat(token_type=TokenType.SEMI)
            fields.append(
                FieldDecl(var_node=var_decl.var_node, type_node=var_decl.type_node)
            )

        methods: list[MethodDef] = []
        self.eat(TokenType.PUBLIC)
        while self.current_token.type in [
            TokenType.CONSTRUCTOR,
            TokenType.FUNCTION,
            TokenType.PROCEDURE,
        ]:
            method_def = self.method_definition()
            methods.append(method_def)
        self.eat(token_type=TokenType.END)
        self.eat(token_type=TokenType.SEMI)

        # create the default constructor and destructor
        constructor: MethodDecl | None = None
        destructor: MethodDecl | None = None
        has_constructor = False
        has_destructor = False
        for m in methods:
            if m.method_name.upper() == "CREATE":
                has_constructor = True
                continue
            if m.method_name.upper() == "FREE":
                has_destructor = True
                continue
        if not has_constructor:
            compound = Compound()
            for f in fields:
                var_name = f.var_node.value
                left = Var(token=Token(type=TokenType.ID, value=var_name))
                right: AST = NoOp()
                # TODO: only consider the primitive type and string type
                match f.type_node.token.type:
                    case TokenType.BOOLEAN:
                        right = Bool(token=Token(type=TokenType.FALSE, value=False))
                    case TokenType.INTEGER:
                        right = Num(token=Token(type=TokenType.INTEGER_CONST, value=0))
                    case TokenType.REAL:
                        right = Num(token=Token(type=TokenType.REAL_CONST, value=0.0))
                    case TokenType.STRING:
                        right = String(
                            token=Token(type=TokenType.STRING_CONST, value="")
                        )
                compound.children.append(
                    Assign(
                        left=left,
                        op=Token(type=TokenType.ASSIGN, value=TokenType.ASSIGN.value),
                        right=right,
                    )
                )

            constructor = MethodDecl(
                method_full_name=SpiUtil.extraClassName(self.current_class_id)
                + "."
                + "Create",
                params=[],
                return_type=self.newVoidType(),
                method_type=MethodType.CONSTRUCTOR,
                block=Block(declarations=[], compound_statement=compound),
            )
        if not has_destructor:
            compound = Compound()
            for f in fields:
                var_name = f.var_node.value
                left = Var(token=Token(type=TokenType.ID, value=var_name))
                right = NoOp()
                match f.type_node.token.type:
                    case TokenType.BOOLEAN:
                        right = Bool(token=Token(type=TokenType.FALSE, value=False))
                    case TokenType.INTEGER:
                        right = Num(token=Token(type=TokenType.INTEGER_CONST, value=0))
                    case TokenType.REAL:
                        right = Num(token=Token(type=TokenType.REAL_CONST, value=0.0))
                    case TokenType.STRING:
                        right = String(
                            token=Token(type=TokenType.STRING_CONST, value="")
                        )
                compound.children.append(
                    Assign(
                        left=left,
                        op=Token(type=TokenType.ASSIGN, value=TokenType.ASSIGN.value),
                        right=right,
                    )
                )
            destructor = MethodDecl(
                method_full_name=SpiUtil.extraClassName(self.current_class_id)
                + "."
                + "Free",
                params=[],
                return_type=self.newVoidType(),
                method_type=MethodType.DESTRUCTOR,
                block=Block(declarations=[], compound_statement=compound),
            )
        class_decl = ClassDecl(
            class_name=self.current_class_id,
            fields=fields,
            methods=methods,
            constructor=constructor,
            destructor=destructor,
        )
        self.current_class_id = ""  # reset here
        return class_decl

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

    def field_definition(self) -> list[VarDecl]:
        """
        field_definition: variable_declaration
        """
        raise ParserError()

    def method_definition(self) -> MethodDef:
        """
        method_definition:
            ( CONSTRUCTOR | PROCEDURE | FUNCTION ) ID DOT ID (LPAREN (formal_parameter_list)? RPAREN)? ( COLON type_spec )? SEMI
        """
        while self.current_token.type in [
            TokenType.CONSTRUCTOR,
            TokenType.PROCEDURE,
            TokenType.FUNCTION,
        ]:
            if self.current_token.type == TokenType.CONSTRUCTOR:
                self.eat(TokenType.CONSTRUCTOR)
                method_type = MethodType.CONSTRUCTOR
            elif self.current_token.type == TokenType.PROCEDURE:
                self.eat(TokenType.PROCEDURE)
                method_type = MethodType.PROCEDURE
            elif self.current_token.type == TokenType.FUNCTION:
                self.eat(TokenType.FUNCTION)
                method_type = MethodType.FUNCTION

        method_name = self.id_expr()

        params = []
        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            params = self.formal_parameter_list()
            self.eat(TokenType.RPAREN)

        return_type: Type = self.newVoidType(
            lineno=self.current_token.lineno, column=self.current_token.column
        )
        if self.current_token.type == TokenType.COLON:
            self.eat(TokenType.COLON)
            return_type = self.type_spec()

        self.eat(TokenType.SEMI)

        node = MethodDef(
            method_name=method_name,
            params=params,
            return_type=return_type,
            method_type=method_type,
        )
        return node

    def method_declaration(self) -> MethodDecl:
        """
        method_declaration :
            method_definition block SEMI
        """
        method_def = self.method_definition()
        block_node = self.block()
        method_decl = MethodDecl(
            method_full_name=method_def.method_name,
            params=method_def.params,
            return_type=method_def.return_type,
            method_type=method_def.method_type,
            block=block_node,
        )
        self.eat(TokenType.SEMI)
        return method_decl

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

    def constructor_declaration(self) -> ConstructorDecl:
        """
        constructor_declaration :
            constructor_definition block SEMI
        """
        constructor_def = self.constructor_definition()
        block_node = self.block()

        proc_decl = ConstructorDecl(
            constructor_name=constructor_def.constructor_name,
            formal_params=constructor_def.formal_params,
            block_node=block_node,
        )
        self.eat(TokenType.SEMI)
        return proc_decl

    def constructor_definition(self) -> ConstructorDef:
        """
        constructor_definition:
            CONSTRUCTOR id_expr (LPAREN formal_parameter_list RPAREN)? SEMI
        """
        self.eat(TokenType.CONSTRUCTOR)
        constructor_name = self.id_expr()

        formal_params = []
        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            formal_params = self.formal_parameter_list()
            self.eat(TokenType.RPAREN)
        self.eat(TokenType.SEMI)

        node = ConstructorDef(
            constructor_name=constructor_name, formal_params=formal_params
        )
        return node

    def procedure_declaration(self) -> ProcedureDecl:
        """
        procedure_declaration :
            procedure_definition block SEMI
        """
        proc_def = self.procedure_definition()
        block_node = self.block()

        proc_decl = ProcedureDecl(
            proc_name=proc_def.proc_name,
            formal_params=proc_def.formal_params,
            block_node=block_node,
        )
        self.eat(TokenType.SEMI)
        return proc_decl

    def procedure_definition(self) -> ProcedureDef:
        """
        procedure_definition:
            PROCEDURE id_expr (LPAREN formal_parameter_list RPAREN)? SEMI
        """
        self.eat(TokenType.PROCEDURE)
        proc_name = self.id_expr()

        formal_params = []
        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            formal_params = self.formal_parameter_list()
            self.eat(TokenType.RPAREN)
        self.eat(TokenType.SEMI)

        node = ProcedureDef(proc_name=proc_name, formal_params=formal_params)
        return node

    def function_declaration(self) -> FunctionDecl:
        """
        function_declaration :
            function_definition block SEMI
        """
        function_def = self.function_definition()

        block_node = self.block()
        func_decl = FunctionDecl(
            func_name=function_def.func_name,
            formal_params=function_def.formal_params,
            return_type=function_def.return_type,
            block_node=block_node,
        )
        self.eat(TokenType.SEMI)
        return func_decl

    def function_definition(self) -> FunctionDef:
        """
        function_definition:
            FUNCTION id_expr LPAREN (formal_parameter_list)? RPAREN COLON type_spec SEMI
        """
        self.eat(TokenType.FUNCTION)
        func_name = self.id_expr()

        formal_params = []
        self.eat(TokenType.LPAREN)
        formal_params = self.formal_parameter_list()
        self.eat(TokenType.RPAREN)
        self.eat(TokenType.COLON)

        return_type = self.type_spec()

        self.eat(TokenType.SEMI)

        node = FunctionDef(
            func_name=func_name, formal_params=formal_params, return_type=return_type
        )
        return node

    def id_expr(self) -> str:
        name: str = self.current_token.value
        self.eat(TokenType.ID)
        while self.current_token.type == TokenType.DOT:
            name += self.current_token.value
            self.eat(TokenType.DOT)
            name += self.current_token.value
            self.eat(TokenType.ID)
        return name

    def type_spec(self) -> Type:
        """
        type_spec : primitive_type_spec | string_type_spec | array_type_spec | class_type_spec
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
        elif self.current_token.type == TokenType.ID:
            return self.class_type_spec()
        else:
            raise UnknownTypeError()

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
            raise ParserError()

    def array_type_spec(self) -> ArrayType:
        """array_type_spec : ARRAY (LBRACKET INTEGER_CONST RANGE INTEGER_CONST RBRACKET)? of type_spec"""
        token = self.current_token
        self.eat(TokenType.ARRAY)
        lower: Factor = self.newZeroNum(
            lineno=self.current_token.lineno, column=self.current_token.column
        )
        upper: Factor = self.newZeroNum(
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

    def class_type_spec(self) -> ClassType:
        token = self.current_token
        node = ClassType(
            token=Token(
                type=TokenType.CLASS,
                value=token.value,
                lineno=token.lineno,
                column=token.column,
            )
        )
        self.eat(TokenType.ID)
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
                  | method_call_statement
                  | assignment_statement
                  | if_statement
                  | for_statement
                  | while_statement
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
        elif self.current_token.type == TokenType.ID:
            next_token_type = self.peek_next_token().type
            if next_token_type == TokenType.LPAREN:
                node = self.proccall_statement()
            elif next_token_type == TokenType.DOT:
                node = self.method_call_statement()
            else:
                node = self.assignment_statement()
        else:
            node = self.empty()
        return node

    def method_call_statement(self) -> MethodCall:
        """
        method_call_statement : ID DOT ID (LPAREN (expr (COMMA expr)*)? RPAREN)?
        """
        token = self.current_token

        inst_name = self.current_token.value
        self.eat(TokenType.ID)
        self.eat(TokenType.DOT)
        method_name = self.current_token.value
        self.eat(TokenType.ID)
        actual_params: list[Expr] = []
        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            if self.current_token.type != TokenType.RPAREN:
                expr = self.expr()
                actual_params.append(expr)
            while self.current_token.type == TokenType.COMMA:
                self.eat(TokenType.COMMA)
                expr = self.expr()
                actual_params.append(expr)
            self.eat(TokenType.RPAREN)

        node = MethodCall(
            method_full_name="{inst_name}.{method_name}".format(
                inst_name=inst_name, method_name=method_name
            ),
            actual_params=actual_params,
            token=token,
        )
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
        actual_params: list[Expr] = []
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

    def method_call_expr(self) -> MethodCall:
        """
        method_call_expr : id_expr (LPAREN (expr (COMMA expr)*)? RPAREN)?
        """
        token = self.current_token
        method_name = self.id_expr()

        actual_params: list[Expr] = []
        actual_params.append(
            Var(Token(TokenType.ID, method_name, token.lineno, token.column))
        )
        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            if self.current_token.type != TokenType.RPAREN:
                expr = self.summation_expr()
                actual_params.append(expr)
            while self.current_token.type == TokenType.COMMA:
                self.eat(TokenType.COMMA)
                expr = self.summation_expr()
                actual_params.append(expr)
            self.eat(TokenType.RPAREN)

        node = MethodCall(
            method_full_name=method_name,
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
        actual_params: list[Expr] = []

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
        variable: id_expr (LBRACKET summation_expr RBRACKET)?
        """
        token = self.current_token
        var_name = self.id_expr()
        node = Var(
            Token(
                type=token.type,
                value=var_name,
                lineno=token.lineno,
                column=token.column,
            )
        )
        if self.current_token.type == TokenType.LBRACKET:
            self.eat(TokenType.LBRACKET)
            index = self.summation_expr()
            self.eat(TokenType.RBRACKET)
            return IndexVar(token=node.token, index=index)
        return node

    def empty(self) -> NoOp:
        """An empty production"""
        return NoOp()

    def expr(self) -> Expr:
        """
        expr : logic_expr
        """
        return self.logic_expr()

    def logic_expr(self) -> Expr:
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

    def comparison_expr(self) -> Expr:
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

    def summation_expr(self) -> Expr:
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

    def multiplication_expr(self) -> Term:
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

    def factor(self) -> Factor:
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
               | method_call_expr
               | variable
        """
        token = self.current_token
        node: Factor
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

        # call method
        if token.type == TokenType.ID and self.peek_next_token().type == TokenType.DOT:
            node = self.method_call_expr()
            return node

        # call procedure
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

        type_declaration:
            TYPE class_definition

        class_definition:
            ID = CLASS (PRIVATE (field_definition SEMI)+)? (PUBLIC  (method_definition)+)?

        field_definition: variable_declaration

        method_definition:
            ( CONSTRUCTOR | PROCEDURE | FUNCTION ) ID DOT ID (LPAREN (formal_parameter_list)? RPAREN)? ( COLON type_spec )? SEMI

        method_declaration :
            method_definition block SEMI

        declarations :
            (type_declaration)?
            (VAR (variable_declaration SEMI)+)?
            procedure_declaration*
            function_declaration*
            (VAR (variable_declaration SEMI)+)?

        variable_declaration : ID (COMMA ID)* COLON type_spec

        constructor_declaration :
            constructor_definition SEMI block SEMI

        constructor_definition:
            CONSTRUCTOR id_expr (LPAREN formal_parameter_list RPAREN)? SEMI

        procedure_declaration :
            procedure_definition SEMI block SEMI

        procedure_definition:
            PROCEDURE id_expr (LPAREN formal_parameter_list RPAREN)? SEMI

        function_declaration :
            function_definition block SEMI

        function_definition:
            FUNCTION id_expr LPAREN (formal_parameter_list)? RPAREN COLON type_spec SEMI

        formal_params_list : formal_parameters
                           | formal_parameters SEMI formal_parameter_list

        formal_parameters : ID (COMMA ID)* COLON type_spec

        type_spec : primitive_type_spec | string_type_spec | array_type_spec | class_type_spec

        primitive_type_spec : INTEGER | REAL | BOOLEAN

        string_type_spec: STRING ( LBRACKET INTEGER_CONST RBRACKET )?

        array_type_spec : ARRAY ( LBRACKET INTEGER_CONST RANGE INTEGER_CONST RBRACKET )? of type_spec

        class_type_spec : ID

        compound_statement : BEGIN statement_list END

        statement_list : statement
                       | statement SEMI statement_list

        statement : compound_statement
                  | proccall_statement
                  | method_call_statement
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

        method_call_statement : ID DOT ID (LPAREN (expr (COMMA expr)*)? RPAREN)?

        func_call_expr : ID LPAREN (expr (COMMA expr)*)? RPAREN

        method_call_expr : id_expr (LPAREN (expr (COMMA expr)*)? RPAREN)?

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
               | method_call_expr
               | variable

        variable: id_expr (LBRACKET summation_expr RBRACKET)?

        id_expr : ID ( DOT  ID )*
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
    LOW = "LOW"
    HIGH = "HIGH"
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

    def to_FieldSymbol(self) -> FieldSymbol:
        field_symbol = FieldSymbol(name=self.name, type=self.type)
        return field_symbol

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


class ClassSymbol(Symbol):
    def __init__(
        self,
        name: str,
        fields: dict[str, FieldSymbol] = {},
        methods: dict[str, MethodSymbol] = {},
    ) -> None:
        super().__init__(name, None)
        self.fields = fields
        self.methods = methods

    def __str__(self) -> str:
        return "<{class_name}(name='{name}', fields='{fields}',  methods='{methods}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            fields=self.fields,
            methods=self.methods,
        )

    __repr__ = __str__


class FieldSymbol(Symbol):
    def __init__(self, name: str, type: Symbol | None) -> None:
        super().__init__(name, type)

    def __str__(self) -> str:
        return "<{class_name}(name='{name}', type='{type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
        )

    def to_VarSymbol(self) -> VarSymbol:
        var_symbol = VarSymbol(name=self.name, type=self.type)
        return var_symbol

    __repr__ = __str__


class MethodSymbol(Symbol):
    def __init__(
        self,
        name: str,
        return_type: Type | None,
        formal_params: list[Symbol],
        method_type: MethodType,
    ) -> None:
        super().__init__(name)
        # a list of VarSymbol objects
        self.formal_params: list[Symbol] = formal_params
        self.return_type = return_type
        self.method_type = method_type
        # a reference to method's body (AST sub-tree)
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
        self.insert(BuiltinTypeSymbol(name="INTEGER"))
        self.insert(BuiltinTypeSymbol(name="REAL"))
        self.insert(BuiltinTypeSymbol(name="BOOLEAN"))
        self.insert(StringTypeSymbol(name="STRING", limit=255))
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
        self.insert(
            BuiltinFunctionSymbol(
                name=NativeMethod.LOW.name,
                return_type=Type(
                    token=Token(type=TokenType.INTEGER, value=0, lineno=-1, column=-1)
                ),
                formal_params=[],
            )
        )
        self.insert(
            BuiltinFunctionSymbol(
                name=NativeMethod.HIGH.name,
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

    def insert_pair(self, name: str, symbol: Symbol) -> None:
        self.log(f"Insert: {name}")
        symbol.scope_level = self.scope_level
        self._symbols[name] = symbol

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
        # used for for-loop
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

    def visit_Decl(self, node: Decl) -> None:
        pass

    def visit_Member(self, node: Member) -> None:
        pass

    def visit_ClassDecl(self, node: ClassDecl) -> None:
        fields: dict[str, FieldSymbol] = {}
        for f in node.fields:
            field_symbol = self.visit_FieldDecl(f)
            fields[field_symbol.name] = field_symbol
        methods: dict[str, MethodSymbol] = {}
        for m in node.methods:
            method_symbol = self.visit_MethodDef(m)
            methods[method_symbol.name] = method_symbol
        class_symbol = ClassSymbol(name=node.class_name, fields=fields, methods=methods)

        if self.current_scope is None:
            raise MissingCurrentScopeError()
        if self.current_scope.lookup(node.class_name, current_scope_only=True):
            raise DuplicateClassError()
        self.current_scope.insert_pair(node.class_name, class_symbol)

        # bind constructor
        if node.constructor is not None:
            method_symbol = self.visit_MethodDef(node.constructor.to_def())
            methods[method_symbol.name] = method_symbol
            self.visit_MethodDecl(node.constructor)
        # bind destructor
        if node.destructor is not None:
            method_symbol = self.visit_MethodDef(node.destructor.to_def())
            methods[method_symbol.name] = method_symbol
            self.visit_MethodDecl(node.destructor)
        pass

    def visit_ConstructorDecl(self, node: ConstructorDecl) -> None:
        pass

    def visit_Def(self, node: Def) -> None:
        pass

    def visit_MethodDef(self, node: MethodDef) -> MethodSymbol:
        method_name = node.method_name
        return_type = node.return_type
        method_symbol = MethodSymbol(
            name=method_name,
            return_type=return_type,
            formal_params=[],
            method_type=node.method_type,
        )
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        # Insert parameters into the function scope
        for param in node.params:
            param_type = self.current_scope.lookup(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            method_symbol.formal_params.append(var_symbol)
        return method_symbol

    def visit_MethodDecl(self, node: MethodDecl) -> MethodSymbol:
        method_full_name = node.method_full_name
        class_name, method_name = method_full_name.split(".")
        return_type = node.return_type
        method_symbol = MethodSymbol(
            name=method_full_name,
            return_type=return_type,
            formal_params=[],
            method_type=node.method_type,
        )
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        self.current_scope.insert(method_symbol)

        class_name = SpiUtil.toClassName(class_name=class_name)
        symbol = self.current_scope.lookup(class_name)
        if symbol is None:
            raise UnknownSymbolError()
        class_symbol = cast(ClassSymbol, symbol)
        # bind method
        class_symbol.methods[method_name].block_ast = node.block

        self.log(f"ENTER method scope: {method_full_name}")
        # Scope for parameters and local variables
        function_scope = ScopedSymbolTable(
            scope_name=method_name,
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope,
        )
        self.current_scope = function_scope
        for _, fs in class_symbol.fields.items():
            self.current_scope.insert_pair(fs.name, fs)
        for _, ms in class_symbol.methods.items():
            self.current_scope.insert_pair(ms.name, ms)

        # insert return value into the function scope
        # pascal support implicit return the value has the same name to the function name
        return_var_name = method_name
        return_var_symbol = VarSymbol(
            return_var_name, self.current_scope.lookup(return_type.value)
        )
        self.current_scope.insert(return_var_symbol)
        method_symbol.formal_params.append(return_var_symbol)

        # Insert parameters into the function scope
        for param in node.params:
            param_type = self.current_scope.lookup(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            self.current_scope.insert(var_symbol)
            method_symbol.formal_params.append(var_symbol)

        self.visit(node.block)

        self.log(function_scope)

        self.current_scope = self.current_scope.enclosing_scope
        self.log(f"LEAVE method scope: {method_name}")

        # accessed by the interpreter when executing procedure call
        method_symbol.block_ast = node.block
        return method_symbol

    def visit_MethodCall(self, node: MethodCall):
        if self.current_scope is None:
            raise MissingCurrentScopeError()

        class_or_instance_name, method_name = node.method_full_name.split(".")
        symbol = self.current_scope.lookup(SpiUtil.toClassName(class_or_instance_name))
        if symbol is None:
            # use instance.method() to call
            symbol = self.current_scope.lookup(class_or_instance_name)
            if symbol is None:
                raise UnknownSymbolError()
            else:
                instance_symbol = cast(VarSymbol, symbol)
                class_symbol = cast(ClassSymbol, instance_symbol.type)
                if method_name in class_symbol.methods:
                    method_symbol = class_symbol.methods[method_name]
                    method_type = method_symbol.method_type
                    node.method_symbol = cast(MethodSymbol, method_symbol)
        else:
            # use class.method() to call
            class_symbol = cast(ClassSymbol, symbol)
            if method_name in class_symbol.methods:
                method_symbol = class_symbol.methods[method_name]
                method_type = method_symbol.method_type
                node.method_symbol = cast(MethodSymbol, method_symbol)

        node.method_type = method_type

        actual_params_len = 0
        if method_type == MethodType.PROCEDURE:
            actual_params_len = len(node.actual_params)
        elif method_type == MethodType.FUNCTION:
            actual_params_len = len(node.actual_params) - 1
        elif method_type == MethodType.CONSTRUCTOR:
            actual_params_len = len(node.actual_params) - 1
        if len(method_symbol.formal_params) < actual_params_len:
            raise TooManyParametersError()
        elif len(method_symbol.formal_params) > actual_params_len:
            raise LackOfParametersError()
        for param_node in node.actual_params[1:]:
            self.visit(param_node)

    def visit_ProcedureDef(self, node: ProcedureDef) -> None:
        pass

    def visit_ConstructorDef(self, node: ConstructorDef) -> None:
        pass

    def visit_FunctionDef(self, node: FunctionDef) -> None:
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
        if node.else_branch != None:
            self.visit(node.else_branch)

    def visit_BinOp(self, node: BinOp) -> None:
        self.visit(node.left)
        self.visit(node.right)

    def visit_Type(self, node: Type):
        pass

    def visit_PrimitiveType(self, node: PrimitiveType):
        pass

    def visit_StringType(self, node: StringType):
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        if isinstance(node.limit, Num):
            limit = int(node.limit.value)
            self.__string_type_limit = limit
            self.current_scope.insert(
                StringTypeSymbol(
                    name=SemanticAnalyzer.string_type_name(limit), limit=int(limit)
                )
            )
        elif node.limit is None:
            self.current_scope.insert(
                StringTypeSymbol(name=SemanticAnalyzer.string_type_name(255), limit=255)
            )
        pass

    def visit_ArrayType(self, node: ArrayType) -> None:
        if isinstance(node.element_type, ArrayType):
            self.visit_ArrayType(node.element_type)
        if self.current_scope is None:
            raise MissingCurrentScopeError
        element_type_symbol = self.current_scope.lookup(str(node.element_type))
        if element_type_symbol is None:
            raise UnknownArrayElementTypeError()
        type_name = str(node)
        type_symbol = self.current_scope.lookup(type_name)
        if type_symbol is None:
            self.current_scope.insert(
                ArrayTypeSymbol(name=type_name, element_type=element_type_symbol)
            )

    def visit_ClassType(self, node: ClassType):
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        symbol = self.current_scope.lookup(str(node))
        if symbol is None:
            raise UnknownClassTypeError()

    def visit_FieldDecl(self, node: FieldDecl) -> FieldSymbol:
        var_symbol = self.visit_VarDecl(node.to_VarDecl())
        return var_symbol.to_FieldSymbol()

    def visit_VarDecl(self, node: VarDecl) -> VarSymbol:
        if self.current_scope is None:
            raise MissingCurrentScopeError()

        type_name = node.type_node.value
        if isinstance(node.type_node, ArrayType):
            self.visit(node.type_node)
            type_name = str(node.type_node)
        elif isinstance(node.type_node, StringType):
            self.visit(node.type_node)
            type_name = SemanticAnalyzer.string_type_name(size=self.__string_type_limit)
        elif isinstance(node.type_node, ClassType):
            self.visit(node.type_node)
            type_name = SpiUtil.toClassName(type_name)
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
        return var_symbol

    def visit_Assign(self, node: Assign) -> None:
        # right-hand side
        self.visit(node.right)
        # left-hand side
        if node.left.value in self.unmodified_vars:
            self.error(ErrorCode.MODIFY_LOOP_VAR_NOT_ALLOW, token=node.left.token)
        self.visit(node.left)
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        var_symbol = self.current_scope.lookup(node.left.value)
        if var_symbol is None:
            raise UnknownSymbolError()
        if var_symbol.type is None:
            raise UnknownSymbolError()
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
            raise MissingCurrentScopeError()
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
            raise MissingCurrentScopeError()
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


class ARType(Enum):
    PROGRAM = "PROGRAM"
    PROCEDURE = "PROCEDURE"
    FUNCTION = "FUNCTION"
    METHOD = "METHOD"


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


class MemberMeta:
    def __init__(self, type: ElementType, dynamic: bool = False, limit: int = -1):
        self.type = type
        self.dynamic = dynamic
        self.limit = limit
        self.is_class = False
        self.is_instance = False
        self.ref_class_name = ""


class ActivationRecord:
    def __init__(self, name: str, type: ARType, nesting_level: int) -> None:
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.members: dict[str, Any] = {}
        self.members_meta: dict[str, MemberMeta] = {}

    def __setitem__(self, key: str, value) -> None:
        self.members[key] = value

    def __getitem__(self, key: str):
        return self.members[key]

    def copy_from(self, other: "ActivationRecord", override: bool):
        for name, val in other.members.items():
            if override or name not in self.members:
                self.members[name] = val
        for name, val in other.members_meta.items():
            if override or name not in self.members_meta:
                self.members_meta[name] = val

    def get(self, key: str):
        return self.members.get(key)

    def set_meta(self, key: str, type: ElementType):
        self.members_meta[key] = MemberMeta(type=type)

    def set_dynamic(self, key: str, dynamic: bool):
        self.members_meta[key].dynamic = dynamic

    def set_limit(self, key: str, limit: int):
        self.members_meta[key].limit = limit

    def get_meta(self, key: str):
        meta = self.members_meta.get(key)
        if meta is None:
            raise InterpreterError()
        else:
            return meta

    def set_is_class(self, key: str, is_class: bool):
        self.members_meta[key].is_class = is_class

    def set_is_instance(self, key: str, is_instance: bool):
        self.members_meta[key].is_instance = is_instance

    def set_ref_class_name(self, key: str, ref_class_name: str):
        self.members_meta[key].ref_class_name = ref_class_name

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
            ar[node.var_node.value] = False
        elif node.type_node.token.type == TokenType.INTEGER:
            ar[node.var_node.value] = 0
        elif node.type_node.token.type == TokenType.REAL:
            ar[node.var_node.value] = 0.0
        elif node.type_node.token.type == TokenType.STRING:
            ar[node.var_node.value] = ""
            string_node = cast(StringType, node.type_node)
            limit: int = 255
            if string_node.limit is not None:
                limit = self.visit(string_node.limit)
            ar.set_meta(key=node.var_node.value, type=ElementType.STRING)
            ar.set_limit(key=node.var_node.value, limit=limit)
        elif node.type_node.token.type == TokenType.ARRAY:
            ar[node.var_node.value] = self.__initArray(node.type_node, ar)
            self.__set_member_type(node, ar)
            if (cast(ArrayType, node.type_node)).dynamic is True:
                ar.set_dynamic(node.var_node.value, True)
        elif node.type_node.token.type == TokenType.CLASS:
            # TODO: should write this in recursion
            class_decl: ClassDecl = ar.get(
                SpiUtil.toClassName(node.type_node.token.value)
            )
            instance: dict[str, Any] = {}
            for field in class_decl.fields:
                if field.type_node.token.type == TokenType.BOOLEAN:
                    instance[field.var_node.value] = False
                elif field.type_node.token.type == TokenType.INTEGER:
                    instance[field.var_node.value] = 0
                elif field.type_node.token.type == TokenType.REAL:
                    instance[field.var_node.value] = 0.0
                elif field.type_node.token.type == TokenType.STRING:
                    instance[field.var_node.value] = ""
                elif field.type_node.token.type == TokenType.ARRAY:
                    instance[field.var_node.value] = []
                elif field.type_node.token.type == TokenType.CLASS:
                    instance[field.var_node.value] = {}
            ar[node.var_node.value] = instance
            ar.set_meta(node.var_node.value, ElementType.INSTANCE)
            ar.set_is_instance(node.var_node.value, True)
            ar.set_ref_class_name(node.var_node.value, class_decl.class_name)
        pass

    def __set_member_type(self, node: VarDecl, ar: ActivationRecord):
        if isinstance(node.type_node, ArrayType):
            if node.type_node.element_type.token.type == TokenType.BOOLEAN:
                ar.set_meta(node.var_node.value, ElementType.BOOL)
            elif node.type_node.element_type.token.type == TokenType.INTEGER:
                ar.set_meta(node.var_node.value, ElementType.INTEGER)
            elif node.type_node.element_type.token.type == TokenType.REAL:
                ar.set_meta(node.var_node.value, ElementType.REAL)
            elif node.type_node.element_type.token.type == TokenType.ARRAY:
                ar.set_meta(node.var_node.value, ElementType.ARRAY)
        else:
            pass

    def __initArray(self, node: Type, ar: ActivationRecord) -> dict[Any, Any]:
        if isinstance(node, ArrayType):
            lower_bound: int = self.visit(node.lower)
            upper_bound: int = self.visit(node.upper)
            if lower_bound > upper_bound:
                raise ArrayRangeInvalidError()
            if node.element_type.token.type == TokenType.BOOLEAN:
                bool_arr: dict[int, bool] = {}
                if node.dynamic is False:
                    for i in range(lower_bound, upper_bound + 1):
                        bool_arr[i] = False
                return bool_arr
            elif node.element_type.token.type == TokenType.INTEGER:
                int_arr: dict[int, int] = {}
                if node.dynamic is False:
                    for i in range(lower_bound, upper_bound + 1):
                        int_arr[i] = 0
                return int_arr
            elif node.element_type.token.type == TokenType.REAL:
                real_arr: dict[int, float] = {}
                if node.dynamic is False:
                    for i in range(lower_bound, upper_bound + 1):
                        real_arr[i] = 0.0
                return real_arr
            elif node.element_type.token.type == TokenType.ARRAY:
                arr_arr: dict[int, dict] = {}
                if node.dynamic is False:
                    for i in range(lower_bound, upper_bound + 1):
                        arr_arr[i] = self.__initArray(node.element_type, ar)
                return arr_arr
        raise UnknownTypeError()

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

    def visit_Decl(self, node: Decl) -> None:
        pass

    def visit_Member(self, node: Member) -> None:
        pass

    def visit_ClassDecl(self, node: ClassDecl) -> None:
        ar = self.call_stack.peek()
        for field in node.fields:
            self.visit(field)
        for method in node.methods:
            self.visit(method)
        ar[node.class_name] = node
        raw_class_name = SpiUtil.extraClassName(node.class_name)
        ar.set_meta(raw_class_name, ElementType.CLASS)
        ar.set_is_class(raw_class_name, True)
        pass

    def visit_ConstructorDecl(self, node: ConstructorDecl) -> None:
        pass

    def visit_Def(self, node: Def) -> None:
        pass

    def visit_MethodDef(self, node: MethodDef):
        pass

    def visit_FieldDecl(self, node: FieldDecl):
        pass

    def visit_MethodDecl(self, node: MethodDecl):
        pass

    def visit_ProcedureDef(self, node: ProcedureDef) -> None:
        pass

    def visit_ConstructorDef(self, node: ConstructorDef) -> None:
        pass

    def visit_FunctionDef(self, node: FunctionDef) -> None:
        pass

    def visit_BinOp(self, node: BinOp) -> Number | bool | str:
        # logic operator
        if node.op.type == TokenType.AND:
            return bool(self.visit(node.left)) and bool(self.visit(node.right))
        elif node.op.type == TokenType.OR:
            return bool(self.visit(node.left)) or bool(self.visit(node.right))

        # arithmetic operator
        if node.op.type == TokenType.PLUS:
            return cast(Number | str, self.visit(node.left) + self.visit(node.right))
        elif node.op.type == TokenType.MINUS:
            return cast(Number, self.visit(node.left) - self.visit(node.right))
        elif node.op.type == TokenType.MUL:
            return cast(Number, self.visit(node.left) * self.visit(node.right))
        elif node.op.type == TokenType.INTEGER_DIV:
            return cast(Number, self.visit(node.left) // self.visit(node.right))
        elif node.op.type == TokenType.FLOAT_DIV:
            return float(self.visit(node.left)) / float(self.visit(node.right))

        # comparison operator
        if node.op.type == TokenType.LT:
            return float(self.visit(node.left)) < float(self.visit(node.right))
        elif node.op.type == TokenType.GT:
            return float(self.visit(node.left)) > float(self.visit(node.right))
        elif node.op.type == TokenType.EQ:
            return float(self.visit(node.left)) == float(self.visit(node.right))
        elif node.op.type == TokenType.NE:
            return float(self.visit(node.left)) != float(self.visit(node.right))
        elif node.op.type == TokenType.LE:
            return float(self.visit(node.left)) <= float(self.visit(node.right))
        elif node.op.type == TokenType.GE:
            return float(self.visit(node.left)) >= float(self.visit(node.right))

        # !!
        raise UnknownOperatorError(ErrorCode.UNKNOWN_BIN_OP, node.token)

    def visit_Num(self, node: Num):
        return node.value

    def visit_String(self, node: String):
        return node.value

    def visit_Bool(self, node: Bool):
        if node.token.type == TokenType.TRUE:
            return True
        elif node.token.type == TokenType.FALSE:
            return False
        raise UnknownBooleanError()

    def visit_UnaryOp(self, node: UnaryOp) -> Number | bool:
        op = node.op.type
        # negative bang
        if op == TokenType.NOT:
            return not self.visit(node.expr)

        # signal bang
        if op == TokenType.PLUS:
            return cast(Number, +self.visit(node.expr))
        elif op == TokenType.MINUS:
            return cast(Number, -self.visit(node.expr))
        raise UnknownOperatorError(ErrorCode.UNKNOWN_UNARY_OP, node.token)

    def visit_Compound(self, node: Compound) -> None:
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node: Assign) -> None:
        var_name = node.left.value
        var_value = self.visit(node.right)
        ar = self.call_stack.peek()

        if isinstance(node.left, IndexVar):
            # array [index] = value
            index: int = self.visit(node.left.index)
            ar[var_name][index] = var_value
        else:
            # identifier = value
            if var_name in ar.members_meta:
                limit = ar.get_meta(var_name).limit
                if limit > 0:
                    if len(var_value) > limit:
                        message = f"Warning: String literal has more characters[{len(var_value)}] than short string length[{limit}]"
                        SpiUtil.print_w(message=message)
                    ar[var_name] = cast(str, var_value)[0:limit]
                else:
                    ar[var_name] = var_value
            else:
                ar[var_name] = var_value

    def visit_Var(self, node: Var) -> Any:
        var_name = node.value

        ar = self.call_stack.peek()
        var_value = ar.get(var_name)

        return var_value

    def visit_IndexVar(self, node: IndexVar) -> Any:
        var_name = node.value
        index: int = self.visit(node.index)

        ar = self.call_stack.peek()
        if ar.get_meta(var_name).type == ElementType.STRING:
            string_const = ar.get(var_name)
            if len(string_const) >= index:
                return string_const[index - 1]
            else:
                return ""
        else:
            array = ar.get(var_name)

            if index in array:
                return array[index]
            else:
                message = f"Warning: range check error while evaluating constants {var_name}[{index}]"
                SpiUtil.print_w(message=message)
                element_type = ar.get_meta(var_name).type
                if element_type == ElementType.BOOL:
                    return False
                if element_type == ElementType.INTEGER:
                    return 0
                if element_type == ElementType.REAL:
                    return 0.0
                if element_type == ElementType.ARRAY:
                    return {}

    def visit_NoOp(self, node: NoOp) -> None:
        pass

    def visit_WhileStatement(self, node: WhileStatement) -> None:
        while self.visit(node.condition) is True:
            self.visit(node.block)

    def visit_ForStatement(self, node: ForStatement) -> None:
        ar = self.call_stack.peek()
        var_name = node.initialization.left.value
        self.visit(node.initialization)
        bound_value = cast(Number, self.visit(node.bound))
        var_value = ar[var_name]
        while var_value <= bound_value:
            self.visit(node.block)
            var_value += 1
            if var_value <= bound_value:
                ar[var_name] = var_value

    def visit_ProcedureDecl(self, node: ProcedureDecl) -> None:
        pass

    def visit_ProcedureCall(self, node: ProcedureCall) -> None:
        proc_name = node.proc_name
        proc_symbol = node.proc_symbol

        if proc_symbol is None:
            raise NullPointerError

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
            if proc_symbol.name.upper() == NativeMethod.WRITE.name:
                actual_params = node.actual_params

                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                # output actual params
                for argument_node in actual_params:
                    print(self.visit(argument_node), end="")

                self.log(f"LEAVE: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return
            elif proc_symbol.name.upper() == NativeMethod.WRITELN.name:
                actual_params = node.actual_params

                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                # output actual params
                for argument_node in actual_params:
                    print(self.visit(argument_node), end="")
                print()

                self.log(f"LEAVE: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return
            elif proc_symbol.name.upper() == NativeMethod.SETLENGTH.name:
                actual_params = node.actual_params

                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                # core
                arr_name = actual_params[0].value
                new_length = actual_params[1].value
                element_type = ar.get_meta(arr_name).type
                if element_type == ElementType.STRING:
                    if len(ar[arr_name]) > new_length:
                        ar[arr_name] = ar[arr_name][0:new_length]
                else:
                    if ar.get_meta(arr_name).dynamic is False:
                        raise StaticArrayModifyLengthError()

                    for i in range(0, new_length):
                        if i in ar[arr_name]:
                            continue
                        if element_type == ElementType.BOOL:
                            ar[arr_name][i] = False
                        if element_type == ElementType.INTEGER:
                            ar[arr_name][i] = 0
                        if element_type == ElementType.REAL:
                            ar[arr_name][i] = 0.0
                        if element_type == ElementType.ARRAY:
                            ar[arr_name][i] = {}

                self.log(f"LEAVE: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return
            else:
                raise UnknownBuiltinProcedureError()
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
                raise NullPointerError
            self.visit(proc_symbol.block_ast)

            self.log(f"LEAVE: PROCEDURE {proc_name}")
            self.log(str(self.call_stack))

            self.call_stack.pop()
            pass

    def visit_FunctionDecl(self, node: FunctionDecl) -> None:
        pass

    def visit_IfStatement(self, node: IfStatement) -> None:
        flag: bool = self.visit(node.condition)

        if flag == True:
            self.visit(node.then_branch)
            return
        else:
            for branch in node.else_if_branches:
                sub_flag: bool = self.visit(branch)
                if sub_flag == True:
                    self.visit(branch.then_branch)
                    return

        if node.else_branch != None:
            self.visit(node.else_branch)

    def visit_FunctionCall(self, node: FunctionCall) -> Any:
        func_name = node.func_name
        func_symbol = node.func_symbol

        if func_symbol is None:
            raise NullPointerError

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
            if func_symbol.name.upper() == NativeMethod.LENGTH.name:
                actual_params = node.actual_params

                # [0] = LENGTH, [1] = ARRAY_NAME
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                ar[RETURN_NUM] = len(self.visit(actual_params[i]))

                self.log(f"LEAVE: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return ar[RETURN_NUM]
            elif func_symbol.name.upper() == NativeMethod.LOW.name:
                actual_params = node.actual_params

                # [0] = LENGTH, [1] = ARRAY_NAME
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                ar[RETURN_NUM] = min(self.visit(actual_params[i]))

                self.log(f"LEAVE: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return ar[RETURN_NUM]
            elif func_symbol.name.upper() == NativeMethod.HIGH.name:
                actual_params = node.actual_params

                # [0] = LENGTH, [1] = ARRAY_NAME
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                ar[RETURN_NUM] = max(self.visit(actual_params[i]))

                self.log(f"LEAVE: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return ar[RETURN_NUM]
            else:
                raise UnknownBuiltinFunctionError()
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
                raise NullPointerError
            self.visit(func_symbol.block_ast)

            self.log(f"LEAVE: FUNCTION {func_name}")
            self.log(str(self.call_stack))

            self.call_stack.pop()

            return ar[func_name]

    def visit_MethodCall(self, node: MethodCall) -> Any:
        method_full_name = node.method_full_name
        class_or_inst_name, method_name = method_full_name.split(".")
        method_symbol = node.method_symbol

        if method_symbol is None:
            raise UnknownMethodSymbolError

        ar = ActivationRecord(
            name=method_full_name,
            type=ARType.METHOD,
            nesting_level=method_symbol.scope_level + 1,
        )
        instance: dict[str, Any] = {}
        pre_ar = self.call_stack.peek()
        if pre_ar is not None:
            ar.copy_from(pre_ar, False)
            meta = pre_ar.get_meta(class_or_inst_name)
            if meta.is_instance:
                inst_name = class_or_inst_name
                instance = pre_ar.get(inst_name)
                for k, v in instance.items():
                    ar[k] = v
            elif meta.is_class:
                class_name = class_or_inst_name
                class_decl = pre_ar.get(SpiUtil.toClassName(class_name))
                for field in class_decl.fields:
                    if field.type_node.token.type == TokenType.BOOLEAN:
                        instance[field.var_node.value] = False
                    elif field.type_node.token.type == TokenType.INTEGER:
                        instance[field.var_node.value] = 0
                    elif field.type_node.token.type == TokenType.REAL:
                        instance[field.var_node.value] = 0.0
                    elif field.type_node.token.type == TokenType.STRING:
                        instance[field.var_node.value] = ""
                    elif field.type_node.token.type == TokenType.ARRAY:
                        instance[field.var_node.value] = []
                    elif field.type_node.token.type == TokenType.CLASS:
                        instance[field.var_node.value] = {}
            else:
                raise UnknownMethodSymbolError()

            pass

        formal_params = method_symbol.formal_params
        actual_params = node.actual_params

        method_type = method_symbol.method_type
        if method_type == MethodType.PROCEDURE:
            for param_symbol, argument_node in zip(formal_params, actual_params):
                ar[param_symbol.name] = self.visit(argument_node)
        else:
            for param_symbol, argument_node in zip(formal_params, actual_params[1:]):
                ar[param_symbol.name] = self.visit(argument_node)

        self.call_stack.push(ar)

        self.log(f"ENTER: METHOD {method_full_name}")
        self.log(str(self.call_stack))

        # evaluate procedure body
        if method_symbol.block_ast is None:
            raise NullPointerError
        self.visit(method_symbol.block_ast)

        self.log(f"LEAVE: METHOD {method_full_name}")
        self.log(str(self.call_stack))

        # before leave method scope, should assign the modified field value outside
        for k, v in instance.items():
            instance[k] = ar[k]

        self.call_stack.pop()

        method_type = method_symbol.method_type
        if method_type == MethodType.CONSTRUCTOR:
            # T.Create; BEGIN a:=?;b:=?; END; will not return any explicit value, but should generate an instance value for assignment
            return instance
        elif method_type == MethodType.FUNCTION:
            # T.m; BEGIN m := ? END; should assign to m as return value
            return ar[method_name]
        else:
            return None

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
