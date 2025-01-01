"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

import argparse
import sys
from enum import Enum
from dataclasses import dataclass
from typing import Any, cast

_SHOULD_LOG_SCOPE = False  # see '--scope' command line option
_SHOULD_LOG_STACK = False  # see '--stack' command line option


class ErrorCode(Enum):
    UNEXPECTED_TOKEN = "Unexpected token"
    ID_NOT_FOUND = "Identifier not found"
    DUPLICATE_ID = "Duplicate id found"
    NULL_POINTER = "Null pointer exception"
    CURRENT_SCOPE_NOT_FOUND = "Current scope not found"
    UNKNOWN_BIN_OP = "Unknown binary operator"
    UNKNOWN_UNARY_OP = "Unknown unary operator"


class Error(Exception):
    def __init__(self, error_code=None, token=None, message=None) -> None:
        self.error_code = error_code
        self.token = token
        # add exception class name before the message
        self.message = f"{self.__class__.__name__}: {message}"


class LexerError(Error):
    pass


class ParserError(Error):
    pass


class SemanticError(Error):
    pass


class NullPointerError(Error):
    pass


class CurrentScopeMissingError(Error):
    pass


class UnknownOperatorError(Error):
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
    SEMI = ";"
    DOT = "."
    COLON = ":"
    COMMA = ","
    # block of reserved words
    PROGRAM = "PROGRAM"  # marks the beginning of the block
    INTEGER = "INTEGER"
    FUNCTION = "FUNCTION"
    REAL = "REAL"
    INTEGER_DIV = "DIV"
    VAR = "VAR"
    PROCEDURE = "PROCEDURE"
    BEGIN = "BEGIN"
    END = "END"  # marks the end of the block
    # misc
    ID = "ID"
    INTEGER_CONST = "INTEGER_CONST"
    REAL_CONST = "REAL_CONST"
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


RESERVED_KEYWORDS = _build_reserved_keywords()


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

        if self.current_char == ".":
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

    def _id(self) -> Token:
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

    def get_next_token(self) -> Token:
        """Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char == "{":
                self.advance()
                self.skip_comment()
                continue

            if self.current_char.isalpha():
                return self._id()

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


class UnaryOp(AST):
    def __init__(self, op: Token, expr: AST) -> None:
        self.token = self.op = op
        self.expr = expr


class Compound(AST):
    """Represents a 'BEGIN ... END' block"""

    def __init__(self) -> None:
        self.children: list[AST] = []


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


class NoOp(AST):
    pass


class Program(AST):
    def __init__(self, name: str, block: Block) -> None:
        self.name = name
        self.block = block


class Block(AST):
    def __init__(self, declarations: list[Decl], compound_statement: Compound) -> None:
        self.declarations = declarations
        self.compound_statement = compound_statement


class VarDecl(AST):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node


class Type(AST):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value = token.value


class Param(AST):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node


class ProcedureDecl(AST):
    def __init__(
        self, proc_name: str, formal_params: list[Param], block_node: Block
    ) -> None:
        self.proc_name = proc_name
        self.formal_params = formal_params  # a list of Param nodes
        self.block_node = block_node


class FunctionDecl(AST):
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


class ProcedureCall(AST):
    def __init__(self, proc_name: str, actual_params: list[Expr], token: Token) -> None:
        self.proc_name = proc_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.proc_symbol: ProcedureSymbol | None = None


class FunctionCall(AST):
    def __init__(self, func_name: str, actual_params: list[Expr], token: Token) -> None:
        self.func_name = func_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.func_symbol: FunctionSymbol | None = None


type Decl = VarDecl | ProcedureDecl | FunctionDecl
type Statement = Compound | ProcedureCall | Assign | NoOp
type Expr = "Factor"
type Term = "Factor"
type Factor = UnaryOp | BinOp | Num | Var | FunctionCall


class Parser:
    def __init__(self, lexer: Lexer) -> None:
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.current_token = self.get_next_token()

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

    def declarations(self) -> list[Decl]:
        """
        declarations : (VAR (variable_declaration SEMI)+)? procedure_declaration* function_declaration*
        """
        declarations: list[Decl] = []

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
        """type_spec : INTEGER
        | REAL
        """
        token = self.current_token
        if self.current_token.type == TokenType.INTEGER:
            self.eat(TokenType.INTEGER)
        else:
            self.eat(TokenType.REAL)
        node = Type(token)
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

    def statement(self) -> Statement:
        """
        statement : compound_statement
                  | proccall_statement
                  | assignment_statement
                  | empty
        """
        node: Statement
        if self.current_token.type == TokenType.BEGIN:
            node = self.compound_statement()
        elif self.current_token.type == TokenType.ID and self.lexer.current_char == "(":
            node = self.proccall_statement()
        elif self.current_token.type == TokenType.ID:
            node = self.assignment_statement()
        else:
            node = self.empty()
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
            expr = self.expr()
            actual_params.append(expr)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            expr = self.expr()
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
        variable : ID
        """
        node = Var(self.current_token)
        self.eat(TokenType.ID)
        return node

    def empty(self) -> NoOp:
        """An empty production"""
        return NoOp()

    def expr(self) -> Expr:
        """
        expr : term ((PLUS | MINUS) term)*
        """
        node = self.term()

        while self.current_token.type in (TokenType.PLUS, TokenType.MINUS):
            token = self.current_token
            if token.type == TokenType.PLUS:
                self.eat(TokenType.PLUS)
            elif token.type == TokenType.MINUS:
                self.eat(TokenType.MINUS)

            node = BinOp(left=node, op=token, right=self.term())

        return node

    def term(self) -> Term:
        """term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*"""
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
        """factor : PLUS factor
        | MINUS factor
        | INTEGER_CONST
        | REAL_CONST
        | LPAREN expr RPAREN
        | variable
        """
        token = self.current_token
        node: Factor
        if token.type == TokenType.PLUS:
            self.eat(TokenType.PLUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == TokenType.MINUS:
            self.eat(TokenType.MINUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == TokenType.INTEGER_CONST:
            self.eat(TokenType.INTEGER_CONST)
            return Num(token)
        elif token.type == TokenType.REAL_CONST:
            self.eat(TokenType.REAL_CONST)
            return Num(token)
        elif token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            node = self.expr()
            self.eat(TokenType.RPAREN)
            return node
        elif (
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

        type_spec : INTEGER | REAL

        compound_statement : BEGIN statement_list END

        statement_list : statement
                       | statement SEMI statement_list

        statement : compound_statement
                  | proccall_statement
                  | func_call_expr
                  | assignment_statement
                  | empty

        proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN

        func_call_expr : ID LPAREN (expr (COMMA expr)*)? RPAREN

        assignment_statement : variable ASSIGN expr

        empty :

        expr : term ((PLUS | MINUS) term)*

        term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*

        factor : PLUS factor
               | MINUS factor
               | INTEGER_CONST
               | REAL_CONST
               | LPAREN expr RPAREN
               | func_call_expr
               | variable

        variable: ID
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

        if current_scope_only:
            return None

        # recursively go up the chain and lookup the name
        if self.enclosing_scope is not None:
            return self.enclosing_scope.lookup(name)
        return None


class SemanticAnalyzer(NodeVisitor):
    def __init__(self) -> None:
        self.current_scope: ScopedSymbolTable | None = None

    def log(self, msg) -> None:
        if _SHOULD_LOG_SCOPE:
            print(msg)

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

    def visit_BinOp(self, node: BinOp) -> None:
        self.visit(node.left)
        self.visit(node.right)

    def visit_VarDecl(self, node: VarDecl) -> None:
        type_name = node.type_node.value
        if self.current_scope is None:
            raise NullPointerError
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
        self.visit(node.left)

    def visit_Var(self, node: Var) -> None:
        var_name = node.value
        if self.current_scope is None:
            self.error(error_code=ErrorCode.NULL_POINTER, token=node.token)
            return
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)

    def visit_Num(self, node: Num) -> None:
        pass

    def visit_UnaryOp(self, node: UnaryOp) -> None:
        pass

    def visit_ProcedureDecl(self, node: ProcedureDecl) -> None:
        proc_name = node.proc_name
        proc_symbol = ProcedureSymbol(proc_name)
        if self.current_scope is None:
            raise CurrentScopeMissingError()
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
            raise CurrentScopeMissingError()
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


class CallStack:
    def __init__(self) -> None:
        self._records: list[ActivationRecord] = []

    def push(self, ar: ActivationRecord) -> None:
        self._records.append(ar)

    def pop(self) -> ActivationRecord:
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
        self.members: dict[str, Any] = {}

    def __setitem__(self, key: str, value) -> None:
        self.members[key] = value

    def __getitem__(self, key: str):
        return self.members[key]

    def get(self, key):
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
        # Do nothing
        pass

    def visit_Type(self, node: Type) -> None:
        # Do nothing
        pass

    def visit_BinOp(self, node: BinOp) -> Number:
        if node.op.type == TokenType.PLUS:
            return cast(Number, self.visit(node.left) + self.visit(node.right))
        elif node.op.type == TokenType.MINUS:
            return cast(Number, self.visit(node.left) - self.visit(node.right))
        elif node.op.type == TokenType.MUL:
            return cast(Number, self.visit(node.left) * self.visit(node.right))
        elif node.op.type == TokenType.INTEGER_DIV:
            return cast(Number, self.visit(node.left) // self.visit(node.right))
        elif node.op.type == TokenType.FLOAT_DIV:
            return float(self.visit(node.left)) / float(self.visit(node.right))
        raise UnknownOperatorError(ErrorCode.UNKNOWN_BIN_OP, node.token)

    def visit_Num(self, node: Num):
        return node.value

    def visit_UnaryOp(self, node: UnaryOp) -> Number:
        op = node.op.type
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
        ar[var_name] = var_value

    def visit_Var(self, node: Var) -> Any:
        var_name = node.value

        ar = self.call_stack.peek()
        var_value = ar.get(var_name)

        return var_value

    def visit_NoOp(self, node: NoOp) -> None:
        pass

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

    def visit_FunctionDecl(self, node: FunctionDecl) -> None:
        pass

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
