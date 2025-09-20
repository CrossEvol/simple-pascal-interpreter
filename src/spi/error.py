"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

from enum import Enum


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
    SEMANTIC_ERROR = "Semantic error"

    # Lexer errors
    LEXER_INVALID_CHARACTER = "Lexer invalid character"
    LEXER_STRING_ERROR = "Lexer string error"

    # Parser errors
    PARSER_UNEXPECTED_TOKEN = "Parser unexpected token"

    # Semantic errors
    SEMANTIC_UNKNOWN_TYPE = "Semantic unknown type"
    SEMANTIC_UNKNOWN_ENUM = "Semantic unknown enum"
    SEMANTIC_UNKNOWN_ARRAY_ELEMENT_TYPE = "Semantic unknown array element type"
    SEMANTIC_UNKNOWN_SYMBOL = "Semantic unknown symbol"
    SEMANTIC_UNKNOWN_BOOLEAN = "Semantic unknown boolean"
    SEMANTIC_UNSUPPORTED_TYPE = "Semantic unsupported type"
    SEMANTIC_INCOMPATIBLE_TYPE = "Semantic incompatible type"
    SEMANTIC_CHAR_TOO_MANY_CHARS = "Semantic char too many characters"
    SEMANTIC_CHAR_INVALID_ASCII = "Semantic char invalid ASCII value"
    SEMANTIC_DUPLICATE_CASE_LABEL = "Semantic duplicate case label"

    # Interpreter errors
    INTERPRETER_STATIC_ARRAY_MODIFY_LENGTH = "Interpreter static array modify length"
    INTERPRETER_UNKNOWN_BUILTIN_FUNCTION = "Interpreter unknown builtin function"
    INTERPRETER_UNKNOWN_FUNCTION = "Interpreter unknown function"
    INTERPRETER_UNKNOWN_BUILTIN_PROCEDURE = "Interpreter unknown builtin procedure"
    INTERPRETER_UNKNOWN_PROCEDURE = "Interpreter unknown procedure"
    INTERPRETER_ARRAY_RANGE_INVALID = "Interpreter array range invalid"
    INTERPRETER_UNKNOWN_OPERATOR = "Interpreter unknown operator"
    INTERPRETER_UNKNOWN_ENUM = "Interpreter unknown enum"
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
