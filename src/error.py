from enum import Enum

from src.spi_token import Token


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
    MODIFY_CONST_NOT_ALLOW = "constant can not be modified"


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


class InvalidCaseStatementError(ParserError):
    pass


class InvalidEnumDeclError(ParserError):
    pass


class InvalidRecordDeclError(ParserError):
    pass


class InvalidConstAssignError(ParserError):
    pass


class VarDuplicateInScopeError(ParserError):
    pass


class UnknownLiteralError(ParserError):
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


class UnknownEnumTypeError(UnknownTypeError):
    pass


class UnknownRecordTypeError(UnknownTypeError):
    pass


class UnknownRecordFieldError(UnknownTypeError):
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
