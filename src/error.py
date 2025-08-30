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
    PROPERTY_ACCESS = "Property access should be handled by expr_get"


class Error(Exception):
    def __init__(self, error_code=None, token=None, message=None) -> None:
        self.error_code = error_code
        self.token = token
        # add exception class name before the message
        self.message = f"{self.__class__.__name__}: {message}"
        super().__init__(self.message)
    
    def __str__(self) -> str:
        return self.message


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


class TypeResolutionError(SemanticError):
    """Raised when type resolution fails during semantic analysis or interpretation."""
    
    def __init__(self, message: str, suggestions: list[str] = None, token: Token = None) -> None:
        self.suggestions = suggestions or []
        self.token = token
        
        # Format the error message with suggestions if available
        formatted_message = message
        if self.suggestions:
            formatted_message += f"\n  Did you mean: {', '.join(self.suggestions)}?"
        
        super().__init__(message=formatted_message, token=token)


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


# Add the new error class
class PropertyAccessError(ParserError):
    pass


###############################################################################
#                                                                             #
#  ModuleError                                                                #
#                                                                             #
###############################################################################


class ModuleError(Error):
    """Base class for module-related errors."""
    pass


class ModuleNotFoundError(ModuleError):
    """Raised when a module file cannot be found in search paths."""
    
    def __init__(self, module_name: str, search_paths: list[str]) -> None:
        self.module_name = module_name
        self.search_paths = search_paths
        message = f"Module '{module_name}' not found in search paths: {search_paths}"
        super().__init__(message=message)


class CircularDependencyError(ModuleError):
    """Raised when circular dependencies are detected between modules."""
    
    def __init__(self, dependency_chain: list[str], suggestions: list[str] = None) -> None:
        self.dependency_chain = dependency_chain
        self.suggestions = suggestions or []
        
        chain_str = " -> ".join(dependency_chain)
        message = f"Circular dependency detected: {chain_str}"
        
        if self.suggestions:
            message += "\n\n" + "\n".join(self.suggestions)
        
        super().__init__(message=message)


class SymbolNotFoundInModuleError(ModuleError):
    """Raised when a symbol is not found in a specific module."""
    
    def __init__(self, symbol_name: str, module_name: str, available_symbols: list[str]) -> None:
        self.symbol_name = symbol_name
        self.module_name = module_name
        self.available_symbols = available_symbols
        message = f"Symbol '{symbol_name}' not found in module '{module_name}'. Available symbols: {available_symbols}"
        super().__init__(message=message)


class InterfaceImplementationMismatchError(ModuleError):
    """Raised when interface and implementation signatures don't match."""
    
    def __init__(self, symbol_name: str, interface_sig: str, impl_sig: str) -> None:
        self.symbol_name = symbol_name
        self.interface_sig = interface_sig
        self.impl_sig = impl_sig
        message = f"Interface/implementation mismatch for '{symbol_name}': interface='{interface_sig}', implementation='{impl_sig}'"
        super().__init__(message=message)


class SymbolVisibilityError(ModuleError):
    """Raised when trying to access a symbol that is not visible from the current context."""
    
    def __init__(self, symbol_name: str, symbol_visibility: str, requesting_module: str, owning_module: str) -> None:
        self.symbol_name = symbol_name
        self.symbol_visibility = symbol_visibility
        self.requesting_module = requesting_module
        self.owning_module = owning_module
        message = f"Symbol '{symbol_name}' with visibility '{symbol_visibility}' in module '{owning_module}' is not accessible from module '{requesting_module}'"
        super().__init__(message=message)
