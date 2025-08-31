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


class SyntaxError(ParserError):
    """Raised for pure syntax errors during parsing phase."""
    
    def __init__(
        self, 
        message: str, 
        token: Token = None,
        expected: str = None,
        got: str = None
    ) -> None:
        self.token = token
        self.expected = expected
        self.got = got
        
        # Format syntax error message with location
        formatted_message = self._format_syntax_message(message)
        
        super().__init__(message=formatted_message, token=token)
    
    def _format_syntax_message(self, base_message: str) -> str:
        """Format a clear syntax error message with location information."""
        lines = []
        
        # Main error message with location if available
        if self.token and hasattr(self.token, 'lineno') and self.token.lineno > 0:
            location = f" at line {self.token.lineno}"
            if hasattr(self.token, 'column') and self.token.column > 0:
                location += f", column {self.token.column}"
            lines.append(f"Syntax Error: {base_message}{location}")
        else:
            lines.append(f"Syntax Error: {base_message}")
        
        # Add expected vs got information if available
        if self.expected and self.got:
            lines.append(f"  Expected: {self.expected}")
            lines.append(f"  Got: {self.got}")
        elif self.expected:
            lines.append(f"  Expected: {self.expected}")
        
        return '\n'.join(lines)


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
    
    def __init__(
        self, 
        message: str, 
        suggestions: list[str] = None, 
        token: Token = None,
        context: str = None,
        available_types: list[str] = None,
        error_phase: str = "type resolution"
    ) -> None:
        self.suggestions = suggestions or []
        self.token = token
        self.context = context
        self.available_types = available_types or []
        self.error_phase = error_phase
        
        # Format comprehensive error message
        formatted_message = self._format_comprehensive_message(message)
        
        super().__init__(message=formatted_message, token=token)
    
    def _format_comprehensive_message(self, base_message: str) -> str:
        """Format a comprehensive error message with all available context."""
        lines = []
        
        # Main error message with location if available
        if self.token and hasattr(self.token, 'lineno') and self.token.lineno > 0:
            location = f" at line {self.token.lineno}"
            if hasattr(self.token, 'column') and self.token.column > 0:
                location += f", column {self.token.column}"
            lines.append(f"Semantic Error ({self.error_phase}): {base_message}{location}")
        else:
            lines.append(f"Semantic Error ({self.error_phase}): {base_message}")
        
        # Add context information if available
        if self.context:
            lines.append(f"  Context: {self.context}")
        
        # Add suggestions if available
        if self.suggestions:
            if len(self.suggestions) == 1:
                lines.append(f"  Did you mean: {self.suggestions[0]}?")
            else:
                lines.append(f"  Did you mean: {', '.join(self.suggestions)}?")
        
        # Add available types if provided and not too many
        if self.available_types and len(self.available_types) <= 10:
            lines.append(f"  Available types: {', '.join(self.available_types)}")
        elif self.available_types and len(self.available_types) > 10:
            # Show first 8 and indicate there are more
            shown_types = self.available_types[:8]
            remaining_count = len(self.available_types) - 8
            lines.append(f"  Available types: {', '.join(shown_types)} (and {remaining_count} more)")
        
        return '\n'.join(lines)


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
