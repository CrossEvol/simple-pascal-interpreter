"""
Unit tests for module-specific error classes.

This module tests all the module-related error classes to ensure they:
1. Store the correct attributes
2. Generate appropriate error messages
3. Handle edge cases properly
4. Provide helpful information for debugging
"""

import unittest
from src.error import (
    ModuleError,
    ModuleNotFoundError,
    CircularDependencyError,
    SymbolNotFoundInModuleError,
    InterfaceImplementationMismatchError,
    SymbolVisibilityError,
    TypeResolutionError
)
from src.spi_token import Token, TokenType


class TestModuleError(unittest.TestCase):
    """Test the base ModuleError class."""
    
    def test_module_error_inheritance(self):
        """Test that ModuleError inherits from Error properly."""
        error = ModuleError(message="Test module error")
        self.assertIn("ModuleError", str(error))
        self.assertIn("Test module error", str(error))


class TestModuleNotFoundError(unittest.TestCase):
    """Test ModuleNotFoundError class."""
    
    def test_module_not_found_error_basic(self):
        """Test basic ModuleNotFoundError functionality."""
        module_name = "TestModule"
        search_paths = [".", "./stdlib"]
        
        error = ModuleNotFoundError(module_name, search_paths)
        
        # Check attributes
        self.assertEqual(error.module_name, module_name)
        self.assertEqual(error.search_paths, search_paths)
        
        # Check error message
        error_message = str(error)
        self.assertIn("TestModule", error_message)
        self.assertIn("not found", error_message)
        self.assertIn(".", error_message)
        self.assertIn("./stdlib", error_message)
    
    def test_module_not_found_error_empty_search_paths(self):
        """Test ModuleNotFoundError with empty search paths."""
        error = ModuleNotFoundError("EmptyModule", [])
        
        self.assertEqual(error.module_name, "EmptyModule")
        self.assertEqual(error.search_paths, [])
        self.assertIn("EmptyModule", str(error))
        self.assertIn("[]", str(error))
    
    def test_module_not_found_error_single_path(self):
        """Test ModuleNotFoundError with single search path."""
        error = ModuleNotFoundError("SingleModule", ["./custom"])
        
        self.assertEqual(error.search_paths, ["./custom"])
        self.assertIn("./custom", str(error))
    
    def test_module_not_found_error_multiple_paths(self):
        """Test ModuleNotFoundError with multiple search paths."""
        paths = [".", "./stdlib", "./custom", "./lib"]
        error = ModuleNotFoundError("MultiModule", paths)
        
        self.assertEqual(error.search_paths, paths)
        error_message = str(error)
        for path in paths:
            self.assertIn(path, error_message)


class TestCircularDependencyError(unittest.TestCase):
    """Test CircularDependencyError class."""
    
    def test_circular_dependency_error_basic(self):
        """Test basic CircularDependencyError functionality."""
        dependency_chain = ["ModuleA", "ModuleB", "ModuleA"]
        
        error = CircularDependencyError(dependency_chain)
        
        # Check attributes
        self.assertEqual(error.dependency_chain, dependency_chain)
        self.assertEqual(error.suggestions, [])
        
        # Check error message
        error_message = str(error)
        self.assertIn("Circular dependency detected", error_message)
        self.assertIn("ModuleA -> ModuleB -> ModuleA", error_message)
    
    def test_circular_dependency_error_with_suggestions(self):
        """Test CircularDependencyError with suggestions."""
        dependency_chain = ["A", "B", "C", "A"]
        suggestions = [
            "Consider refactoring module A to remove dependency on C",
            "Move shared functionality to a separate module"
        ]
        
        error = CircularDependencyError(dependency_chain, suggestions)
        
        # Check attributes
        self.assertEqual(error.dependency_chain, dependency_chain)
        self.assertEqual(error.suggestions, suggestions)
        
        # Check error message includes suggestions
        error_message = str(error)
        self.assertIn("A -> B -> C -> A", error_message)
        for suggestion in suggestions:
            self.assertIn(suggestion, error_message)
    
    def test_circular_dependency_error_empty_suggestions(self):
        """Test CircularDependencyError with empty suggestions list."""
        dependency_chain = ["X", "Y", "X"]
        
        error = CircularDependencyError(dependency_chain, [])
        
        self.assertEqual(error.suggestions, [])
        error_message = str(error)
        self.assertIn("X -> Y -> X", error_message)
        # Should not contain suggestion section
        self.assertNotIn("\n\n", error_message)
    
    def test_circular_dependency_error_single_module(self):
        """Test CircularDependencyError with self-dependency."""
        dependency_chain = ["SelfModule", "SelfModule"]
        
        error = CircularDependencyError(dependency_chain)
        
        error_message = str(error)
        self.assertIn("SelfModule -> SelfModule", error_message)
    
    def test_circular_dependency_error_long_chain(self):
        """Test CircularDependencyError with long dependency chain."""
        dependency_chain = ["A", "B", "C", "D", "E", "A"]
        
        error = CircularDependencyError(dependency_chain)
        
        error_message = str(error)
        self.assertIn("A -> B -> C -> D -> E -> A", error_message)


class TestSymbolNotFoundInModuleError(unittest.TestCase):
    """Test SymbolNotFoundInModuleError class."""
    
    def test_symbol_not_found_error_basic(self):
        """Test basic SymbolNotFoundInModuleError functionality."""
        symbol_name = "MyFunction"
        module_name = "TestModule"
        available_symbols = ["Function1", "Function2", "Variable1"]
        
        error = SymbolNotFoundInModuleError(symbol_name, module_name, available_symbols)
        
        # Check attributes
        self.assertEqual(error.symbol_name, symbol_name)
        self.assertEqual(error.module_name, module_name)
        self.assertEqual(error.available_symbols, available_symbols)
        
        # Check error message
        error_message = str(error)
        self.assertIn("MyFunction", error_message)
        self.assertIn("TestModule", error_message)
        self.assertIn("not found", error_message)
        self.assertIn("Available symbols", error_message)
        for symbol in available_symbols:
            self.assertIn(symbol, error_message)
    
    def test_symbol_not_found_error_empty_available_symbols(self):
        """Test SymbolNotFoundInModuleError with no available symbols."""
        error = SymbolNotFoundInModuleError("MissingSymbol", "EmptyModule", [])
        
        self.assertEqual(error.available_symbols, [])
        error_message = str(error)
        self.assertIn("MissingSymbol", error_message)
        self.assertIn("EmptyModule", error_message)
        self.assertIn("[]", error_message)
    
    def test_symbol_not_found_error_single_available_symbol(self):
        """Test SymbolNotFoundInModuleError with single available symbol."""
        error = SymbolNotFoundInModuleError("Wrong", "Module", ["Correct"])
        
        self.assertEqual(error.available_symbols, ["Correct"])
        error_message = str(error)
        self.assertIn("Wrong", error_message)
        self.assertIn("Correct", error_message)
    
    def test_symbol_not_found_error_many_available_symbols(self):
        """Test SymbolNotFoundInModuleError with many available symbols."""
        available = [f"Symbol{i}" for i in range(10)]
        error = SymbolNotFoundInModuleError("NotFound", "BigModule", available)
        
        self.assertEqual(len(error.available_symbols), 10)
        error_message = str(error)
        self.assertIn("NotFound", error_message)
        self.assertIn("BigModule", error_message)
        # Check that all symbols are mentioned
        for symbol in available:
            self.assertIn(symbol, error_message)


class TestInterfaceImplementationMismatchError(unittest.TestCase):
    """Test InterfaceImplementationMismatchError class."""
    
    def test_interface_implementation_mismatch_error_basic(self):
        """Test basic InterfaceImplementationMismatchError functionality."""
        symbol_name = "TestFunction"
        interface_sig = "function TestFunction(x: integer): string"
        impl_sig = "function TestFunction(x: real): string"
        
        error = InterfaceImplementationMismatchError(symbol_name, interface_sig, impl_sig)
        
        # Check attributes
        self.assertEqual(error.symbol_name, symbol_name)
        self.assertEqual(error.interface_sig, interface_sig)
        self.assertEqual(error.impl_sig, impl_sig)
        
        # Check error message
        error_message = str(error)
        self.assertIn("TestFunction", error_message)
        self.assertIn("mismatch", error_message)
        self.assertIn("interface=", error_message)
        self.assertIn("implementation=", error_message)
        self.assertIn("function TestFunction(x: integer): string", error_message)
        self.assertIn("function TestFunction(x: real): string", error_message)
    
    def test_interface_implementation_mismatch_error_procedure(self):
        """Test InterfaceImplementationMismatchError with procedure."""
        symbol_name = "TestProcedure"
        interface_sig = "procedure TestProcedure(var x: integer)"
        impl_sig = "procedure TestProcedure(x: integer)"
        
        error = InterfaceImplementationMismatchError(symbol_name, interface_sig, impl_sig)
        
        error_message = str(error)
        self.assertIn("TestProcedure", error_message)
        self.assertIn("var x: integer", error_message)
        self.assertIn("x: integer", error_message)
    
    def test_interface_implementation_mismatch_error_return_type(self):
        """Test InterfaceImplementationMismatchError with different return types."""
        symbol_name = "Calculate"
        interface_sig = "function Calculate(): integer"
        impl_sig = "function Calculate(): real"
        
        error = InterfaceImplementationMismatchError(symbol_name, interface_sig, impl_sig)
        
        error_message = str(error)
        self.assertIn("Calculate", error_message)
        self.assertIn(": integer", error_message)
        self.assertIn(": real", error_message)
    
    def test_interface_implementation_mismatch_error_empty_signatures(self):
        """Test InterfaceImplementationMismatchError with empty signatures."""
        error = InterfaceImplementationMismatchError("Empty", "", "")
        
        self.assertEqual(error.interface_sig, "")
        self.assertEqual(error.impl_sig, "")
        error_message = str(error)
        self.assertIn("Empty", error_message)
        self.assertIn("interface=''", error_message)
        self.assertIn("implementation=''", error_message)


class TestSymbolVisibilityError(unittest.TestCase):
    """Test SymbolVisibilityError class."""
    
    def test_symbol_visibility_error_basic(self):
        """Test basic SymbolVisibilityError functionality."""
        symbol_name = "PrivateFunction"
        symbol_visibility = "implementation"
        requesting_module = "ClientModule"
        owning_module = "ServerModule"
        
        error = SymbolVisibilityError(symbol_name, symbol_visibility, requesting_module, owning_module)
        
        # Check attributes
        self.assertEqual(error.symbol_name, symbol_name)
        self.assertEqual(error.symbol_visibility, symbol_visibility)
        self.assertEqual(error.requesting_module, requesting_module)
        self.assertEqual(error.owning_module, owning_module)
        
        # Check error message
        error_message = str(error)
        self.assertIn("PrivateFunction", error_message)
        self.assertIn("implementation", error_message)
        self.assertIn("ClientModule", error_message)
        self.assertIn("ServerModule", error_message)
        self.assertIn("not accessible", error_message)
    
    def test_symbol_visibility_error_interface_access(self):
        """Test SymbolVisibilityError for interface symbol access."""
        error = SymbolVisibilityError("PublicFunc", "interface", "Client", "Server")
        
        error_message = str(error)
        self.assertIn("PublicFunc", error_message)
        self.assertIn("interface", error_message)
    
    def test_symbol_visibility_error_private_access(self):
        """Test SymbolVisibilityError for private symbol access."""
        error = SymbolVisibilityError("InternalVar", "private", "External", "Internal")
        
        error_message = str(error)
        self.assertIn("InternalVar", error_message)
        self.assertIn("private", error_message)
        self.assertIn("External", error_message)
        self.assertIn("Internal", error_message)


class TestTypeResolutionError(unittest.TestCase):
    """Test TypeResolutionError class."""
    
    def test_type_resolution_error_basic(self):
        """Test basic TypeResolutionError functionality."""
        message = "Unknown type 'MyClass' at line 15"
        
        error = TypeResolutionError(message)
        
        # Check attributes
        self.assertEqual(error.suggestions, [])
        self.assertIsNone(error.token)
        
        # Check error message
        error_message = str(error)
        self.assertIn("TypeResolutionError", error_message)
        self.assertIn("Unknown type 'MyClass' at line 15", error_message)
        # Should not contain suggestions section when no suggestions provided
        self.assertNotIn("Did you mean:", error_message)
    
    def test_type_resolution_error_with_suggestions(self):
        """Test TypeResolutionError with type suggestions."""
        message = "Unknown type 'MyClss'"
        suggestions = ["MyClass", "MyRecord"]
        
        error = TypeResolutionError(message, suggestions)
        
        # Check attributes
        self.assertEqual(error.suggestions, suggestions)
        
        # Check error message includes suggestions
        error_message = str(error)
        self.assertIn("Unknown type 'MyClss'", error_message)
        self.assertIn("Did you mean: MyClass, MyRecord?", error_message)
    
    def test_type_resolution_error_with_token(self):
        """Test TypeResolutionError with token information."""
        token = Token(TokenType.ID, "UnknownType", lineno=10, column=5)
        message = "Type 'UnknownType' not found"
        
        error = TypeResolutionError(message, token=token)
        
        # Check attributes
        self.assertEqual(error.token, token)
        self.assertEqual(error.suggestions, [])
        
        # Check error message
        error_message = str(error)
        self.assertIn("Type 'UnknownType' not found", error_message)
    
    def test_type_resolution_error_with_suggestions_and_token(self):
        """Test TypeResolutionError with both suggestions and token."""
        token = Token(TokenType.ID, "Integr", lineno=5, column=12)
        message = "Unknown type 'Integr' at line 5, column 12"
        suggestions = ["Integer", "Int"]
        
        error = TypeResolutionError(message, suggestions, token)
        
        # Check attributes
        self.assertEqual(error.token, token)
        self.assertEqual(error.suggestions, suggestions)
        
        # Check error message
        error_message = str(error)
        self.assertIn("Unknown type 'Integr' at line 5, column 12", error_message)
        self.assertIn("Did you mean: Integer, Int?", error_message)
    
    def test_type_resolution_error_empty_suggestions(self):
        """Test TypeResolutionError with empty suggestions list."""
        message = "Type not found"
        
        error = TypeResolutionError(message, [])
        
        self.assertEqual(error.suggestions, [])
        error_message = str(error)
        self.assertIn("Type not found", error_message)
        self.assertNotIn("Did you mean:", error_message)
    
    def test_type_resolution_error_single_suggestion(self):
        """Test TypeResolutionError with single suggestion."""
        message = "Unknown type 'Bolean'"
        suggestions = ["Boolean"]
        
        error = TypeResolutionError(message, suggestions)
        
        error_message = str(error)
        self.assertIn("Did you mean: Boolean?", error_message)
    
    def test_type_resolution_error_many_suggestions(self):
        """Test TypeResolutionError with many suggestions."""
        message = "Unknown type 'T'"
        suggestions = ["TMap", "TArray", "TRecord", "TClass", "TEnum"]
        
        error = TypeResolutionError(message, suggestions)
        
        error_message = str(error)
        self.assertIn("Did you mean: TMap, TArray, TRecord, TClass, TEnum?", error_message)
    
    def test_type_resolution_error_inheritance(self):
        """Test that TypeResolutionError inherits from SemanticError properly."""
        from src.error import SemanticError
        
        error = TypeResolutionError("Test error")
        
        # Check inheritance
        self.assertIsInstance(error, SemanticError)
        self.assertIn("TypeResolutionError", str(error))
    
    def test_type_resolution_error_message_formatting(self):
        """Test TypeResolutionError message formatting with various inputs."""
        # Test with special characters in type name
        error1 = TypeResolutionError("Unknown type 'My_Type$1'", ["MyType1"])
        self.assertIn("My_Type$1", str(error1))
        self.assertIn("MyType1", str(error1))
        
        # Test with long message
        long_message = "A very long error message that describes in detail what went wrong with type resolution"
        error2 = TypeResolutionError(long_message, ["SuggestedType"])
        error_str = str(error2)
        self.assertIn(long_message, error_str)
        self.assertIn("SuggestedType", error_str)


if __name__ == "__main__":
    unittest.main()