"""
Unit tests for enhanced error messages in type resolution failures.

Tests the implementation of clear distinction between syntax and semantic errors,
contextual information in error messages, line/column information, and helpful
suggestions for similar type names.
"""

import unittest
from unittest.mock import Mock, patch

from src.error import TypeResolutionError, SyntaxError
from src.type_resolver import TypeResolver, TypeResolutionContext
from src.spi_ast import UnresolvedType
from src.spi_token import Token, TokenType
from src.module import ModuleRegistry


class TestEnhancedErrorMessages(unittest.TestCase):
    """Test enhanced error message formatting and content."""

    def setUp(self):
        """Set up test fixtures."""
        self.module_registry = Mock(spec=ModuleRegistry)
        self.type_resolver = TypeResolver(self.module_registry)

    def test_syntax_error_formatting_with_location(self):
        """Test that syntax errors include proper location information."""
        token = Token(TokenType.ID, "BadSyntax", lineno=15, column=8)

        error = SyntaxError(
            message="Expected type specification",
            token=token,
            expected="type name or primitive type",
            got="invalid token",
        )

        error_str = str(error)

        # Check that it's clearly marked as a syntax error
        self.assertIn("Syntax Error:", error_str)

        # Check location information
        self.assertIn("at line 15", error_str)
        self.assertIn("column 8", error_str)

        # Check expected vs got information
        self.assertIn("Expected: type name or primitive type", error_str)
        self.assertIn("Got: invalid token", error_str)

    def test_syntax_error_formatting_without_location(self):
        """Test syntax error formatting when location information is not available."""
        error = SyntaxError(
            message="Malformed type specification", expected="valid type syntax"
        )

        error_str = str(error)

        # Should still be marked as syntax error
        self.assertIn("Syntax Error:", error_str)

        # Should not contain location info
        self.assertNotIn("at line", error_str)
        self.assertNotIn("column", error_str)

        # Should contain expected information
        self.assertIn("Expected: valid type syntax", error_str)

    def test_type_resolution_error_comprehensive_formatting(self):
        """Test comprehensive formatting of type resolution errors."""
        token = Token(TokenType.ID, "UnknownType", lineno=25, column=12)

        error = TypeResolutionError(
            message="Unknown type 'UnknownType'",
            suggestions=["UnknownClass", "UnknownRecord"],
            token=token,
            context="in variable declaration with imported modules",
            available_types=["Integer", "Real", "Class:MyClass", "Module.ExternalType"],
            error_phase="variable type resolution",
        )

        error_str = str(error)

        # Check semantic error marking with phase
        self.assertIn("Semantic Error (variable type resolution):", error_str)

        # Check location information
        self.assertIn("at line 25", error_str)
        self.assertIn("column 12", error_str)

        # Check context information
        self.assertIn(
            "Context: in variable declaration with imported modules", error_str
        )

        # Check suggestions
        self.assertIn("Did you mean: UnknownClass, UnknownRecord?", error_str)

        # Check available types
        self.assertIn(
            "Available types: Integer, Real, Class:MyClass, Module.ExternalType",
            error_str,
        )

    def test_type_resolution_error_with_many_available_types(self):
        """Test that error messages handle many available types gracefully."""
        token = Token(TokenType.ID, "MissingType", lineno=10, column=5)

        # Create a list with more than 10 types
        many_types = [f"Type{i}" for i in range(15)]

        error = TypeResolutionError(
            message="Unknown type 'MissingType'",
            token=token,
            available_types=many_types,
            error_phase="type resolution",
        )

        error_str = str(error)

        # Should show first 8 types and indicate there are more
        self.assertIn(
            "Available types: Type0, Type1, Type2, Type3, Type4, Type5, Type6, Type7 (and 7 more)",
            error_str,
        )

    def test_type_resolution_error_single_suggestion(self):
        """Test formatting when there's only one suggestion."""
        token = Token(TokenType.ID, "Integr", lineno=5, column=1)

        error = TypeResolutionError(
            message="Unknown type 'Integr'",
            suggestions=["Integer"],
            token=token,
            error_phase="type resolution",
        )

        error_str = str(error)

        # Should use singular form for single suggestion
        self.assertIn("Did you mean: Integer?", error_str)

    def test_type_resolution_error_no_location_info(self):
        """Test error formatting when token has no location information."""
        token = Token(TokenType.ID, "BadType")  # No lineno/column

        error = TypeResolutionError(
            message="Unknown type 'BadType'", token=token, error_phase="type resolution"
        )

        error_str = str(error)

        # Should not include location information
        self.assertNotIn("at line", error_str)
        self.assertNotIn("column", error_str)

        # Should still be properly formatted
        self.assertIn("Semantic Error (type resolution):", error_str)

    def test_type_resolver_enhanced_error_context_building(self):
        """Test that TypeResolver builds comprehensive error context."""
        token = Token(TokenType.ID, "MissingType", lineno=20, column=15)
        unresolved_type = UnresolvedType(token, "MissingType")

        context = TypeResolutionContext(
            current_module="TestModule",
            imported_modules=["Math", "Utils"],
            local_classes=["MyClass", "AnotherClass"],
            local_enums=["Status", "Priority"],
            local_records=["Point", "Rectangle"],
            module_registry=self.module_registry,
        )

        # Mock module registry to return empty modules
        mock_module = Mock()
        mock_module.is_loaded = True
        mock_module.interface_symbols.get_interface_symbols.return_value = {}
        self.module_registry.get_module.return_value = mock_module

        with self.assertRaises(TypeResolutionError) as cm:
            self.type_resolver.resolve_type(unresolved_type, context)

        error = cm.exception
        error_str = str(error)

        # Check that context includes module information
        self.assertIn("in module 'TestModule'", error_str)
        self.assertIn("with imports: Math, Utils", error_str)
        self.assertIn("2 classes", error_str)
        self.assertIn("2 enums", error_str)
        self.assertIn("2 records", error_str)

    def test_type_resolver_available_types_collection(self):
        """Test that TypeResolver collects all available types correctly."""
        token = Token(TokenType.ID, "UnknownType", lineno=1, column=1)
        unresolved_type = UnresolvedType(token, "UnknownType")

        context = TypeResolutionContext(
            local_classes=["UserClass"],
            local_enums=["Color"],
            local_records=["Point"],
            imported_modules=["TestModule"],
            module_registry=self.module_registry,
        )

        # Mock module with some interface symbols
        mock_module = Mock()
        mock_module.is_loaded = True
        mock_module.interface_symbols.get_interface_symbols.return_value = {
            "ExternalClass": Mock(),
            "ExternalEnum": Mock(),
        }
        self.module_registry.get_module.return_value = mock_module

        with self.assertRaises(TypeResolutionError) as cm:
            self.type_resolver.resolve_type(unresolved_type, context)

        error = cm.exception
        error_str = str(error)

        # Check that all types are included
        self.assertIn("Boolean", error_str)  # Primitive types
        self.assertIn("Integer", error_str)
        self.assertIn("Class:UserClass", error_str)  # Local types with prefixes
        self.assertIn("Enum:Color", error_str)
        self.assertIn("Record:Point", error_str)
        self.assertIn("TestModule.ExternalClass", error_str)  # Module types
        self.assertIn("TestModule.ExternalEnum", error_str)

    def test_qualified_type_resolution_error_formatting(self):
        """Test error formatting for qualified type resolution failures."""
        token = Token(TokenType.ID, "MissingType", lineno=30, column=20)

        # Mock module registry to return None (module not found)
        self.module_registry.get_module.return_value = None
        self.module_registry.loaded_modules = {
            "Math": Mock(),
            "Utils": Mock(),
            "Graphics": Mock(),
        }

        with self.assertRaises(TypeResolutionError) as cm:
            self.type_resolver.resolve_qualified_type(
                "NonExistent", "MissingType", token
            )

        error = cm.exception
        error_str = str(error)

        # Check error phase and context
        self.assertIn("Semantic Error (module resolution):", error_str)
        self.assertIn(
            "qualified type resolution for 'NonExistent.MissingType'", error_str
        )

        # Check available modules are listed
        self.assertIn("Module:Math", error_str)
        self.assertIn("Module:Utils", error_str)
        self.assertIn("Module:Graphics", error_str)

    def test_qualified_type_symbol_not_found_error(self):
        """Test error when qualified type's symbol is not found in module."""
        token = Token(TokenType.ID, "MissingSymbol", lineno=40, column=25)

        # Mock module that exists but doesn't have the symbol
        mock_module = Mock()
        mock_module.is_loaded = True
        mock_module.interface_symbols.get_interface_symbols.return_value = {
            "AvailableType1": Mock(),
            "AvailableType2": Mock(),
            "SimilarSymbol": Mock(),
        }
        self.module_registry.get_module.return_value = mock_module

        with self.assertRaises(TypeResolutionError) as cm:
            self.type_resolver.resolve_qualified_type(
                "ExistingModule", "MissingSymbol", token
            )

        error = cm.exception
        error_str = str(error)

        # Check error details
        self.assertIn("Semantic Error (qualified type resolution):", error_str)
        self.assertIn(
            "Type 'MissingSymbol' not found in module 'ExistingModule'", error_str
        )
        self.assertIn("qualified type resolution in module 'ExistingModule'", error_str)

        # Check available types from module
        self.assertIn("ExistingModule.AvailableType1", error_str)
        self.assertIn("ExistingModule.AvailableType2", error_str)
        self.assertIn("ExistingModule.SimilarSymbol", error_str)

    def test_error_message_distinction_syntax_vs_semantic(self):
        """Test clear distinction between syntax and semantic error messages."""
        # Syntax error
        syntax_token = Token(TokenType.ID, "BadSyntax", lineno=10, column=5)
        syntax_error = SyntaxError(
            message="Expected type specification", token=syntax_token
        )

        # Semantic error
        semantic_token = Token(TokenType.ID, "UnknownType", lineno=15, column=8)
        semantic_error = TypeResolutionError(
            message="Unknown type 'UnknownType'",
            token=semantic_token,
            error_phase="type resolution",
        )

        syntax_str = str(syntax_error)
        semantic_str = str(semantic_error)

        # Syntax error should be clearly marked as syntax
        self.assertIn("Syntax Error:", syntax_str)
        self.assertNotIn("Semantic Error", syntax_str)

        # Semantic error should be clearly marked as semantic
        self.assertIn("Semantic Error", semantic_str)
        self.assertNotIn("Syntax Error:", semantic_str)

        # Both should have location information
        self.assertIn("at line 10", syntax_str)
        self.assertIn("at line 15", semantic_str)

    def test_error_suggestions_fuzzy_matching_quality(self):
        """Test that error suggestions use good fuzzy matching."""
        token = Token(TokenType.ID, "Integr", lineno=1, column=1)
        unresolved_type = UnresolvedType(token, "Integr")

        context = TypeResolutionContext(
            local_classes=["Integer", "IntegerArray", "RealNumber", "StringType"],
            module_registry=self.module_registry,
        )

        # Mock empty module registry
        self.module_registry.get_module.return_value = None

        with self.assertRaises(TypeResolutionError) as cm:
            self.type_resolver.resolve_type(unresolved_type, context)

        error = cm.exception

        # Should suggest "Integer" as it's the closest match to "Integr"
        self.assertIn("Integer", error.suggestions)

        # Should not suggest very different types
        self.assertNotIn("StringType", error.suggestions)


if __name__ == "__main__":
    unittest.main()
