"""
Integration tests for TypeResolver with real module scenarios.

Tests more complex scenarios involving multiple modules, circular dependencies,
and real-world type resolution patterns.
"""

import pytest
import tempfile
import os
from unittest.mock import Mock

from src.type_resolver import TypeResolver, TypeResolutionContext
from src.spi_ast import UnresolvedType, ClassType, EnumType, RecordType, PrimitiveType
from src.spi_token import Token, TokenType
from src.error import TypeResolutionError
from src.module import ModuleRegistry, Unit, ModuleSymbolTable
from src.symbol import Symbol, ClassSymbol, EnumSymbol, RecordSymbol


class TestTypeResolverIntegration:
    """Integration tests for TypeResolver with realistic scenarios."""

    def setup_method(self):
        """Set up test fixtures before each test method."""
        self.module_registry = ModuleRegistry()
        self.type_resolver = TypeResolver(self.module_registry)
        
        # Create test token
        self.test_token = Token(
            type=TokenType.ID,
            value="TestType",
            lineno=10,
            column=5
        )

    def create_mock_module_with_types(self, module_name: str, types_dict: dict) -> Unit:
        """
        Create a mock module with specified types.
        
        Args:
            module_name: Name of the module
            types_dict: Dictionary mapping type names to type kinds ('class', 'enum', 'record')
            
        Returns:
            Mock Unit with configured interface symbols
        """
        mock_module = Mock(spec=Unit)
        mock_module.name = module_name
        mock_module.is_loaded = True
        
        # Create mock symbol table
        mock_symbol_table = Mock(spec=ModuleSymbolTable)
        interface_symbols = {}
        
        for type_name, type_kind in types_dict.items():
            mock_symbol = Mock(spec=Symbol)
            if type_kind == 'class':
                mock_symbol.symbol_type = "ClassSymbol"
            elif type_kind == 'enum':
                mock_symbol.symbol_type = "EnumSymbol"
            elif type_kind == 'record':
                mock_symbol.symbol_type = "RecordSymbol"
            
            interface_symbols[type_name] = mock_symbol
        
        mock_symbol_table.get_interface_symbols.return_value = interface_symbols
        mock_module.interface_symbols = mock_symbol_table
        
        return mock_module

    def test_complex_module_resolution_scenario(self):
        """Test complex scenario with multiple modules and type conflicts."""
        # Create modules with overlapping type names
        graphics_module = self.create_mock_module_with_types("Graphics", {
            "Color": "enum",
            "Point": "record",
            "Shape": "class"
        })
        
        math_module = self.create_mock_module_with_types("Math", {
            "Point": "record",  # Same name as Graphics.Point
            "Vector": "record",
            "Number": "class"
        })
        
        # Register modules
        self.module_registry.loaded_modules["Graphics"] = graphics_module
        self.module_registry.loaded_modules["Math"] = math_module
        
        # Test resolving Color from Graphics
        unresolved_color = UnresolvedType(self.test_token, "Color")
        context = TypeResolutionContext(
            imported_modules=["Graphics", "Math"]
        )
        
        resolved_color = self.type_resolver.resolve_type(unresolved_color, context)
        assert isinstance(resolved_color, EnumType)
        
        # Test resolving Point (should resolve to first found - Graphics)
        unresolved_point = UnresolvedType(self.test_token, "Point")
        resolved_point = self.type_resolver.resolve_type(unresolved_point, context)
        assert isinstance(resolved_point, RecordType)

    def test_local_types_override_imported_types(self):
        """Test that local types have priority over imported module types."""
        # Create module with a type
        external_module = self.create_mock_module_with_types("External", {
            "MyType": "class"
        })
        self.module_registry.loaded_modules["External"] = external_module
        
        # Create context with local type of same name but different kind
        unresolved = UnresolvedType(self.test_token, "MyType")
        context = TypeResolutionContext(
            local_enums=["MyType"],  # Local enum should override imported class
            imported_modules=["External"]
        )
        
        resolved = self.type_resolver.resolve_type(unresolved, context)
        
        # Should resolve to local enum, not imported class
        assert isinstance(resolved, EnumType)

    def test_primitive_types_as_fallback(self):
        """Test that primitive types work as fallback when not found elsewhere."""
        unresolved = UnresolvedType(self.test_token, "Boolean")
        context = TypeResolutionContext(
            local_classes=["SomeClass"],
            imported_modules=["SomeModule"]
        )
        
        # Even with other types available, should resolve to primitive Boolean
        resolved = self.type_resolver.resolve_type(unresolved, context)
        assert isinstance(resolved, PrimitiveType)
        assert resolved.token.type == TokenType.BOOLEAN

    def test_comprehensive_error_message_with_multiple_sources(self):
        """Test error message includes information from all available sources."""
        # Set up modules
        graphics_module = self.create_mock_module_with_types("Graphics", {
            "Color": "enum",
            "Shape": "class"
        })
        self.module_registry.loaded_modules["Graphics"] = graphics_module
        
        unresolved = UnresolvedType(self.test_token, "UnknownType")
        context = TypeResolutionContext(
            local_classes=["LocalClass"],
            local_enums=["LocalEnum"],
            local_records=["LocalRecord"],
            imported_modules=["Graphics"]
        )
        
        with pytest.raises(TypeResolutionError) as exc_info:
            self.type_resolver.resolve_type(unresolved, context)
        
        error_message = str(exc_info.value)
        
        # Should include local types
        assert "LocalClass" in error_message
        assert "LocalEnum" in error_message
        assert "LocalRecord" in error_message
        
        # Should include imported types
        assert "Graphics.Color" in error_message or "Color" in error_message
        assert "Graphics.Shape" in error_message or "Shape" in error_message

    def test_fuzzy_matching_suggestions_across_modules(self):
        """Test that fuzzy matching works across local and imported types."""
        # Set up module with similar type names
        module = self.create_mock_module_with_types("TestModule", {
            "UserClass": "class",
            "UserRecord": "record"
        })
        self.module_registry.loaded_modules["TestModule"] = module
        
        unresolved = UnresolvedType(self.test_token, "UserClas")  # Typo
        context = TypeResolutionContext(
            local_classes=["LocalUser"],
            imported_modules=["TestModule"]
        )
        
        with pytest.raises(TypeResolutionError) as exc_info:
            self.type_resolver.resolve_type(unresolved, context)
        
        suggestions = exc_info.value.suggestions
        
        # Should suggest similar names from both local and imported types
        assert len(suggestions) > 0
        # Should include close matches
        similar_names = [s for s in suggestions if "User" in s]
        assert len(similar_names) > 0

    def test_qualified_type_resolution_with_suggestions(self):
        """Test qualified type resolution provides good suggestions on failure."""
        # Create module with some types
        module = self.create_mock_module_with_types("Graphics", {
            "Color": "enum",
            "Colour": "enum",  # Similar spelling
            "Shape": "class"
        })
        self.module_registry.loaded_modules["Graphics"] = module
        
        # Try to resolve non-existent type with typo
        with pytest.raises(TypeResolutionError) as exc_info:
            self.type_resolver.resolve_qualified_type("Graphics", "Colr", self.test_token)
        
        error = exc_info.value
        suggestions = error.suggestions
        
        # Should suggest similar types from the module
        assert len(suggestions) > 0
        assert any("Color" in s or "Colour" in s for s in suggestions)

    def test_caching_with_different_contexts(self):
        """Test that caching works correctly with different resolution contexts."""
        # Create two different contexts
        context1 = TypeResolutionContext(
            current_module="Module1",
            local_classes=["TestType"]
        )
        
        context2 = TypeResolutionContext(
            current_module="Module2",
            local_enums=["TestType"]
        )
        
        unresolved1 = UnresolvedType(self.test_token, "TestType")
        unresolved2 = UnresolvedType(self.test_token, "TestType")
        
        # Resolve in different contexts
        resolved1 = self.type_resolver.resolve_type(unresolved1, context1)
        resolved2 = self.type_resolver.resolve_type(unresolved2, context2)
        
        # Should resolve to different types based on context
        assert isinstance(resolved1, ClassType)
        assert isinstance(resolved2, EnumType)
        
        # Cache should have separate entries for different contexts
        cache_stats = self.type_resolver.get_cache_stats()
        assert cache_stats['cache_size'] >= 2

    def test_module_not_loaded_graceful_handling(self):
        """Test graceful handling when imported module is not loaded."""
        # Create module but mark it as not loaded
        mock_module = Mock(spec=Unit)
        mock_module.is_loaded = False
        self.module_registry.loaded_modules["UnloadedModule"] = mock_module
        
        unresolved = UnresolvedType(self.test_token, "SomeType")
        context = TypeResolutionContext(
            imported_modules=["UnloadedModule"]
        )
        
        # Should not crash, should try other resolution methods
        with pytest.raises(TypeResolutionError):
            self.type_resolver.resolve_type(unresolved, context)

    def test_empty_module_interface_symbols(self):
        """Test handling of modules with empty interface symbols."""
        # Create module with empty interface
        mock_module = Mock(spec=Unit)
        mock_module.is_loaded = True
        
        mock_symbol_table = Mock(spec=ModuleSymbolTable)
        mock_symbol_table.get_interface_symbols.return_value = {}
        mock_module.interface_symbols = mock_symbol_table
        
        self.module_registry.loaded_modules["EmptyModule"] = mock_module
        
        unresolved = UnresolvedType(self.test_token, "SomeType")
        context = TypeResolutionContext(
            imported_modules=["EmptyModule"]
        )
        
        # Should handle empty modules gracefully
        with pytest.raises(TypeResolutionError):
            self.type_resolver.resolve_type(unresolved, context)

    def test_large_number_of_types_performance(self):
        """Test performance with large number of available types."""
        # Create context with many types
        large_class_list = [f"Class{i}" for i in range(100)]
        large_enum_list = [f"Enum{i}" for i in range(100)]
        large_record_list = [f"Record{i}" for i in range(100)]
        
        context = TypeResolutionContext(
            local_classes=large_class_list,
            local_enums=large_enum_list,
            local_records=large_record_list
        )
        
        # Test resolving existing type
        unresolved_existing = UnresolvedType(self.test_token, "Class50")
        resolved = self.type_resolver.resolve_type(unresolved_existing, context)
        assert isinstance(resolved, ClassType)
        
        # Test resolving non-existing type (should still provide suggestions)
        unresolved_missing = UnresolvedType(self.test_token, "Class999")
        with pytest.raises(TypeResolutionError) as exc_info:
            self.type_resolver.resolve_type(unresolved_missing, context)
        
        # Should still work efficiently and provide suggestions
        error = exc_info.value
        assert len(error.suggestions) > 0

    def test_case_sensitivity_in_type_resolution(self):
        """Test case sensitivity in type name resolution."""
        context = TypeResolutionContext(
            local_classes=["MyClass"],
            local_enums=["MyEnum"]
        )
        
        # Test exact case match
        unresolved_exact = UnresolvedType(self.test_token, "MyClass")
        resolved = self.type_resolver.resolve_type(unresolved_exact, context)
        assert isinstance(resolved, ClassType)
        
        # Test different case (should not match, but should suggest)
        unresolved_wrong_case = UnresolvedType(self.test_token, "myclass")
        with pytest.raises(TypeResolutionError) as exc_info:
            self.type_resolver.resolve_type(unresolved_wrong_case, context)
        
        # Should suggest the correctly cased version
        suggestions = exc_info.value.suggestions
        assert any("MyClass" in s for s in suggestions)

    def test_special_characters_in_type_names(self):
        """Test handling of type names with special characters."""
        # Some Pascal implementations allow underscores in identifiers
        context = TypeResolutionContext(
            local_classes=["My_Class", "Another_Type_Name"]
        )
        
        unresolved = UnresolvedType(self.test_token, "My_Class")
        resolved = self.type_resolver.resolve_type(unresolved, context)
        assert isinstance(resolved, ClassType)

    def test_concurrent_resolution_safety(self):
        """Test that type resolver is safe for concurrent access."""
        import threading
        import time
        
        context = TypeResolutionContext(
            local_classes=["ThreadSafeClass"]
        )
        
        results = []
        errors = []
        
        def resolve_type():
            try:
                unresolved = UnresolvedType(self.test_token, "ThreadSafeClass")
                resolved = self.type_resolver.resolve_type(unresolved, context)
                results.append(resolved)
            except Exception as e:
                errors.append(e)
        
        # Create multiple threads
        threads = []
        for _ in range(10):
            thread = threading.Thread(target=resolve_type)
            threads.append(thread)
        
        # Start all threads
        for thread in threads:
            thread.start()
        
        # Wait for all threads to complete
        for thread in threads:
            thread.join()
        
        # All should succeed
        assert len(errors) == 0
        assert len(results) == 10
        assert all(isinstance(r, ClassType) for r in results)


if __name__ == "__main__":
    pytest.main([__file__])