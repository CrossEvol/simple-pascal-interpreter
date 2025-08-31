"""
Unit tests for TypeResolver service class.

Tests cover type resolution scenarios, error cases, caching, and suggestion system.
"""

import pytest
from unittest.mock import Mock, MagicMock

from src.type_resolver import TypeResolver, TypeResolutionContext, TypeResolutionResult
from src.spi_ast import (
    UnresolvedType, ClassType, EnumType, RecordType, 
    PrimitiveType, StringType
)
from src.spi_token import Token, TokenType
from src.error import TypeResolutionError
from src.module import ModuleRegistry, Unit, ModuleSymbolTable
from src.symbol import Symbol


class TestTypeResolver:
    """Test cases for TypeResolver class."""

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

    def test_init_without_module_registry(self):
        """Test TypeResolver initialization without module registry."""
        resolver = TypeResolver()
        assert resolver.module_registry is None
        assert resolver._type_cache == {}

    def test_init_with_module_registry(self):
        """Test TypeResolver initialization with module registry."""
        assert self.type_resolver.module_registry is self.module_registry
        assert self.type_resolver._type_cache == {}

    def test_resolve_local_class_type(self):
        """Test resolving a local class type."""
        # Create unresolved type
        unresolved = UnresolvedType(self.test_token, "MyClass")
        
        # Create context with local class
        context = TypeResolutionContext(
            local_classes=["MyClass", "OtherClass"]
        )
        
        # Resolve type
        resolved = self.type_resolver.resolve_type(unresolved, context)
        
        # Verify result
        assert isinstance(resolved, ClassType)
        assert resolved.token == self.test_token
        assert unresolved.resolved_type == resolved

    def test_resolve_local_enum_type(self):
        """Test resolving a local enum type."""
        unresolved = UnresolvedType(self.test_token, "MyEnum")
        
        context = TypeResolutionContext(
            local_enums=["MyEnum", "OtherEnum"]
        )
        
        resolved = self.type_resolver.resolve_type(unresolved, context)
        
        assert isinstance(resolved, EnumType)
        assert resolved.token == self.test_token

    def test_resolve_local_record_type(self):
        """Test resolving a local record type."""
        unresolved = UnresolvedType(self.test_token, "MyRecord")
        
        context = TypeResolutionContext(
            local_records=["MyRecord", "OtherRecord"]
        )
        
        resolved = self.type_resolver.resolve_type(unresolved, context)
        
        assert isinstance(resolved, RecordType)
        assert resolved.token == self.test_token

    def test_resolve_primitive_type_integer(self):
        """Test resolving primitive Integer type."""
        unresolved = UnresolvedType(self.test_token, "Integer")
        context = TypeResolutionContext()
        
        resolved = self.type_resolver.resolve_type(unresolved, context)
        
        assert isinstance(resolved, PrimitiveType)
        assert resolved.token.type == TokenType.INTEGER

    def test_resolve_primitive_type_string(self):
        """Test resolving primitive String type."""
        unresolved = UnresolvedType(self.test_token, "String")
        context = TypeResolutionContext()
        
        resolved = self.type_resolver.resolve_type(unresolved, context)
        
        assert isinstance(resolved, StringType)
        assert resolved.token.type == TokenType.STRING

    def test_resolve_already_resolved_type(self):
        """Test that already resolved types return cached result."""
        # Create unresolved type with pre-resolved type
        unresolved = UnresolvedType(self.test_token, "MyClass")
        cached_type = ClassType(self.test_token)
        unresolved.resolved_type = cached_type
        
        context = TypeResolutionContext()
        
        resolved = self.type_resolver.resolve_type(unresolved, context)
        
        # Should return the cached result
        assert resolved is cached_type

    def test_resolve_type_with_caching(self):
        """Test that type resolution results are cached."""
        unresolved1 = UnresolvedType(self.test_token, "MyClass")
        unresolved2 = UnresolvedType(self.test_token, "MyClass")
        
        context = TypeResolutionContext(
            local_classes=["MyClass"]
        )
        
        # Resolve first time
        resolved1 = self.type_resolver.resolve_type(unresolved1, context)
        
        # Resolve second time - should use cache
        resolved2 = self.type_resolver.resolve_type(unresolved2, context)
        
        # Both should be ClassType instances
        assert isinstance(resolved1, ClassType)
        assert isinstance(resolved2, ClassType)
        
        # Cache should contain the entry
        cache_stats = self.type_resolver.get_cache_stats()
        assert cache_stats['cache_size'] > 0

    def test_resolve_unknown_type_raises_error(self):
        """Test that unknown types raise TypeResolutionError."""
        unresolved = UnresolvedType(self.test_token, "UnknownType")
        context = TypeResolutionContext()
        
        with pytest.raises(TypeResolutionError) as exc_info:
            self.type_resolver.resolve_type(unresolved, context)
        
        error = exc_info.value
        assert "Unknown type 'UnknownType'" in str(error)
        assert error.token == self.test_token

    def test_type_suggestions_for_similar_names(self):
        """Test that type resolution provides suggestions for similar names."""
        unresolved = UnresolvedType(self.test_token, "MyClss")  # Typo in "MyClass"
        
        context = TypeResolutionContext(
            local_classes=["MyClass", "YourClass"],
            local_enums=["MyEnum"]
        )
        
        with pytest.raises(TypeResolutionError) as exc_info:
            self.type_resolver.resolve_type(unresolved, context)
        
        error = exc_info.value
        assert len(error.suggestions) > 0
        assert "MyClass" in error.suggestions

    def test_resolve_from_imported_module(self):
        """Test resolving types from imported modules."""
        # Create a mock module with interface symbols
        mock_module = Mock(spec=Unit)
        mock_module.is_loaded = True
        
        # Create mock symbol table with interface symbols
        mock_symbol_table = Mock(spec=ModuleSymbolTable)
        mock_symbol = Mock(spec=Symbol)
        mock_symbol.symbol_type = "ClassSymbol"
        
        mock_symbol_table.get_interface_symbols.return_value = {
            "ModuleClass": mock_symbol
        }
        mock_module.interface_symbols = mock_symbol_table
        
        # Set up module registry to return our mock module
        self.module_registry.loaded_modules["TestModule"] = mock_module
        
        unresolved = UnresolvedType(self.test_token, "ModuleClass")
        context = TypeResolutionContext(
            imported_modules=["TestModule"]
        )
        
        resolved = self.type_resolver.resolve_type(unresolved, context)
        
        assert isinstance(resolved, ClassType)

    def test_resolve_qualified_type_success(self):
        """Test resolving module-qualified type names."""
        # Create mock module
        mock_module = Mock(spec=Unit)
        mock_module.is_loaded = True
        
        mock_symbol_table = Mock(spec=ModuleSymbolTable)
        mock_symbol = Mock(spec=Symbol)
        mock_symbol.symbol_type = "EnumSymbol"
        
        mock_symbol_table.get_interface_symbols.return_value = {
            "Color": mock_symbol
        }
        mock_module.interface_symbols = mock_symbol_table
        
        self.module_registry.loaded_modules["Graphics"] = mock_module
        
        resolved = self.type_resolver.resolve_qualified_type("Graphics", "Color", self.test_token)
        
        assert isinstance(resolved, EnumType)

    def test_resolve_qualified_type_module_not_found(self):
        """Test resolving qualified type when module is not found."""
        with pytest.raises(TypeResolutionError) as exc_info:
            self.type_resolver.resolve_qualified_type("NonExistentModule", "SomeType", self.test_token)
        
        error = exc_info.value
        assert "Module 'NonExistentModule' not found" in str(error)

    def test_resolve_qualified_type_symbol_not_found(self):
        """Test resolving qualified type when symbol is not found in module."""
        # Create mock module without the requested symbol
        mock_module = Mock(spec=Unit)
        mock_module.is_loaded = True
        
        mock_symbol_table = Mock(spec=ModuleSymbolTable)
        mock_symbol_table.get_interface_symbols.return_value = {
            "ExistingType": Mock()
        }
        mock_module.interface_symbols = mock_symbol_table
        
        self.module_registry.loaded_modules["TestModule"] = mock_module
        
        with pytest.raises(TypeResolutionError) as exc_info:
            self.type_resolver.resolve_qualified_type("TestModule", "NonExistentType", self.test_token)
        
        error = exc_info.value
        assert "Type 'NonExistentType' not found in module 'TestModule'" in str(error)

    def test_clear_cache(self):
        """Test clearing the type resolution cache."""
        # Add something to cache first
        unresolved = UnresolvedType(self.test_token, "MyClass")
        context = TypeResolutionContext(local_classes=["MyClass"])
        
        self.type_resolver.resolve_type(unresolved, context)
        
        # Verify cache has content
        assert self.type_resolver.get_cache_stats()['cache_size'] > 0
        
        # Clear cache
        self.type_resolver.clear_cache()
        
        # Verify cache is empty
        assert self.type_resolver.get_cache_stats()['cache_size'] == 0

    def test_get_cache_stats(self):
        """Test getting cache statistics."""
        stats = self.type_resolver.get_cache_stats()
        
        assert 'cache_size' in stats
        assert 'cached_types' in stats
        assert isinstance(stats['cache_size'], int)
        assert isinstance(stats['cached_types'], list)

    def test_type_resolution_context_defaults(self):
        """Test TypeResolutionContext default values."""
        context = TypeResolutionContext()
        
        assert context.current_module is None
        assert context.imported_modules == []
        assert context.local_classes == []
        assert context.local_enums == []
        assert context.local_records == []
        assert context.module_registry is None

    def test_type_resolution_result_defaults(self):
        """Test TypeResolutionResult default values."""
        result = TypeResolutionResult(success=True)
        
        assert result.success is True
        assert result.resolved_type is None
        assert result.error_message is None
        assert result.suggestions == []

    def test_error_message_formatting_with_location(self):
        """Test that error messages include location information."""
        token_with_location = Token(
            type=TokenType.ID,
            value="BadType",
            lineno=15,
            column=8
        )
        
        unresolved = UnresolvedType(token_with_location, "BadType")
        context = TypeResolutionContext()
        
        with pytest.raises(TypeResolutionError) as exc_info:
            self.type_resolver.resolve_type(unresolved, context)
        
        error_message = str(exc_info.value)
        assert "line 15" in error_message
        assert "column 8" in error_message

    def test_error_message_includes_available_types(self):
        """Test that error messages include available types information."""
        unresolved = UnresolvedType(self.test_token, "BadType")
        context = TypeResolutionContext(
            local_classes=["GoodClass"],
            local_enums=["GoodEnum"],
            local_records=["GoodRecord"]
        )
        
        with pytest.raises(TypeResolutionError) as exc_info:
            self.type_resolver.resolve_type(unresolved, context)
        
        error_message = str(exc_info.value)
        assert "Available types:" in error_message
        assert "Class:GoodClass" in error_message
        assert "Enum:GoodEnum" in error_message
        assert "Record:GoodRecord" in error_message

    def test_resolution_priority_order(self):
        """Test that type resolution follows correct priority order."""
        # Create context where same name exists in multiple scopes
        context = TypeResolutionContext(
            local_classes=["TestType"],  # Should have highest priority
            local_enums=["TestType"],
            imported_modules=["SomeModule"]
        )
        
        unresolved = UnresolvedType(self.test_token, "TestType")
        resolved = self.type_resolver.resolve_type(unresolved, context)
        
        # Should resolve to ClassType (local classes have priority over enums)
        assert isinstance(resolved, ClassType)

    def test_cache_key_generation(self):
        """Test that cache keys are generated correctly for different contexts."""
        resolver = TypeResolver()
        
        context1 = TypeResolutionContext(
            current_module="Module1",
            local_classes=["Class1"]
        )
        
        context2 = TypeResolutionContext(
            current_module="Module2",
            local_classes=["Class1"]
        )
        
        key1 = resolver._create_cache_key("TestType", context1)
        key2 = resolver._create_cache_key("TestType", context2)
        
        # Keys should be different for different contexts
        assert key1 != key2
        assert "Module1" in key1
        assert "Module2" in key2

    def test_resolve_without_module_registry(self):
        """Test type resolution when no module registry is available."""
        resolver = TypeResolver()  # No module registry
        
        unresolved = UnresolvedType(self.test_token, "MyClass")
        context = TypeResolutionContext(
            local_classes=["MyClass"]
        )
        
        # Should still work for local types
        resolved = resolver.resolve_type(unresolved, context)
        assert isinstance(resolved, ClassType)

    def test_resolve_with_empty_context(self):
        """Test type resolution with completely empty context."""
        unresolved = UnresolvedType(self.test_token, "Integer")
        context = TypeResolutionContext()
        
        # Should still resolve primitive types
        resolved = self.type_resolver.resolve_type(unresolved, context)
        assert isinstance(resolved, PrimitiveType)


if __name__ == "__main__":
    pytest.main([__file__])