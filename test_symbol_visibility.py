"""
Unit tests for symbol visibility enforcement in the module system.

Tests cover:
- Symbol visibility tracking (interface vs implementation)
- Symbol lookup with visibility enforcement
- Cross-module symbol access restrictions
- Interface symbol filtering during module import
"""

import unittest
from src.symbol import (
    VarSymbol, ProcedureSymbol, FunctionSymbol, BuiltinTypeSymbol
)
from src.visibility import VisibilityLevel
from src.module import ModuleSymbolTable
from src.sematic_analyzer import ScopedSymbolTable
from src.spi_ast import Type
from src.spi_token import Token, TokenType


class TestSymbolVisibility(unittest.TestCase):
    """Test cases for symbol visibility enforcement."""
    
    def setUp(self):
        """Set up test fixtures."""
        # Create a sample type for testing
        self.integer_type = Type(
            token=Token(type=TokenType.INTEGER, value=0, lineno=1, column=1)
        )
        
        # Create module symbol tables
        self.module_a = ModuleSymbolTable(
            scope_name="ModuleA",
            scope_level=1,
            enclosing_scope=None,
            module_name="ModuleA"
        )
        
        self.module_b = ModuleSymbolTable(
            scope_name="ModuleB", 
            scope_level=1,
            enclosing_scope=None,
            module_name="ModuleB"
        )
    
    def test_symbol_visibility_initialization(self):
        """Test that symbols are initialized with correct visibility levels."""
        # Test default visibility (PRIVATE)
        var_symbol = VarSymbol("test_var", BuiltinTypeSymbol("INTEGER"))
        self.assertEqual(var_symbol.visibility, VisibilityLevel.PRIVATE)
        
        # Test explicit visibility
        interface_var = VarSymbol("public_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.INTERFACE)
        self.assertEqual(interface_var.visibility, VisibilityLevel.INTERFACE)
        
        impl_var = VarSymbol("impl_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.IMPLEMENTATION)
        self.assertEqual(impl_var.visibility, VisibilityLevel.IMPLEMENTATION)
    
    def test_builtin_symbols_default_visibility(self):
        """Test that builtin symbols have appropriate default visibility."""
        # Builtin types should be interface by default
        builtin_type = BuiltinTypeSymbol("INTEGER")
        self.assertEqual(builtin_type.visibility, VisibilityLevel.INTERFACE)
        
        # User-defined symbols should be private by default
        user_var = VarSymbol("user_var", builtin_type)
        self.assertEqual(user_var.visibility, VisibilityLevel.PRIVATE)
    
    def test_insert_with_visibility(self):
        """Test inserting symbols with explicit visibility."""
        var_symbol = VarSymbol("test_var", BuiltinTypeSymbol("INTEGER"))
        
        # Insert with interface visibility
        self.module_a.insert_with_visibility(var_symbol, VisibilityLevel.INTERFACE)
        self.assertEqual(var_symbol.visibility, VisibilityLevel.INTERFACE)
        
        # Verify symbol is in the table
        retrieved = self.module_a.lookup("test_var")
        self.assertIsNotNone(retrieved)
        self.assertEqual(retrieved.visibility, VisibilityLevel.INTERFACE)
    
    def test_get_interface_symbols(self):
        """Test filtering interface symbols."""
        # Add symbols with different visibility levels
        interface_var = VarSymbol("interface_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.INTERFACE)
        impl_var = VarSymbol("impl_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.IMPLEMENTATION)
        private_var = VarSymbol("private_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.PRIVATE)
        
        self.module_a.insert(interface_var)
        self.module_a.insert(impl_var)
        self.module_a.insert(private_var)
        
        # Get interface symbols
        interface_symbols = self.module_a.get_interface_symbols()
        
        # Should only contain interface symbols
        self.assertIn("interface_var", interface_symbols)
        self.assertNotIn("impl_var", interface_symbols)
        self.assertNotIn("private_var", interface_symbols)
        self.assertEqual(len(interface_symbols), 1)
    
    def test_get_implementation_symbols(self):
        """Test filtering implementation symbols."""
        # Add symbols with different visibility levels
        interface_var = VarSymbol("interface_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.INTERFACE)
        impl_var = VarSymbol("impl_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.IMPLEMENTATION)
        private_var = VarSymbol("private_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.PRIVATE)
        
        self.module_a.insert(interface_var)
        self.module_a.insert(impl_var)
        self.module_a.insert(private_var)
        
        # Get implementation symbols
        impl_symbols = self.module_a.get_implementation_symbols()
        
        # Should contain implementation and private symbols
        self.assertNotIn("interface_var", impl_symbols)
        self.assertIn("impl_var", impl_symbols)
        self.assertIn("private_var", impl_symbols)
        self.assertEqual(len(impl_symbols), 2)
    
    def test_import_module_symbols_filters_visibility(self):
        """Test that importing module symbols only imports interface symbols."""
        # Create source module with mixed visibility symbols
        source_module = ScopedSymbolTable("SourceModule", 1, None)
        
        interface_var = VarSymbol("interface_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.INTERFACE)
        impl_var = VarSymbol("impl_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.IMPLEMENTATION)
        private_var = VarSymbol("private_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.PRIVATE)
        
        source_module.insert(interface_var)
        source_module.insert(impl_var)
        source_module.insert(private_var)
        
        # Import symbols into target module
        self.module_a.import_module_symbols("SourceModule", source_module)
        
        # Check that only interface symbols are accessible
        self.assertTrue(self.module_a.has_imported_module("SourceModule"))
        
        # Should be able to find interface symbol
        found_interface = self.module_a.lookup_with_modules("interface_var")
        self.assertIsNotNone(found_interface)
        
        # Should not be able to find implementation or private symbols
        found_impl = self.module_a.lookup_with_modules("impl_var")
        self.assertIsNone(found_impl)
        
        found_private = self.module_a.lookup_with_modules("private_var")
        self.assertIsNone(found_private)
    
    def test_lookup_with_visibility_same_module(self):
        """Test that all symbols are accessible within the same module."""
        # Add symbols with different visibility levels
        interface_var = VarSymbol("interface_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.INTERFACE)
        impl_var = VarSymbol("impl_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.IMPLEMENTATION)
        private_var = VarSymbol("private_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.PRIVATE)
        
        self.module_a.insert(interface_var)
        self.module_a.insert(impl_var)
        self.module_a.insert(private_var)
        
        # All symbols should be accessible from the same module
        self.assertIsNotNone(self.module_a.lookup_with_visibility("interface_var", "ModuleA"))
        self.assertIsNotNone(self.module_a.lookup_with_visibility("impl_var", "ModuleA"))
        self.assertIsNotNone(self.module_a.lookup_with_visibility("private_var", "ModuleA"))
        
        # Also test with None (current module)
        self.assertIsNotNone(self.module_a.lookup_with_visibility("interface_var", None))
        self.assertIsNotNone(self.module_a.lookup_with_visibility("impl_var", None))
        self.assertIsNotNone(self.module_a.lookup_with_visibility("private_var", None))
    
    def test_lookup_with_visibility_different_module(self):
        """Test that only interface symbols are accessible from different modules."""
        # Add symbols with different visibility levels
        interface_var = VarSymbol("interface_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.INTERFACE)
        impl_var = VarSymbol("impl_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.IMPLEMENTATION)
        private_var = VarSymbol("private_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.PRIVATE)
        
        self.module_a.insert(interface_var)
        self.module_a.insert(impl_var)
        self.module_a.insert(private_var)
        
        # Only interface symbols should be accessible from different module
        self.assertIsNotNone(self.module_a.lookup_with_visibility("interface_var", "ModuleB"))
        self.assertIsNone(self.module_a.lookup_with_visibility("impl_var", "ModuleB"))
        self.assertIsNone(self.module_a.lookup_with_visibility("private_var", "ModuleB"))
    
    def test_procedure_symbol_visibility(self):
        """Test visibility enforcement for procedure symbols."""
        # Create procedures with different visibility levels
        interface_proc = ProcedureSymbol("interface_proc", [], VisibilityLevel.INTERFACE)
        impl_proc = ProcedureSymbol("impl_proc", [], VisibilityLevel.IMPLEMENTATION)
        private_proc = ProcedureSymbol("private_proc", [], VisibilityLevel.PRIVATE)
        
        self.module_a.insert(interface_proc)
        self.module_a.insert(impl_proc)
        self.module_a.insert(private_proc)
        
        # Test access from different module
        self.assertIsNotNone(self.module_a.lookup_with_visibility("interface_proc", "ModuleB"))
        self.assertIsNone(self.module_a.lookup_with_visibility("impl_proc", "ModuleB"))
        self.assertIsNone(self.module_a.lookup_with_visibility("private_proc", "ModuleB"))
    
    def test_function_symbol_visibility(self):
        """Test visibility enforcement for function symbols."""
        # Create functions with different visibility levels
        interface_func = FunctionSymbol("interface_func", self.integer_type, [], VisibilityLevel.INTERFACE)
        impl_func = FunctionSymbol("impl_func", self.integer_type, [], VisibilityLevel.IMPLEMENTATION)
        private_func = FunctionSymbol("private_func", self.integer_type, [], VisibilityLevel.PRIVATE)
        
        self.module_a.insert(interface_func)
        self.module_a.insert(impl_func)
        self.module_a.insert(private_func)
        
        # Test access from different module
        self.assertIsNotNone(self.module_a.lookup_with_visibility("interface_func", "ModuleB"))
        self.assertIsNone(self.module_a.lookup_with_visibility("impl_func", "ModuleB"))
        self.assertIsNone(self.module_a.lookup_with_visibility("private_func", "ModuleB"))
    
    def test_cross_module_symbol_resolution(self):
        """Test symbol resolution across modules with visibility enforcement."""
        # Set up ModuleA with interface and implementation symbols
        interface_var = VarSymbol("shared_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.INTERFACE)
        private_var = VarSymbol("private_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.PRIVATE)
        
        self.module_a.insert(interface_var)
        self.module_a.insert(private_var)
        
        # ModuleB imports from ModuleA
        self.module_b.import_module_symbols("ModuleA", self.module_a)
        
        # ModuleB should be able to access interface symbols from ModuleA
        found_shared = self.module_b.lookup_with_modules("shared_var")
        self.assertIsNotNone(found_shared)
        self.assertEqual(found_shared.name, "shared_var")
        
        # ModuleB should not be able to access private symbols from ModuleA
        found_private = self.module_b.lookup_with_modules("private_var")
        self.assertIsNone(found_private)
    
    def test_symbol_precedence_with_visibility(self):
        """Test that local symbols take precedence over imported symbols."""
        # Create interface symbol in ModuleA
        module_a_var = VarSymbol("common_var", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.INTERFACE)
        self.module_a.insert(module_a_var)
        
        # Create local symbol in ModuleB with same name
        module_b_var = VarSymbol("common_var", BuiltinTypeSymbol("REAL"), VisibilityLevel.PRIVATE)
        self.module_b.insert(module_b_var)
        
        # ModuleB imports from ModuleA
        self.module_b.import_module_symbols("ModuleA", self.module_a)
        
        # Local symbol should take precedence
        found_var = self.module_b.lookup_with_modules("common_var")
        self.assertIsNotNone(found_var)
        self.assertEqual(found_var.type.name, "REAL")  # Should be the local one
    
    def test_current_section_visibility(self):
        """Test setting and getting current section visibility."""
        # Default should be PRIVATE
        self.assertEqual(self.module_a.get_current_section_visibility(), VisibilityLevel.PRIVATE)
        
        # Set to INTERFACE
        self.module_a.set_current_section_visibility(VisibilityLevel.INTERFACE)
        self.assertEqual(self.module_a.get_current_section_visibility(), VisibilityLevel.INTERFACE)
        
        # Set to IMPLEMENTATION
        self.module_a.set_current_section_visibility(VisibilityLevel.IMPLEMENTATION)
        self.assertEqual(self.module_a.get_current_section_visibility(), VisibilityLevel.IMPLEMENTATION)
    
    def test_auto_visibility_assignment(self):
        """Test that symbols get visibility assigned based on current section."""
        # Set current section to INTERFACE
        self.module_a.set_current_section_visibility(VisibilityLevel.INTERFACE)
        
        # Insert a symbol with default (PRIVATE) visibility
        var_symbol = VarSymbol("test_var", BuiltinTypeSymbol("INTEGER"))
        self.assertEqual(var_symbol.visibility, VisibilityLevel.PRIVATE)  # Initially private
        
        self.module_a.insert(var_symbol)
        
        # After insertion, should have INTERFACE visibility
        self.assertEqual(var_symbol.visibility, VisibilityLevel.INTERFACE)
        
        # Set current section to IMPLEMENTATION
        self.module_a.set_current_section_visibility(VisibilityLevel.IMPLEMENTATION)
        
        # Insert another symbol
        impl_var = VarSymbol("impl_var", BuiltinTypeSymbol("INTEGER"))
        self.module_a.insert(impl_var)
        
        # Should have IMPLEMENTATION visibility
        self.assertEqual(impl_var.visibility, VisibilityLevel.IMPLEMENTATION)


if __name__ == '__main__':
    unittest.main()