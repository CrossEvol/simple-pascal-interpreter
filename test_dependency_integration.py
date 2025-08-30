"""
Integration test for dependency resolution with the module system.

This test verifies that dependency resolution works correctly with
the existing module loading and symbol resolution functionality.
"""

import unittest
from src.module import ModuleRegistry, Unit
from src.symbol import VarSymbol, BuiltinTypeSymbol
from src.visibility import VisibilityLevel
from src.error import CircularDependencyError


class TestDependencyIntegration(unittest.TestCase):
    """Integration tests for dependency resolution with module system."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.registry = ModuleRegistry()
    
    def test_dependency_resolution_with_symbol_import(self):
        """Test that dependency resolution works with actual symbol importing."""
        # Create three modules: App -> Utils -> Core
        app_unit = Unit("App", "./App.pas")
        utils_unit = Unit("Utils", "./Utils.pas")
        core_unit = Unit("Core", "./Core.pas")
        
        # Add modules to registry
        self.registry.loaded_modules["App"] = app_unit
        self.registry.loaded_modules["Utils"] = utils_unit
        self.registry.loaded_modules["Core"] = core_unit
        
        # Set up dependencies
        self.registry.add_dependency("App", "Utils")
        self.registry.add_dependency("Utils", "Core")
        
        # Add symbols to Core (interface)
        core_var = VarSymbol("CoreVar", BuiltinTypeSymbol("INTEGER"))
        core_var.visibility = VisibilityLevel.INTERFACE
        core_unit.interface_symbols.insert(core_var)
        
        # Add symbols to Utils (interface)
        utils_var = VarSymbol("UtilsVar", BuiltinTypeSymbol("STRING"))
        utils_var.visibility = VisibilityLevel.INTERFACE
        utils_unit.interface_symbols.insert(utils_var)
        
        # Import Core symbols into Utils
        utils_unit.interface_symbols.import_module_symbols("Core", core_unit.interface_symbols)
        
        # Import Utils symbols into App
        app_unit.interface_symbols.import_module_symbols("Utils", utils_unit.interface_symbols)
        
        # Resolve dependencies for App
        load_order = self.registry.resolve_dependencies("App")
        
        # Verify load order
        self.assertEqual(load_order, ["Core", "Utils", "App"])
        
        # Verify symbol resolution works across dependencies
        # App should be able to access UtilsVar (direct import)
        utils_symbol = app_unit.interface_symbols.lookup_with_modules("UtilsVar")
        self.assertIsNotNone(utils_symbol)
        self.assertEqual(utils_symbol.name, "UtilsVar")
        
        # Utils should be able to access CoreVar (direct import)
        core_symbol = utils_unit.interface_symbols.lookup_with_modules("CoreVar")
        self.assertIsNotNone(core_symbol)
        self.assertEqual(core_symbol.name, "CoreVar")
    
    def test_circular_dependency_with_symbol_tables(self):
        """Test circular dependency detection with actual symbol tables."""
        # Create two modules with circular dependency
        module_a = Unit("ModuleA", "./ModuleA.pas")
        module_b = Unit("ModuleB", "./ModuleB.pas")
        
        # Add modules to registry
        self.registry.loaded_modules["ModuleA"] = module_a
        self.registry.loaded_modules["ModuleB"] = module_b
        
        # Set up circular dependency
        self.registry.add_dependency("ModuleA", "ModuleB")
        self.registry.add_dependency("ModuleB", "ModuleA")
        
        # Add symbols to both modules
        var_a = VarSymbol("VarA", BuiltinTypeSymbol("INTEGER"))
        var_a.visibility = VisibilityLevel.INTERFACE
        module_a.interface_symbols.insert(var_a)
        
        var_b = VarSymbol("VarB", BuiltinTypeSymbol("STRING"))
        var_b.visibility = VisibilityLevel.INTERFACE
        module_b.interface_symbols.insert(var_b)
        
        # Attempting to resolve dependencies should raise CircularDependencyError
        with self.assertRaises(CircularDependencyError) as context:
            self.registry.resolve_dependencies("ModuleA")
        
        # Verify the error contains both modules
        error = context.exception
        self.assertIn("ModuleA", error.dependency_chain)
        self.assertIn("ModuleB", error.dependency_chain)
        
        # Verify error includes helpful suggestions
        self.assertIsNotNone(error.suggestions)
        self.assertGreater(len(error.suggestions), 0)
        
        # Check that suggestions contain expected recovery options
        suggestions_text = "\n".join(error.suggestions)
        self.assertIn("Extract Common Functionality", suggestions_text)
        self.assertIn("Merge Modules", suggestions_text)
    
    def test_enhanced_circular_dependency_detection_methods(self):
        """Test the enhanced circular dependency detection methods."""
        # Create complex circular dependency: A -> B -> C -> A
        modules = ["A", "B", "C"]
        for name in modules:
            unit = Unit(name, f"./{name}.pas")
            self.registry.loaded_modules[name] = unit
        
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("B", "C")
        self.registry.add_dependency("C", "A")
        
        # Test check_circular_dependencies method
        self.assertTrue(self.registry.check_circular_dependencies("A"))
        
        # Test find_circular_dependency_chain method
        chain = self.registry.find_circular_dependency_chain("A")
        self.assertIsNotNone(chain)
        self.assertEqual(len(chain), 4)  # A -> B -> C -> A
        self.assertEqual(chain[0], chain[-1])  # Starts and ends with same module
        
        # Test get_circular_dependency_suggestions method
        suggestions = self.registry.get_circular_dependency_suggestions(chain)
        self.assertGreater(len(suggestions), 5)
        
        suggestions_text = "\n".join(suggestions)
        self.assertIn("Restructure Dependencies", suggestions_text)
        self.assertIn("A -> B -> C -> A", suggestions_text)
    
    def test_circular_dependency_error_message_quality(self):
        """Test that circular dependency error messages are clear and helpful."""
        # Create circular dependency with meaningful module names
        ui_unit = Unit("UserInterface", "./UserInterface.pas")
        db_unit = Unit("Database", "./Database.pas")
        auth_unit = Unit("Authentication", "./Authentication.pas")
        
        self.registry.loaded_modules["UserInterface"] = ui_unit
        self.registry.loaded_modules["Database"] = db_unit
        self.registry.loaded_modules["Authentication"] = auth_unit
        
        # Create cycle: UserInterface -> Database -> Authentication -> UserInterface
        self.registry.add_dependency("UserInterface", "Database")
        self.registry.add_dependency("Database", "Authentication")
        self.registry.add_dependency("Authentication", "UserInterface")
        
        # Test error message quality
        with self.assertRaises(CircularDependencyError) as context:
            self.registry.resolve_dependencies("UserInterface")
        
        error = context.exception
        error_message = str(error)
        
        # Should contain clear dependency chain
        self.assertIn("UserInterface -> Database -> Authentication -> UserInterface", error_message)
        
        # Should contain helpful suggestions
        self.assertIn("Extract Common Functionality", error_message)
        self.assertIn("Dependency Inversion", error_message)
        self.assertIn("Forward Declarations", error_message)
        
        # Should provide specific module names in suggestions
        self.assertIn("UserInterface", error_message)
        self.assertIn("Authentication", error_message)
    
    def test_complex_dependency_with_standard_library(self):
        """Test dependency resolution with standard library modules."""
        # Create app module that depends on multiple standard library modules
        app_unit = Unit("MyApp", "./MyApp.pas")
        math_unit = Unit("Math", "./stdlib/Math.pas")
        utils_unit = Unit("ArrayUtils", "./stdlib/ArrayUtils.pas")
        
        # Add modules to registry
        self.registry.loaded_modules["MyApp"] = app_unit
        self.registry.loaded_modules["Math"] = math_unit
        self.registry.loaded_modules["ArrayUtils"] = utils_unit
        
        # Set up dependencies: MyApp -> Math, MyApp -> ArrayUtils
        self.registry.add_dependency("MyApp", "Math")
        self.registry.add_dependency("MyApp", "ArrayUtils")
        
        # Resolve dependencies
        load_order = self.registry.resolve_dependencies("MyApp")
        
        # Math and ArrayUtils should come before MyApp
        self.assertIn("Math", load_order[:-1])
        self.assertIn("ArrayUtils", load_order[:-1])
        self.assertEqual(load_order[-1], "MyApp")
        
        # Verify all dependencies are tracked
        all_deps = self.registry.get_all_dependencies("MyApp")
        self.assertEqual(set(all_deps), {"Math", "ArrayUtils"})
    
    def test_transitive_dependency_resolution(self):
        """Test that transitive dependencies are properly resolved."""
        # Create chain: App -> UI -> Graphics -> Core
        modules = ["App", "UI", "Graphics", "Core"]
        for name in modules:
            unit = Unit(name, f"./{name}.pas")
            self.registry.loaded_modules[name] = unit
        
        # Set up linear dependency chain
        self.registry.add_dependency("App", "UI")
        self.registry.add_dependency("UI", "Graphics")
        self.registry.add_dependency("Graphics", "Core")
        
        # Resolve dependencies for App
        load_order = self.registry.resolve_dependencies("App")
        
        # Should be in reverse dependency order
        self.assertEqual(load_order, ["Core", "Graphics", "UI", "App"])
        
        # Test transitive dependency checking
        self.assertTrue(self.registry.has_dependency("App", "Core"))
        self.assertTrue(self.registry.has_dependency("UI", "Core"))
        self.assertFalse(self.registry.has_dependency("Core", "App"))
    
    def test_dependency_resolution_preserves_module_state(self):
        """Test that dependency resolution doesn't affect module state."""
        # Create modules with symbols
        module_a = Unit("ModuleA", "./ModuleA.pas")
        module_b = Unit("ModuleB", "./ModuleB.pas")
        
        # Add symbols before dependency resolution
        var_a = VarSymbol("TestVar", BuiltinTypeSymbol("INTEGER"))
        var_a.visibility = VisibilityLevel.INTERFACE
        module_a.interface_symbols.insert(var_a)
        
        # Add to registry
        self.registry.loaded_modules["ModuleA"] = module_a
        self.registry.loaded_modules["ModuleB"] = module_b
        self.registry.add_dependency("ModuleA", "ModuleB")
        
        # Resolve dependencies
        load_order = self.registry.resolve_dependencies("ModuleA")
        
        # Verify module state is preserved
        self.assertEqual(load_order, ["ModuleB", "ModuleA"])
        
        # Verify symbols are still accessible
        found_var = module_a.interface_symbols.lookup("TestVar")
        self.assertIsNotNone(found_var)
        self.assertEqual(found_var.name, "TestVar")
        
        # Verify module properties are preserved
        self.assertEqual(module_a.name, "ModuleA")
        self.assertEqual(module_a.file_path, "./ModuleA.pas")


if __name__ == '__main__':
    unittest.main()