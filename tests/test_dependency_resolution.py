"""
Unit tests for module dependency resolution functionality.

This module tests the dependency resolution system including:
- Topological sorting of module dependencies
- Circular dependency detection
- Dependency graph management
- Load order determination
"""

import unittest
from src.module import ModuleRegistry, Unit
from src.error import CircularDependencyError


class TestDependencyResolution(unittest.TestCase):
    """Test cases for module dependency resolution."""
    
    def setUp(self):
        """Set up test fixtures before each test method."""
        self.registry = ModuleRegistry()
    
    def test_simple_dependency_resolution(self):
        """Test resolving dependencies for a simple linear dependency chain."""
        # Create modules: A -> B -> C
        self.registry.load_module("A", "./A.pas")
        self.registry.load_module("B", "./B.pas")
        self.registry.load_module("C", "./C.pas")
        
        # Set up dependencies: A depends on B, B depends on C
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("B", "C")
        
        # Resolve dependencies for A
        load_order = self.registry.resolve_dependencies("A")
        
        # Expected order: C, B, A (dependencies first)
        self.assertEqual(load_order, ["C", "B", "A"])
    
    def test_multiple_dependencies_resolution(self):
        """Test resolving dependencies when a module depends on multiple modules."""
        # Create modules: A -> B, A -> C, B -> D, C -> D
        for name in ["A", "B", "C", "D"]:
            self.registry.load_module(name, f"./{name}.pas")
        
        # Set up dependencies
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("A", "C")
        self.registry.add_dependency("B", "D")
        self.registry.add_dependency("C", "D")
        
        # Resolve dependencies for A
        load_order = self.registry.resolve_dependencies("A")
        
        # D should come first, then B and C (in some order), then A
        self.assertEqual(load_order[0], "D")  # D has no dependencies
        self.assertEqual(load_order[-1], "A")  # A depends on everything
        self.assertIn("B", load_order[1:3])  # B and C should be in middle
        self.assertIn("C", load_order[1:3])
    
    def test_no_dependencies_resolution(self):
        """Test resolving dependencies for a module with no dependencies."""
        self.registry.load_module("Standalone", "./Standalone.pas")
        
        load_order = self.registry.resolve_dependencies("Standalone")
        
        # Should just return the module itself
        self.assertEqual(load_order, ["Standalone"])
    
    def test_circular_dependency_detection_simple(self):
        """Test detection of simple circular dependency (A -> B -> A)."""
        # Create modules with circular dependency
        self.registry.load_module("A", "./A.pas")
        self.registry.load_module("B", "./B.pas")
        
        # Set up circular dependency: A -> B -> A
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("B", "A")
        
        # Should raise CircularDependencyError
        with self.assertRaises(CircularDependencyError) as context:
            self.registry.resolve_dependencies("A")
        
        # Check that the error contains the circular dependency chain
        error = context.exception
        self.assertIn("A", error.dependency_chain)
        self.assertIn("B", error.dependency_chain)
    
    def test_circular_dependency_detection_complex(self):
        """Test detection of complex circular dependency (A -> B -> C -> A)."""
        # Create modules with circular dependency
        for name in ["A", "B", "C"]:
            self.registry.load_module(name, f"./{name}.pas")
        
        # Set up circular dependency: A -> B -> C -> A
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("B", "C")
        self.registry.add_dependency("C", "A")
        
        # Should raise CircularDependencyError
        with self.assertRaises(CircularDependencyError) as context:
            self.registry.resolve_dependencies("A")
        
        # Check that the error contains the full circular dependency chain
        error = context.exception
        self.assertIn("A", error.dependency_chain)
        self.assertIn("B", error.dependency_chain)
        self.assertIn("C", error.dependency_chain)
    
    def test_check_circular_dependencies_method(self):
        """Test the check_circular_dependencies method."""
        # Create modules without circular dependency
        self.registry.load_module("A", "./A.pas")
        self.registry.load_module("B", "./B.pas")
        self.registry.add_dependency("A", "B")
        
        # Should return False (no circular dependency)
        self.assertFalse(self.registry.check_circular_dependencies("A"))
        
        # Add circular dependency
        self.registry.add_dependency("B", "A")
        
        # Should return True (circular dependency detected)
        self.assertTrue(self.registry.check_circular_dependencies("A"))
    
    def test_find_circular_dependency_chain_simple(self):
        """Test finding circular dependency chain for simple cycle."""
        # Create modules with circular dependency: A -> B -> A
        self.registry.load_module("A", "./A.pas")
        self.registry.load_module("B", "./B.pas")
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("B", "A")
        
        # Find the circular dependency chain
        chain = self.registry.find_circular_dependency_chain("A")
        
        # Should find the cycle
        self.assertIsNotNone(chain)
        self.assertIn("A", chain)
        self.assertIn("B", chain)
        # Chain should start and end with the same module
        self.assertEqual(chain[0], chain[-1])
    
    def test_find_circular_dependency_chain_complex(self):
        """Test finding circular dependency chain for complex cycle."""
        # Create modules with circular dependency: A -> B -> C -> D -> A
        for name in ["A", "B", "C", "D"]:
            self.registry.load_module(name, f"./{name}.pas")
        
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("B", "C")
        self.registry.add_dependency("C", "D")
        self.registry.add_dependency("D", "A")
        
        # Find the circular dependency chain
        chain = self.registry.find_circular_dependency_chain("A")
        
        # Should find the complete cycle
        self.assertIsNotNone(chain)
        self.assertEqual(len(chain), 5)  # A -> B -> C -> D -> A
        self.assertEqual(chain[0], chain[-1])  # Starts and ends with same module
        
        # Verify all modules are in the chain
        for module in ["A", "B", "C", "D"]:
            self.assertIn(module, chain)
    
    def test_find_circular_dependency_chain_no_cycle(self):
        """Test finding circular dependency chain when no cycle exists."""
        # Create modules without circular dependency: A -> B -> C
        for name in ["A", "B", "C"]:
            self.registry.load_module(name, f"./{name}.pas")
        
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("B", "C")
        
        # Should return None (no cycle)
        chain = self.registry.find_circular_dependency_chain("A")
        self.assertIsNone(chain)
    
    def test_get_circular_dependency_suggestions(self):
        """Test generation of circular dependency recovery suggestions."""
        # Test with simple cycle
        simple_chain = ["ModuleA", "ModuleB", "ModuleA"]
        suggestions = self.registry.get_circular_dependency_suggestions(simple_chain)
        
        # Should contain multiple suggestions
        self.assertGreater(len(suggestions), 5)
        
        # Should contain key suggestion types
        suggestion_text = "\n".join(suggestions)
        self.assertIn("Extract Common Functionality", suggestion_text)
        self.assertIn("Merge Modules", suggestion_text)
        self.assertIn("Dependency Inversion", suggestion_text)
        self.assertIn("Forward Declarations", suggestion_text)
        
        # Test with complex cycle
        complex_chain = ["A", "B", "C", "D", "A"]
        complex_suggestions = self.registry.get_circular_dependency_suggestions(complex_chain)
        
        # Should contain restructuring suggestions for complex cycles
        complex_text = "\n".join(complex_suggestions)
        self.assertIn("Restructure Dependencies", complex_text)
        self.assertIn("A -> B -> C -> D -> A", complex_text)
    
    def test_circular_dependency_error_with_suggestions(self):
        """Test that CircularDependencyError includes helpful suggestions."""
        # Create circular dependency
        self.registry.load_module("A", "./A.pas")
        self.registry.load_module("B", "./B.pas")
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("B", "A")
        
        # Should raise CircularDependencyError with suggestions
        with self.assertRaises(CircularDependencyError) as context:
            self.registry.resolve_dependencies("A")
        
        error = context.exception
        
        # Should have dependency chain
        self.assertIsNotNone(error.dependency_chain)
        self.assertIn("A", error.dependency_chain)
        self.assertIn("B", error.dependency_chain)
        
        # Should have suggestions
        self.assertIsNotNone(error.suggestions)
        self.assertGreater(len(error.suggestions), 0)
        
        # Error message should include suggestions
        self.assertIn("Extract Common Functionality", str(error))
    
    def test_check_circular_dependencies_performance(self):
        """Test that circular dependency checking is efficient for large graphs."""
        # Create a large dependency graph without cycles
        num_modules = 100
        for i in range(num_modules):
            self.registry.load_module(f"Module{i}", f"./Module{i}.pas")
            if i > 0:
                self.registry.add_dependency(f"Module{i}", f"Module{i-1}")
        
        # Should quickly determine no circular dependencies
        import time
        start_time = time.time()
        has_cycle = self.registry.check_circular_dependencies("Module99")
        end_time = time.time()
        
        self.assertFalse(has_cycle)
        # Should complete in reasonable time (less than 1 second)
        self.assertLess(end_time - start_time, 1.0)
    
    def test_circular_dependency_detection_with_self_dependency(self):
        """Test circular dependency detection when a module depends on itself."""
        self.registry.load_module("SelfDependent", "./SelfDependent.pas")
        self.registry.add_dependency("SelfDependent", "SelfDependent")
        
        # Should detect self-dependency as circular
        self.assertTrue(self.registry.check_circular_dependencies("SelfDependent"))
        
        # Should find the self-dependency chain
        chain = self.registry.find_circular_dependency_chain("SelfDependent")
        self.assertIsNotNone(chain)
        self.assertEqual(chain, ["SelfDependent", "SelfDependent"])
    
    def test_circular_dependency_detection_multiple_entry_points(self):
        """Test circular dependency detection from different entry points in the same cycle."""
        # Create cycle: A -> B -> C -> A
        for name in ["A", "B", "C"]:
            self.registry.load_module(name, f"./{name}.pas")
        
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("B", "C")
        self.registry.add_dependency("C", "A")
        
        # Should detect cycle from any entry point
        self.assertTrue(self.registry.check_circular_dependencies("A"))
        self.assertTrue(self.registry.check_circular_dependencies("B"))
        self.assertTrue(self.registry.check_circular_dependencies("C"))
        
        # Should find chains from any entry point
        chain_a = self.registry.find_circular_dependency_chain("A")
        chain_b = self.registry.find_circular_dependency_chain("B")
        chain_c = self.registry.find_circular_dependency_chain("C")
        
        self.assertIsNotNone(chain_a)
        self.assertIsNotNone(chain_b)
        self.assertIsNotNone(chain_c)
    
    def test_circular_dependency_with_disconnected_components(self):
        """Test circular dependency detection in graphs with disconnected components."""
        # Create two separate cycles: A -> B -> A and C -> D -> C
        for name in ["A", "B", "C", "D"]:
            self.registry.load_module(name, f"./{name}.pas")
        
        # First cycle
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("B", "A")
        
        # Second cycle
        self.registry.add_dependency("C", "D")
        self.registry.add_dependency("D", "C")
        
        # Should detect cycles in both components
        self.assertTrue(self.registry.check_circular_dependencies("A"))
        self.assertTrue(self.registry.check_circular_dependencies("C"))
        
        # Chains should be separate
        chain_a = self.registry.find_circular_dependency_chain("A")
        chain_c = self.registry.find_circular_dependency_chain("C")
        
        self.assertIn("A", chain_a)
        self.assertIn("B", chain_a)
        self.assertNotIn("C", chain_a)
        self.assertNotIn("D", chain_a)
        
        self.assertIn("C", chain_c)
        self.assertIn("D", chain_c)
        self.assertNotIn("A", chain_c)
        self.assertNotIn("B", chain_c)
    
    def test_get_all_dependencies(self):
        """Test getting all transitive dependencies for a module."""
        # Create dependency chain: A -> B -> C -> D
        for name in ["A", "B", "C", "D"]:
            self.registry.load_module(name, f"./{name}.pas")
        
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("B", "C")
        self.registry.add_dependency("C", "D")
        
        # Get all dependencies for A
        all_deps = self.registry.get_all_dependencies("A")
        
        # A should depend on B, C, and D (transitively)
        self.assertEqual(set(all_deps), {"B", "C", "D"})
        
        # Get all dependencies for B
        all_deps_b = self.registry.get_all_dependencies("B")
        
        # B should depend on C and D (transitively)
        self.assertEqual(set(all_deps_b), {"C", "D"})
    
    def test_get_load_order_for_all_modules(self):
        """Test getting load order for all modules in the registry."""
        # Create complex dependency graph
        for name in ["A", "B", "C", "D", "E"]:
            self.registry.load_module(name, f"./{name}.pas")
        
        # Dependencies: A->B, A->C, B->D, C->D, E (standalone)
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("A", "C")
        self.registry.add_dependency("B", "D")
        self.registry.add_dependency("C", "D")
        
        load_order = self.registry.get_load_order_for_all_modules()
        
        # D should come before B and C
        d_index = load_order.index("D")
        b_index = load_order.index("B")
        c_index = load_order.index("C")
        a_index = load_order.index("A")
        
        self.assertLess(d_index, b_index)
        self.assertLess(d_index, c_index)
        self.assertLess(b_index, a_index)
        self.assertLess(c_index, a_index)
        
        # E should be included (standalone module)
        self.assertIn("E", load_order)
    
    def test_has_dependency(self):
        """Test checking if a module has a specific dependency."""
        # Create dependency chain: A -> B -> C
        for name in ["A", "B", "C"]:
            self.registry.load_module(name, f"./{name}.pas")
        
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("B", "C")
        
        # Test direct dependency
        self.assertTrue(self.registry.has_dependency("A", "B"))
        
        # Test transitive dependency
        self.assertTrue(self.registry.has_dependency("A", "C"))
        
        # Test non-existent dependency
        self.assertFalse(self.registry.has_dependency("C", "A"))
    
    def test_get_dependents(self):
        """Test getting modules that depend on a specific module."""
        # Create dependency graph: A->C, B->C, D->B
        for name in ["A", "B", "C", "D"]:
            self.registry.load_module(name, f"./{name}.pas")
        
        self.registry.add_dependency("A", "C")
        self.registry.add_dependency("B", "C")
        self.registry.add_dependency("D", "B")
        
        # C should have dependents A and B
        dependents_c = self.registry.get_dependents("C")
        self.assertEqual(set(dependents_c), {"A", "B"})
        
        # B should have dependent D
        dependents_b = self.registry.get_dependents("B")
        self.assertEqual(dependents_b, ["D"])
        
        # A should have no dependents
        dependents_a = self.registry.get_dependents("A")
        self.assertEqual(dependents_a, [])
    
    def test_add_dependency_prevents_duplicates(self):
        """Test that adding the same dependency multiple times doesn't create duplicates."""
        self.registry.load_module("A", "./A.pas")
        self.registry.load_module("B", "./B.pas")
        
        # Add the same dependency multiple times
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("A", "B")
        self.registry.add_dependency("A", "B")
        
        # Should only have one dependency
        self.assertEqual(self.registry.dependency_graph["A"], ["B"])
    
    def test_dependency_resolution_with_missing_module(self):
        """Test dependency resolution when a dependency module is not loaded."""
        self.registry.load_module("A", "./A.pas")
        
        # Add dependency to non-existent module
        self.registry.add_dependency("A", "NonExistent")
        
        # Should still work (dependency graph tracks relationships even if modules aren't loaded)
        load_order = self.registry.resolve_dependencies("A")
        self.assertEqual(load_order, ["NonExistent", "A"])
    
    def test_complex_dependency_graph(self):
        """Test a complex dependency graph with multiple interconnected modules."""
        # Create modules: App->UI, App->DB, UI->Utils, DB->Utils, Utils->Core
        modules = ["App", "UI", "DB", "Utils", "Core"]
        for name in modules:
            self.registry.load_module(name, f"./{name}.pas")
        
        # Set up dependencies
        self.registry.add_dependency("App", "UI")
        self.registry.add_dependency("App", "DB")
        self.registry.add_dependency("UI", "Utils")
        self.registry.add_dependency("DB", "Utils")
        self.registry.add_dependency("Utils", "Core")
        
        load_order = self.registry.resolve_dependencies("App")
        
        # Core should be first (no dependencies)
        self.assertEqual(load_order[0], "Core")
        
        # Utils should come after Core but before UI and DB
        core_index = load_order.index("Core")
        utils_index = load_order.index("Utils")
        ui_index = load_order.index("UI")
        db_index = load_order.index("DB")
        app_index = load_order.index("App")
        
        self.assertLess(core_index, utils_index)
        self.assertLess(utils_index, ui_index)
        self.assertLess(utils_index, db_index)
        self.assertLess(ui_index, app_index)
        self.assertLess(db_index, app_index)
        
        # App should be last
        self.assertEqual(load_order[-1], "App")


if __name__ == '__main__':
    unittest.main()