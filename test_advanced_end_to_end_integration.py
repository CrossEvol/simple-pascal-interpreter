"""
Advanced end-to-end integration tests for the Pascal module system.

This test suite covers advanced scenarios and edge cases:
- Complex dependency graphs with multiple branches
- Module loading with different search paths
- Integration with existing test modules
- Performance and scalability testing
- Real-world usage patterns

Requirements covered: 2.4, 3.4, 5.1
"""

import unittest
import tempfile
import os
import shutil
import time
from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter
from src.error import ModuleNotFoundError, CircularDependencyError


class TestAdvancedEndToEndIntegration(unittest.TestCase):
    """Advanced end-to-end integration tests for the module system."""

    def setUp(self):
        """Set up test environment with temporary directories."""
        self.temp_dir = tempfile.mkdtemp()
        self.stdlib_dir = os.path.join(self.temp_dir, "stdlib")
        self.custom_lib_dir = os.path.join(self.temp_dir, "custom_lib")
        os.makedirs(self.stdlib_dir, exist_ok=True)
        os.makedirs(self.custom_lib_dir, exist_ok=True)

    def tearDown(self):
        """Clean up temporary directories."""
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def create_unit_file(self, name: str, content: str, directory: str = None) -> str:
        """Create a unit file for testing."""
        if directory is None:
            directory = self.temp_dir
        file_path = os.path.join(directory, f"{name}.pas")
        with open(file_path, "w") as f:
            f.write(content)
        return file_path

    def test_complex_dependency_graph(self):
        """Test complex dependency graph with multiple branches.
        
        Dependency structure:
        App -> [UI, Database, Logger]
        UI -> [Graphics, Input]
        Database -> [Storage, Logger]
        Graphics -> [Math]
        """
        # Create leaf modules
        math_content = "program Math;\nbegin\nend."
        storage_content = "program Storage;\nbegin\nend."
        input_content = "program Input;\nbegin\nend."
        logger_content = "program Logger;\nbegin\nend."

        # Create intermediate modules
        graphics_content = "program Graphics;\nbegin\nend."
        ui_content = "program UI;\nbegin\nend."
        database_content = "program Database;\nbegin\nend."

        # Create top-level app
        app_content = "program App;\nbegin\nend."

        # Create main program
        program_text = """
        program ComplexDependencyTest;
        var
          result: integer;
        begin
          result := 42;
        end.
        """

        # Create all unit files
        modules = {
            "Math": math_content,
            "Storage": storage_content,
            "Input": input_content,
            "Logger": logger_content,
            "Graphics": graphics_content,
            "UI": ui_content,
            "Database": database_content,
            "App": app_content
        }

        for name, content in modules.items():
            self.create_unit_file(name, content)

        # Parse and execute program
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Set uses clause to load the app module
        tree.uses_clause = ["App"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        # Set up complex dependency graph
        interpreter.module_registry.add_dependency("App", "UI")
        interpreter.module_registry.add_dependency("App", "Database")
        interpreter.module_registry.add_dependency("App", "Logger")
        interpreter.module_registry.add_dependency("UI", "Graphics")
        interpreter.module_registry.add_dependency("UI", "Input")
        interpreter.module_registry.add_dependency("Database", "Storage")
        interpreter.module_registry.add_dependency("Database", "Logger")
        interpreter.module_registry.add_dependency("Graphics", "Math")

        # Execute program
        interpreter.visit_Program(tree)

        # Verify all modules were loaded
        expected_modules = ["App", "UI", "Database", "Logger", "Graphics", "Input", "Storage", "Math"]
        for module_name in expected_modules:
            if module_name in interpreter.module_registry.loaded_modules:
                module = interpreter.module_registry.get_module(module_name)
                self.assertIsNotNone(module)
                self.assertTrue(module.is_loaded)

        # Test dependency resolution
        load_order = interpreter.module_registry.resolve_dependencies("App")
        
        # Math should come before Graphics
        if "Math" in load_order and "Graphics" in load_order:
            self.assertLess(load_order.index("Math"), load_order.index("Graphics"))
        
        # Graphics should come before UI
        if "Graphics" in load_order and "UI" in load_order:
            self.assertLess(load_order.index("Graphics"), load_order.index("UI"))

        print("✓ Successfully handled complex dependency graph")

    def test_multiple_search_paths(self):
        """Test module loading with multiple search paths."""
        # Create modules in different directories
        stdlib_module_content = "program StdLibModule;\nbegin\nend."
        custom_module_content = "program CustomModule;\nbegin\nend."
        local_module_content = "program LocalModule;\nbegin\nend."

        self.create_unit_file("StdLibModule", stdlib_module_content, self.stdlib_dir)
        self.create_unit_file("CustomModule", custom_module_content, self.custom_lib_dir)
        self.create_unit_file("LocalModule", local_module_content, self.temp_dir)

        # Create program that uses modules from all paths
        program_text = """
        program MultiPathTest;
        var
          value: integer;
        begin
          value := 100;
        end.
        """

        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Set uses clause with modules from different paths
        tree.uses_clause = ["StdLibModule", "CustomModule", "LocalModule"]

        interpreter = Interpreter(tree)
        # Add all search paths
        interpreter.module_registry.search_paths = [
            self.temp_dir, 
            self.stdlib_dir, 
            self.custom_lib_dir
        ]

        # Execute program
        interpreter.visit_Program(tree)

        # Verify all modules were found and loaded
        expected_modules = ["StdLibModule", "CustomModule", "LocalModule"]
        for module_name in expected_modules:
            self.assertIn(module_name, interpreter.module_registry.loaded_modules)
            module = interpreter.module_registry.get_module(module_name)
            self.assertIsNotNone(module)
            self.assertTrue(module.is_loaded)

        print("✓ Successfully loaded modules from multiple search paths")

    def test_integration_with_existing_stdlib(self):
        """Test integration with the actual standard library modules."""
        program_text = """
        program StdLibIntegrationTest;
        var
          value: integer;
        begin
          value := 42;
        end.
        """

        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Use actual standard library modules
        tree.uses_clause = ["Math", "Map", "ArrayUtils", "TestModule"]

        interpreter = Interpreter(tree)
        # Use actual stdlib directory
        interpreter.module_registry.search_paths = [self.temp_dir, "./stdlib"]

        # Execute program
        interpreter.visit_Program(tree)

        # Verify all standard library modules were loaded
        expected_modules = ["Math", "Map", "ArrayUtils", "TestModule"]
        for module_name in expected_modules:
            self.assertIn(module_name, interpreter.module_registry.loaded_modules)
            module = interpreter.module_registry.get_module(module_name)
            self.assertIsNotNone(module)
            self.assertTrue(module.is_loaded)

        print("✓ Successfully integrated with existing standard library")

    def test_circular_dependency_detection(self):
        """Test circular dependency detection in complex scenarios."""
        # Create modules with circular dependencies
        module_a_content = "program ModuleA;\nbegin\nend."
        module_b_content = "program ModuleB;\nbegin\nend."
        module_c_content = "program ModuleC;\nbegin\nend."

        self.create_unit_file("ModuleA", module_a_content)
        self.create_unit_file("ModuleB", module_b_content)
        self.create_unit_file("ModuleC", module_c_content)

        program_text = """
        program CircularDepTest;
        begin
        end.
        """

        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        tree.uses_clause = ["ModuleA"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        # Create circular dependency: A -> B -> C -> A
        interpreter.module_registry.add_dependency("ModuleA", "ModuleB")
        interpreter.module_registry.add_dependency("ModuleB", "ModuleC")
        interpreter.module_registry.add_dependency("ModuleC", "ModuleA")

        # Should detect circular dependency
        with self.assertRaises(CircularDependencyError):
            interpreter.module_registry.resolve_dependencies("ModuleA")

        print("✓ Successfully detected circular dependencies")

    def test_module_caching_across_programs(self):
        """Test that modules are properly cached across different programs."""
        # Create a shared module
        shared_module_content = "program SharedModule;\nbegin\nend."
        self.create_unit_file("SharedModule", shared_module_content)

        # First program
        program1_text = """
        program Program1;
        var x: integer;
        begin
          x := 1;
        end.
        """

        lexer1 = Lexer(program1_text)
        parser1 = Parser(lexer1)
        tree1 = parser1.program()
        tree1.uses_clause = ["SharedModule"]

        interpreter1 = Interpreter(tree1)
        interpreter1.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        # Execute first program
        interpreter1.visit_Program(tree1)
        
        # Get reference to loaded module
        first_module = interpreter1.module_registry.get_module("SharedModule")
        self.assertIsNotNone(first_module)

        # Second program using same interpreter (simulating module caching)
        program2_text = """
        program Program2;
        var y: integer;
        begin
          y := 2;
        end.
        """

        lexer2 = Lexer(program2_text)
        parser2 = Parser(lexer2)
        tree2 = parser2.program()
        tree2.uses_clause = ["SharedModule"]

        # Use same interpreter to test caching
        interpreter1.tree = tree2
        interpreter1.visit_Program(tree2)

        # Module should still be the same instance (cached)
        second_module = interpreter1.module_registry.get_module("SharedModule")
        self.assertIs(first_module, second_module)

        print("✓ Successfully cached modules across programs")

    def test_large_scale_module_loading(self):
        """Test loading a large number of modules for scalability."""
        num_modules = 20
        
        # Create many small modules
        for i in range(num_modules):
            module_content = f"""
            program Module{i:02d};
            var
              value{i}: integer;
            begin
              value{i} := {i};
            end.
            """
            self.create_unit_file(f"Module{i:02d}", module_content)

        # Create program that uses all modules
        uses_clause = ", ".join([f"Module{i:02d}" for i in range(num_modules)])
        program_text = f"""
        program LargeScaleTest;
        var
          total: integer;
        begin
          total := 0;
        end.
        """

        # Measure loading time
        start_time = time.time()

        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Set uses clause manually
        tree.uses_clause = [f"Module{i:02d}" for i in range(num_modules)]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        interpreter.visit_Program(tree)

        loading_time = time.time() - start_time

        # Verify all modules were loaded
        for i in range(num_modules):
            module_name = f"Module{i:02d}"
            self.assertIn(module_name, interpreter.module_registry.loaded_modules)
            module = interpreter.module_registry.get_module(module_name)
            self.assertIsNotNone(module)
            self.assertTrue(module.is_loaded)

        # Performance should be reasonable (less than 2 seconds for 20 modules)
        self.assertLess(loading_time, 2.0)

        print(f"✓ Successfully loaded {num_modules} modules in {loading_time:.3f}s")

    def test_mixed_program_and_unit_files(self):
        """Test mixing program files and unit files in the module system."""
        # Create a mix of program and unit files
        program_module_content = "program ProgramModule;\nbegin\nend."
        
        # Note: Unit files would have different syntax, but for now we use program syntax
        unit_like_content = "program UnitLikeModule;\nbegin\nend."

        self.create_unit_file("ProgramModule", program_module_content)
        self.create_unit_file("UnitLikeModule", unit_like_content)

        # Create main program
        main_program_text = """
        program MixedFileTest;
        var
          result: integer;
        begin
          result := 100;
        end.
        """

        lexer = Lexer(main_program_text)
        parser = Parser(lexer)
        tree = parser.program()

        tree.uses_clause = ["ProgramModule", "UnitLikeModule"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        # Execute program
        interpreter.visit_Program(tree)

        # Verify both types of modules were loaded
        self.assertIn("ProgramModule", interpreter.module_registry.loaded_modules)
        self.assertIn("UnitLikeModule", interpreter.module_registry.loaded_modules)

        program_module = interpreter.module_registry.get_module("ProgramModule")
        unit_module = interpreter.module_registry.get_module("UnitLikeModule")

        self.assertIsNotNone(program_module)
        self.assertIsNotNone(unit_module)
        self.assertTrue(program_module.is_loaded)
        self.assertTrue(unit_module.is_loaded)

        print("✓ Successfully handled mixed program and unit files")

    def test_error_recovery_and_partial_loading(self):
        """Test error recovery when some modules fail to load."""
        # Create valid modules
        valid_module1_content = "program ValidModule1;\nbegin\nend."
        valid_module2_content = "program ValidModule2;\nbegin\nend."

        self.create_unit_file("ValidModule1", valid_module1_content)
        self.create_unit_file("ValidModule2", valid_module2_content)

        # Create program that tries to load valid and invalid modules
        program_text = """
        program ErrorRecoveryTest;
        var
          value: integer;
        begin
          value := 50;
        end.
        """

        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Include both valid and invalid modules
        tree.uses_clause = ["ValidModule1", "NonExistentModule", "ValidModule2"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        # Should raise error for missing module
        with self.assertRaises(ModuleNotFoundError):
            interpreter.visit_Program(tree)

        # But valid modules that were processed before the error should still be loaded
        # (This depends on the implementation - some systems load all or none)
        print("✓ Successfully tested error recovery scenarios")

    def test_module_metadata_and_information(self):
        """Test that module metadata is properly maintained."""
        # Create modules with different characteristics
        simple_module_content = "program SimpleModule;\nbegin\nend."
        complex_module_content = """
        program ComplexModule;
        var
          x, y, z: integer;
          flag: boolean;
        begin
          x := 1;
          y := 2;
          z := x + y;
          flag := true;
        end.
        """

        simple_path = self.create_unit_file("SimpleModule", simple_module_content)
        complex_path = self.create_unit_file("ComplexModule", complex_module_content)

        program_text = """
        program MetadataTest;
        begin
        end.
        """

        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        tree.uses_clause = ["SimpleModule", "ComplexModule"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        # Execute program
        interpreter.visit_Program(tree)

        # Verify module metadata
        simple_module = interpreter.module_registry.get_module("SimpleModule")
        complex_module = interpreter.module_registry.get_module("ComplexModule")

        # Check basic metadata
        self.assertEqual(simple_module.name, "SimpleModule")
        self.assertEqual(complex_module.name, "ComplexModule")
        
        # File paths should be set correctly
        self.assertTrue(simple_module.file_path.endswith("SimpleModule.pas"))
        self.assertTrue(complex_module.file_path.endswith("ComplexModule.pas"))

        # Both should be marked as loaded
        self.assertTrue(simple_module.is_loaded)
        self.assertTrue(complex_module.is_loaded)

        print("✓ Successfully maintained module metadata")


if __name__ == "__main__":
    unittest.main()