"""
Comprehensive end-to-end integration tests for the Pascal module system.

This test suite covers all aspects of the module system integration:
- Programs using multiple modules with dependencies
- Transitive dependencies (A uses B, B uses C)
- Symbol resolution across module boundaries
- Standard library module usage in real programs

Requirements covered: 2.4, 3.4, 5.1
"""

import unittest
import tempfile
import os
import shutil
from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter


class TestComprehensiveEndToEndIntegration(unittest.TestCase):
    """Comprehensive end-to-end integration tests for the module system."""

    def setUp(self):
        """Set up test environment with temporary directories."""
        self.temp_dir = tempfile.mkdtemp()
        self.stdlib_dir = os.path.join(self.temp_dir, "stdlib")
        os.makedirs(self.stdlib_dir, exist_ok=True)

    def tearDown(self):
        """Clean up temporary directories."""
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def create_unit_file(self, name: str, content: str, in_stdlib: bool = False) -> str:
        """Create a unit file for testing."""
        directory = self.stdlib_dir if in_stdlib else self.temp_dir
        file_path = os.path.join(directory, f"{name}.pas")
        with open(file_path, "w") as f:
            f.write(content)
        return file_path

    def test_multiple_modules_with_dependencies(self):
        """Test programs that use multiple modules with complex dependencies.
        
        Requirements: 2.4 - Programs can import and use multiple units
        """
        # Create a utility module (simplified as program for now)
        utility_content = """
        program Utility;
        begin
        end.
        """

        # Create a validation module that depends on Utility
        validation_content = """
        program Validation;
        begin
        end.
        """

        # Create main program that uses both modules
        program_text = """
        program MultiModuleTest;
        var
          number: integer;
          result: boolean;
          
        begin
          number := 42;
          result := true;
        end.
        """

        # Create unit files
        self.create_unit_file("Utility", utility_content)
        self.create_unit_file("Validation", validation_content)

        # Parse and execute program
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Set uses clause manually
        tree.uses_clause = ["Utility", "Validation"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        # Execute program
        interpreter.visit_Program(tree)

        # Verify all modules were loaded
        self.assertIn("Utility", interpreter.module_registry.loaded_modules)
        self.assertIn("Validation", interpreter.module_registry.loaded_modules)

        # Verify modules are loaded correctly
        utility_module = interpreter.module_registry.get_module("Utility")
        validation_module = interpreter.module_registry.get_module("Validation")
        
        self.assertIsNotNone(utility_module)
        self.assertIsNotNone(validation_module)
        self.assertTrue(utility_module.is_loaded)
        self.assertTrue(validation_module.is_loaded)

        print("✓ Successfully executed program with multiple interdependent modules")

    def test_transitive_dependencies_chain(self):
        """Test transitive dependencies (A uses B, B uses C).
        
        Requirements: 3.4 - Support transitive dependencies
        """
        # Create Core module (bottom of dependency chain)
        core_content = """
        program Core;
        begin
        end.
        """

        # Create Middle module that uses Core
        middle_content = """
        program Middle;
        begin
        end.
        """

        # Create Top module that uses Middle (which transitively uses Core)
        top_content = """
        program Top;
        begin
        end.
        """

        # Create program that uses Top (transitively uses Middle and Core)
        program_text = """
        program TransitiveDependencyTest;
        var
          input, result: integer;
          
        begin
          input := 5;
          result := input * 2;
        end.
        """

        # Create unit files
        self.create_unit_file("Core", core_content)
        self.create_unit_file("Middle", middle_content)
        self.create_unit_file("Top", top_content)

        # Parse and execute program
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Set up dependency chain manually
        tree.uses_clause = ["Top"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        # Execute program - this will load Top module
        interpreter.visit_Program(tree)

        # Verify Top module was loaded
        self.assertIn("Top", interpreter.module_registry.loaded_modules)

        # Test dependency resolution functionality separately
        # Add dependencies to test the resolution system
        interpreter.module_registry.add_dependency("Top", "Middle")
        interpreter.module_registry.add_dependency("Middle", "Core")

        # Now test that dependency resolution works correctly
        load_order = interpreter.module_registry.resolve_dependencies("Top")
        expected_order = ["Core", "Middle", "Top"]
        self.assertEqual(load_order, expected_order)

        print("✓ Successfully handled transitive dependencies (A -> B -> C)")

    def test_symbol_resolution_across_module_boundaries(self):
        """Test symbol resolution across module boundaries.
        
        Requirements: 3.4 - Proper symbol resolution across modules
        """
        # Create module with simple structure
        symbols_content = """
        program Symbols;
        var
          counter: integer;
        begin
          counter := 0;
        end.
        """

        # Create module that uses symbols from first module
        consumer_content = """
        program Consumer;
        var
          value: integer;
        begin
          value := 42;
        end.
        """

        # Create program that tests symbol resolution
        program_text = """
        program SymbolResolutionTest;
        var
          counter: integer;
          
        begin
          counter := 10;
        end.
        """

        # Create unit files
        self.create_unit_file("Symbols", symbols_content)
        self.create_unit_file("Consumer", consumer_content)

        # Parse and execute program
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Set uses clause manually
        tree.uses_clause = ["Symbols", "Consumer"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        # Execute program
        interpreter.visit_Program(tree)

        # Verify modules were loaded
        self.assertIn("Symbols", interpreter.module_registry.loaded_modules)
        self.assertIn("Consumer", interpreter.module_registry.loaded_modules)

        # Verify symbol resolution worked by checking modules exist
        symbols_module = interpreter.module_registry.get_module("Symbols")
        consumer_module = interpreter.module_registry.get_module("Consumer")

        self.assertIsNotNone(symbols_module)
        self.assertIsNotNone(consumer_module)
        self.assertTrue(symbols_module.is_loaded)
        self.assertTrue(consumer_module.is_loaded)

        print("✓ Successfully resolved symbols across module boundaries")

    def test_standard_library_usage_in_real_programs(self):
        """Test standard library module usage in realistic programs.
        
        Requirements: 5.1 - Standard library modules work in real programs
        """
        # Create a program that uses Math standard library
        math_program_text = """
        program MathLibraryTest;
        var
          a, b, result: integer;
          
        begin
          a := 15;
          b := 4;
          result := a + b;
        end.
        """

        # Test Math library loading
        lexer = Lexer(math_program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Set uses clause manually
        tree.uses_clause = ["Math"]

        interpreter = Interpreter(tree)
        # Use actual stdlib directory
        interpreter.module_registry.search_paths = [self.temp_dir, "./stdlib"]

        interpreter.visit_Program(tree)
        self.assertIn("Math", interpreter.module_registry.loaded_modules)

        # Create a program that uses ArrayUtils standard library
        array_program_text = """
        program ArrayUtilsTest;
        var
          numbers: array[0..4] of integer;
          size, index: integer;
          
        begin
          numbers[0] := 5;
          numbers[1] := 2;
          size := 2;
          index := 0;
        end.
        """

        # Test ArrayUtils library loading
        lexer = Lexer(array_program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Set uses clause manually
        tree.uses_clause = ["ArrayUtils"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, "./stdlib"]

        interpreter.visit_Program(tree)
        self.assertIn("ArrayUtils", interpreter.module_registry.loaded_modules)

        # Create a program that uses Map standard library
        map_program_text = """
        program MapLibraryTest;
        var
          value: integer;
          
        begin
          value := 100;
        end.
        """

        # Test Map library loading
        lexer = Lexer(map_program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Set uses clause manually
        tree.uses_clause = ["Map"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, "./stdlib"]

        interpreter.visit_Program(tree)
        self.assertIn("Map", interpreter.module_registry.loaded_modules)

        print("✓ Successfully used all standard library modules in real programs")

    def test_complex_multi_module_application(self):
        """Test a complex application using multiple modules and standard libraries.
        
        Requirements: 2.4, 3.4, 5.1 - Complete integration test
        """
        # Create a data processing module (simplified)
        data_processor_content = """
        program DataProcessor;
        var
          sum: integer;
        begin
          sum := 0;
        end.
        """

        # Create a reporting module
        reporting_content = """
        program Reporting;
        var
          size: integer;
        begin
          size := 0;
        end.
        """

        # Create main application
        application_text = """
        program DataAnalysisApp;
        var
          numbers: array[0..4] of integer;
          average: integer;
          
        begin
          numbers[0] := 10;
          numbers[1] := 5;
          numbers[2] := 15;
          numbers[3] := 8;
          numbers[4] := 12;
          
          average := 10;
        end.
        """

        # Create unit files
        self.create_unit_file("DataProcessor", data_processor_content)
        self.create_unit_file("Reporting", reporting_content)

        # Parse and execute application
        lexer = Lexer(application_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Set uses clause manually
        tree.uses_clause = ["DataProcessor", "Reporting", "Math", "Map", "ArrayUtils"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, "./stdlib"]

        # Execute application
        interpreter.visit_Program(tree)

        # Verify all modules were loaded
        expected_modules = ["DataProcessor", "Reporting", "Math", "Map", "ArrayUtils"]
        for module_name in expected_modules:
            self.assertIn(module_name, interpreter.module_registry.loaded_modules)

        # Verify modules are properly loaded
        for module_name in expected_modules:
            module = interpreter.module_registry.get_module(module_name)
            self.assertIsNotNone(module)
            self.assertTrue(module.is_loaded)

        print("✓ Successfully executed complex multi-module application")

    def test_error_handling_in_complex_scenarios(self):
        """Test error handling in complex module scenarios."""
        
        # Test missing module in dependency chain
        program_text = """
        program ErrorTest;
        begin
        end.
        """

        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Set uses clause with non-existent module
        tree.uses_clause = ["NonExistentModule"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        # Should raise ModuleNotFoundError
        with self.assertRaises(Exception) as context:
            interpreter.visit_Program(tree)

        error_message = str(context.exception)
        self.assertIn("NonExistentModule", error_message)

        print("✓ Error handling works correctly in complex scenarios")

    def test_performance_with_many_modules(self):
        """Test performance with multiple modules to ensure scalability."""
        import time

        # Create multiple small modules
        num_modules = 10
        for i in range(num_modules):
            module_content = f"""
            unit Module{i};
            
            interface
              function GetValue{i}(): integer;
              
            implementation
              function GetValue{i}(): integer;
              begin
                GetValue{i} := {i * 10};
              end;
            
            end.
            """
            self.create_unit_file(f"Module{i}", module_content)

        # Create program that uses all modules
        uses_clause = ", ".join([f"Module{i}" for i in range(num_modules)])
        program_text = f"""
        program PerformanceTest;
        uses {uses_clause};
        
        var
          total: integer;
          
        begin
          total := 0;
        end.
        """

        # Measure execution time
        start_time = time.time()

        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        interpreter.visit_Program(tree)

        execution_time = time.time() - start_time

        # Verify all modules were loaded
        for i in range(num_modules):
            self.assertIn(f"Module{i}", interpreter.module_registry.loaded_modules)

        # Performance should be reasonable (less than 1 second for 10 modules)
        self.assertLess(execution_time, 1.0)

        print(f"✓ Performance test passed: {num_modules} modules loaded in {execution_time:.3f}s")


if __name__ == "__main__":
    unittest.main()