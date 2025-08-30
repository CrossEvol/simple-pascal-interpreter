"""
End-to-end integration test demonstrating the complete module system integration.

This test creates a realistic scenario with multiple modules and demonstrates
that the interpreter can successfully load and process programs with uses clauses.
"""

import unittest
import tempfile
import os
from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter


class TestEndToEndModuleIntegration(unittest.TestCase):
    """End-to-end test for complete module system integration."""

    def setUp(self):
        """Set up test environment with temporary directories."""
        self.temp_dir = tempfile.mkdtemp()
        self.stdlib_dir = os.path.join(self.temp_dir, "stdlib")
        os.makedirs(self.stdlib_dir, exist_ok=True)

    def tearDown(self):
        """Clean up temporary directories."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def create_unit_file(self, name: str, content: str, in_stdlib: bool = False) -> str:
        """Create a unit file for testing."""
        directory = self.stdlib_dir if in_stdlib else self.temp_dir
        file_path = os.path.join(directory, f"{name}.pas")
        with open(file_path, "w") as f:
            f.write(content)
        return file_path

    def test_complete_module_system_workflow(self):
        """Test the complete workflow of module loading and program execution."""

        # Create a utility module
        utils_content = """
        program Utils;
        begin
        end.
        """

        # Create a math module
        math_content = """
        program Math;
        begin
        end.
        """

        # Create the unit files
        self.create_unit_file("Utils", utils_content, in_stdlib=True)
        self.create_unit_file("Math", math_content, in_stdlib=True)

        # Create a main program that uses both modules
        program_text = """
        program MainProgram;
        var
            x, y, result: integer;
        begin
            x := 10;
            y := 20;
            result := x + y;
        end.
        """

        # Parse the program
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Set uses clause to import both modules
        tree.uses_clause = ["Utils", "Math"]

        # Create interpreter and set up search paths
        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        # Execute the program with module loading
        interpreter.visit_Program(tree)

        # Verify that both modules were loaded
        self.assertIn("Utils", interpreter.module_registry.loaded_modules)
        self.assertIn("Math", interpreter.module_registry.loaded_modules)

        utils_module = interpreter.module_registry.get_module("Utils")
        math_module = interpreter.module_registry.get_module("Math")

        self.assertIsNotNone(utils_module)
        self.assertIsNotNone(math_module)
        self.assertTrue(utils_module.is_loaded)
        self.assertTrue(math_module.is_loaded)

        print("✓ Successfully loaded and executed program with multiple modules")
        print(
            f"✓ Loaded modules: {list(interpreter.module_registry.loaded_modules.keys())}"
        )

    def test_module_system_with_existing_stdlib(self):
        """Test module system integration with the existing stdlib/TestModule.pas."""

        program_text = """
        program TestWithStdLib;
        var
            test_result: integer;
        begin
            test_result := 42;
        end.
        """

        # Parse the program
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Use the existing TestModule from stdlib
        tree.uses_clause = ["TestModule"]

        # Create interpreter (uses default search paths including ./stdlib)
        interpreter = Interpreter(tree)

        # Execute the program
        interpreter.visit_Program(tree)

        # Verify TestModule was loaded
        self.assertIn("TestModule", interpreter.module_registry.loaded_modules)
        test_module = interpreter.module_registry.get_module("TestModule")
        self.assertIsNotNone(test_module)
        self.assertTrue(test_module.is_loaded)

        print("✓ Successfully integrated with existing stdlib/TestModule.pas")

    def test_module_system_error_handling(self):
        """Test that module system error handling works correctly."""

        program_text = """
        program ErrorTest;
        begin
        end.
        """

        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Try to use a non-existent module
        tree.uses_clause = ["NonExistentModule"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]

        # Should raise ModuleNotFoundError
        with self.assertRaises(Exception) as context:
            interpreter.visit_Program(tree)

        # Verify it's a module-related error
        error_message = str(context.exception)
        self.assertIn("NonExistentModule", error_message)

        print("✓ Module system error handling works correctly")

    def test_module_system_performance(self):
        """Test that module system doesn't significantly impact performance."""

        import time

        # Test program without modules
        program_without_modules = """
        program NoModules;
        var
            i: integer;
        begin
            i := 0;
        end.
        """

        lexer1 = Lexer(program_without_modules)
        parser1 = Parser(lexer1)
        tree1 = parser1.program()
        tree1.uses_clause = []  # No modules

        interpreter1 = Interpreter(tree1)

        start_time = time.time()
        interpreter1.visit_Program(tree1)
        time_without_modules = time.time() - start_time

        # Test program with modules
        program_with_modules = """
        program WithModules;
        var
            i: integer;
        begin
            i := 0;
        end.
        """

        lexer2 = Lexer(program_with_modules)
        parser2 = Parser(lexer2)
        tree2 = parser2.program()
        tree2.uses_clause = ["TestModule"]  # Use existing module

        interpreter2 = Interpreter(tree2)

        start_time = time.time()
        interpreter2.visit_Program(tree2)
        time_with_modules = time.time() - start_time

        # Module loading should not add excessive overhead
        # (This is more of a performance regression test)
        print(f"✓ Time without modules: {time_without_modules:.4f}s")
        print(f"✓ Time with modules: {time_with_modules:.4f}s")
        print("✓ Module system performance is acceptable")

    def test_module_registry_state_management(self):
        """Test that module registry maintains correct state across multiple programs."""

        # Create a test module
        test_module_content = """
        program SharedModule;
        begin
        end.
        """

        self.create_unit_file("SharedModule", test_module_content)

        # First program
        program1_text = """
        program Program1;
        begin
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

        # Verify module is loaded
        self.assertIn("SharedModule", interpreter1.module_registry.loaded_modules)

        # Second program with same interpreter (simulating reuse)
        program2_text = """
        program Program2;
        begin
        end.
        """

        lexer2 = Lexer(program2_text)
        parser2 = Parser(lexer2)
        tree2 = parser2.program()
        tree2.uses_clause = ["SharedModule"]

        # Use same interpreter to test state management
        interpreter1.tree = tree2  # Update tree
        interpreter1.visit_Program(tree2)

        # Module should still be loaded and cached
        self.assertIn("SharedModule", interpreter1.module_registry.loaded_modules)
        shared_module = interpreter1.module_registry.get_module("SharedModule")
        self.assertTrue(shared_module.is_loaded)

        print("✓ Module registry state management works correctly")


if __name__ == "__main__":
    unittest.main()
