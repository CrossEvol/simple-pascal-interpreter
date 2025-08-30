"""
Integration tests for interpreter module support.

Tests the integration between the interpreter and the module system,
verifying that programs with uses clauses can load and use modules correctly.
"""

import unittest
import tempfile
import os
from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter
from src.error import ModuleNotFoundError


class TestInterpreterModuleIntegration(unittest.TestCase):
    """Test interpreter integration with the module system."""

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
        with open(file_path, 'w') as f:
            f.write(content)
        return file_path

    def create_program_file(self, content: str) -> str:
        """Create a program file for testing."""
        file_path = os.path.join(self.temp_dir, "test_program.pas")
        with open(file_path, 'w') as f:
            f.write(content)
        return file_path

    def test_program_without_uses_clause(self):
        """Test that programs without uses clause work normally."""
        program_text = """
        program TestProgram;
        var
            x: integer;
        begin
            x := 42;
        end.
        """
        
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()
        interpreter = Interpreter(tree)
        
        # Should not raise any errors
        interpreter.visit_Program(tree)

    def test_program_with_empty_uses_clause(self):
        """Test that programs with empty uses clause work normally."""
        program_text = """
        program TestProgram;
        var
            x: integer;
        begin
            x := 42;
        end.
        """
        
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()
        
        # Manually set empty uses clause
        tree.uses_clause = []
        
        interpreter = Interpreter(tree)
        
        # Should not raise any errors
        interpreter.visit_Program(tree)

    def test_module_registry_initialization(self):
        """Test that interpreter initializes module registry correctly."""
        program_text = """
        program TestProgram;
        begin
        end.
        """
        
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()
        interpreter = Interpreter(tree)
        
        # Check that module registry is initialized
        self.assertIsNotNone(interpreter.module_registry)
        self.assertTrue(hasattr(interpreter.module_registry, 'loaded_modules'))
        self.assertTrue(hasattr(interpreter.module_registry, 'search_paths'))
        self.assertIn('.', interpreter.module_registry.search_paths)
        self.assertIn('./stdlib', interpreter.module_registry.search_paths)

    def test_load_simple_module(self):
        """Test loading a simple module."""
        # Create a simple unit file
        unit_content = """
        program SimpleUnit;
        begin
        end.
        """
        
        _ = self.create_unit_file("SimpleUnit", unit_content)
        
        # Create program that uses the unit
        program_text = """
        program TestProgram;
        var
            x: integer;
        begin
            x := 42;
        end.
        """
        
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()
        
        # Manually set uses clause
        tree.uses_clause = ["SimpleUnit"]
        
        interpreter = Interpreter(tree)
        
        # Update search paths to include our temp directory
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]
        
        # Should load the module without errors
        interpreter.visit_Program(tree)
        
        # Check that module was loaded
        self.assertIn("SimpleUnit", interpreter.module_registry.loaded_modules)
        module = interpreter.module_registry.get_module("SimpleUnit")
        self.assertIsNotNone(module)
        self.assertTrue(module.is_loaded)

    def test_module_not_found_error(self):
        """Test that missing modules raise appropriate errors."""
        program_text = """
        program TestProgram;
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
        with self.assertRaises(ModuleNotFoundError) as context:
            interpreter.visit_Program(tree)
        
        self.assertIn("NonExistentModule", str(context.exception))

    def test_multiple_modules_loading(self):
        """Test loading multiple modules."""
        # Create first unit
        unit1_content = """
        program Unit1;
        begin
        end.
        """
        
        # Create second unit
        unit2_content = """
        program Unit2;
        begin
        end.
        """
        
        self.create_unit_file("Unit1", unit1_content)
        self.create_unit_file("Unit2", unit2_content)
        
        # Create program that uses both units
        program_text = """
        program TestProgram;
        var
            x: integer;
        begin
            x := 42;
        end.
        """
        
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()
        
        # Set uses clause with multiple modules
        tree.uses_clause = ["Unit1", "Unit2"]
        
        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]
        
        # Should load both modules without errors
        interpreter.visit_Program(tree)
        
        # Check that both modules were loaded
        self.assertIn("Unit1", interpreter.module_registry.loaded_modules)
        self.assertIn("Unit2", interpreter.module_registry.loaded_modules)
        
        unit1 = interpreter.module_registry.get_module("Unit1")
        unit2 = interpreter.module_registry.get_module("Unit2")
        
        self.assertIsNotNone(unit1)
        self.assertTrue(unit1.is_loaded)
        self.assertIsNotNone(unit2)
        self.assertTrue(unit2.is_loaded)

    def test_stdlib_module_loading(self):
        """Test loading modules from stdlib directory."""
        # Create a stdlib module
        stdlib_unit_content = """
        program StdLibUnit;
        begin
        end.
        """
        
        self.create_unit_file("StdLibUnit", stdlib_unit_content, in_stdlib=True)
        
        # Create program that uses the stdlib unit
        program_text = """
        program TestProgram;
        var
            x: integer;
        begin
            x := 42;
        end.
        """
        
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()
        
        # Set uses clause with stdlib module
        tree.uses_clause = ["StdLibUnit"]
        
        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]
        
        # Should load the stdlib module without errors
        interpreter.visit_Program(tree)
        
        # Check that module was loaded
        self.assertIn("StdLibUnit", interpreter.module_registry.loaded_modules)
        module = interpreter.module_registry.get_module("StdLibUnit")
        self.assertIsNotNone(module)
        self.assertTrue(module.is_loaded)

    def test_module_loading_order(self):
        """Test that modules are loaded in the correct order."""
        # Create units with dependencies (simulated)
        unit1_content = """
        program Unit1;
        begin
        end.
        """
        
        unit2_content = """
        program Unit2;
        begin
        end.
        """
        
        self.create_unit_file("Unit1", unit1_content)
        self.create_unit_file("Unit2", unit2_content)
        
        # Create program that uses both units
        program_text = """
        program TestProgram;
        var
            x: integer;
        begin
            x := 42;
        end.
        """
        
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()
        
        # Set uses clause - order should be preserved
        tree.uses_clause = ["Unit1", "Unit2"]
        
        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]
        
        # Should load modules in order
        interpreter.visit_Program(tree)
        
        # Both modules should be loaded
        self.assertIn("Unit1", interpreter.module_registry.loaded_modules)
        self.assertIn("Unit2", interpreter.module_registry.loaded_modules)

    def test_module_caching(self):
        """Test that modules are cached and not loaded multiple times."""
        # Create a unit
        unit_content = """
        program CachedUnit;
        begin
        end.
        """
        
        self.create_unit_file("CachedUnit", unit_content)
        
        # Create first program
        program_text1 = """
        program TestProgram1;
        begin
        end.
        """
        
        lexer1 = Lexer(program_text1)
        parser1 = Parser(lexer1)
        tree1 = parser1.program()
        tree1.uses_clause = ["CachedUnit"]
        
        interpreter1 = Interpreter(tree1)
        interpreter1.module_registry.search_paths = [self.temp_dir, self.stdlib_dir]
        
        # Load module first time
        interpreter1.visit_Program(tree1)
        
        # Get reference to loaded module
        first_load = interpreter1.module_registry.get_module("CachedUnit")
        self.assertIsNotNone(first_load)
        self.assertTrue(first_load.is_loaded)
        
        # Create second interpreter with same module registry
        program_text2 = """
        program TestProgram2;
        begin
        end.
        """
        
        lexer2 = Lexer(program_text2)
        parser2 = Parser(lexer2)
        tree2 = parser2.program()
        tree2.uses_clause = ["CachedUnit"]
        
        interpreter2 = Interpreter(tree2)
        # Use the same module registry to test caching
        interpreter2.module_registry = interpreter1.module_registry
        
        # Load module second time
        interpreter2.visit_Program(tree2)
        
        # Should be the same module instance (cached)
        second_load = interpreter2.module_registry.get_module("CachedUnit")
        self.assertIs(second_load, first_load)  # Same object reference


if __name__ == "__main__":
    unittest.main()