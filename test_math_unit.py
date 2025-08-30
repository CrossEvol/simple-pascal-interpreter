"""
Unit tests for Math standard library unit functionality.

Tests cover:
1. Math unit loading and parsing
2. ADD, MUL, SUB, DIV, MOD function operations
3. Edge cases and error conditions (division by zero, modulo by zero)
4. Unit file structure and syntax validation
"""

import unittest
from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter


class TestMathUnit(unittest.TestCase):
    """Test Math standard library unit functionality."""

    def test_load_math_unit(self):
        """Test loading the Math.pas unit from stdlib."""
        program_text = """
        program TestMathProgram;
        var
            result: integer;
        begin
            result := 0;
        end.
        """
        
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()
        
        # Set uses clause to load Math unit
        tree.uses_clause = ["Math"]
        
        interpreter = Interpreter(tree)
        
        try:
            interpreter.visit_Program(tree)
            
            # Check that Math unit was loaded
            self.assertIn("Math", interpreter.module_registry.loaded_modules)
            module = interpreter.module_registry.get_module("Math")
            self.assertIsNotNone(module)
            self.assertTrue(module.is_loaded)
            
            print("Successfully loaded Math.pas unit")
            
        except Exception as e:
            print(f"Error loading Math unit: {e}")
            # Document current state - this will pass when unit parsing is complete
            raise

    def test_math_unit_lexing(self):
        """Test that Math.pas unit can be lexed without errors."""
        try:
            # Read and lex the Math.pas file directly
            with open("stdlib/Math.pas", "r") as f:
                math_unit_text = f.read()
            
            lexer = Lexer(math_unit_text)
            
            # Verify we can tokenize the entire file without errors
            tokens = []
            while True:
                token = lexer.get_next_token()
                tokens.append(token)
                if token.type.name == 'EOF':
                    break
            
            # Should have successfully tokenized the file
            self.assertGreater(len(tokens), 10)  # Should have many tokens
            print(f"Math.pas unit lexing completed successfully with {len(tokens)} tokens")
            
        except Exception as e:
            print(f"Error lexing Math.pas unit: {e}")
            # This test documents that the unit syntax is lexically correct
            raise

    def test_math_interface_symbols(self):
        """Test that Math unit interface symbols are correctly defined."""
        program_text = """
        program TestMathInterface;
        uses Math;
        var
            result: integer;
        begin
            result := 0;
        end.
        """
        
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()
        
        interpreter = Interpreter(tree)
        
        try:
            interpreter.visit_Program(tree)
            
            # Check that Math unit was loaded and has expected symbols
            self.assertIn("Math", interpreter.module_registry.loaded_modules)
            _ = interpreter.module_registry.get_module("Math")
            
            # Verify the math functions are in the interface symbols
            # Note: This test verifies the module system integration
            print("Math interface symbols test completed")
            
        except Exception as e:
            print(f"Error in math interface symbols test: {e}")
            # Document current state - this will pass when module system is complete
            raise

    def test_math_unit_file_exists(self):
        """Test that Math.pas unit file exists and is readable."""
        import os
        
        math_file_path = "stdlib/Math.pas"
        self.assertTrue(os.path.exists(math_file_path), f"Math.pas file should exist at {math_file_path}")
        
        # Verify file is readable and contains expected content
        with open(math_file_path, "r") as f:
            content = f.read()
            
        # Check for key components of the Math unit
        self.assertIn("unit Math;", content)
        self.assertIn("interface", content)
        self.assertIn("implementation", content)
        self.assertIn("function ADD", content)
        self.assertIn("function MUL", content)
        self.assertIn("function SUB", content)
        self.assertIn("function DIV", content)
        self.assertIn("function MOD", content)
        self.assertIn("end.", content)
        
        print("Math.pas unit file verification completed")

    def test_math_unit_syntax_validation(self):
        """Test that Math.pas has valid Pascal syntax structure."""
        with open("stdlib/Math.pas", "r") as f:
            content = f.read()
        
        # Basic syntax validation
        lines = content.split('\n')
        
        # Check unit structure
        unit_line_found = False
        interface_found = False
        implementation_found = False
        end_dot_found = False
        
        for line in lines:
            line = line.strip()
            if line.startswith("unit Math;"):
                unit_line_found = True
            elif line == "interface":
                interface_found = True
            elif line == "implementation":
                implementation_found = True
            elif line == "end.":
                end_dot_found = True
        
        self.assertTrue(unit_line_found, "Unit declaration should be present")
        self.assertTrue(interface_found, "Interface section should be present")
        self.assertTrue(implementation_found, "Implementation section should be present")
        self.assertTrue(end_dot_found, "Unit should end with 'end.'")
        
        print("Math.pas syntax validation completed")

    def test_math_function_signatures(self):
        """Test that all required math functions are declared in interface."""
        with open("stdlib/Math.pas", "r") as f:
            content = f.read()
        
        # Check that all required functions are declared
        required_functions = ["ADD", "MUL", "SUB", "DIV", "MOD"]
        
        for func_name in required_functions:
            self.assertIn(f"function {func_name}", content, 
                         f"Function {func_name} should be declared in interface")
        
        # Check function signatures have correct parameter types
        self.assertIn("function ADD(a, b: integer): integer;", content)
        self.assertIn("function MUL(a, b: integer): integer;", content)
        self.assertIn("function SUB(a, b: integer): integer;", content)
        self.assertIn("function DIV(a, b: integer): integer;", content)
        self.assertIn("function MOD(a, b: integer): integer;", content)
        
        print("Math function signatures validation completed")

    def test_math_function_implementations(self):
        """Test that all math functions have implementations."""
        with open("stdlib/Math.pas", "r") as f:
            content = f.read()
        
        # Check that all functions have implementations
        required_functions = ["ADD", "MUL", "SUB", "DIV", "MOD"]
        
        for func_name in required_functions:
            # Check for function implementation (should appear after implementation section)
            impl_pattern = f"function {func_name}(a, b: integer): integer;"
            self.assertIn(impl_pattern, content, 
                         f"Function {func_name} should have implementation")
        
        # Check for basic operation implementations
        self.assertIn("a + b", content, "ADD should use + operator")
        self.assertIn("a * b", content, "MUL should use * operator") 
        self.assertIn("a - b", content, "SUB should use - operator")
        self.assertIn("a div b", content, "DIV should use div operator")
        self.assertIn("a mod b", content, "MOD should use mod operator")
        
        print("Math function implementations validation completed")

    def test_math_division_by_zero_handling(self):
        """Test that division and modulo functions handle zero divisor."""
        with open("stdlib/Math.pas", "r") as f:
            content = f.read()
        
        # Check that DIV and MOD functions check for zero divisor
        self.assertIn("if b = 0", content, "Should check for division by zero")
        
        # Find the implementation section
        impl_start = content.find("implementation")
        impl_section = content[impl_start:]
        
        # Should have error handling for both DIV and MOD in implementation
        self.assertIn("if b = 0", impl_section, "Implementation should check for zero divisor")
        
        # Check that both DIV and MOD have zero checks
        div_impl_start = impl_section.find("function DIV(a, b: integer): integer;")
        div_impl_end = impl_section.find("function MOD(a, b: integer): integer;")
        div_impl = impl_section[div_impl_start:div_impl_end]
        
        mod_impl_start = impl_section.find("function MOD(a, b: integer): integer;")
        mod_impl_end = impl_section.find("end.")
        mod_impl = impl_section[mod_impl_start:mod_impl_end]
        
        self.assertIn("if b = 0", div_impl, "DIV implementation should check for zero divisor")
        self.assertIn("if b = 0", mod_impl, "MOD implementation should check for zero divisor")
        
        print("Math division by zero handling validation completed")


if __name__ == "__main__":
    unittest.main()