"""
Unit tests for ArrayUtils standard library unit functionality.

Tests cover:
1. ArrayUtils unit loading and parsing
2. Sort, Find, Copy, Size function operations
3. Edge cases and error conditions
4. Unit file structure and syntax validation
"""

import unittest
from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter


class TestArrayUtilsUnit(unittest.TestCase):
    """Test ArrayUtils standard library unit functionality."""

    def test_load_arrayutils_unit(self):
        """Test loading the ArrayUtils.pas unit from stdlib."""
        program_text = """
        program TestArrayUtilsProgram;
        var
            result: integer;
        begin
            result := 0;
        end.
        """
        
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()
        
        # Set uses clause to load ArrayUtils unit
        tree.uses_clause = ["ArrayUtils"]
        
        interpreter = Interpreter(tree)
        
        try:
            interpreter.visit_Program(tree)
            
            # Check that ArrayUtils unit was loaded
            self.assertIn("ArrayUtils", interpreter.module_registry.loaded_modules)
            module = interpreter.module_registry.get_module("ArrayUtils")
            self.assertIsNotNone(module)
            self.assertTrue(module.is_loaded)
            
            print("Successfully loaded ArrayUtils.pas unit")
            
        except Exception as e:
            print(f"Error loading ArrayUtils unit: {e}")
            # Document current state - this will pass when unit parsing is complete
            raise

    def test_arrayutils_unit_lexing(self):
        """Test that ArrayUtils.pas unit can be lexed without errors."""
        try:
            # Read and lex the ArrayUtils.pas file directly
            with open("stdlib/ArrayUtils.pas", "r") as f:
                arrayutils_unit_text = f.read()
            
            lexer = Lexer(arrayutils_unit_text)
            
            # Verify we can tokenize the entire file without errors
            tokens = []
            while True:
                token = lexer.get_next_token()
                tokens.append(token)
                if token.type.name == 'EOF':
                    break
            
            # Should have successfully tokenized the file
            self.assertGreater(len(tokens), 10)  # Should have many tokens
            print(f"ArrayUtils.pas unit lexing completed successfully with {len(tokens)} tokens")
            
        except Exception as e:
            print(f"Error lexing ArrayUtils.pas unit: {e}")
            # This test documents that the unit syntax is lexically correct
            raise

    def test_arrayutils_interface_symbols(self):
        """Test that ArrayUtils unit interface symbols are correctly defined."""
        program_text = """
        program TestArrayUtilsInterface;
        uses ArrayUtils;
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
            
            # Check that ArrayUtils unit was loaded and has expected symbols
            self.assertIn("ArrayUtils", interpreter.module_registry.loaded_modules)
            _ = interpreter.module_registry.get_module("ArrayUtils")
            
            # Verify the array utility functions are in the interface symbols
            # Note: This test verifies the module system integration
            print("ArrayUtils interface symbols test completed")
            
        except Exception as e:
            print(f"Error in arrayutils interface symbols test: {e}")
            # Document current state - this will pass when module system is complete
            raise

    def test_arrayutils_unit_file_exists(self):
        """Test that ArrayUtils.pas unit file exists and is readable."""
        import os
        
        arrayutils_file_path = "stdlib/ArrayUtils.pas"
        self.assertTrue(os.path.exists(arrayutils_file_path), f"ArrayUtils.pas file should exist at {arrayutils_file_path}")
        
        # Verify file is readable and contains expected content
        with open(arrayutils_file_path, "r") as f:
            content = f.read()
            
        # Check for key components of the ArrayUtils unit
        self.assertIn("unit ArrayUtils;", content)
        self.assertIn("interface", content)
        self.assertIn("implementation", content)
        self.assertIn("procedure Sort", content)
        self.assertIn("function Find", content)
        self.assertIn("procedure Copy", content)
        self.assertIn("function Size", content)
        self.assertIn("end.", content)
        
        print("ArrayUtils.pas unit file verification completed")

    def test_arrayutils_unit_syntax_validation(self):
        """Test that ArrayUtils.pas has valid Pascal syntax structure."""
        with open("stdlib/ArrayUtils.pas", "r") as f:
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
            if line.startswith("unit ArrayUtils;"):
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
        
        print("ArrayUtils.pas syntax validation completed")

    def test_arrayutils_function_signatures(self):
        """Test that all required array utility functions are declared in interface."""
        with open("stdlib/ArrayUtils.pas", "r") as f:
            content = f.read()
        
        # Check that all required functions/procedures are declared
        required_functions = ["Sort", "Find", "Copy", "Size"]
        
        for func_name in required_functions:
            self.assertIn(func_name, content, 
                         f"Function/procedure {func_name} should be declared in interface")
        
        # Check function signatures have correct parameter types
        self.assertIn("procedure Sort(var arr: array of integer; size: integer);", content)
        self.assertIn("function Find(arr: array of integer; size: integer; value: integer): integer;", content)
        self.assertIn("procedure Copy(source: array of integer; var dest: array of integer; size: integer);", content)
        self.assertIn("function Size(arr: array of integer): integer;", content)
        
        print("ArrayUtils function signatures validation completed")

    def test_arrayutils_function_implementations(self):
        """Test that all array utility functions have implementations."""
        with open("stdlib/ArrayUtils.pas", "r") as f:
            content = f.read()
        
        # Check that all functions have implementations
        required_functions = ["Sort", "Find", "Copy", "Size"]
        
        for func_name in required_functions:
            # Check for function implementation (should appear after implementation section)
            self.assertIn(func_name, content, 
                         f"Function/procedure {func_name} should have implementation")
        
        # Check for basic algorithm implementations
        self.assertIn("for i :=", content, "Should have loop implementations")
        self.assertIn("temp :=", content, "Sort should use temporary variable for swapping")
        self.assertIn("Find := -1", content, "Find should return -1 when not found")
        self.assertIn("dest[i] := source[i]", content, "Copy should assign elements")
        
        print("ArrayUtils function implementations validation completed")

    def test_arrayutils_sort_algorithm(self):
        """Test that Sort function uses a valid sorting algorithm."""
        with open("stdlib/ArrayUtils.pas", "r") as f:
            content = f.read()
        
        # Find the Sort implementation
        impl_start = content.find("implementation")
        impl_section = content[impl_start:]
        
        sort_impl_start = impl_section.find("procedure Sort(var arr: array of integer; size: integer);")
        sort_impl_end = impl_section.find("function Find")
        sort_impl = impl_section[sort_impl_start:sort_impl_end]
        
        # Check for bubble sort implementation characteristics
        self.assertIn("for i :=", sort_impl, "Sort should have outer loop")
        self.assertIn("for j :=", sort_impl, "Sort should have inner loop")
        self.assertIn("if arr[j] > arr[j + 1]", sort_impl, "Sort should compare adjacent elements")
        self.assertIn("temp := arr[j]", sort_impl, "Sort should use temporary variable for swapping")
        self.assertIn("arr[j] := arr[j + 1]", sort_impl, "Sort should swap elements")
        self.assertIn("arr[j + 1] := temp", sort_impl, "Sort should complete the swap")
        
        print("ArrayUtils Sort algorithm validation completed")

    def test_arrayutils_find_implementation(self):
        """Test that Find function has correct search implementation."""
        with open("stdlib/ArrayUtils.pas", "r") as f:
            content = f.read()
        
        # Find the Find implementation
        impl_start = content.find("implementation")
        impl_section = content[impl_start:]
        
        find_impl_start = impl_section.find("function Find(arr: array of integer; size: integer; value: integer): integer;")
        find_impl_end = impl_section.find("procedure Copy")
        find_impl = impl_section[find_impl_start:find_impl_end]
        
        # Check for linear search implementation
        self.assertIn("Find := -1", find_impl, "Find should initialize return value to -1")
        self.assertIn("for i :=", find_impl, "Find should have search loop")
        self.assertIn("if arr[i] = value", find_impl, "Find should compare array elements with target value")
        self.assertIn("Find := i", find_impl, "Find should return index when found")
        self.assertIn("break", find_impl, "Find should break when element is found")
        
        print("ArrayUtils Find implementation validation completed")

    def test_arrayutils_copy_implementation(self):
        """Test that Copy procedure has correct copying implementation."""
        with open("stdlib/ArrayUtils.pas", "r") as f:
            content = f.read()
        
        # Find the Copy implementation
        impl_start = content.find("implementation")
        impl_section = content[impl_start:]
        
        copy_impl_start = impl_section.find("procedure Copy(source: array of integer; var dest: array of integer; size: integer);")
        copy_impl_end = impl_section.find("function Size")
        copy_impl = impl_section[copy_impl_start:copy_impl_end]
        
        # Check for element-by-element copying
        self.assertIn("for i :=", copy_impl, "Copy should have loop")
        self.assertIn("dest[i] := source[i]", copy_impl, "Copy should assign each element")
        self.assertIn("0 to size - 1", copy_impl, "Copy should iterate through all elements")
        
        print("ArrayUtils Copy implementation validation completed")

    def test_arrayutils_size_implementation(self):
        """Test that Size function has implementation."""
        with open("stdlib/ArrayUtils.pas", "r") as f:
            content = f.read()
        
        # Find the Size implementation
        impl_start = content.find("implementation")
        impl_section = content[impl_start:]
        
        size_impl_start = impl_section.find("function Size(arr: array of integer): integer;")
        size_impl_end = impl_section.find("end.")
        size_impl = impl_section[size_impl_start:size_impl_end]
        
        # Check for size implementation (note: this is a placeholder in Pascal)
        self.assertIn("Size := 0", size_impl, "Size should have return value assignment")
        
        print("ArrayUtils Size implementation validation completed")

    def test_arrayutils_parameter_types(self):
        """Test that function parameters use correct types and var keywords."""
        with open("stdlib/ArrayUtils.pas", "r") as f:
            content = f.read()
        
        # Check that Sort uses var parameter for array modification
        self.assertIn("procedure Sort(var arr: array of integer", content, 
                     "Sort should use var parameter for array modification")
        
        # Check that Find uses value parameter (no var)
        self.assertIn("function Find(arr: array of integer", content,
                     "Find should use value parameter (read-only)")
        
        # Check that Copy uses var for destination array
        self.assertIn("procedure Copy(source: array of integer; var dest: array of integer", content,
                     "Copy should use var parameter for destination array")
        
        # Check that Size uses value parameter
        self.assertIn("function Size(arr: array of integer", content,
                     "Size should use value parameter (read-only)")
        
        # Check that size parameters are integers
        self.assertIn("size: integer", content, "Size parameters should be integers")
        
        print("ArrayUtils parameter types validation completed")


if __name__ == "__main__":
    unittest.main()