"""
Test integration with real module files like TestModule.pas
"""

import unittest
from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter


class TestRealModuleIntegration(unittest.TestCase):
    """Test interpreter integration with real module files."""

    def test_load_test_module(self):
        """Test loading the existing TestModule.pas from stdlib."""
        program_text = """
        program TestProgram;
        var
            result: integer;
        begin
            result := 0;
        end.
        """
        
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()
        
        # Set uses clause to load TestModule
        tree.uses_clause = ["TestModule"]
        
        interpreter = Interpreter(tree)
        
        # Should load TestModule without errors
        try:
            interpreter.visit_Program(tree)
            
            # Check that TestModule was loaded
            self.assertIn("TestModule", interpreter.module_registry.loaded_modules)
            module = interpreter.module_registry.get_module("TestModule")
            self.assertIsNotNone(module)
            self.assertTrue(module.is_loaded)
            
            print("Successfully loaded TestModule.pas")
            
        except Exception as e:
            print(f"Error loading TestModule: {e}")
            # For now, we expect this might fail due to incomplete unit parsing
            # This test documents the current state and will pass when unit parsing is complete
            raise


if __name__ == "__main__":
    unittest.main()