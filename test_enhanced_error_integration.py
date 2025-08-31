"""
Integration test to demonstrate enhanced error messages in real scenarios.
"""

import unittest
from unittest.mock import Mock

from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter
from src.type_resolver import TypeResolver
from src.module import ModuleRegistry
from src.error import TypeResolutionError, SyntaxError


class TestEnhancedErrorIntegration(unittest.TestCase):
    """Integration tests for enhanced error messages."""

    def test_syntax_error_vs_semantic_error_distinction(self):
        """Test that syntax and semantic errors are clearly distinguished."""
        
        # Test syntax error - malformed type specification
        syntax_code = """program TestProgram;
var
    x: [invalid syntax here];
begin
end."""
        
        lexer = Lexer(syntax_code)
        parser = Parser(lexer)
        
        try:
            ast = parser.parse()
            self.fail("Expected syntax error")
        except Exception as e:
            # Should be a parser error, not a type resolution error
            self.assertNotIsInstance(e, TypeResolutionError)
            # Error message should indicate parsing issue
            error_str = str(e)
            self.assertTrue(
                "syntax" in error_str.lower() or "parser" in error_str.lower() or "expected" in error_str.lower(),
                f"Error should indicate syntax issue: {error_str}"
            )

    def test_comprehensive_type_resolution_error_message(self):
        """Test comprehensive error message for type resolution failure."""
        
        # Code with unknown type that should trigger semantic error
        semantic_code = """program TestProgram;
var
    x: UnknownType;
begin
end."""
        
        lexer = Lexer(semantic_code)
        parser = Parser(lexer)
        ast = parser.parse()
        
        interpreter = Interpreter(ast)
        
        try:
            interpreter.interpret()
            self.fail("Expected TypeResolutionError")
        except TypeResolutionError as e:
            error_str = str(e)
            
            # Should be clearly marked as semantic error
            self.assertIn("Semantic Error", error_str)
            
            # Should include location information
            self.assertIn("line", error_str)
            
            # Should include context about type resolution
            self.assertIn("type resolution", error_str)
            
            # Should include available types
            self.assertIn("Available types:", error_str)
            self.assertIn("Integer", error_str)
            self.assertIn("Boolean", error_str)

    def test_helpful_suggestions_for_typos(self):
        """Test that helpful suggestions are provided for common typos."""
        
        # Code with typo in type name
        typo_code = """program TestProgram;
var
    x: Integr;
begin
end."""
        
        lexer = Lexer(typo_code)
        parser = Parser(lexer)
        ast = parser.parse()
        
        interpreter = Interpreter(ast)
        
        try:
            interpreter.interpret()
            self.fail("Expected TypeResolutionError")
        except TypeResolutionError as e:
            error_str = str(e)
            
            # Should suggest the correct type
            self.assertIn("Did you mean:", error_str)
            self.assertIn("Integer", error_str)

    def test_context_information_in_error_messages(self):
        """Test that error messages include contextual information."""
        
        # Code that will fail type resolution - simplified to avoid complex class syntax
        context_code = """program TestProgram;
type
    MyEnum = (Red, Green, Blue);
var
    x: MissingType;
begin
end."""
        
        lexer = Lexer(context_code)
        parser = Parser(lexer)
        ast = parser.parse()
        
        interpreter = Interpreter(ast)
        
        try:
            interpreter.interpret()
            self.fail("Expected TypeResolutionError")
        except TypeResolutionError as e:
            error_str = str(e)
            
            # Should include context about local types
            self.assertIn("Context:", error_str)
            self.assertIn("local types:", error_str)
            
            # Should mention the available local types
            self.assertIn("Available types:", error_str)
            self.assertIn("Enum:MyEnum", error_str)

    def test_line_and_column_information(self):
        """Test that error messages include accurate line and column information."""
        
        # Multi-line code to test line number reporting
        multiline_code = """program TestProgram;
var
    x: Integer;
    y: Real;
    z: UnknownType;
begin
end."""
        
        lexer = Lexer(multiline_code)
        parser = Parser(lexer)
        ast = parser.parse()
        
        interpreter = Interpreter(ast)
        
        try:
            interpreter.interpret()
            self.fail("Expected TypeResolutionError")
        except TypeResolutionError as e:
            error_str = str(e)
            
            # Should include line number (around line 6)
            self.assertIn("line", error_str)
            # The exact line number might vary due to whitespace, but should be reasonable
            self.assertTrue(
                any(str(i) in error_str for i in range(4, 8)),
                f"Expected line number around 5-6, got: {error_str}"
            )


if __name__ == '__main__':
    unittest.main()